
#----------------------------------------------#
# File:    search_functions.R 
# Author: Eduard Brüll
# Date creation: 2024-05-17 10:07:29 CEST
# ~: Cotains functions to search candidates in 
#   target data or duplciates in base data
#----------------------------------------------#

#' Search for Matching Candidates
#'
#' This function searches for matching candidates between a base table and a target table using
#' token-based heuristic linkage. It calculates the identification potential for each pair of records
#' based on the rarity of shared tokens and specified weights.
#'
#' @param .base_table A data table containing the base records, with tokens and columns.
#' @param .target_table A data table containing the target records, with tokens and columns.
#' @param .base_key A character string specifying the key column in the base table.
#' @param .target_key A character string specifying the key column in the target table.
#' @param .threshold A numeric value specifying the minimum identification potential required to consider a match.
#' @param .weights A named numeric vector specifying the weights for each column. If NULL, equal weights are used.
#' @param .chunksize An integer specifying the number of target records to process in each chunk. Set to 0 for no chunking.
#'
#' @return A data.table containing the pairs of matched records with their identification potential.
#' @examples
#' candidates <- search_candidates(
#'   .base_table = search_table_base,
#'   .target_table = search_table_target,
#'   .base_key = "key_base",
#'   .target_key = "key_target",
#'   .threshold = 0.6,
#'   .weights = c(Hausnummer = 0.1, Nachname = 0.5, Vorname = 0.2, Strasse = 0.1, Ort = 0.1),
#'   .chunksize = 10000
#' )
#' @export
search_candidates <- function(.base_table, .target_table, .base_key, .target_key, .threshold, .weights = NULL, .chunksize = 0) {
  # Use equal weights if weights aren't set
  if (is.null(.weights)) {
    n_columns <- length(unique(.base_table$column))
    .weights <- rep(1 / n_columns, n_columns)
    names(.weights) <- unique(.base_table$column)
  }
  
  dictionary <- .base_table[, .N, by = c("column", "tokens")][, rarity := 1 / N][order(-N)]
  base_tokens <- .base_table[dictionary, on = c("column", "tokens"), nomatch = 0][
    , rIP := rarity / sum(rarity), by = c(.base_key, "column")]
  
  # Get unique target keys and split into chunks
  unique_keys <- unique(.target_table[[.target_key]])
  if (.chunksize > 0) {
    chunk_indices <- split(unique_keys, ceiling(seq_along(unique_keys) / .chunksize))
  } else {
    chunk_indices <- list(unique_keys)
  }
  num_chunks <- length(chunk_indices)
  
  
  result_list <- lapply(seq_along(chunk_indices), function(i) {
    keys <- chunk_indices[[i]]
    target_chunk <- .target_table[.target_table[[.target_key]] %in% keys]
    join_results <- base_tokens[target_chunk, on = c("column", "tokens"), nomatch = 0, allow.cartesian = TRUE]
    
    # Build the match table
    match_table <- join_results[
      , .(identification_potential = sum(rIP, na.rm = TRUE)), by = c("column", .base_key, .target_key)][
        , weight := .weights[column]][
          , .(identification_potential = sum(identification_potential * weight, na.rm = TRUE)), by = c(.base_key, .target_key)][
            identification_potential >= .threshold][]
    
    # Update progress bar and print the current chunk
    cat(sprintf("Processed chunk %d/%d\n", i, num_chunks))
    
    return(match_table)
  })
  
  # Combine all results
  final_results <- rbindlist(result_list)
  return(final_results)
}

#' Detect Duplicate Records
#'
#' This function detects duplicate records within a base table using token-based heuristic linkage.
#' It calculates the identification potential for each pair of records based on the rarity of shared tokens and specified weights.
#'
#' @param .base_table A data table containing the base records, with tokens and columns.
#' @param .base_key A character string specifying the key column in the base table.
#' @param .threshold A numeric value specifying the minimum identification potential required to consider a match.
#' @param .weights A named numeric vector specifying the weights for each column. If NULL, equal weights are used.
#'
#' @return A data.table containing the groups of duplicate records with their identification potential.
#' @examples
#' likely_duplicates <- detect_duplicates(
#'   .base_table = search_table_base,
#'   .base_key = "key_base",
#'   .threshold = 0.8
#' )
#' @export
detect_duplicates <- function(.base_table,.base_key, .threshold, .weights = NULL) {
  
  # Use equal weights if weights aren't set
  if (is.null(.weights)) {
    n_columns <- length(unique(.base_table$column))
    .weights <- rep(1 / n_columns, n_columns)
    names(.weights) <- unique(.base_table$column)
  }
  
  dictionary <- .base_table[, .N, by = c("column", "tokens")][, rarity := 1 / N][order(-N)]
  base_tokens <- .base_table[dictionary, on = c("column", "tokens"), nomatch = 0][
    , rIP := rarity / sum(rarity), by = c(.base_key, "column")]
  
  
  join_results <- base_tokens[base_tokens[,.(key_dup=key_base,tokens,column)], on = c("column", "tokens"), nomatch = 0, allow.cartesian = TRUE]
  
  # Build the match table
  match_table <- join_results[
    , .(identification_potential = sum(rIP, na.rm = TRUE)), by = c("column", .base_key, "key_dup")][
      , weight := .weights[column]][
        , .(identification_potential = sum(identification_potential * weight, na.rm = TRUE)), by = c(.base_key, "key_dup")][
          identification_potential >= .threshold]
  
  # Filter out self-matches
  match_table <- match_table[e_base_key != key_dup,env=list(e_base_key = .base_key)]
  
  # Create an edge list
  edges <- match_table[, .(e_base_key, key_dup),,env=list(e_base_key = .base_key)]
  
  # Create a graph from the edge list
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  
  # Find all connected components
  components <- igraph::components(g)
  
  # Create a data.table with the results
  duplicate_groups <- data.table(
    component = components$membership,
    target   = names(components$membership)
  )
  names(duplicate_groups) <- c("duplicates_group",.base_key)
  
  return(duplicate_groups)
}


#' Deduplicate Records
#'
#' This function takes a data table and a list of likely duplicates and removes duplicates, keeping only the first occurrence.
#'
#' @param .table A data table containing the records to be deduplicated.
#' @param .duplicates A data table containing the list of likely duplicates.
#' @param .key A character string specifying the key column in the data table.
#'
#' @return A data table with duplicates removed.
#' @examples
#' deduplicated_table <- deduplicate(base_table, likely_duplicates, "key_base")
deduplicate_table <- function(.table, .duplicates, .key) {
  non_first_in_list_of_duplicates <- .duplicates[, n := 1:.N, by = duplicates_group][n != 1][]
  deduplicated_table <- .table[!(key %in% non_first_in_list_of_duplicates[[.key]]),env=list(key=.key)]
  return(deduplicated_table)
}


