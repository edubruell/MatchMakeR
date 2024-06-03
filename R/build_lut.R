#' Build a similarity dictionary for tokens.
#'
#' This function creates a similarity dictionary by comparing tokens from two datasets based on a specified method
#' and threshold. It returns a data table with tokens and their respective groups based on similarity.
#'
#' @param .fml A formula defining the preprocessing steps to be applied to the tokens.
#' @param .base A data table containing the base dataset.
#' @param .target A data table containing the target dataset.
#' @param .method A string indicating the method to be used for computing string distance (e.g., "jw" for Jaro-Winkler).
#' @param .threshold A numeric value indicating the maximum allowable distance for two tokens to be considered similar.
#'
#' @return Returns a data table with tokens and their corresponding similarity groups.
#'
#' @examples
#' build_similarity_dict(
#'   .fml = Nachname ~ normalize_text + word_tokens(.min_length = 3),
#'   .base = base_table,
#'   .target = target_table,
#'   .method = "jw",
#'   .threshold = 0.05
#' )
#' @export
build_similarity_dict <- function(.fml, 
                                  .base, 
                                  .target, 
                                  .method, 
                                  .threshold) {
  # Validate inputs to the function
  c("Input .fml must be a formula" = inherits(.fml, "formula"),
    "Input .base must be a data.table" = data.table::is.data.table(.base),
    "Input .target must be a data.table" = data.table::is.data.table(.target),
    "Input .method must be a string" = is.character(.method),
    "Input .threshold must be a numeric value" = is.numeric(.threshold)
  ) |>
    validate_inputs()
  
  # Prepare the formula
  prepared_fml <- search_preparers(.fml)
  var_name <- names(prepared_fml)
  fml_function <-  prepared_fml[[var_name]]
  
  # Apply the preprocessing function to the base and target datasets
  input1 <-  fml_function(.base[[var_name]])
  input2 <-  fml_function(.target[[var_name]])
  
  # Combine and get unique tokens
  token_list <- unlist(c(input1,input2)) |> 
    unique() 
  
  # Create comparison data table
  comaprison_dt <- CJ(t1 =   seq_along(token_list), t2 = seq_along(token_list))
  comaprison_dt <-  comaprison_dt[t1 < t2]
  comaprison_dt <-  comaprison_dt[,`:=`(t1=token_list[t1],t2=token_list[t2])]
  comaprison_dt <-  comaprison_dt[,dist :=stringdist::stringdist(t1,t2,method = .method)] 
  
  # Filter based on threshold
  adj_list <- comaprison_dt[dist<=.threshold][, .(t1, t2)]
  
  # Convert adjacency list to an igraph object
  graph <- igraph::graph_from_data_frame(adj_list, directed = FALSE)
  groups <- igraph::components(graph)
  
  # Create similarity groups
  similarity_groups <- data.table(
    group = groups$membership,
    tokens   = names(groups$membership)
  )
  
  # Organize and name groups
  similarity_groups <- similarity_groups[order(group)]
  group_names <- similarity_groups[, .(token_group = paste(tokens, collapse = "/")), by = group]
  similarity_groups <- similarity_groups[group_names,on="group"][,group:=NULL]
  
  # Add non-group tokens 
  non_group_tokens <- data.table(tokens = token_list[!(token_list %in% similarity_groups$tokens)],
             token_group = token_list[!(token_list %in% similarity_groups$tokens)])
             
  rbind(non_group_tokens,similarity_groups)
}


#nachname_LUT <- build_similarity_dict(
#  .fml = Nachname ~ normalize_text + word_tokens(.min_length = 3),
#  .base     = base_table,
#  .target   = target_table,
#  .method   = "jw",
#  .threshold = 0.05)
