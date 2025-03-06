
#----------------------------------------------#
# File:    search_preparers.R 
# Author: Eduard Brüll
# Date creation: 2024-05-17 10:07:29 CEST
# ~: Search preparer functions for the MatchMakeR package
#----------------------------------------------#


#' Normalize text string
#'
#' This function converts a text string to upper case, transliterates it based on the specified
#' transliteration scheme, retains only alphanumeric characters and spaces, and removes extra spaces.
#'
#' @param .text A character string or vector to be normalized.
#' @param .transliteration A character string specifying the transliteration scheme to be used,
#'        defaulting to "De-ASCII".
#'
#' @return Returns a normalized, upper-case version of the input text, with non-alphanumeric characters
#'         and extra spaces removed.
#'
#' @examples
#' normalize_text("Café Coñac")
#' normalize_text("Straße", .transliteration = "Latin-ASCII")
#' @export
#' @import stringi
normalize_text <- function(.text, .transliteration = "De-ASCII") {
  #Validate inputes to the generate_ngrams function
  c("Input .text must be a string" = is.character(.text),
    "Input .transliteration must be a string" = is.character(.transliteration)
  ) |>
    validate_inputs()
  
  # String to upper case characters
  .text <- stri_trans_toupper(.text)
  # Transliterate other language specific characters
  .text <- stri_trans_general(.text, .transliteration)
  # Keep only alphanumeric characters and spaces
  .text <- stri_replace_all_regex(.text, "[^A-Za-z0-9 ]", "")
  # Remove additional spaces if they exist
  .text <- stri_trim_both(.text)
  .text <- stri_replace_all_regex(.text, "\\s+", " ")
  return(.text)
}

#' Convert Text to Metaphone Encoding
#'
#' This function converts a text string to its Metaphone encoding. The Metaphone algorithm is used to
#' encode words phonetically by reducing them to a simplified representation based on their pronunciation.
#'
#' @param .text A character string or vector to be converted to Metaphone encoding.
#'
#' @return Returns the Metaphone encoded version of the input text.
#' @examples
#' as_metaphone("Café")
#' as_metaphone("Straße")
#' @export
#' @import phonics
as_metaphone <- function(.text){
  # Validate inputs to the function
  c("Input .text must be a string" = is.character(.text)
  ) |>
    validate_inputs()
  
  .text <- phonics::metaphone(.text)
  return(.text)
}

#' Convert Text to Soundex Encoding
#'
#' This function converts a text string to its Soundex encoding. The Soundex algorithm is used to
#' encode words phonetically by reducing them to a simplified representation based on their pronunciation.
#'
#' @param .text A character string or vector to be converted to Soundex encoding.
#'
#' @return Returns the Soundex encoded version of the input text.
#' @examples
#' as_soundex("Café")
#' as_soundex("Straße")
#' @export
#' @import phonics
as_soundex <- function(.text){
  # Validate inputs to the function
  c("Input .text must be a string" = is.character(.text)
  ) |>
    validate_inputs()
  
  .text <- phonics::soundex(.text)
  return(.text)
}


#' Return a list of word tokens for the .text separated by spaces.
#'
#' This function splits the input text into words based on spaces. It returns a vector of the words
#' found in the text. This function is useful for natural language processing tasks where word-level
#' manipulation of text is required.
#'
#' @param .text A character string from which words will be extracted.
#'
#' @return Returns a vector of words extracted from the input text.
#'
#' @examples
#' word_tokens("This is an example.")
#' word_tokens("Another, test; string.")
#' @export
word_tokens <- function(.text,.min_length=0){
  # Validate inputs to the function
  c("Input .text must be a string" = is.character(.text),
    "Input .min_length must be a integer valued numeric" = is_integer_valued(.min_length)
  ) |>
    validate_inputs()
  
  # Split the text into words based on spaces
  words <- strsplit(.text, "\\s+")
  # Remove empty elements if any (this can happen with multiple spaces)
  words <- words[nzchar(words)]
  
  # Filter out words shorter than .min_length
  if (.min_length > 0) {
    words <- lapply(words,function(x){
        filter <- nchar(x)>=.min_length
        x[filter]
      })
  }
  return(words)
}


#' Generate n-grams from text
#'
#' This function generates n-grams from a given text string. An n-gram is a contiguous sequence of n items
#' from a given sample of text or speech. This function will return a list of all possible n-grams of length n.
#'
#' @param .text A character string or vector from which to generate n-grams.
#' @param .n An integer specifying the length of each n-gram.
#'
#' @return Returns a list of n-grams generated from the input text. If the text length is less than n, returns
#'         an empty character vector.
#'
#' @examples
#' generate_ngrams("hello", 2)
#' generate_ngrams("an example", 3)
#' @export
#' @import data.table
#' @import stringi
generate_ngrams <- function(.text,.n) {
  #Validate inputes to the generate_ngrams function
  c(
    "Input .text must be a string" = is.character(.text),
    "Input .n must be an integer valued numeric" = is_integer_valued(.n)
  ) |>
    validate_inputs()
  int_df <- data.table::data.table(text = .text) 
  int_df[,len_text := stri_length(.text)]
  
  # Function to generate n-grams for each string
  generate_ngrams_single <- function(s, n) {
    len_s <- stri_length(s)
    if (len_s >= n) {
      # Generate all n-grams by sliding over the string
      sapply(1:(len_s - n + 1), function(i) stri_sub(s, i, i + n - 1))
    } else {
      # Return empty character vector if n-grams can't be generated
      character(0)
    }
  }
  
  # Apply the n-gram generation function to each row
  int_df[, ngrams := lapply(text, generate_ngrams_single, n = .n)]
  return(int_df$ngrams)
}

#' Use similarity dictionary to group similar tokens together
#'
#' This function looks up a token in the similarity dictionary and returns the corresponding token group for a token.
#'
#' @param .text A character string or vector representing the token to be looked up.
#' @param .dict A data table containing the similarity dictionary with tokens and their respective groups.
#'
#' @return Returns the token group corresponding to the input token.
#'
#' @examples
#' dict <- data.table(tokens = c("example", "sample"), token_group = c("example/sample", "example/sample"))
#' use_dictionary("example", dict)
#' use_dictionary("nonexistent", dict)
#' @export
use_dictionary <- function(.text, .dict) {
  # Validate inputs to the function
  c("Input .dict must be a data.table" = data.table::is.data.table(.dict)) |>
    validate_inputs()
  
  lookup_row <- function(.r){
    .dict[tokens %in% .r]$token_group
  }
  
  lapply(.text,lookup_row)
}


# A small helper function to parse the formulas for the search_preparers
#' Parse Formula for Search Preparers
#'
#' This internal function parses a formula and converts it into a function that can be used
#' for chaining multiple text processing steps.
#'
#' @param fml A formula specifying the text processing steps.
#'
#' @return A function that applies the specified text processing steps in sequence.
#'
#' @examples
#' parse_formula(Nachname ~ normalize_text + word_tokens)
#' @keywords internal
#' @noRd
parse_formula <- function(fml) {
  # Convert formula to character and remove spaces
  formula_str <- gsub(" ", "", deparse(fml))
  
  # Split formula into LHS and RHS
  parts <- strsplit(formula_str, "~")[[1]]
  lhs <- parts[1]
  rhs <- parts[2]
  
  # Split RHS into individual function calls
  functions <- strsplit(rhs, "\\+")[[1]]
  
  # Function to parse the arguments of a function call
  parse_args <- function(arg_str) {
    if (arg_str == "") return(list())
    args <- strsplit(arg_str, ",")[[1]]
    args_list <- lapply(args, function(arg) {
      if (grepl("=", arg)) {
        kv <- strsplit(arg, "=")[[1]]
        setNames(list(eval(parse(text=kv[2]))), kv[1])
      } else {
        list(eval(parse(text=arg)))
      }
    })
    return(do.call(c, args_list))
  }
  
  # Function to parse each function call in the RHS
  parse_function <- function(func_str) {
    func_name <- sub("\\(.*\\)$", "", func_str)
    args_str <- sub(func_name,"",func_str)
    args_str <- sub("^[^\\(]*\\(", "", args_str) 
    args_str <- sub("\\)$", "", args_str)
    args <- parse_args(args_str)
    return(list(f = func_name, args = args))
  }
  
  #Return a single input function for chaining 
  encapsulate_function <- function(func_call){
    out_fun <- function(x){
      added_args <- c(list(.text=x),func_call$args)
      do.call(func_call$f, added_args)
    }
    return(out_fun)
  }
  
  #Chain functions via Reduce
  chain_functions <- function(fn_list) {
    # Return a new function that applies the function chain
    function(x) {
      Reduce(function(value, f) f(value), fn_list, init = x)
    }
  }
  
  #Parse and encapsulate to single input functions
  preparer <- lapply(functions, parse_function) |>
    lapply(encapsulate_function) |>
    chain_functions()
  
  return(preparer)
}

#' Create a list of preparer functions based on a formula syntax.
#'
#' This function accepts a formula where the left-hand side specifies the column name,
#' and the right-hand side specifies the sequence of functions to be applied.
#'
#' @param ... Formula(s) specifying the preparers.
#'
#' @return A list of preparer functions for each specified column.
#'
#' @examples
#' search_preparers(
#'   Nachname ~ normalize_text + word_tokens,
#'   Vorname ~ normalize_text + generate_ngrams(3)
#' )
#' @export
search_preparers <- function(...) {
  preparer_formulas <- list(...)
  
  preparers <- lapply(preparer_formulas,parse_formula)
  names(preparers) <- sapply(preparer_formulas, function(fml){
    # Convert formula to character and remove spaces
    formula_str <- gsub(" ", "", deparse(fml))
    
    # Split formula into LHS and RHS
    parts <- strsplit(formula_str, "~")[[1]]
    lhs <- parts[1]
    return(lhs)
  })
  return(preparers)
}

#' Prepare Search Data
#'
#' This function applies the preparer functions to the specified columns in the input data frame to create a search table.
#'
#' @param .preparers A list of preparer functions created by `search_preparers`.
#' @param .df A data frame containing the data to be prepared for search.
#' @param .key A character string specifying the key column in the data frame.
#'
#' @return A data.table with prepared search data, including tokens and token counts for each specified column.
#' @examples
#' search_table_base <- preapare_search_data(prepaers, base_table, "key_base")
#' @export
preapare_search_data <- function (.preparers, .df, .key) {
  columns <- names(.preparers)
  out_df <- data.table()
  for (.c in columns) {
    df <- copy(.df)
    df <- df[, `:=`(tokens, lapply(.SD, .preparers[[.c]])), 
             .SDcols = eval(.c)]
    df <- df[, .(tokens = unlist(tokens)), by = eval((.key))]
    df <- df[, `:=`(n_tokens, .N), by = eval((.key))]
    df <- df[, `:=`(column, .c)]
    out_df <- rbind(out_df, df)
  }
  out_df <- unique(out_df)
  return(out_df)
}



