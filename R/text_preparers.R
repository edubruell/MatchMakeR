

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
word_tokens <- function(.text){
  # Validate input to ensure it is a string
  if (!is.character(.text)) {
    stop("Input .text must be a string")
  }
  
  # Split the text into words based on spaces
  words <- strsplit(.text, "\\s+")
  # Remove empty elements if any (this can happen with multiple spaces)
  words <- words[nzchar(words)]
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


search_preparers <- function(...) {
  prepare_functions <- list(...)
  
  # Define a function that chains your list of functions
  chain_functions <- function(fn_list) {
    # Return a new function that applies the function chain
    function(x) {
      Reduce(function(value, f) f(value), fn_list, init = x)
    }
  }
  
  lapply(prepare_functions,chain_functions)
}

#Apply the preparer strategies to the columns
preapare_search_data <- function(.preparers,
                                 .df,
                                 .key){

  #Get the names of columns to add to the prepare directives
  columns <- names(.preparers)
  out_df <- data.table()

  for (.c in columns) {
    df <- .df[,tokens:= lapply(.SD,t_a[[.c]]), .SDcols = (.c)]
    df <- df[,.(tokens = unlist(tokens)), by = eval((.key))] 
    df <- df[,n_tokens:=.N, by = eval((.key))] 
    df <- df[,column := .c] 
    #print(df)
    out_df <- rbind(out_df,df)
  } 
  return(out_df)
}

base_table   <- copy(yp_base[year==2016 & entry_line==1][,key_base:=entry])
target_table <- copy(yp_base[year==2017 & entry_line==1][,key_target:=entry])

# Define normalization and tokenization strategy
#We only use standard normalization and word tokenizers
t_a <- search_preparers(
    Nachname = list(normalize_text, word_tokens),  
    Vorname  = list(normalize_text, word_tokens),
    Strasse  = list(normalize_text, word_tokens),
    Hausnummer = list(normalize_text, word_tokens),
    Ort = list(normalize_text, word_tokens)
  )


#Apply the text preparers
search_table_base <- preapare_search_data(t_a,base_table,"key_base")
column_weights <- c(Hausnummer=0.1,Nachname=0.5,Vorname=0.2,Strasse=0.1,Ort=0.1) 
dictionary <- search_table_base[,.N,by=c("column","tokens")][,rarity:= 1/N][order(-N)]
search_table_target <- preapare_search_data(t_a,target_table,"key_target")
threshold <- 0.6

base_tokens <- search_table_base[dictionary, on=c("column","tokens"), nomatch = 0][,rIP:=rarity/sum(rarity),by=c("key_base","column")][order(key_base,column,rIP)]


# Join tokens in the target table efficiently
join_results <- base_tokens[search_table_target, on=c("column", "tokens"), nomatch = 0, allow.cartesian = TRUE]

match_table <- join_results[,.(identification_potential = sum(rIP, na.rm = TRUE)),by=c("column","key_base","key_target")][
    order(key_base,key_target,column)][
      ,weight:=column_weights[column]
    ][,.(identification_potential = sum(identification_potential*weight, na.rm = TRUE)),by=c("key_base","key_target")][
      identification_potential>=0.6][]

match_table[base_table[,.(key_base,bVorname = Vorname,bNachname = Nachname,bStrasse = Strasse,bHausnummer=Hausnummer,bOrt=Ort)], on="key_base"][
  target_table[,.(key_target,tVorname = Vorname,tNachname = Nachname,tStrasse = Strasse,tHausnummer=Hausnummer,tOrt=Ort)], on="key_target"][order(key_base,identification_potential)] |> View()


base_table[key_base==2037]
base_table[key_base%in%c(2037,2463)]


|> View()

#A simple candidate retrieval function for one target key.
retrieve_candidates <- function(.candidate_key,.threshold){
  glue::glue("Searching for canidate key {.candidate_key}") |> cat("\n")
  base_tokens <- search_table_base[key_base==.candidate_key][dictionary, on=c("column","tokens"), nomatch = 0][,rIP:=rarity/sum(rarity),by="column"][order(column,rIP)]
    
  search_results <- base_tokens[search_table_target, on=c("column","tokens"), nomatch = 0][,
      .(identification_potential = sum(rIP, na.rm = TRUE)),by=c("column","key_target")][
        order(key_target,column)][
          ,weight:=column_weights[column]
          ][,.(identification_potential = sum(identification_potential*weight, na.rm = TRUE)),by=key_target][
            identification_potential>=.threshold][
              ,.(base_key=.candidate_key,key_target,identification_potential)]

  return(search_results)
}

search <- rbind(lapply(unique(search_table_base$key_base),retrieve_candidates,0.7))


search_table_base[,lapply(.SD,retrieve_candidates,0.6),.SD="key_base"]


retrieve_candidates(122,0.1)


  ggplot2::ggplot(ggplot2::aes(x=sum_ip)) +
  ggplot2::geom_histogram()

search_table_target[key_target==109]
search_table_base[key_base==122]




.c <- "Nachname"
.key <- "entry"

df <- base_table[,tokens:= lapply(.SD,t_a[[.c]]), .SDcols = (.c)]
df <- df[,.(tokens = unlist(tokens)), by = eval((.key))] 


df




a <- 

a[entry==122]



apply_strategy <- function(.df, .strategy) {
  browser()
  for (field in names(.strategy)) {
    # Apply each function in the pipeline stored in strategy for the field
    .df[, (field) := lapply(.SD, function(x) {
      result <- x
      for (func in .strategy[[field]]) {
        result <- func(result)
      }
      return(result)
    }), .SDcols = field]
  }
  return(.df)
}

apply_strategy(base_table, strat)

base_table





strat$Vorname(yp100$Vorname)


a <- chain_functions(strat$Nachname)(yp100$Nachname)

a(yp100$Nachname)


# Ensure the dictionary is set for fast look-up
setkey(dictionary, transformed)


dictionary <- yp2016[,.(tokens = normalize_text(Nachname) |>
        word_tokens() |> unlist())][,.N,by=transformed][,identification_potential:= 1/N][order(-N)]

with_ip2017 <- yp2017[,.(Nachname,transformed = normalize_text(Nachname) |>
           word_tokens() |> unlist()),by = .(RowID = .I)][dictionary, on=c("transformed")][,N:=NULL]


yp2016[,.(Nachname,transformed = normalize_text(Nachname) |>
            word_tokens() |> unlist()),by = .(RowID = .I)][I==1]








yp100 |>
  build_dictonary(Nachname= ~{.x |>
                    normalize_text() |>
                    generate_ngrams(3)},
                  
                  )


yp100$Nachname |>
  normalize_text() |>
  generate_ngrams(3) 


dt <- as.data.table(yp100)
dt[, Nachname_normalized := normalize_text(Nachname)][
   , Nachname_tokens := word_tokenize(Nachname_normalized)
]

unlist(dt$Nachname_tokens)

[,
  Nachname4grams_normalized := generate_ngrams(Nachname_normalized,4)]
dt_long <- dt[, .(Nachname, Nachname_normalized, Unnested_Ngram = unlist(Nachname4grams_normalized)), by = .(RowID = .I)]
dt_long[!is.na(Unnested_Ngram), .N, by = Unnested_Ngram][order(-N)][]

int_df <- NULL
.n <- 3
int_df <- data.table::data.table(text = dt$Nachname_normalized) 
int_df[,start_positions:= lapply(.SD, function(s) {1:(stri_length(s) - .n + 1)}),.SDcols = "text"][]



int_df[, ngrams := lapply(.SD, function(s) {
  lapply(1:(stri_length(s) - .n + 1), function(i) {
    stri_sub(s, i, i + .n - 1)
  })
}), .SDcols = "text"]


3:34







  
  if (len_text < .n) {
    return(character(0))  # Return an empty character vector if n is larger than text length
  }
  # Generate the n-grams
  start_positions <- 1:(len_text - .n + 1)
  end_positions <- .n:(len_text)
  ngrams <- stri_sub(.text, from = start_positions, to = end_positions)
  return(ngrams)
}


dt[, Nachname_ngrams := generate_ngrams(Nachname_normalized,3)]

print(dt$Nachname_normalized)

generate_ngrams("EDUARD",3)