# MatchMakeR: Heursitc index-based record linkage in R

**MatchMakeR** is an experimental first step implementation of an index-based heuristic record linkage method. This package reimplements the main ideas from [Thorsten Doherr's search engine](https://github.com/ThorstenDoherr/searchengine/), originally written in FoxPro. The aim of the package is to link large-scale data by fuzzy criteria such as names and addresses using a word/token-driven heuristic. This approach provides an efficient candidate retrieval mechanism, replacing traditional blocking strategies. Reimplementing this in R makes it easier to use the approach in existing data pipelines. 

## Features

- **Efficient Candidate Retrieval:** Quickly find potential matches using a word/token-driven heuristic.
- **Flexible Text Normalization:** Standardize text data for better comparison.
- **Advanced Tokenization:** Create tokens using word and n-gram tokenizers.
- **Phonetic Encoding:** Enhance matching accuracy with Metaphone and Soundex encoding.
- **Duplicate Detection:** Identify and group likely duplicate records within a dataset.
- **Scalable Processing:** Handle large datasets with chunk processing capabilities.

## Installation


Install **MatchMakeR** directly from GitHub using devtools:

```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install TidyLLM from GitHub
devtools::install_github("edubruell/MatchMakeR")
```
## Basic Usage Example

Below is an example of how to use the MatchMakeR package for detecting duplicates and searching for matching candidates.
```R
library(MatchMakeR)

# Load base and target tables
yp_base <- readstata13::read.dta13("yellow_pages_hausaerzte.dta")) |> as.data.table()
base_table <- copy(yp_base[year == 2016 & entry_line == 1][, key_base := entry])
target_table <- copy(yp_base[year == 2017 & entry_line == 1][, key_target := entry])

# Define normalization and tokenization strategy
preparers <- search_preparers(
  Nachname ~ normalize_text + word_tokens(.min_length = 3),
  Vorname ~ normalize_text + word_tokens(.min_length = 3),
  Strasse ~ normalize_text + word_tokens,
  Hausnummer ~ normalize_text + word_tokens,
  Ort ~ normalize_text + word_tokens
)

# Prepare search data
search_table_base <- preapare_search_data(preparers, base_table, "key_base")
search_table_target <- preapare_search_data(preparers, target_table, "key_target")

# Detect duplicates within the base table
likely_duplicates <- detect_duplicates(
  .base_table = search_table_base,
  .base_key = "key_base",
  .threshold = 0.8
)

# Deduplicate the base table
deduplicated_base_table <- deduplicate_table(base_table, likely_duplicates, "key_base")

# Search for matching candidates between base and target tables (Please deduplicate and inspect before doing this)
candidates <- search_candidates(
  .base_table = search_table_base,
  .target_table = search_table_target,
  .base_key = "key_base",
  .target_key = "key_target",
  .threshold = 0.6,
  .weights = c(Hausnummer = 0.1, Nachname = 0.5, Vorname = 0.2, Strasse = 0.1, Ort = 0.1),
  .chunksize = 10000
)

```
## Functions Overview

- **`normalize_text()`**: Normalize a text string by converting to uppercase, transliterating special characters, retaining only alphanumeric characters and spaces, and removing extra spaces.
- **`as_metaphone()`**: Convert a text string to its Metaphone encoding.
- **`as_soundex()`**: Convert a text string to its Soundex encoding.
- **`word_tokens()`**: Return a list of word tokens for the input text.
- **`use_dictionary()`**: Group similar tokens together using a pre-defined dictionary.
- **`generate_ngrams()`**: Generate n-gram tokens from a text string.
- **`search_preparers()`**: Create a list of preparer functions based on a formula syntax.
- **`preapare_search_data()`**: Apply the preparer functions to the specified columns in the input data frame to create a search table
- **`search_candidates()`**: Search for matching candidates between a base table and a target table using token-based heuristic linkage.
- **`detect_duplicates()`**: Detect duplicate records within a base table using token-based heuristic linkage.
- **`deduplicate_table()`**: Use the results of `detect_duplicates` to remove duplicate records from a data table.
- **`build_similarity_dict()`**: Build a dicionary of similar tokens for a base and a target table.

## TODO
- Add code to smooth rIP in the search functions (softmax or log rIP).
- Add feedback mechanism discusses in original search engine























