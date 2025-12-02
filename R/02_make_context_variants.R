#!/usr/bin/env Rscript
# Create context-size variants from base sentence-level pairs
# Generates 4 variants: sentence-only, ±1 sentence, ±2 sentences, full abstract

suppressPackageStartupMessages({
  library(data.table)
  library(textpress)
  library(tokenizers)
})

# Source utils - try multiple paths
utils_paths <- c(
  file.path(getwd(), "R", "utils.R"),
  file.path(dirname(getwd()), "R", "utils.R"),
  "utils.R"
)

utils_loaded <- FALSE
for (path in utils_paths) {
  if (file.exists(path)) {
    source(path)
    utils_loaded <- TRUE
    break
  }
}

if (!utils_loaded) {
  stop("Could not find utils.R. Please ensure it exists in the R/ directory.")
}

# Configuration
PROJECT_ROOT <- getwd()
INPUT_FILE <- file.path(PROJECT_ROOT, "data", "processed", "lsd600_pairs_base.csv")
OUTPUT_DIR <- file.path(PROJECT_ROOT, "data", "processed")
LSD600_DIR <- file.path(PROJECT_ROOT, "LSD600")

if (!file.exists(INPUT_FILE)) {
  stop(sprintf("Input file not found: %s\nPlease run 01_build_lsd600_pairs.R first", INPUT_FILE))
}

message("Loading base pairs dataset...")
dt_base <- fread(INPUT_FILE)
# Ensure doc_id is character for consistent joins
dt_base[, doc_id := as.character(doc_id)]
message(sprintf("  Loaded %d pairs", nrow(dt_base)))

message("Loading sentence data for context extraction...")
# Read text files to get full abstracts
txt_files <- list.files(LSD600_DIR, pattern = "\\.txt$", 
                        full.names = TRUE, recursive = TRUE)

process_txt_file <- function(file_path) {
  text <- readChar(file_path, file.info(file_path)$size)
  data.frame(doc_id = gsub('.txt', '', basename(file_path)), 
             text = text, stringsAsFactors = FALSE)
}

pubmed_data <- do.call(rbind, lapply(txt_files, process_txt_file))
setDT(pubmed_data)
# Ensure doc_id is character for consistent joins
pubmed_data[, doc_id := as.character(doc_id)]

# Tokenize sentences
ss <- pubmed_data |> 
  nlp_split_paragraphs() |>
  nlp_split_sentences(text_hierarchy = c('doc_id', 'paragraph_id'))

ss1 <- ss[, {
  if (paragraph_id == 1L && .N > 1) {
    .(
      text = paste(text, collapse = " "),
      start = min(start),
      end = max(end),
      sentence_id = .I[1]
    )
  } else {
    .(text, start, end, sentence_id = .I)
  }
}, by = .(doc_id, paragraph_id)]

ss1[, sentence_id := seq_len(.N), by = doc_id]
ss1 <- ss1[, .(doc_id, sentence_id, text, start, end)]
# Ensure doc_id is character for consistent joins
ss1[, doc_id := as.character(doc_id)]

message(sprintf("  Loaded %d sentences from %d documents", 
                nrow(ss1), length(unique(ss1$doc_id))))

# Create full abstract text per document
abstracts <- pubmed_data[, .(abstract_text = text), by = doc_id]
# Ensure doc_id is character
abstracts[, doc_id := as.character(doc_id)]

#' Create context variant with specified window size
#' 
#' @param dt Base pairs data.table
#' @param ss1 Sentences data.table
#' @param abstracts Full abstract texts
#' @param window_size Number of sentences before/after (0 = sentence only, 1 = ±1, 2 = ±2, NULL = abstract)
#' @return data.table with context_text and masked_context_text columns
create_context_variant <- function(dt, ss1, abstracts, window_size = NULL) {
  dt_out <- copy(dt)
  
  if (is.null(window_size)) {
    # Full abstract context
    message("  Creating abstract-level context...")
    dt_out[abstracts, on = .(doc_id), context_text := i.abstract_text]
    
    # Entity positions are already absolute, so they work with abstract text
    # Apply mask_entities row by row using sapply
    dt_out[, masked_context_text := sapply(seq_len(.N), function(i) {
      mask_entities(
        context_text[i],
        1L,  # Abstract starts at position 1
        LSF_entity_start[i],
        LSF_entity_end[i],
        disease_entity_start[i],
        disease_entity_end[i]
      )
    })]
    
  } else if (window_size == 0) {
    # Sentence-only context
    message("  Creating sentence-only context...")
    dt_out[, context_text := text]
    dt_out[, masked_context_text := mapply(
      mask_entities,
      text,
      start,
      LSF_entity_start,
      LSF_entity_end,
      disease_entity_start,
      disease_entity_end
    )]
    
  } else {
    # Window-based context (±1 or ±2)
    message(sprintf("  Creating ±%d sentence context...", window_size))
    
    # Create helper columns for sentence IDs
    dt_out[, `:=`(
      prev_id = sentence_id - 1,
      next_id = sentence_id + 1
    )]
    
    # Join to get previous/next sentences
    dt_out[ss1, on = .(doc_id, prev_id = sentence_id), text_before := i.text]
    dt_out[ss1, on = .(doc_id, next_id = sentence_id), text_after := i.text]
    
    if (window_size >= 2) {
      dt_out[, `:=`(
        prev_prev_id = sentence_id - 2,
        next_next_id = sentence_id + 2
      )]
      dt_out[ss1, on = .(doc_id, prev_prev_id = sentence_id), text_before2 := i.text]
      dt_out[ss1, on = .(doc_id, next_next_id = sentence_id), text_after2 := i.text]
    }
    
    # Exclude titles as context
    dt_out[sentence_id == 1, text_before := ""]
    if (window_size >= 2) {
      dt_out[sentence_id == 1, text_before2 := ""]
      dt_out[prev_prev_id <= 1, text_before2 := ""]
    }
    dt_out[prev_id == 1, text_before := ""]
    
    # Concatenate context
    if (window_size == 1) {
      dt_out[, context_text := paste(
        fifelse(is.na(text_before) | text_before == "", "", text_before),
        text,
        fifelse(is.na(text_after) | text_after == "", "", text_after),
        sep = " "
      )]
    } else if (window_size == 2) {
      dt_out[, context_text := paste(
        fifelse(is.na(text_before2) | text_before2 == "", "", text_before2),
        fifelse(is.na(text_before) | text_before == "", "", text_before),
        text,
        fifelse(is.na(text_after) | text_after == "", "", text_after),
        fifelse(is.na(text_after2) | text_after2 == "", "", text_after2),
        sep = " "
      )]
    }
    
    # Compute new start position for context (start of first sentence in window)
    # We need to join with ss1 to get the actual start positions
    dt_out[, context_start := start]  # Default to current sentence start
    
    if (window_size == 1) {
      # Get start of previous sentence if it exists
      dt_out[ss1, on = .(doc_id, prev_id = sentence_id), 
             context_start := fifelse(!is.na(i.start) & !is.na(text_before) & text_before != "", 
                                      i.start, context_start)]
    } else if (window_size == 2) {
      # Try prev_prev first, then prev, then current
      dt_out[ss1, on = .(doc_id, prev_prev_id = sentence_id), 
             context_start := fifelse(!is.na(i.start) & !is.na(text_before2) & text_before2 != "", 
                                      i.start, context_start)]
      dt_out[ss1, on = .(doc_id, prev_id = sentence_id), 
             context_start := fifelse(!is.na(i.start) & !is.na(text_before) & text_before != "" & context_start == start, 
                                      i.start, context_start)]
    }
    
    # Mask entities in context (entity positions are absolute, context_start is absolute)
    dt_out[, masked_context_text := mapply(
      mask_entities,
      context_text,
      context_start,
      LSF_entity_start,
      LSF_entity_end,
      disease_entity_start,
      disease_entity_end
    )]
    
    # Clean up helper columns
    if (window_size == 1) {
      dt_out[, c("prev_id", "next_id", "text_before", "text_after", "context_start") := NULL]
    } else {
      dt_out[, c("prev_id", "next_id", "prev_prev_id", "next_next_id", 
                 "text_before", "text_after", "text_before2", "text_after2", 
                 "context_start") := NULL]
    }
  }
  
  # Add word count for context
  dt_out[, context_word_count := sapply(context_text, function(x) count_words(x)[1])]
  
  return(dt_out)
}

# Create all variants
message("\nCreating context variants...")

# 1. Sentence-only
dt_sentence <- create_context_variant(dt_base, ss1, abstracts, window_size = 0)
fwrite(dt_sentence, file.path(OUTPUT_DIR, "lsd600_sentence.csv"), row.names = FALSE, quote = TRUE)
message(sprintf("  ✓ Written: lsd600_sentence.csv (%d rows)", nrow(dt_sentence)))

# 2. ±1 sentence
dt_plus1 <- create_context_variant(dt_base, ss1, abstracts, window_size = 1)
fwrite(dt_plus1, file.path(OUTPUT_DIR, "lsd600_sent_plus1.csv"), row.names = FALSE, quote = TRUE)
message(sprintf("  ✓ Written: lsd600_sent_plus1.csv (%d rows)", nrow(dt_plus1)))

# 3. ±2 sentences
dt_plus2 <- create_context_variant(dt_base, ss1, abstracts, window_size = 2)
fwrite(dt_plus2, file.path(OUTPUT_DIR, "lsd600_sent_plus2.csv"), row.names = FALSE, quote = TRUE)
message(sprintf("  ✓ Written: lsd600_sent_plus2.csv (%d rows)", nrow(dt_plus2)))

# 4. Full abstract
dt_abstract <- create_context_variant(dt_base, ss1, abstracts, window_size = NULL)
# Replace newlines in context_text to avoid CSV parsing issues
dt_abstract[, context_text := gsub("\n", " ", context_text)]
dt_abstract[, context_text := gsub("\r", " ", context_text)]
dt_abstract[, masked_context_text := gsub("\n", " ", masked_context_text)]
dt_abstract[, masked_context_text := gsub("\r", " ", masked_context_text)]
fwrite(dt_abstract, file.path(OUTPUT_DIR, "lsd600_abstract.csv"), row.names = FALSE, quote = TRUE)
message(sprintf("  ✓ Written: lsd600_abstract.csv (%d rows)", nrow(dt_abstract)))

message("\n✓ All context variants created successfully!")
message(sprintf("\nOutput directory: %s", OUTPUT_DIR))
message("\nFiles created:")
message("  - lsd600_sentence.csv (sentence-only context)")
message("  - lsd600_sent_plus1.csv (±1 sentence window)")
message("  - lsd600_sent_plus2.csv (±2 sentence window)")
message("  - lsd600_abstract.csv (full abstract context)")

