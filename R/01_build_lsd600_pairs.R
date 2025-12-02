#!/usr/bin/env Rscript
# Build sentence-level LSF-disease pairs from raw LSD600 data
# This script ingests the original LSD600 annotations and creates all possible
# entity pairs within sentences, adding explicit 'no_relation' labels

suppressPackageStartupMessages({
  library(data.table)
  library(readr)
  library(tidyr)
  library(textpress)
  library(uuid)
  library(dplyr)
})

# Configuration
PROJECT_ROOT <- getwd()
LSD600_DIR <- file.path(PROJECT_ROOT, "LSD600")
METADATA_FILE <- file.path(LSD600_DIR, "Consolidated_Relations_Dataset.tsv")
OUTPUT_DIR <- file.path(PROJECT_ROOT, "data", "processed")
OUTPUT_FILE <- file.path(OUTPUT_DIR, "lsd600_pairs_base.csv")

# Create output directory if it doesn't exist
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

message("Step 1: Reading annotation files...")
# Read all .ann files
ann_files <- list.files(LSD600_DIR, pattern = "\\.ann$", 
                        full.names = TRUE, recursive = TRUE)

process_ann_file <- function(file_path) {
  suppressWarnings({
    df <- tryCatch({
      read_delim(file_path, delim = "\t", 
                 col_names = FALSE, 
                 show_col_types = FALSE, 
                 trim_ws = TRUE, 
                 progress = FALSE)
    }, error = function(e) return(NULL))
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    colnames(df) <- c("entity_id", "Label_Info", "entity")
    df <- df |> 
      separate(Label_Info, 
               into = c("entity_type", "entity_start", "entity_end"), 
               extra = "merge", 
               fill = "right", 
               sep = " ", 
               convert = FALSE)
    
    df$doc_id <- gsub('.ann', '', basename(file_path))
    return(df)
  })
}

ann_data_list <- lapply(ann_files, process_ann_file)
ann_data_list <- Filter(Negate(is.null), ann_data_list)
ann_data <- bind_rows(ann_data_list) |>
  filter(!grepl('Out-of-scope|Equiv|AnnotatorNotes', entity_type)) |>
  setDT()

message(sprintf("  Read %d annotation files", length(ann_files)))
message(sprintf("  Total annotation rows: %d", nrow(ann_data)))

# Split entities and relations
entities <- ann_data[grepl("^T", entity_id)]
relations <- ann_data[grepl("^R", entity_id)]

message(sprintf("  Entities: %d", nrow(entities)))
message(sprintf("  Relations: %d", nrow(relations)))

# Extract relation links
rel1 <- relations[, .(relation_id = entity_id, relation_type = entity_type, 
                      doc_id, role = "Arg1", entity_id = sub("Arg1:", "", entity_start))]
rel2 <- relations[, .(relation_id = entity_id, relation_type = entity_type, 
                      doc_id, role = "Arg2", entity_id = sub("Arg2:", "", entity_end))]

rel_long <- rbind(rel1, rel2)
setkey(rel_long, doc_id, entity_id, relation_id, role)
rel_long <- unique(rel_long)

# Join entities with relations
entities_w_relations <- merge(
  entities,
  rel_long,
  by = c("doc_id", "entity_id"),
  all.x = TRUE
)

message("Step 2: Reading text files...")
# Read all .txt files
txt_files <- list.files(LSD600_DIR, pattern = "\\.txt$", 
                        full.names = TRUE, recursive = TRUE)

process_txt_file <- function(file_path) {
  text <- readChar(file_path, file.info(file_path)$size)
  data.frame(doc_id = gsub('.txt', '', basename(file_path)), 
             text = text, stringsAsFactors = FALSE)
}

pubmed_data <- do.call(rbind, lapply(txt_files, process_txt_file))
setDT(pubmed_data)

message(sprintf("  Read %d text files", length(txt_files)))

message("Step 3: Tokenizing sentences...")
# Tokenize sentences
ss <- pubmed_data |> 
  nlp_split_paragraphs() |>
  nlp_split_sentences(text_hierarchy = c('doc_id', 'paragraph_id'))

# Process: merge first paragraph sentences if needed
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

# Renumber sentence_id within doc
ss1[, sentence_id := seq_len(.N), by = doc_id]
ss1 <- ss1[, .(doc_id, sentence_id, text, start, end)]

message(sprintf("  Total sentences: %d", nrow(ss1)))

message("Step 4: Mapping entities to sentences...")
# Ensure correct types
entities_w_relations[, entity_start := as.integer(entity_start) + 1]
entities_w_relations[, entity_end := as.integer(entity_end)]
ss1[, start := as.integer(start)]
ss1[, end := as.integer(end)]

# Set keys for foverlaps
setkey(ss1, doc_id, start, end)
setkey(entities_w_relations, doc_id, entity_start, entity_end)

# Run foverlaps to map entities to sentences
dt1 <- foverlaps(
  entities_w_relations,
  ss1,
  by.x = c("doc_id", "entity_start", "entity_end"),
  by.y = c("doc_id", "start", "end"),
  nomatch = NA
)

message(sprintf("  Entities mapped to sentences: %d", nrow(dt1[!is.na(sentence_id)])))

message("Step 5: Building sentence-level pairs...")
# Split into sentence-level groups
dt_grouped <- split(dt1, by = c("doc_id", "sentence_id"), drop = TRUE)

# Process each group to create all LSF-disease pairs
pair_rows <- rbindlist(lapply(dt_grouped, function(group) {
  b <- group[entity_type == "lifestyle_factor"]
  h <- group[entity_type == "disease"]
  
  if (nrow(b) == 0 || nrow(h) == 0) return(NULL)
  
  pairs <- CJ(b_idx = seq_len(nrow(b)), h_idx = seq_len(nrow(h)))
  
  rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
    pid <- UUIDgenerate()
    
    bi <- pairs$b_idx[i]
    hi <- pairs$h_idx[i]
    
    lifestyle <- b[bi, .(
      doc_id, 
      sentence_id,
      pair_id = pid,
      entity,
      entity_type,
      entity_id,
      entity_start = entity_start,
      entity_end = entity_end,
      start = start,
      end = end,
      text = text
    )]
    
    disease <- h[hi, .(
      doc_id, 
      sentence_id,
      pair_id = pid,
      entity,
      entity_type,
      entity_id,
      entity_start = entity_start,
      entity_end = entity_end,
      start = start,
      end = end,
      text = text
    )]
    
    rbind(lifestyle, disease)
  }))
}))

# Create stable hash key per pair (independent of row order)
pair_rows[, pair_key := paste(
  doc_id,
  sentence_id,
  paste(sort(entity_id), collapse = "_")
), by = pair_id]

# Deduplicate: keep first 2 rows per unique pair_key
pair_rows[, pair_rank := seq_len(.N), by = pair_key]
pair_rows_dedup <- pair_rows[pair_rank <= 2]
pair_rows_dedup[, c("pair_rank") := NULL]

message(sprintf("  Total pairs created: %d", nrow(pair_rows_dedup) / 2))

message("Step 6: Attaching relation labels...")
# Extract relation information
rel_rows <- dt1[!is.na(relation_id)]
rel_counts <- rel_rows[, .N, by = .(doc_id, sentence_id, relation_id)]

# Keep only complete relation pairs (exactly 2 rows)
valid_rels <- rel_counts[N == 2]

dt_rels_complete <- rel_rows[
  valid_rels,
  on = .(doc_id, sentence_id, relation_id)
]

# Create pair_id and pair_key for relations
dt_rels_complete[, pair_id := ceiling(.I / 2)]
dt_rels_complete[, pair_key := paste(
  doc_id[1],
  sentence_id[1],
  paste(sort(entity_id), collapse = "_")
), by = pair_id]

# Keep only first two rows per pair_key
dt_rels_complete <- dt_rels_complete[
  order(pair_key),
  .SD[1:2],
  by = pair_key
]

rels_subset <- dt_rels_complete[, .(pair_key, relation_id, relation_type)] |> unique()

# Join with pairs
pairs_ <- merge(
  pair_rows_dedup,
  rels_subset,
  by = "pair_key",
  all.x = TRUE
)

pairs_[, c('pair_id', 'relation_id', 'entity_id') := NULL]

message("Step 7: Reshaping to wide format...")
# Reshape to wide format (one row per pair)
dt <- copy(pairs_)
dt[, entity_type_short := fifelse(entity_type == "lifestyle_factor", "LSF", entity_type)]

entity_cols <- c("entity", "entity_start", "entity_end")

long_dt <- melt(
  dt,
  id.vars = c("pair_key", "doc_id", "sentence_id", "text", "relation_type", 
              "start", "end", "entity_type_short"),
  measure.vars = entity_cols,
  variable.name = "field",
  value.name = "value"
)

long_dt[, new_field := paste0(entity_type_short, "_", field)]

wide_dt <- dcast(
  long_dt,
  pair_key + doc_id + sentence_id + start + end + text + relation_type ~ new_field,
  value.var = "value"
)

setcolorder(
  wide_dt,
  c(
    "pair_key", "doc_id", "sentence_id", "start", "end",
    "text", "relation_type",
    "LSF_entity", "LSF_entity_start", "LSF_entity_end",
    "disease_entity", "disease_entity_start", "disease_entity_end"
  )
)

wide_dt[, c(
  "LSF_entity_start", "LSF_entity_end",
  "disease_entity_start", "disease_entity_end"
) := lapply(.SD, as.integer), .SDcols = c(
  "LSF_entity_start", "LSF_entity_end",
  "disease_entity_start", "disease_entity_end"
)]

# Add explicit 'no_relation' label
wide_dt[, relation_type := fifelse(is.na(relation_type), 'no_relation', relation_type)]

message("Step 8: Adding metadata from original dataset...")
# Join with original metadata to get Data_Set labels
if (file.exists(METADATA_FILE)) {
  lsd600_meta <- fread(METADATA_FILE)
  
  # Extract entity IDs from pair_key (format: "doc_id sentence_id entity1_entity2")
  # Entity IDs are sorted, but metadata uses Disease_LSF order
  wide_dt[, entity_ids := {
    parts <- tstrsplit(pair_key, " ")
    parts[[3]]  # The entity IDs part (e.g., "T12_T13")
  }]
  
  # Create metadata keys in both possible orderings (Disease_LSF and LSF_Disease)
  # since pair_key has sorted entity IDs
  lsd600_meta[, key_disease_lsf := paste0(Publication_ID, "_", Disease_BRAT_ID, "_", LSF_BRAT_ID)]
  lsd600_meta[, key_lsf_disease := paste0(Publication_ID, "_", LSF_BRAT_ID, "_", Disease_BRAT_ID)]
  
  # Create both possible keys from pair_key
  wide_dt[, key1 := paste0(doc_id, "_", entity_ids)]
  
  # Try matching with Disease_LSF order first
  wide_dt[lsd600_meta, on = .(key1 = key_disease_lsf), Data_Set := i.Data_Set]
  
  # For unmatched rows, try LSF_Disease order
  unmatched <- wide_dt[is.na(Data_Set) | Data_Set == ""]
  if (nrow(unmatched) > 0) {
    unmatched[lsd600_meta, on = .(key1 = key_lsf_disease), Data_Set := i.Data_Set]
    wide_dt[unmatched, on = .(pair_key), Data_Set := i.Data_Set]
  }
  
  # Clean up temporary columns
  wide_dt[, c("entity_ids", "key1") := NULL]
  
  message(sprintf("  Matched %d pairs with original metadata", 
                  nrow(wide_dt[!is.na(Data_Set) & Data_Set != ""])))
} else {
  message("  Warning: Metadata file not found, skipping Data_Set assignment")
  wide_dt[, Data_Set := NA_character_]
}

message("Step 9: Adding section labels...")
# Add section information
ss1[, section := fifelse(sentence_id == 1, 
                         "title", 
                         stringr::str_extract(text, "^[A-Za-z]+:"))]

ss1[, section_fill := fifelse(section == "title", NA_character_, section)]
ss1[, section_fill := zoo::na.locf(section_fill, na.rm = FALSE), by = doc_id]
ss1[, section := fifelse(sentence_id != 1 & is.na(section), section_fill, section)]
ss1[, section_fill := NULL]

wide_dt[ss1, on = .(doc_id, sentence_id), section := i.section]

message("Step 10: Assigning Data_Set to no_relation pairs...")
# Assign Data_Set to no_relation pairs based on proportions
if (any(!is.na(wide_dt$Data_Set))) {
  set_proportions <- wide_dt[relation_type != "no_relation" & !is.na(Data_Set), 
                             .N, by = Data_Set][, proportion := N / sum(N)]
  
  no_rel_dt <- wide_dt[relation_type == "no_relation" & is.na(Data_Set)]
  n_total_no_rel <- nrow(no_rel_dt)
  
  if (n_total_no_rel > 0) {
    set_proportions[, no_rel_n := round(proportion * n_total_no_rel)]
    diff <- n_total_no_rel - sum(set_proportions$no_rel_n)
    if (diff != 0) {
      set_proportions[1:abs(diff), no_rel_n := no_rel_n + sign(diff)]
    }
    
    set_names <- rep(set_proportions$Data_Set, times = set_proportions$no_rel_n)
    set.seed(42)
    assigned_sets <- sample(set_names)
    
    wide_dt[relation_type == "no_relation" & is.na(Data_Set), 
            Data_Set := assigned_sets[seq_len(.N)]]
    
    message(sprintf("  Assigned Data_Set to %d no_relation pairs", n_total_no_rel))
  }
}

message("Step 11: Downsampling no_relation pairs...")
# Downsample no_relation pairs to 600 (as in original processing)
no_rel_count <- nrow(wide_dt[relation_type == "no_relation"])
if (no_rel_count > 600) {
  set.seed(42)
  no_rel_indices <- wide_dt[, which(relation_type == "no_relation")]
  keep_indices <- sample(no_rel_indices, 600)
  drop_indices <- setdiff(no_rel_indices, keep_indices)
  
  wide_dt <- wide_dt[-drop_indices]
  message(sprintf("  Downsampled from %d to 600 no_relation pairs", no_rel_count))
}

message("Step 12: Adding word counts...")
wide_dt[, word_count := tokenizers::count_words(text)]

message("Step 13: Writing output...")
fwrite(wide_dt, OUTPUT_FILE, row.names = FALSE, quote = TRUE)

message(sprintf("\n✓ Complete! Output written to: %s", OUTPUT_FILE))
message(sprintf("  Total pairs: %d", nrow(wide_dt)))
message(sprintf("  Relation types: %s", 
                paste(names(table(wide_dt$relation_type)), collapse = ", ")))

