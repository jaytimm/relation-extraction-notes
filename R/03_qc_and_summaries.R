#!/usr/bin/env Rscript
# Quality checks and summary statistics for processed LSD600 datasets

suppressPackageStartupMessages({
  library(data.table)
  library(tokenizers)
})

# Configuration
PROJECT_ROOT <- getwd()
PROCESSED_DIR <- file.path(PROJECT_ROOT, "data", "processed")
METADATA_FILE <- file.path(PROJECT_ROOT, "LSD600", "Consolidated_Relations_Dataset.tsv")

message("LSD600 Dataset Quality Checks and Summaries")
message(paste0("=", strrep("=", 50)))

# Check which files exist
files_to_check <- c(
  "lsd600_pairs_base.csv",
  "lsd600_sentence.csv",
  "lsd600_sent_plus1.csv",
  "lsd600_sent_plus2.csv",
  "lsd600_abstract.csv"
)

message("\n1. File availability check:")
for (f in files_to_check) {
  filepath <- file.path(PROCESSED_DIR, f)
  if (file.exists(filepath)) {
    size_mb <- file.info(filepath)$size / 1024^2
    message(sprintf("  ✓ %s (%.2f MB)", f, size_mb))
  } else {
    message(sprintf("  ✗ %s (not found)", f))
  }
}

# Load base dataset
base_file <- file.path(PROCESSED_DIR, "lsd600_pairs_base.csv")
if (!file.exists(base_file)) {
  stop("Base dataset not found. Please run 01_build_lsd600_pairs.R first.")
}

message("\n2. Loading base dataset...")
dt_base <- fread(base_file)
message(sprintf("  Total pairs: %d", nrow(dt_base)))

# Relation type distribution
message("\n3. Relation type distribution:")
rel_dist <- dt_base[, .N, by = relation_type][order(-N)]
print(rel_dist)
message(sprintf("  Total with relations: %d", 
                nrow(dt_base[relation_type != "no_relation"])))
message(sprintf("  Total no_relation: %d", 
                nrow(dt_base[relation_type == "no_relation"])))

# Data set distribution
message("\n4. Data set distribution:")
if ("Data_Set" %in% names(dt_base)) {
  set_dist <- dt_base[, .N, by = Data_Set][order(Data_Set)]
  print(set_dist)
  
  # By relation type and data set
  message("\n5. Relation types by data set:")
  rel_by_set <- dt_base[, .N, by = .(Data_Set, relation_type)][order(Data_Set, -N)]
  print(rel_by_set)
} else {
  message("  Data_Set column not found")
}

# Compare with original metadata
if (file.exists(METADATA_FILE)) {
  message("\n6. Comparison with original metadata:")
  lsd600_orig <- fread(METADATA_FILE)
  message(sprintf("  Original relations in metadata: %d", nrow(lsd600_orig)))
  message(sprintf("  Relations in processed data: %d", 
                  nrow(dt_base[relation_type != "no_relation"])))
  
  # Check relation type distribution
  if ("Relationship_Type" %in% names(lsd600_orig)) {
    orig_rel_dist <- lsd600_orig[, .N, by = Relationship_Type][order(-N)]
    message("\n  Original relation types:")
    print(orig_rel_dist)
  }
}

# Context size statistics
message("\n7. Context size statistics:")
context_files <- c(
  "lsd600_sentence.csv",
  "lsd600_sent_plus1.csv",
  "lsd600_sent_plus2.csv",
  "lsd600_abstract.csv"
)

for (f in context_files) {
  filepath <- file.path(PROCESSED_DIR, f)
  if (file.exists(filepath)) {
    dt <- fread(filepath)
    if ("context_text" %in% names(dt)) {
      word_counts <- dt[, count_words(context_text)]
      message(sprintf("\n  %s:", f))
      message(sprintf("    Mean words: %.1f", mean(word_counts)))
      message(sprintf("    Median words: %.1f", median(word_counts)))
      message(sprintf("    Min words: %d", min(word_counts)))
      message(sprintf("    Max words: %d", max(word_counts)))
      message(sprintf("    Total pairs: %d", nrow(dt)))
    } else if ("text" %in% names(dt)) {
      word_counts <- dt[, count_words(text)]
      message(sprintf("\n  %s:", f))
      message(sprintf("    Mean words: %.1f", mean(word_counts)))
      message(sprintf("    Median words: %.1f", median(word_counts)))
      message(sprintf("    Min words: %d", min(word_counts)))
      message(sprintf("    Max words: %d", max(word_counts)))
      message(sprintf("    Total pairs: %d", nrow(dt)))
    }
  }
}

# Entity statistics
message("\n8. Entity statistics:")
message(sprintf("  Unique LSF entities: %d", 
                length(unique(dt_base$LSF_entity))))
message(sprintf("  Unique disease entities: %d", 
                length(unique(dt_base$disease_entity))))

# Document statistics
message("\n9. Document statistics:")
message(sprintf("  Unique documents: %d", length(unique(dt_base$doc_id))))
message(sprintf("  Average pairs per document: %.1f", 
                nrow(dt_base) / length(unique(dt_base$doc_id))))

# Section distribution
if ("section" %in% names(dt_base)) {
  message("\n10. Section distribution:")
  section_dist <- dt_base[, .N, by = section][order(-N)]
  print(section_dist)
}

message(paste0("\n", strrep("=", 60)))
message("Quality checks complete!")

