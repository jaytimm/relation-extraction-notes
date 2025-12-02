# LSD600 Relation Extraction: Context Size Experiments

This repository contains a restructured version of the **LSD600** corpus with explicit `no_relation` labels and multiple context-size variants for studying the effect of context on relation classification performance.

## Overview

**LSD600** is a corpus of 600 biomedical abstracts annotated with lifestyle factor (LSF)–disease relations. The original corpus contains 1,900 manually annotated relations across 8 distinct relation types. This repository extends the original corpus by:

1. **Explicit negative examples**: Creating all possible LSF–disease entity pairs within sentences and explicitly labeling pairs without relations as `no_relation`
2. **Context-size variants**: Generating multiple context representations (sentence-only, ±1 sentence, ±2 sentences, full abstract) to study how context size affects relation classification

## Relation to Original Project

This work builds on the original LSD600 corpus:

- **Original paper**: [LSD600: The First Corpus of Biomedical Abstracts Annotated with Lifestyle–Disease Relations](https://pubmed.ncbi.nlm.nih.gov/28678823/)
- **Original corpus**: [Zenodo](https://zenodo.org/records/13952449)
- **Original code**: [GitHub](https://github.com/EsmaeilNourani/LSF_Disease_RE)

### What's Modified

- **No changes to original annotations**: All positive relation labels remain unchanged
- **Added `no_relation` class**: All LSF–disease pairs within the same sentence that don't have an explicit relation are labeled as `no_relation`
- **Context variants**: Four different context representations are generated for each pair:
  - Sentence-only: Just the sentence containing the entities
  - ±1 sentence: The sentence plus one sentence before and after
  - ±2 sentences: The sentence plus two sentences before and after
  - Full abstract: The entire document text

## Project Structure

```
.
├── R/                          # Processing scripts
│   ├── 01_build_lsd600_pairs.R      # Build sentence-level pairs from raw data
│   ├── 02_make_context_variants.R   # Create context-size variants
│   ├── 03_qc_and_summaries.R        # Quality checks and statistics
│   └── utils.R                      # Helper functions
├── data/
│   └── processed/              # Generated datasets
│       ├── lsd600_pairs_base.csv
│       ├── lsd600_sentence.csv
│       ├── lsd600_sent_plus1.csv
│       ├── lsd600_sent_plus2.csv
│       └── lsd600_abstract.csv
└── README.md                   # This file
```

## LSD600 Data Structure

The `LSD600/` directory contains the original corpus downloaded from [Zenodo](https://zenodo.org/records/13952449). 

```
LSD600/
├── train/                      # 360 .ann + 360 .txt files
├── dev/                        # 120 .ann + 120 .txt files
├── test/                       # 120 .ann + 120 .txt files
├── Consolidated_Relations_Dataset.tsv
└── LSD600_metadata.tsv
```

## Data Processing Pipeline

### Step 1: Build Base Pairs

```bash
Rscript R/01_build_lsd600_pairs.R
```

This script:
- Reads all `.ann` and `.txt` files from `LSD600/`
- Extracts entities (lifestyle factors and diseases) and relations
- Tokenizes abstracts into sentences
- Creates all possible LSF–disease pairs within each sentence
- Labels pairs without explicit relations as `no_relation`
- Joins with original metadata to preserve `Data_Set` labels (train/dev/test)
- Outputs: `data/processed/lsd600_pairs_base.csv`

### Step 2: Create Context Variants

```bash
Rscript R/02_make_context_variants.R
```

This script:
- Reads the base pairs dataset
- Creates four context-size variants:
  - **Sentence-only**: `lsd600_sentence.csv`
  - **±1 sentence**: `lsd600_sent_plus1.csv`
  - **±2 sentences**: `lsd600_sent_plus2.csv`
  - **Full abstract**: `lsd600_abstract.csv`
- For each variant, creates:
  - `context_text`: The full context text
  - `masked_context_text`: Context with entities replaced by `[FACTOR]` and `[OUTCOME]`

### Step 3: Quality Checks

```bash
Rscript R/03_qc_and_summaries.R
```

This script generates:
- File availability checks
- Relation type distributions
- Data set splits (train/dev/test)
- Context size statistics (word counts)
- Entity and document statistics
- Comparison with original metadata


## Output File Schema

Each output CSV file contains the following columns:

### Core Columns
- `pair_key`: Unique identifier for each LSF–disease pair
- `doc_id`: Document identifier (PubMed ID)
- `sentence_id`: Sentence number within document
- `relation_type`: Relation label (one of the 8 original types or `no_relation`)
- `Data_Set`: Original split (`train-set`, `dev-set`, `test-set`)

### Entity Information
- `LSF_entity`: Lifestyle factor entity text
- `LSF_entity_start`: Start position (absolute, character offset in document)
- `LSF_entity_end`: End position (absolute)
- `disease_entity`: Disease entity text
- `disease_entity_start`: Start position (absolute)
- `disease_entity_end`: End position (absolute)

### Context Columns (varies by file)
- `text`: Original sentence text (sentence-only file)
- `context_text`: Full context text (all variant files)
- `masked_context_text`: Context with entities masked as `[FACTOR]` and `[OUTCOME]`
- `context_word_count`: Number of words in context (variant files)

### Metadata
- `start`: Start position of sentence/context in document
- `end`: End position of sentence/context in document
- `section`: Section label (e.g., "BACKGROUND", "RESULTS", "title")
- `word_count`: Word count for sentence (base file only)

## Relation Types

The corpus includes 8 original relation types plus `no_relation`:

1. **statistical_association**: General statistical association
2. **positive_statistical_association**: Positive effect (most common, ~32%)
3. **causes**: Causality clearly implied
4. **negative_statistical_association**: Negative effect
5. **controls**: Beneficial impact of LSF on disease
6. **prevents**: LSF hinders disease from occurring
7. **treats**: LSF has therapeutic effect
8. **no_statistical_association**: Absence of statistical association
9. **no_relation**: No relation exists (added in this work)

## Requirements

### R Packages
- `data.table` (>= 1.14.0)
- `readr` (>= 2.0.0)
- `tidyr` (>= 1.2.0)
- `textpress` (for sentence tokenization)
- `tokenizers` (for word counting)
- `uuid` (for generating pair IDs)
- `stringr` (for string operations)
- `zoo` (for forward-fill operations)


## Context Size Experiments

The different context sizes are designed to study:

- **Sentence-only**: Baseline performance with minimal context
- **±1 sentence**: Effect of immediate neighboring sentences
- **±2 sentences**: Effect of broader local context
- **Full abstract**: Maximum available context (document-level)

Each variant maintains the same entity positions (absolute character offsets), allowing direct comparison of model performance across context sizes.

## Notes

- Entity positions are **absolute** (character offsets from document start), not relative to context
- Titles (sentence_id == 1) are excluded from context windows to avoid noise
- The `no_relation` class is downsampled to 600 examples to balance the dataset
- Original train/dev/test splits are preserved via `Data_Set` column
