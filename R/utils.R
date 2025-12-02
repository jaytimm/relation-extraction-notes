# Utility functions for LSD600 data processing

#' Mask entities in text with placeholders
#' 
#' @param text Character vector of text strings
#' @param text_start Integer vector of start positions for the text context
#' @param lsf_start Integer vector of LSF entity start positions (absolute)
#' @param lsf_end Integer vector of LSF entity end positions (absolute)
#' @param dis_start Integer vector of disease entity start positions (absolute)
#' @param dis_end Integer vector of disease entity end positions (absolute)
#' @return Character vector with entities replaced by [FACTOR] and [OUTCOME]
mask_entities <- function(text, text_start, lsf_start, lsf_end, dis_start, dis_end) {
  text_start <- as.numeric(text_start)
  spans <- list(
    list(start = lsf_start - text_start, end = lsf_end - text_start + 1, replacement = "[FACTOR]"),
    list(start = dis_start - text_start, end = dis_end - text_start + 1, replacement = "[OUTCOME]")
  )
  
  # Order spans in reverse to preserve indexing
  spans <- spans[order(sapply(spans, function(x) x$start), decreasing = TRUE)]
  
  for (span in spans) {
    text <- paste0(
      substring(text, 1, span$start),
      span$replacement,
      substring(text, span$end + 1)
    )
  }
  
  trimws(text)
}

#' Highlight entities in text with HTML spans
#' 
#' @param text Character vector of text strings
#' @param text_start Integer vector of start positions for the text context
#' @param lsf_start Integer vector of LSF entity start positions (absolute)
#' @param lsf_end Integer vector of LSF entity end positions (absolute)
#' @param dis_start Integer vector of disease entity start positions (absolute)
#' @param dis_end Integer vector of disease entity end positions (absolute)
#' @return Character vector with HTML span tags for highlighting
highlight_entities <- function(text, text_start, lsf_start, lsf_end, dis_start, dis_end) {
  # Compute relative positions
  lsf_start_rel <- lsf_start - text_start + 1
  lsf_end_rel   <- lsf_end   - text_start + 1
  dis_start_rel <- dis_start - text_start + 1
  dis_end_rel   <- dis_end   - text_start + 1
  
  # Determine order so that the first highlighted segment appears first
  if (lsf_start_rel < dis_start_rel) {
    first_start  <- lsf_start_rel
    first_end    <- lsf_end_rel
    first_color  <- "#BCE3C1"
    
    second_start <- dis_start_rel
    second_end   <- dis_end_rel
    second_color <- "#8FAEE3"
  } else {
    first_start  <- dis_start_rel
    first_end    <- dis_end_rel
    first_color  <- "#8FAEE3"
    
    second_start <- lsf_start_rel
    second_end   <- lsf_end_rel
    second_color <- "#BCE3C1"
  }
  
  # Extract segments from the text
  part1 <- if (first_start > 1) substr(text, 1, first_start - 1) else ""
  highlight1 <- substr(text, first_start, first_end)
  part2 <- if (second_start > first_end + 1) substr(text, first_end + 1, second_start - 1) else ""
  highlight2 <- substr(text, second_start, second_end)
  part3 <- if (second_end < nchar(text)) substr(text, second_end + 1, nchar(text)) else ""
  
  # Paste together with HTML span tags
  paste0(part1,
         sprintf("<span style='background-color: %s;'>%s</span>", first_color, highlight1),
         part2,
         sprintf("<span style='background-color: %s;'>%s</span>", second_color, highlight2),
         part3)
}

#' Concatenate context text with proper spacing
#' 
#' @param before Character vector of text before
#' @param current Character vector of current text
#' @param after Character vector of text after
#' @return Character vector of concatenated text
concatenate_context <- function(before, current, after) {
  # Remove NA and empty strings, then paste with space
  parts <- list()
  if (!is.null(before) && length(before) > 0) {
    parts <- c(parts, before[!is.na(before) & before != ""])
  }
  parts <- c(parts, current)
  if (!is.null(after) && length(after) > 0) {
    parts <- c(parts, after[!is.na(after) & after != ""])
  }
  paste(parts, collapse = " ")
}

