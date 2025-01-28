library(jsonlite)
library(tidyverse)

lines <- readLines("imm_segments_with_tone_and_metadata.jsonlist")
lines <- lines[lines != ""]
data <- lapply(lines, function(line) {
  tryCatch({
    fromJSON(line)
  }, error = function(e) {
    NULL  # Return NULL for malformed lines
  })
})
data <- data[!sapply(data, is.null)]

data_part_2 <- data[145401:290800]
data_flattened <- unlist(data_part_2, recursive = FALSE)
data_cleaned <- lapply(data_flattened, function(x) {
  if (is.null(x) || length(x) == 0) {
    NULL  
  } else {
    as.data.frame(x) 
  }
})
data_cleaned <- Filter(Negate(is.null), data_cleaned)
column_names <- unique(unlist(lapply(data_cleaned, colnames)))
data_standardized <- lapply(data_cleaned, function(df) {
  missing_cols <- setdiff(column_names, colnames(df))
  df[missing_cols] <- NA
  df[column_names] 
})

result <- do.call(rbind, data_standardized)

result$variable <- rownames(result)
rownames(result) <- NULL
colnames(result) <- c("value", "variable")
result$variable <- gsub("[0-9]+", "", result$variable)
result$variable <- as.factor(result$variable)
result <- result %>%
  mutate(
    speech_number = cumsum(variable == "speech_id") # Sequential numbers for "speech_id"
  )

reshaped_df_part_2 <- result %>%
  group_by(speech_number) %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = list(value = ~ first(.)) 
  )

saveRDS(reshaped_df_part_2, "reshaped_df_part_2.RDS")

final_df <- rbind(reshaped_df_part_1, reshaped_df_part_2)
saveRDS(final_df, "final_df.RDS")

