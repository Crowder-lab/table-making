#!/usr/bin/env Rscript

library(tidyverse)
library(gt)

raw <- read_tsv("./all_drugbank.tsv", col_names = FALSE) %>%
  setNames(c("step", "num_drugs")) %>%
  mutate(
    step = str_remove(step, fixed(":")),
    num_drugs = as.integer(num_drugs)
  )

gt_table <- raw %>%
  gt() %>%
  cols_label(step = "", num_drugs = "Number") %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  # colors and styles
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = step)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = seq(1, nrow(raw), 2)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey90")
    ),
    locations = cells_body(columns = step)
  ) %>%
  # universal
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = -c(step))
  ) %>%
  # universal cont.
  fmt_markdown(columns = everything()) %>%
  opt_table_outline() %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("all_drugbank.", extension))
}
