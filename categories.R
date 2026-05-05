#!/usr/bin/env Rscript

library(tidyverse)
library(gt)

df <- read_tsv("categories.tsv")

# make pretty table
gt_table <- df |>
  gt() |>
  cols_label(
    Category = md("**Category**"),
    Explanation = md("**Explanation**"),
  ) |>
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) |>
  # colors and styles
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = Category)
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = seq(1, nrow(df), 2)
    )
  ) |>
  tab_style(
    style = list(
      cell_fill(color = "grey90")
    ),
    locations = cells_body(columns = Category)
  ) |>
  # universal
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = everything())
  ) |>
  # universal cont.
  fmt_markdown(columns = everything()) |>
  opt_table_outline() |>
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("categories.", extension))
}
