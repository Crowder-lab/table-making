#!/usr/bin/env Rscript

library(tidyverse)
library(gt)

read_scores <- function(path) {
  read_tsv(path) |>
    rename(name = `Main Name`) |>
    select(c(name, score)) |>
    filter(!is.na(as.numeric(score))) |>
    mutate(score = as.numeric(score)) |>
    arrange(desc(score), name) |>
    group_by(score) |>
    mutate(row_idx = row_number()) |>
    pivot_wider(
      names_from = score,
      values_from = name,
      id_cols = row_idx,
      values_fill = ""
    ) |>
    select(-row_idx)
    # group_by(score) |>
    # reframe(drugs = paste(name, collapse = ", ")) |>
    # arrange(desc(score))
}

drugbank <- read_scores("./DrugBank BEACON results - Clinician.tsv")
jip3 <- read_scores("./JIP3 BEACON results - Clinician.tsv")

# make pretty table
gt_table <- drugbank |>
  gt() |>
  cols_label(
    `6` = md("**6**"),
    `5` = md("**5**"),
    `4` = md("**4**"),
    `3` = md("**3**")
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
  # tab_style(
  #   style = list(
  #     cell_text(weight = "bold")
  #   ),
  #   locations = cells_body(columns = score)
  # ) |>
  tab_style(
    style = list(
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = seq(1, nrow(drugbank), 2)
    )
  ) |>
  # tab_style(
  #   style = list(
  #     cell_fill(color = "grey90")
  #   ),
  #   locations = cells_body(columns = score)
  # ) |>
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
  gtsave(gt_table, paste0("all_drugbank.", extension))
}
