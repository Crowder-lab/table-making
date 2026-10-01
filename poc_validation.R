#!/usr/bin/env Rscript

library(tidyverse)
library(gt)

genes <- c(
  "GRIN1", "GRIN2A", "GRIN2B", "GRIN2D", "GRIN",
  "KCNC1", "ALDH7A1", "BTD", "FMR1", "MAPK3"
)
gene_regex <- paste0("\\b(", paste(genes, collapse = "|"), ")\\b")

df <- read_tsv("poc_validation_table.tsv") |>
  # make genes markdown italicized
  mutate(across(
    c(`Gene (variant effect)`, Disorder, `Translator query entry point (n results)`),
    ~ str_replace_all(.x, gene_regex, "*\\1*")
  )) |>
  # let long slash-separated gene lists wrap at the slashes
  mutate(`Gene (variant effect)` = str_replace_all(`Gene (variant effect)`, "/", "/\u200b"))

# make pretty table
gt_table <- df |>
  gt() |>
  cols_label(
    `Repurposing strategy` = md("**Strategy**"),
    `Gene (variant effect)` = md("**Gene (variant effect)**"),
    Disorder = md("**Disorder**"),
    `Drug (mechanism of action)` = md("**Drug (mechanism)**"),
    `Translator query entry point (n results)` = md("**Translator query (*n*)**"),
    `BEACON rank` = md("**BEACON score**"),
    `Clinical evidence` = md("**Clinical evidence**"),
  ) |>
  cols_width(
    `Repurposing strategy` ~ px(85),
    `Gene (variant effect)` ~ px(90),
    Disorder ~ px(110),
    `Drug (mechanism of action)` ~ px(100),
    `Translator query entry point (n results)` ~ px(95),
    `BEACON rank` ~ px(50),
    `Clinical evidence` ~ px(210)
  ) |>
  # sized to fit a portrait manuscript page
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(4),
    data_row.padding.horizontal = px(3),
    column_labels.padding = px(4),
    column_labels.padding.horizontal = px(3)
  ) |>
  tab_style(
    style = list(
      cell_text(align = "center", v_align = "middle")
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
    locations = cells_body(columns = `Repurposing strategy`)
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
    locations = cells_body(columns = `Repurposing strategy`)
  ) |>
  # universal
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = -`Clinical evidence`)
  ) |>
  # long free text reads better left-aligned
  tab_style(
    style = list(
      cell_text(align = "left")
    ),
    locations = cells_body(columns = `Clinical evidence`)
  ) |>
  # universal cont.
  fmt_markdown(columns = everything()) |>
  opt_table_outline() |>
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "docx")) {
  gtsave(gt_table, paste0("poc_validation.", extension))
}
# widen the screenshot viewport so the png isn't clipped
gtsave(gt_table, "poc_validation.png", vwidth = 1200)
