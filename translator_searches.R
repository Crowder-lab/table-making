#!/usr/bin/env Rscript

library(tidyverse)
library(gt)

raw <- read_tsv(
  "translator_searches.tsv",
  col_names = FALSE,
  show_col_types = FALSE
) %>%
  setNames(c(
    "item",
    "source",
    "query",
    "filtered_hits"
  ))

# keep rows for the main table
main <- raw %>%
  slice(1:34) %>%
  mutate(row_id = row_number())

# pull section titles explicitly
section_map <- tibble(
  row_id  = c(1, 15, 22),
  section = main$item[c(1, 15, 22)]
)

# attach section labels and remove header rows
main_grouped <- main %>%
  left_join(section_map, by = "row_id") %>%
  fill(section, .direction = "down") %>%
  filter(!row_id %in% section_map$row_id) %>%
  filter(!is.na(item)) %>%
  select(-row_id) %>%
  group_by(section)

# make pretty table
gt_table <- main_grouped %>%
  select(c(item, `source`, filtered_hits)) %>%
  gt() %>%
  cols_label(
    item = "",
    `source` = md("**Source**"),
    # query = md("**Query**"),
    filtered_hits = md("**Hits**")
  ) %>%
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
    locations = cells_body(columns = item)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = seq(1, nrow(main_grouped), 2)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey90")
    ),
    locations = cells_body(columns = item)
  ) %>%
  # universal
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = -c(item))
  ) %>%
  # universal cont.
  fmt_markdown(columns = everything()) %>%
  opt_table_outline() %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("translator_searches.", extension))
}
