#!/usr/bin/env Rscript
library(tidyverse)
library(readxl)
# the packages used to make the tables to save
library(gt)
library(gtExtras)

raw <- read_excel("Table 2. Clinical phenotypes.xlsx", col_names = FALSE)

spanner_label <- raw %>%
  slice(1) %>%
  pull(3) %>%
  str_squish()
variant_cols <- c("n = 1", "n = 5", "n = 12")

cognitive_set <- c(
  "Intellectual disability",
  "Delay in language",
  "Autism",
  "Sensory integration disorder",
  "ADHD",
  "Anxiety"
)
motor_set <- c(
  "Delay in gross motor",
  "Delay in fine motor",
  "Hypotonia",
  "Hypertonia",
  "Dystonia",
  "Myoclonus",
  "Tremor",
  "Ataxia"
)

df <- raw %>%
  slice(4:n()) %>%
  set_names(c("phenotype", "n = 1", "n = 5", "n = 12")) %>%
  mutate(across(all_of(variant_cols), as.numeric)) %>%
  mutate(domain = case_when(
    phenotype %in% cognitive_set ~ "Cognitive Domains",
    phenotype %in% motor_set ~ "Motor Domains",
    phenotype == "Microcephaly" ~ ""
  )) %>%
  group_by(domain) %>%
  mutate(
    is_group_end = row_number() == n(),
    is_table_start = phenotype == "Microcephaly"
  )

gt_table <- df %>%
  # start using the gt package to style the table
  gt() %>%
  # add extra stuff above
  cols_label(
    phenotype = "",
    `n = 1` = md("*n = 1*"),
    `n = 5` = md("*n = 5*"),
    `n = 12` = md("*n = 12*")
  ) %>%
  tab_spanner(
    label = "p.E27X",
    columns = `n = 1`
  ) %>%
  tab_spanner(
    label = "p.R578C",
    columns = `n = 5`
  ) %>%
  tab_spanner(
    label = "p.R1146C",
    columns = `n = 12`
  ) %>%
  tab_spanner(
    label = md(paste0("**", spanner_label, "**")),
    columns = variant_cols
  ) %>%
  # center the text of the column labels
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  # format data as percentages
  fmt_percent(
    columns = variant_cols,
    decimals = 1
  ) %>%
  # tab styling
  tab_style(
    style = list(
      # grey95 = 95% lightness
      # nearly white
      cell_fill(color = "grey95")
    ),
    # do only the body cells - i.e. exclude the header row(s)
    locations = cells_body(
      rows = seq(1, nrow(df), 2)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey90")
    ),
    locations = cells_body(columns = phenotype)
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = variant_cols)
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    row_group.as_column = TRUE,
  ) %>%
  # tab_style(
  #   style = cell_borders(
  #     sides = c("top", "right", "bottom", "left"),
  #     color = "#D9D9D9",
  #     weight = px(2)
  #   ),
  #   locations = cells_body()
  # ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "#CCCCCC",
      weight = px(3)
    ),
    locations = cells_body(columns = `n = 12`)
  ) %>%
  tab_style(
    style = list(
      cell_text(size = px(18), align = "center", v_align = "middle")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "#CCCCCC",
      weight = px(3)
    ),
    locations = cells_body(rows = is_group_end)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "#CCCCCC",
      weight = px(3)
    ),
    locations = cells_body(rows = is_table_start)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left"),
      color = "#CCCCCC",
      weight = px(3)
    ),
    locations = cells_row_groups()
  ) %>%
  cols_hide(c(is_group_end, is_table_start)) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("Variants 2026-02.", extension))
}
