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
  group_by(domain)

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
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = domain %in% c("", "Motor Domains")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = variant_cols)
  ) %>%
  gt_color_rows(
    columns = variant_cols,
    palette = c("#A90C38FF", "#FFFCFCFF", "#2E5A87FF"),
    domain = c(0, 1)
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    row_group.as_column = TRUE
  ) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("Variants 2026-02.", extension))
}
