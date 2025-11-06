#!/usr/bin/env Rscript

library(tidyverse)
library(janitor)
library(readxl)
library(gt)
library(gtExtras)

Sys.setenv(CHROMOTE_CHROME = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")

phenotypes <- read_excel("./phenotypes.xlsx", sheet = "Sheet1") %>%
  rename(patient_variant = `Variant\r\nNM_015133.5`) %>%
  mutate(patient_variant = na_if(patient_variant, "N with clinical data available")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(patient_variant), paste0("*n*=", .), .))) %>%
  mutate(patient_variant = ifelse(is.na(patient_variant), "patient_variant", patient_variant)) %>%
  row_to_names(row_number = 1) %>%
  mutate(across(-patient_variant, ~ as.numeric(.)))

print(phenotypes)

gt_table <- phenotypes %>%
  gt() %>%
  cols_label(
    patient_variant = md(""),
    `*n*=1` = md("*n*=1"),
    `*n*=5` = md("*n*=5"),
    `*n*=12` = md("*n*=12"),
  ) %>%
  tab_spanner(
    label = "p.E27X",
    columns = `*n*=1`,
  ) %>%
  tab_spanner(
    label = "p.R578C",
    columns = `*n*=5`,
  ) %>%
  tab_spanner(
    label = "p.R1146C",
    columns = `*n*=12`,
  ) %>%
  tab_spanner(
    label = md("**Variant NM_015133.5**"),
    columns = -patient_variant,
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = patient_variant,
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body(columns = -patient_variant)
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  fmt_percent(
    columns = -patient_variant,
    decimals = 1,
  ) %>%
  # tab_style(
  #   style = list(
  #     cell_fill(color = "grey75")
  #   ),
  #   locations = cells_column_spanners()
  # ) %>%
  # tab_style(
  #   style = list(
  #     cell_fill(color = "grey75")
  #   ),
  #   locations = cells_column_labels()
  # ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = seq(1, nrow(phenotypes), 2)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey90")
    ),
    locations = cells_body(
      columns = patient_variant
    )
  ) %>%
  opt_table_outline() %>%
  tab_options(
    row_group.as_column = TRUE,
  ) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

# Save gt table in real formats
gtsave(gt_table, "table.html")
gtsave(gt_table, "table.png")
gtsave(gt_table, "table.docx")
