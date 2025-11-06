#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)

Sys.setenv(CHROMOTE_CHROME = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")

phenotypes <- read_excel("./phenotypes.xlsx", sheet = "Sheet1") %>%
  rename(patient_variant = `Variant\r\nNM_015133.5`) %>%
  mutate(patient_variant = na_if(patient_variant, "N with clinical data available")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(patient_variant), paste0("*n*=", .), .))) %>%
  mutate(across(c(E27X, R578C, R1146C), ~ case_when(row_number() == 1 ~ ., TRUE ~ paste0(round(as.numeric(.) * 100, 1), "%"))))

print(phenotypes)

gt_table <- phenotypes %>%
  gt() %>%
  tab_header(title = md("**Variant NM_015133.5**")) %>%
  cols_label(
    patient_variant = md(""),
    `E27X` = md("**p.E27X**"),
    `R578C` = md("**p.R578C**"),
    `R1146C` = md("**p.R1146C**"),
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
    locations = cells_body()
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    row_group.as_column = TRUE
  ) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

# Save gt table in real formats
gtsave(gt_table, "table.html")
gtsave(gt_table, "table.png")
gtsave(gt_table, "table.docx")
