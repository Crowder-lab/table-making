#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)

Sys.setenv(CHROMOTE_CHROME = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")

phenotypes <- read_excel("./phenotypes.xlsx", sheet = "Sheet1")

gt_table <- phenotypes %>%
  gt() %>%
  tab_header(title = md("***MAPK8IP3* Variant Phenotype Summary**")) %>%
  cols_label(
    `Variant\r\nNM_015133.5` = md(""),
    `E27X` = md("**E27X**"),
    `R578C` = md("**R578C**"),
    `R1146C` = md("**R1146C**"),
  ) %>%
  fmt_markdown(columns = everything()) %>%
  tab_style(
    style = list(
      cell_text(style = "italic")
    ),
    locations = cells_row_groups()
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
  fmt_percent(
    columns = -c(`Variant\r\nNM_015133.5`),
    rows = `Variant\r\nNM_015133.5` != "N with clinical data available",
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
