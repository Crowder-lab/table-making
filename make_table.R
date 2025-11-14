#!/usr/bin/env Rscript
library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)

Sys.setenv(CHROMOTE_CHROME = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")

df <- read_excel("./Cross species variant table.xlsx", sheet = "Sheet1") %>%
  rename(blank = `...1`)

gt_table <- df %>%
  gt() %>%
  cols_label(
    blank = "",
    Human = md("**Human**"),
    Mouse = md("**Mouse**"),
    Zebrafish = md("**Zebrafish**")
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = blank)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey95")
    ),
    locations = cells_body(
      rows = seq(1, nrow(df), 2)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "grey90")
    ),
    locations = cells_body(columns = blank)
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body()
  ) %>%
  fmt_markdown(columns = everything()) %>%
  opt_table_outline() %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("Cross species variants.", extension))
}
