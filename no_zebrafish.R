#!/usr/bin/env Rscript
library(tidyverse)
library(readxl)
# the packages used to make the tables to save
library(gt)
library(gtExtras)

# This is needed to output a .png photo file
# Replace inside the " " with the path to Google Chrome
# (or another chromium-based browser)
Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

# adjust the contents of the table
df <- read_excel("./Cross species variant table.xlsx", sheet = "Sheet1") %>%
  rename(blank = `...1`) %>%
  select(-Zebrafish)

gt_table <- df %>%
  # start using the gt package to style the table
  gt() %>%
  # make column labels bold and add emojis
  cols_label(
    blank = "",
    Human = md("**Human**"),
    Mouse = md("**Mouse**"),
  ) %>%
  # center the text of the column labels
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  # bold the left-most column
  # blank is the name of that column
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = blank)
  ) %>%
  # set every other body row to be light grey
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
  gtsave(gt_table, paste0("No zebrafish.", extension))
}
