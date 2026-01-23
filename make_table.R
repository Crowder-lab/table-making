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

updown <- c(
  slc6a1 = "↑",
  stx1a = "↓",
  kctd8 = "↑",
  kctd12 = "↑",
  kctd16 = "↑",
  grk2 = "↑",
  grk3 = "↑",
  arrb2 = "↑",
  rgs4 = "↑",
  rgs6 = "↑",
  rgs7 = "↑",
  rest = "↑",
  mecp2 = "↑",
  gabra1 = "↓",
  gabrb2 = "↓",
  gabrg2 = "↓",
  gabbr1 = "↓",
  gabbr2 = "↓",
  prkcg = "↑",
  prkcb = "↑",
  prkca = "↑",
  cdk5 = "↑",
  gsk3b = "↑",
  rnf34 = "↑",
  slc12a2 = "↑",
  sting1 = "↑",
  tbk1 = "↑",
  irf3 = "↑"
)

df <- read_csv("./SLC6A1 Drug Data Story - Genes Only.csv") %>%
  select(c(`DrugBank:Main Name`, `Score`, `Pathway modulated`, `Molecular Mechanism`)) %>%
  filter(`Score` >= 5) %>%
  mutate(
    gat1 = ifelse(str_detect(`Molecular Mechanism`, "GAT1"), "↑ GAT1", ""),
    gabab = ifelse(str_detect(`Molecular Mechanism`, "GABA B"), "↓ GABA B receptor", ""),
    gabaa = ifelse(str_detect(`Molecular Mechanism`, "GABA A"), "↓ GABA A receptor", "")
  ) %>%
  mutate(
    `Molecular Mechanism` =
      str_c(gat1, gabab, gabaa, sep = ", ") %>%
      str_replace_all(", , ", ", ") %>%
      str_remove_all("^, |, $")
  ) %>%
  mutate(
    the_sorter = case_when(
      str_count(`Molecular Mechanism`, fixed("GAT1")) > 0 ~ 1,
      str_count(`Molecular Mechanism`, fixed("GABA B")) > 0 ~ 2,
      str_count(`Molecular Mechanism`, fixed("GABA A")) > 0 ~ 3,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    `Pathway modulated` = map_chr(
      str_split(`Pathway modulated`, ", "),
      ~ str_c(
        updown[.x],
        " ",
        str_to_upper(.x),
        collapse = ", "
      )
    )
  ) %>%
  arrange(the_sorter, desc(Score), `DrugBank:Main Name`) %>%
  select(-c(the_sorter, gat1, gabaa, gabab))

gt_table <- df %>%
  # start using the gt package to style the table
  gt() %>%
  # make column labels bold and add emojis
  cols_label(
    `DrugBank:Main Name` = md("**Drug**"),
    `Score` = md("**Score**"),
    `Pathway modulated` = md("**Pathways Modulated**"),
    `Molecular Mechanism` = md("**Molecular Hypothesis**")
  ) %>%
  # center the text of the column labels
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_column_labels()
  ) %>%
  # bold the left-most column
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = `DrugBank:Main Name`)
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
    locations = cells_body(columns = `DrugBank:Main Name`)
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_body()
  ) %>%
  # add A and B subscripts
  text_replace(
    locations = cells_body(columns = `Molecular Mechanism`),
    pattern = " ([AB])",
    replacement = "<sub>\\1</sub>"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  opt_table_outline() %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("SLC6A1 Grant.", extension))
}
