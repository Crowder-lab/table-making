#!/usr/bin/env Rscript
library(tidyverse)
library(readxl)
# the packages used to make the tables to save
library(gt)
library(gtExtras)

# This is needed to output a .png photo file
# Replace inside the " " with the path to Google Chrome
# (or another chromium-based browser)
# Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

# adjust the contents of the table
drugs <- c(
  "Acamprosate",
  "Azelaic acid",
  "Betamethasone",
  "Buspirone",
  "Clemastine",
  "Desflurane",
  "Dextromethorphan",
  "Escitalopram",
  "Fluvoxamine",
  "Gabapentin",
  "Lidocaine",
  "Lovastatin",
  "Nalbuphine",
  "Pregabalin",
  "Pyridostigmine",
  "Pyridoxine",
  "Sertraline",
  "Triamcinolone"
)
df <- read_csv("./SLC6A1 Drug Data Story - Genes Only.csv") %>%
  select(c(`DrugBank:Main Name`, `Score`, `Pathway modulated`, `Molecular Mechanism`)) %>%
  filter(`DrugBank:Main Name` %in% drugs) %>%
  mutate(
    `Molecular Mechanism` = case_when(
      str_count(`Molecular Mechanism`, fixed(",")) > 0 ~ "↓ GABA A receptor",
      str_count(`Molecular Mechanism`, fixed("GABA A")) > 0 ~ "↓ GABA A receptor",
      str_count(`Molecular Mechanism`, fixed("GABA B")) > 0 ~ "↓ GABA B receptor",
      str_count(`Molecular Mechanism`, fixed("everything")) > 0 ~ "↓ GABA B receptor",
      str_count(`Molecular Mechanism`, fixed("GAT1")) > 0 ~ "↑ GAT1",
      TRUE ~ `Molecular Mechanism`
    )
  ) %>%
  mutate(
    the_sorter = case_when(
      str_count(`Molecular Mechanism`, fixed("GABA A")) > 0 ~ 3,
      str_count(`Molecular Mechanism`, fixed("GABA B")) > 0 ~ 2,
      str_count(`Molecular Mechanism`, fixed("GAT1")) > 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    `Pathway modulated` = str_replace_all(`Pathway modulated`, fixed("upregulate"), "↑"),
    `Pathway modulated` = str_replace_all(`Pathway modulated`, fixed("inhibit"), "↓"),
  ) %>%
  arrange(the_sorter, desc(Score), `DrugBank:Main Name`) %>%
  select(-c(the_sorter))

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
  cols_width(
    `Pathway modulated` ~ px(400)
  ) %>%
  fmt_markdown(columns = everything()) %>%
  opt_table_outline() %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("SLC6A1 Grant.", extension))
}
