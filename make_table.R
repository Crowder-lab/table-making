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

unravel = c(
  "terbinafine",
  "lovastatin",
  "hydroxyurea",
  "calcitriol",
  "memantine",
  "metoprolol",
  "gemfibrozil",
  "betamethasone",
  "levetiracetam",
  "clonidine",
  "ezetimibe",
  "metformin",
  "fexofenadine",
  "loratadine",
  "progesterone",
  "fluvoxamine",
  "bisoprolol",
  "dextromethorphan",
  "methylprednisolone",
  "amitriptyline",
  "sertraline",
  "lidocaine",
  "acetylcysteine",
  "pyridoxine",
  "captopril",
  "naloxone",
  "gabapentin",
  "buspirone",
  "amantadine",
  "propranolol",
  "budesonide",
  "tretinoin",
  "cyproheptadine",
  "trihexyphenidyl",
  "alfacalcidol",
  "hydroxyzine",
  "triamcinolone"
)

# for genes
all_ranked <- read_csv("zc4h2_all.csv")
# for categories
old_clinician <- read_csv("ZC4H2 Drug Data Story - Final Clinician Ranking.csv")

df <- read_csv("ZC4H2 Drug Data Story - Oral route only.csv") %>%
  filter(Score >= 4) %>%
  mutate(unravel = ifelse(`Main Name` %in% unravel, "✓", "")) %>%
  left_join(select(old_clinician, c(`DrugBank:Main Name`, `Therapeutic Category`, `Targeted Symptoms`)), by = join_by(`DrugBank:Main Name`)) %>%
  left_join(select(all_ranked, c(`DrugBank:Main Name`, `search term`)), by = join_by(`DrugBank:Main Name`)) %>%
  mutate(`search term` = str_replace_all(`search term`, "[\\[\\]’‘']", "")) %>%
  mutate(`search term` = str_split(`search term`, ", ")) %>%
  rowwise() %>%
  mutate(
    `search term` = `search term` |>
      str_subset("\\(human\\)") |>
      str_c(collapse = ", ") |>
      str_replace_all(fixed("(human)"), "") |>
      str_replace_all(fixed(" ,"), ",") |>
      str_to_upper()
  ) %>%
  ungroup() %>%
  select(c(`DrugBank:Main Name`, Score, unravel, `Therapeutic Category`, `Targeted Symptoms`, `search term`)) %>%
  filter(`Therapeutic Category` != "")

gt_table <- df %>%
  # start using the gt package to style the table
  gt() %>%
  # make column labels bold and add emojis
  cols_label(
    `DrugBank:Main Name` = md("**Drug**"),
    `Score` = md("**Score**"),
    `unravel` = md("**Unravel**"),
    `Therapeutic Category` = md("**Therapeutic Category**"),
    `Targeted Symptoms` = md("**Targeted Symptoms**"),
    `search term` = md("**Targeted Genes**")
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
  fmt_markdown(columns = everything()) %>%
  opt_table_outline() %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

for (extension in c("html", "png", "docx")) {
  gtsave(gt_table, paste0("ZC4H2 Unravel overlap.", extension))
}
