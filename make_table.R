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

all_ranked <- read_csv("ranked.csv")

updown <- c(
  fzd10 = "↑",
  rbm24 = "↑",
  itgb3 = "↑",
  golga7b = "↑",
  tac1 = "↑",
  phox2b = "↑",
  cdk5r2 = "↑",
  nrn1 = "↑",
  col3a1 = "↑",
  loxl4 = "↑",
  cpa4 = "↑",
  nppb = "↑",
  itga11 = "↑",
  isl1 = "↑",
  kcnc3 = "↑",
  ebf1 = "↑",
  cebpd = "↑",
  acta2 = "↑",
  chl1 = "↑",
  snap25 = "↑",
  znf558 = "↓",
  insl3 = "↓",
  znf681 = "↓",
  ctsf = "↓",
  nkapl = "↓",
  tceal5 = "↓",
  slc30a8 = "↓",
  rtp1 = "↓",
  large1 = "↓",
  htr1a = "↓",
  ect2l = "↓",
  fsip2 = "↓",
  arrb1 = "↓",
  tdrd1 = "↓",
  znf283 = "↓",
  spata16 = "↓",
  oxsm = "↓"
)

df <- read_csv("threes.csv") %>%
  select(c(`DrugBank:Main Name`, `Score`)) %>%
  filter(`Score` >= 3) %>%
  left_join(select(all_ranked, c(`DrugBank:Main Name`, `search term`)), by = join_by(`DrugBank:Main Name`)) %>%
  mutate(`search term` = str_replace_all(`search term`, "[\\[\\]’‘']", "")) %>%
  mutate(`search term` = str_split(`search term`, ", ")) %>%
  rowwise() %>%
  mutate(
    `search term` = `search term` %>%
      str_subset("\\(human\\)") %>%
      str_c(collapse = ", ") %>%
      str_replace_all(fixed(" (human)"), "") %>%
      str_replace_all(fixed(" ,"), ",") %>%
      str_to_upper()
  ) %>%
  ungroup() %>%
  mutate(
    `search term` = map_chr(
      str_split(`search term`, ", "),
      ~ str_c(
        updown[str_to_lower(.x)],
        " ",
        str_to_upper(.x),
        collapse = ", "
      )
    )
  ) %>%
  mutate(
    `DEs in iPSC dataset` = str_replace_all(`search term`, c("↑" = "d", "↓" = "↑", "d" = "↓"))
  )

dfs <- list(filter(df, Score == 6), filter(df, Score %in% c(4, 5)), filter(df, Score == 3))
df_names <- list("iPSC gene modulation 6s.", "iPSC gene modulation 4s 5s.", "iPSC gene modulation 3s.")

for (i in seq_along(dfs)) {
  df <- dfs[[i]]
  df_name <- df_names[[i]]

  gt_table <- df %>%
    # start using the gt package to style the table
    gt() %>%
    # make column labels bold and add emojis
    cols_label(
      `DrugBank:Main Name` = md("**Drug**"),
      `Score` = md("**Score**"),
      `search term` = md("**Genes Modulated**"),
      `DEs in iPSC dataset` = md("**DEs in iPSC Dataset**"),
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
    gtsave(gt_table, paste0(df_name, extension))
  }
}
