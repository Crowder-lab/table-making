#!/usr/bin/env Rscript

library(tidyverse)
library(gt)

jip3_ranking <- function(tsv_path) {
  read_tsv(tsv_path) |>
    # remove brackets if they exist
    mutate(`search term` = str_remove_all(`search term`, "[\\[\\]']")) |>
    # make genes markdown italicized and remove ' (human)'
    mutate(
      `search term` = str_replace_all(
        `search term`,
        "\\w+?\\s\\(human\\)",
        ~ toupper(paste0("*", .x, "*"))
      )
    ) |>
    mutate(`search term` = str_remove_all(`search term`, fixed(" (HUMAN)"))) |>
    # split up each term so we have a list!
    mutate(`search term` = str_split(`search term`, ", ")) |>
    mutate(`search term` = map(`search term`, ~ sort(.x))) |>
    mutate(`search term` = map_chr(`search term`, ~ paste0(.x, collapse = ", "))) |>

    # routes becomes logical based on if there's an oral route
    mutate(`DrugBank:Routes` = str_detect(`DrugBank:Routes`, fixed("oral"))) |>

    # make fda approval logical if it isn't already
    mutate(`DrugBank:FDA Approved` = as.logical(`DrugBank:FDA Approved`))
}

drugbank_ranking <- function(tsv_path) {
  read_tsv(tsv_path) |>
    # routes becomes logical based on if there's an oral route
    mutate(`DrugBank:Routes` = str_detect(`DrugBank:Routes`, fixed("oral"))) |>

    # make fda approval logical if it isn't already
    mutate(`DrugBank:FDA Approved` = as.logical(`DrugBank:FDA Approved`))
}

df <- jip3_ranking("jip3.tsv")
# df <- drugbank_ranking("all_drugbank.tsv")

for (this_score in unique(df$score)) {
  gt_table <- df |>
    filter(score == this_score) |>
    select(c(
      `Main Name`,
      # `Human Intestinal Absorption`,
      # `Blood Brain Barrier`,
      # `P-glycoprotein Inhibition`,
      # `Drug Induced Liver Injury`,
      # `CYP3A4 Inhibition`,
      # `CYP2C9 Inhibition`,
      # `DrugBank:Routes`
      `search term`
    )) |>
    arrange(`Main Name`) |>

    gt() |>
    cols_label(
      `Main Name` = md("**Drug**"),
      # `Human Intestinal Absorption` = md("**Human intestinal absorption**"),
      # `Blood Brain Barrier` = md("**Blood brain barrier**"),
      # `P-glycoprotein Inhibition` = md("**P-glycoprotein inhibition**"),
      # `Drug Induced Liver Injury` = md("**Drug induced liver injury**"),
      # `CYP3A4 Inhibition` = md("**CYP3A4 inhibition**"),
      # `CYP2C9 Inhibition` = md("**CYP2C9 inhibition**"),
      # `DrugBank:Routes` = md("**Oral route**")
      `search term` = md("**Translator searches**")
    ) |>
    fmt_markdown(columns = everything()) |>
    # fmt_percent(
    #   columns = c(
    #     `Human Intestinal Absorption`,
    #     `Blood Brain Barrier`,
    #     `P-glycoprotein Inhibition`,
    #     `Drug Induced Liver Injury`,
    #     `CYP3A4 Inhibition`,
    #     `CYP2C9 Inhibition`,
    #   )
    # ) |>
    # fmt_tf(columns = `DrugBank:Routes`, tf_style = "check-mark") |>
    tab_style(
      style = list(
        cell_text(align = "center")
      ),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(columns = `Main Name`)
    ) |>
    tab_style(
      style = list(
        cell_fill(color = "grey95")
      ),
      locations = cells_body(
        rows = seq(1, nrow(filter(df, score == this_score)), 2)
      )
    ) |>
    tab_style(
      style = list(
        cell_fill(color = "grey90")
      ),
      locations = cells_body(columns = `Main Name`)
    ) |>
    tab_style(
      style = list(
        cell_text(align = "center")
      ),
      locations = cells_body(columns = everything())
    ) |>
    opt_table_outline() |>
    sub_missing(columns = everything(), rows = everything(), missing_text = "")

  for (extension in c("html", "png", "docx")) {
    gtsave(gt_table, paste0("jip3_", this_score, ".", extension))
    # gtsave(gt_table, paste0("all_drugbank_", this_score, ".", extension))
  }
}
