#!/usr/bin/env Rscript
library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)

Sys.setenv(CHROMOTE_CHROME = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")

unique_df <- read_excel(
  "Combined Drug List Analysis.xlsx",
  sheet = "Commonality"
) %>%
  rename(
    mapk8ip3 = `MAPK8IP3 Unique Drugs`,
    zc4h2 = `ZC4H2 Unique Drugs`,
    slc6a1 = `SLC6A1 Unique Drugs`
  ) %>%
  select(c(mapk8ip3, zc4h2, slc6a1))

for (genotype in c("MAPK8IP3", "ZC4H2", "SLC6A1")) {
  df <- read_excel(
    paste(genotype, "Drug Data Story.xlsx"),
    sheet = "Final Clinician Ranking"
  ) %>%
    # make column name shorter
    rename(Name = `DrugBank:Main Name`) %>%
    # get only unique drugs
    filter(Name %in% unique_df[[str_to_lower(genotype)]]) %>%
    # get only the columns we need
    select(
      c(
        Name,
        Score,
        `Drug Type`,
        `Therapeutic Category`,
        `Targeted Symptoms`,
        `Proteins of Interest`
      )
    ) %>%
    mutate(Score = as.integer(Score)) %>%
    # sort Proteins of Interest
    separate_longer_delim(`Proteins of Interest`, delim = ", ") %>%
    group_by(
      Name, Score, `Drug Type`, `Therapeutic Category`, `Targeted Symptoms`
    ) %>%
    arrange(`Proteins of Interest`) %>%
    summarize(
      `Proteins of Interest` = paste0(`Proteins of Interest`,
        collapse = ", "
      )
    ) %>%
    ungroup() %>%
    # sort Targeted Symptoms
    separate_longer_delim(`Targeted Symptoms`, delim = ", ") %>%
    group_by(
      Name,
      Score,
      `Drug Type`,
      `Therapeutic Category`,
      `Proteins of Interest`
    ) %>%
    arrange(`Targeted Symptoms`) %>%
    summarize(
      `Targeted Symptoms` = paste0(`Targeted Symptoms`, collapse = ", ")
    ) %>%
    ungroup() %>%
    # sort Therapeutic Categories
    separate_longer_delim(`Therapeutic Category`, delim = ", ") %>%
    group_by(Name) %>%
    arrange(`Therapeutic Category`) %>%
    # add symptoms and proteins to Therapeutic Category column
    mutate(
      `Therapeutic Category` = ifelse(
        `Therapeutic Category` == "Protein of interest modulator",
        paste0("Protein of interest (", `Proteins of Interest`, ") modulator"),
        `Therapeutic Category`
      )
    ) %>%
    mutate(
      `Therapeutic Category` = ifelse(
        `Therapeutic Category` == "Symptomatic relief",
        paste0("Symptomatic relief (", `Targeted Symptoms`, ")"),
        `Therapeutic Category`
      )
    ) %>%
    # reassemble Therapeutic Categories
    summarize(
      Score = first(Score),
      `Drug Type` = first(`Drug Type`),
      `Therapeutic Category` = paste0(`Therapeutic Category`, collapse = ", ")
    ) %>%
    mutate(
      `Therapeutic Category` = str_replace(`Therapeutic Category`, ", $", "")
    ) %>%
    # sort correctly
    arrange(desc(Score), Name)

  print(df)

  gt_table <- df %>%
    gt() %>%
    # column labels
    cols_label(
      Name = md("**Drug Name**"),
      Score = md("**Score**"),
      `Drug Type` = md("**Drug Type**"),
      `Therapeutic Category` = md("**Therapeutic Category**")
    ) %>%
    tab_style(
      style = list(
        cell_text(align = "center")
      ),
      locations = cells_column_labels()
    ) %>%
    # colors and styles
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(columns = Name)
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
      locations = cells_body(columns = Name)
    ) %>%
    # universal
    tab_style(
      style = list(
        cell_text(align = "center")
      ),
      locations = cells_body()
    ) %>%
    # special cases
    {
      if (genotype == "ZC4H2") {
        cols_width(
          .,
          `Therapeutic Category` ~ px(550)
        )
      } else if (genotype == "MAPK8IP3") {
        cols_width(
          .,
          `Therapeutic Category` ~ px(405)
        )
      } else {
        .
      }
    } %>%
    # universal cont.
    fmt_markdown(columns = everything()) %>%
    opt_table_outline() %>%
    sub_missing(columns = everything(), rows = everything(), missing_text = "")

  for (extension in c("html", "png", "docx")) {
    gtsave(gt_table, paste0(genotype, " Unique.", extension))
  }
}

for (genotype in c("MAPK8IP3", "ZC4H2", "SLC6A1")) {
  df <- read_excel(
    paste(genotype, "Drug Data Story.xlsx"),
    sheet = "Final Clinician Ranking"
  ) %>%
    # make column name shorter
    rename(Name = `DrugBank:Main Name`) %>%
    # get only high scores
    filter(Score == 6) %>%
    # get only the columns we need
    select(
      c(
        Name,
        `Drug Type`,
        `Therapeutic Category`,
        `Targeted Symptoms`,
        `Proteins of Interest`
      )
    ) %>%
    # sort Proteins of Interest
    separate_longer_delim(`Proteins of Interest`, delim = ", ") %>%
    group_by(Name, `Drug Type`, `Therapeutic Category`, `Targeted Symptoms`) %>%
    arrange(`Proteins of Interest`) %>%
    summarize(
      `Proteins of Interest` = paste0(`Proteins of Interest`,
        collapse = ", "
      )
    ) %>%
    ungroup() %>%
    # sort Targeted Symptoms
    separate_longer_delim(`Targeted Symptoms`, delim = ", ") %>%
    group_by(
      Name,
      `Drug Type`,
      `Therapeutic Category`,
      `Proteins of Interest`
    ) %>%
    arrange(`Targeted Symptoms`) %>%
    summarize(
      `Targeted Symptoms` = paste0(`Targeted Symptoms`, collapse = ", ")
    ) %>%
    ungroup() %>%
    # sort Therapeutic Categories
    separate_longer_delim(`Therapeutic Category`, delim = ", ") %>%
    group_by(Name) %>%
    arrange(`Therapeutic Category`) %>%
    # add symptoms and proteins to Therapeutic Category column
    mutate(
      `Therapeutic Category` = ifelse(
        `Therapeutic Category` == "Protein of interest modulator",
        paste0("Protein of interest (", `Proteins of Interest`, ") modulator"),
        `Therapeutic Category`
      )
    ) %>%
    mutate(
      `Therapeutic Category` = ifelse(
        `Therapeutic Category` == "Symptomatic relief",
        paste0("Symptomatic relief (", `Targeted Symptoms`, ")"),
        `Therapeutic Category`
      )
    ) %>%
    # reassemble Therapeutic Categories
    summarize(
      `Drug Type` = first(`Drug Type`),
      `Therapeutic Category` = paste0(`Therapeutic Category`, collapse = ", ")
    ) %>%
    mutate(
      `Therapeutic Category` = str_replace(`Therapeutic Category`, ", $", "")
    ) %>%
    # sort correctly
    arrange(Name)

  print(df)

  gt_table <- df %>%
    gt() %>%
    # column labels
    cols_label(
      Name = md("**Drug Name**"),
      `Drug Type` = md("**Drug Type**"),
      `Therapeutic Category` = md("**Therapeutic Category**")
    ) %>%
    tab_style(
      style = list(
        cell_text(align = "center")
      ),
      locations = cells_column_labels()
    ) %>%
    # colors and styles
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(columns = Name)
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
      locations = cells_body(columns = Name)
    ) %>%
    # universal
    tab_style(
      style = list(
        cell_text(align = "center")
      ),
      locations = cells_body()
    ) %>%
    # special cases
    {
      if (genotype == "ZC4H2") {
        cols_width(
          .,
          `Therapeutic Category` ~ px(550)
        )
      } else if (genotype == "MAPK8IP3") {
        cols_width(
          .,
          `Therapeutic Category` ~ px(405)
        )
      } else {
        .
      }
    } %>%
    # universal cont.
    fmt_markdown(columns = everything()) %>%
    opt_table_outline() %>%
    sub_missing(columns = everything(), rows = everything(), missing_text = "")

  for (extension in c("html", "png", "docx")) {
    gtsave(gt_table, paste0(genotype, " 6s.", extension))
  }
}
