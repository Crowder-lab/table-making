{ # load libraries and i/o directories --------------------------------------------------------------------
library(tidyverse)
library(gt) # package for handling tables
library(gtExtras) # extra gt themes
library(rmarkdown)
library(readxl) # directly import excel files
# i/o directories
if (Sys.info() |> as_tibble() |> slice(4) |> pull() == "DOM-0026686-LT" |
      Sys.info() |> as_tibble() |> slice(4) |> pull() =="NEURO-9SZ8613") { # my surface pro or work desktop at sparks
  workingdirectory <- "C:/Users/hcwalker/Box/Harrison files/Harrison projects/other projects/Cammie projects and analyses/analyses/Tables for SFARI" # working directory
  }
if (Sys.info() |> as_tibble() |> slice(4) |> pull() == "CORSAIRONE") { # my desktop computer at home
  workingdirectory <- "C:/Users/harri/Box/Harrison files/Harrison projects/other projects/Cammie projects and analyses/analyses/Tables for SFARI" # working directory
}
  if (Sys.info() |> as_tibble() |> slice(4) |> pull() == "camerron-crowders-MacBook-Pro-2.local") { # my desktop computer at home
    workingdirectory <- "/Users/camerroncrowder/Desktop/Tables for SFARI" # working directory
  }
setwd(workingdirectory) # set working directory
outputdirectory <- workingdirectory # set output directory
}
{ # Table 1 --------------------------------------------------------------
read_xlsx("SFARI Cross Species KG Tables v2.xlsx", sheet="Sheet1") |> # table 1
  group_by(Category) |>
  select(-`MILD (*n*=18)`, -`SEVERE (*n*=16)`) |>
  gt(groupname_col = "Category", rowname_col = "row_names") |>
    fmt_markdown(columns=everything(), rows = everything()) |>
    tab_header(
      title = md("**Clinical spectrum of *SLC6A1*-NDD**")
    ) |>
    #gt_theme_538() |>
    tab_options(row_group.as_column = TRUE) |>
    cols_label(
        Category = md("**Category**"),
        `MILD2 (*n*=18)` = md("**Mild (*n*=18)**"),
        `SEVERE2 (*n*=16)` = md("**Severe (*n*=16)**"),
        row_labels = md("")
      ) |>
    sub_missing(columns=everything(), rows=everything(), missing_text = "") |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
        ),
    locations = cells_row_groups()
    ) |>
    cols_align(
        align = c("center"),
        columns = contains("n")
      ) |>
    gtsave("clinical spectrum.html")

  # Table 3 ---------------------------------------------------------
  read_xlsx("SFARI Cross Species KG Tables v2.xlsx", sheet="Sheet2") |>
    group_by(Functional_domain) |>
    gt(groupname_col = "Functional_domain") |>
    fmt_markdown(columns=everything(), rows = everything()) |>
    tab_header(title=md("***SLC6A1*-NDD Clinical Assessments**")) |>
    cols_label(
      Functional_domain = md("**Symptom**"),
      Metric = md("**Neurogenetic Registry / Cohort Study**"),
      Description = md("**SFARI Cross Species Study**")
    ) |>
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_row_groups()
    ) |>
    tab_options(row_group.as_column = TRUE) |>
    sub_missing(columns=everything(), rows=everything(), missing_text = "") |>
    cols_width(
      1 ~ px(100)
    ) #|>
    gtsave("severity parameters.html")
}

# Table 3 ---------------------------------------------------------
read_xlsx("Zebrafish behavioral assays to measure Patient symptoms .xlsx", sheet="Sheet1") |>
  group_by(symptom) |>
  gt(groupname_col = "symptom") |>
  fmt_markdown(columns=everything(), rows = everything()) |>
  tab_header(title=md("**Validated Zebrafish Assays to Model *SLC6A1*-NDD Patient Behaviors**")) |>
  cols_label(
    symptom = md("**Behavior**"),
    assay = md("**Assay**"),
    publication = md("**Publication**")
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |>
  tab_options(row_group.as_column = TRUE) |>
  sub_missing(columns=everything(), rows=everything(), missing_text = "") |>
  cols_width(
    1 ~ px(120),
    publication ~ px(300)
  ) |>
  gtsave("zebrafish assays.html")

