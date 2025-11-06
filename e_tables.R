#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)
library(stringr)

# Read all sheets from Excel file
drug_screens_2023 <- read_excel("all_drug_screens.xlsx", sheet = "2023 Drug Screens")
drug_screens_2024 <- read_excel("all_drug_screens.xlsx", sheet = "2024 Drug Screens")
drug_library_2024 <- read_excel("all_drug_screens.xlsx", sheet = "Drug Library (2024)", skip = 1) %>%
  mutate(`Generic Name` = str_remove_all(`Generic Name`, "\\s*\\([^)]*\\)"))
non_drug_library <- read_excel("all_drug_screens.xlsx", sheet = "Non-library Drugs")

# Clean and combine 2023 and 2024 drug screens
drug_screens_combined <- bind_rows(
  # Process 2023 data
  drug_screens_2023 %>%
    select(
      drug_name = `Drug Name`,
      concentration = Concentration,
      outcome = `Phenotype Improved`,
      dv_zantiks = `DV/Zantiks prism`  # Add this column temporarily
    ) %>%
    mutate(
      outcome = case_when(
        dv_zantiks == "Lethal" ~ "Lethal",
        TRUE ~ outcome
      )  # Lethal in this column should override the outcome column
    ) %>%
    select(-dv_zantiks),  # Remove the temporary column
  drug_screens_2024 %>%
    select(
      drug_name = Drug,
      concentration = Concentration,
      outcome = `Phenotype Improved?`,
    ) %>%
    filter(drug_name %in% c("Amantadine", "Roscovitine", "Sertraline HCL"))
) %>%
  # Remove rows with missing outcomes or concentrations
  filter(!is.na(outcome)) %>%
  filter(!is.na(concentration)) %>%
  # Cammie didn't want these in there
  filter(!drug_name %in% c("Carbidopa", "Daprodustat", "Dinaciclib", "Oxindole", "RH115", "TrkB agonist (BDNF like)")) %>%
  filter(!(drug_name == "Amantadine" & concentration %in% c("1 uM", "10 uM", "100 uM"))) %>%
  filter(!(drug_name == "Ambroxol" & concentration == "1 uM")) %>%
  filter(!(drug_name == "Disulfiram" & concentration == "0.1 uM")) %>%
  filter(!(drug_name == "Entacapone" & concentration == "25 uM")) %>%
  filter(!(drug_name == "Fluvoxamine" & concentration == "0.05 uM")) %>%
  filter(!(drug_name == "Levodopa" & concentration %in% c("1 uM", "25 uM", "10 mM"))) %>%
  filter(!(drug_name == "Resveratrol" & concentration == "0.1 uM")) %>%
  # No combinations
  filter(!str_detect(drug_name, "\\+")) %>%
  # Fix concentration notation
  mutate(concentration = str_replace_all(concentration, "u", "μ")) %>%
  # Fix outcome names
  mutate(outcome = str_replace_all(outcome, fixed("Rescue (WT and KO)"), "Non-specific Improvement")) %>%
  mutate(outcome = str_replace_all(outcome, fixed("Rescue (KO only)"), "Rescue")) %>%
  mutate(outcome = str_replace_all(outcome, "Non-Rescue", "No Difference"))

drug_library <- bind_rows(
  drug_library_2024,
  non_drug_library
)

# Function to find best matching drug name in library
find_matching_drug <- function(test_name, library_names) {
  # Special case for Carbidopa and Levodopa
  if (test_name %in% c("Carbidopa", "Levodopa", "Amantadine + TrkB Agonist")) {
    return(test_name)
  }

  # Convert to lowercase for case-insensitive matching
  test_name_lower <- tolower(test_name)
  library_names_lower <- tolower(library_names)

  # Find matches where test name is contained in library name or vice versa
  matches <- which(str_detect(library_names_lower, fixed(test_name_lower)) |
                     str_detect(test_name_lower, fixed(library_names_lower)))

  if (length(matches) > 0) {
    # Return the original (properly cased) library name
    return(library_names[matches[1]])
  } else {
    return(test_name)
  }
}

# match further specification to drug repurposing categories
combine_drc_fs <- function(drc, fs) {
  if (is.na(drc) || drc == "") return("")
  if (is.na(fs) || fs == "") return(drc)

  # Split both strings by comma
  drc_parts <- str_split(drc, ",\\s*")[[1]]
  fs_parts <- str_split(fs, ";\\s*")[[1]]

  # Ensure fs_parts has same length as drc_parts by padding with empty strings
  if (length(fs_parts) < length(drc_parts)) {
    fs_parts <- c(fs_parts, rep("", length(drc_parts) - length(fs_parts)))
  }

  # Combine corresponding parts
  combined_parts <- mapply(function(drc_part, fs_part) {
    if (fs_part == "") {
      return(drc_part)
    } else {
      return(paste0(drc_part, " (", fs_part, ")"))
    }
  }, drc_parts, fs_parts)

  # Join back together with commas
  return(paste(combined_parts, collapse = ", "))
}

get_repurposing_category <- function(drug_name, library_df) {
  # Special case for Carbidopa and Levodopa
  # if (drug_name %in% c("Carbidopa", "Levodopa")) {
  #   return("")
  # }

  # Otherwise look up in library
  # cat <- library_df$`Drug Repurposing Category`[library_df$`Generic Name` == drug_name][1]
  # return(cat)
  # Get the row index for the drug
  idx <- which(library_df$`Generic Name` == drug_name)[1]

  if (length(idx) == 0 || is.na(idx)) {
    return("")
  }

  # Get both DRC and FS values
  drc <- library_df$`Drug Repurposing Category`[idx]
  fs <- library_df$`Further Specification`[idx]

  # Combine them using the helper function
  return(combine_drc_fs(drc, fs))
}

get_drug_category <- function(drug_name, library_df) {
  # Special case for Carbidopa and Levodopa
  # if (drug_name %in% c("Carbidopa", "Levodopa")) {
  #   return("Decarboxylase inhibitor")
  # }

  # Otherwise look up in library
  reason <- library_df$`Drug category`[library_df$`Generic Name` == drug_name][1]
  return(reason)
}

convert_to_uM <- function(conc_str) {
  num <- as.numeric(str_extract(conc_str, "[0-9.]+"))
  unit <- str_extract(conc_str, "[μmu]M")

  if (unit == "mM") {
    return(num * 1000)
  } else {
    return(num)
  }
}

# Create final combined table
final_table <- drug_screens_combined %>%
  # Create a temporary column with numeric concentrations for sorting
  mutate(
    conc_numeric = sapply(concentration, convert_to_uM)
  ) %>%
  # Sort concentrations within each group
  arrange(conc_numeric, .by_group = TRUE) %>%
  # Add matching library information
  mutate(
    matched_name = sapply(drug_name,
                          find_matching_drug,
                          library_names = drug_library$`Generic Name`),
  ) %>%
  # Find lowest lethal concentration for each drug
  group_by(matched_name) %>%
  mutate(
    min_lethal_conc = if (any(outcome == "Lethal")) {
      min(conc_numeric[outcome == "Lethal"])
    } else {
      Inf
    }
  ) %>%
  # Filter out concentrations higher than lethal
  filter(conc_numeric <= min_lethal_conc) %>%
  # Continue with the transformations
  mutate(
    reason = map_chr(matched_name,
                     ~get_repurposing_category(.x, drug_library)),
    cat = map_chr(matched_name,
                  ~get_drug_category(.x, drug_library))
  ) %>%
  # Clean up and select final columns
  select(
    `Drug Name` = matched_name,
    `Drug Repurposing Category` = reason,
    `Drug Category` = cat,
    `Concentration` = concentration,
    Outcome = outcome
  ) %>%
  # Ensure rows are grouped together
  arrange(`Drug Name`)

# Save table as csv
write_csv(final_table, "table.csv", na = "NA")

# Create formatted GT table
final_gt_table <- final_table %>%
  gt(
    groupname_col = "Drug Name",
  ) %>%
  tab_header(
    title = md("**Drug Screen Results Summary**"),
  ) %>%
  cols_label(
    `Drug Name` = md("**Drug Name**"),
    `Drug Repurposing Category` = md("**Drug Repurposing Category**"),
    `Drug Category` = md("**Drug Category**"),
    `Concentration` = md("**Concentration**"),
    `Outcome` = md("**Outcome**")
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
      cell_fill(color = "#E5E5E5")
    ),
    locations = cells_body(
      columns = "Outcome",
      rows = Outcome == "Rescue"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#C3C3C3")
    ),
    locations = cells_body(
      columns = "Outcome",
      rows = Outcome == "Non-specific Improvement"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#A3A3A3")
    ),
    locations = cells_body(
      columns = "Outcome",
      rows = Outcome == "No Difference" | Outcome == "Decreased NS"
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#848484")
    ),
    locations = cells_body(
      columns = "Outcome",
      rows = Outcome == "Lethal"
    )
  ) %>%
  # cols_width(
  #   Outcome ~ px(120),
  # ) %>%
  # opt_table_font(font = "Arial") %>%
  tab_options(
    table.border.top.style = "hidden",
    row_group.as_column = TRUE
  ) %>%
  sub_missing(columns = everything(), rows = everything(), missing_text = "")

# Save gt table in real formats
gtsave(final_gt_table, "table.html")
gtsave(final_gt_table, "table.png")
gtsave(final_gt_table, "table.docx")
