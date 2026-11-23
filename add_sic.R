library(tidyverse)

sic <- read_csv("permitted_facilities.csv", col_types = cols(.default = col_character())) |>
  distinct(FACILITY_ID, .keep_all = TRUE) |>
  select(Primary_Facility_ID = FACILITY_ID, Primary_SIC_Code = PRIMARY_SIC_CODE)

read_csv("edmr_csvs/combined_data.csv", col_types = cols(.default = col_character())) |>
  left_join(sic, by = "Primary_Facility_ID") |>
  relocate(Primary_SIC_Code, .after = Primary_Facility_Name) |>
  write_csv("edmr_csvs/combined_data_with_SIC.csv", na = "")