library(tidyverse)
all_data <- read_csv("edmr_csvs/combined_data.csv")

flags <- list(E = "E", FF = "FF", GG = "GG", E_FF = c("E", "FF"), E_FF_GG = c("E", "FF", "GG"))

iwalk(flags, \(v, nm) all_data |>
        filter(str_trim(DMR_Value) %in% v) |>
        write_csv(paste0(nm, "_records.csv")))