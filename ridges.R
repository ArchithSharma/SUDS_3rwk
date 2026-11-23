library(tidyverse)
library(ggridges)

all_data <- read_csv("edmr_csvs/combined_data.csv")

plot_data <- all_data |>
  mutate(
    DMR_Value = str_trim(DMR_Value),
    # flag BEFORE stripping the "<"
    Group = if_else(str_starts(DMR_Value, "<"),
                    "Below detection (<X)", "Measured value"),
    DMR_Value = str_trim(str_remove(DMR_Value, "<"))
  ) |>
  filter(!DMR_Value %in% c("E", "FF", "GG")) |>
  mutate(DMR_Value = suppressWarnings(as.numeric(DMR_Value))) |>
  filter(!is.na(DMR_Value), DMR_Value > 0) |>   # log scale needs > 0
  # density needs at least 2 points per group within each parameter
  group_by(Parameter_Name, Group) |>
  filter(n() >= 50) |>
  ungroup()

ggplot(plot_data, aes(x = DMR_Value, y = Group, fill = Group)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2) +
  facet_wrap(~Parameter_Name, scales = "free", ncol = 4) +
  scale_x_log10() +
  scale_fill_viridis_d(end = 0.8) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        strip.text = element_text(size = 8)) +
  labs(title = "Distribution of DMR values: measured vs. below-detection (<X)",
       x = "DMR value (log scale)", y = NULL, fill = NULL)