library(tidyverse)

all_data <- read_csv("edmr_csvs/combined_data.csv")
View(all_data)
names(all_data)

unique(all_data$Parameter_Name)


all_nitrogen = all_data %>%
  filter(Parameter_Name == "Total Nitrogen" | Parameter_Name == "Total Phosphorus" | Parameter_Name == "Ammonia-Nitrogen" | Parameter_Name == "Nitrate-Nitrogen" | Parameter_Name == "Nitrate-Nitrite as N")

all_nitrogen |>
 mutate(numeric = if_else(is.na(suppressWarnings(as.numeric(DMR_Value))),
 "Non-numeric", "Numeric")) |>
 count(Parameter_Name, numeric) |>
 ggplot(aes(x = reorder(Parameter_Name, n, sum), y = n, fill = numeric)) +
 geom_col() +
 coord_flip() +
 scale_fill_viridis_d(end = 0.8) +
 theme_minimal() +
 labs(title = "Records per group", x = NULL, y = "Number of records", fill = NULL)

records_with_E = all_nitrogen |>
 filter(str_detect(DMR_Value, "E"))

records_with_FF = all_nitrogen |>
 filter(str_detect(DMR_Value, "FF"))

records_with_GG = all_nitrogen |>
 filter(str_detect(DMR_Value, "GG"))


records_with_EorFF = all_nitrogen |>
 filter(str_detect(DMR_Value, "E|FF"))


# Makes values that are "< X" numeric by removing the "<" and converting to numeric. Also removes "GG" values.
all_nitrogen_cleaned = all_nitrogen |>
  mutate(DMR_Value = if_else(str_detect(DMR_Value, "<"),
                              str_remove(DMR_Value, "<"),
                              DMR_Value)) |>
  filter(!str_detect(DMR_Value, "GG")) |>
  mutate(DMR_Value = as.numeric(DMR_Value))



summary(all_nitrogen_cleaned$DMR_Value)

all_nitrogen_cleaned |>
filter(DMR_Value > 0) |>
ggplot(aes(x = DMR_Value, fill = Parameter_Name)) +
geom_histogram(bins = 30, color = "white") +
scale_x_log10() +
scale_fill_viridis_d(guide = "none") +
facet_wrap(~ Parameter_Name, scales = "free") +
theme_minimal() +
labs(title = "Distribution of reported values by group",
x = "DMR value (log scale)", y = "Count")

# maximum row for dmr in each category
View(all_nitrogen_cleaned |>
  group_by(Parameter_Name) |>
  slice_max(DMR_Value, n = 5))

View(all_nitrogen[which(all_nitrogen$Statistical_Base_Code != "Daily Maximum"),])


# time plot, date stored as Monitoring_Period_Begin_Date, which is a character. Need to convert to date first.
# Get mean DMR for each of the four categories by Monitoring_Period_Begin_Date, formatted as 07/01/2021 (MM-DD-YYYY)
all_nitrogen_cleaned |>
  mutate(Monitoring_Period_Begin_Date = as.Date(Monitoring_Period_Begin_Date, format = "%m/%d/%Y")) |>
  group_by(Parameter_Name, Monitoring_Period_Begin_Date) |>
  summarise(mean_DMR = mean(DMR_Value, na.rm = TRUE)) |>
  ggplot(aes(x = Monitoring_Period_Begin_Date, y = mean_DMR, color = Parameter_Name)) +
  geom_line() +
  scale_y_log10() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Mean DMR value by Monitoring Period", x = "Monitoring Period Begin Date", y = "Mean DMR value (log scale)", color = NULL)
# Median
all_nitrogen_cleaned |>
  mutate(Monitoring_Period_Begin_Date = as.Date(Monitoring_Period_Begin_Date, format = "%m/%d/%Y")) |>
  group_by(Parameter_Name, Monitoring_Period_Begin_Date) |>
  summarise(median_DMR = median(DMR_Value, na.rm = TRUE)) |>
  ggplot(aes(x = Monitoring_Period_Begin_Date, y = median_DMR, color = Parameter_Name)) +
  geom_line() +
  scale_y_log10() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Median DMR value by Monitoring Period", x = "Monitoring Period Begin Date", y = "Median DMR value (log scale)", color = NULL)

# Group by every half year for this plot, e.g. 01/01/2021 to 06/30/2021, 07/01/2021 to 12/31/2021, etc.
all_nitrogen_cleaned |>
  mutate(Monitoring_Period_Begin_Date = as.Date(Monitoring_Period_Begin_Date, format = "%m/%d/%Y")) |>
  mutate(half_year = if_else(month(Monitoring_Period_Begin_Date) <= 6, 
                           paste0(year(Monitoring_Period_Begin_Date), "-H1"), 
                           paste0(year(Monitoring_Period_Begin_Date), "-H2"))) |>
  group_by(Parameter_Name, half_year) |>
  summarise(mean_DMR = mean(DMR_Value, na.rm = TRUE)) |>
  ggplot(aes(x = half_year, y = mean_DMR, color = Parameter_Name, group = Parameter_Name)) +
  geom_line() +
  scale_y_log10() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Mean DMR value by half year", x = "Half Year", y = "Mean DMR value (log scale)", color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Which centers had the biggest delta between any two Monitoring Periods? 
# e.g. for Total Nitrogen, which center had the biggest difference between monitoring period 1 and 2, 2 and 3, etc.
# Just the max over all the time differences
# Plot as a line graph with the x-axis as the Primary_Facility_ID and the y-axis as the max delta, colored by Parameter_Name

# Top 5 Primary_Facility_IDs with the highest max delta for each Parameter_Name
all_nitrogen_cleaned |>
  mutate(Monitoring_Period_Begin_Date = as.Date(Monitoring_Period_Begin_Date, format = "%m/%d/%Y")) |>
  group_by(Parameter_Name, Primary_Facility_ID) |>
  summarise(max_delta = max(DMR_Value) - min(DMR_Value)) |>
  slice_max(max_delta, n = 5) |>
  ggplot(aes(x = reorder(Primary_Facility_ID, max_delta), y = max_delta, fill = Parameter_Name)) +
  geom_col() +
  scale_y_log10() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Top 5 Primary Facility IDs with highest max delta DMR value by Parameter Name", x = "Primary Facility ID", y = "Max delta DMR value (log scale)", fill = NULL) +
  coord_flip()

# Between any two reporting periods instead of max - min
all_nitrogen_cleaned |>
  mutate(Monitoring_Period_Begin_Date = as.Date(Monitoring_Period_Begin_Date, format = "%m/%d/%Y")) |>
  group_by(Parameter_Name, Primary_Facility_ID) |>
  arrange(Monitoring_Period_Begin_Date) |>
  summarise(max_delta = max(DMR_Value - lag(DMR_Value), na.rm = TRUE)) |>
  slice_max(max_delta, n = 5) |>
  ggplot(aes(x = reorder(Primary_Facility_ID, max_delta), y = max_delta, fill = Parameter_Name)) +
  geom_col() +
  scale_y_log10() +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(title = "Top 5 Primary Facility IDs with highest max delta DMR value between reporting periods by Parameter Name", x = "Primary Facility ID", y = "Max delta DMR value (log scale)", fill = NULL) +
  coord_flip()

# Spatial EDA
pad <- 0.02
lat_lim <- range(all_nitrogen_cleaned$Latitude, na.rm = TRUE) + c(-pad, pad)
lon_lim <- range(all_nitrogen_cleaned$Longitude, na.rm = TRUE) + c(-pad, pad)

nitrogen_map <- all_nitrogen_cleaned |>
filter(!is.na(Latitude), !is.na(Longitude), Longitude < -76)

facility_nitrogen <- nitrogen_map |>
filter(DMR_Value > 0) |>
group_by(Parameter_Name, Primary_Facility_ID, Latitude, Longitude) |>
summarise(median_value = median(DMR_Value), n = n(), .groups = "drop")

ggplot(facility_nitrogen, aes(x = Longitude, y = Latitude, color = median_value)) +
geom_point(size = 2, alpha = 0.8) +
scale_color_viridis_c(trans = "log10") +
coord_quickmap(xlim = c(lon_lim[1], -79), ylim = lat_lim) +
facet_wrap(~ Parameter_Name) +
theme_minimal() +
labs(title = "Median reported value by facility",
color = "Median (mg/L)")


# Numerical value of less thans versus observations that were already numbers
all_nitrogen |>
  mutate(numeric = if_else(is.na(suppressWarnings(as.numeric(DMR_Value))),
                           "Non-numeric", "Numeric")) |>
  count(Parameter_Name, numeric) |>
  ggplot(aes(x = reorder(Parameter_Name, n, sum), y = n, fill = numeric)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_d(end = 0.8) +
  theme_minimal() +
  labs(title = "Records per group", x = NULL, y = "Number of records", fill = NULL)

# Compare average of "< X" versus X observations by category

all_nitrogen |>
  mutate(DMR_Value = if_else(str_detect(DMR_Value, "<"),
                             str_remove(DMR_Value, "<"),
                             DMR_Value)) |>
  filter(!str_detect(DMR_Value, "GG")) |>
  mutate(DMR_Value = as.numeric(DMR_Value)) |>
  group_by(Parameter_Name) |>
  summarise(mean_value = mean(DMR_Value, na.rm = TRUE),
            mean_less_than_value = mean(as.numeric(str_remove(DMR_Value[str_detect(all_nitrogen$DMR_Value, "<")], "<")), na.rm = TRUE)) |>
  pivot_longer(cols = c(mean_value, mean_less_than_value), names_to = "type", values_to = "mean") |>
  ggplot(aes(x = reorder(Parameter_Name, mean), y = mean, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(end = 0.8) +
  theme_minimal() +
  labs(title = "Mean reported value by group", x = NULL, y = "Mean value (mg/L)", fill = NULL) +
  coord_flip()

# medians

all_nitrogen |>
  mutate(DMR_Value = if_else(str_detect(DMR_Value, "<"),
                             str_remove(DMR_Value, "<"),
                             DMR_Value)) |>
  filter(!str_detect(DMR_Value, "GG")) |>
  mutate(DMR_Value = as.numeric(DMR_Value)) |>
  group_by(Parameter_Name) |>
  summarise(median_value = median(DMR_Value, na.rm = TRUE),
            median_less_than_value = median(as.numeric(str_remove(DMR_Value[str_detect(all_nitrogen$DMR_Value, "<")], "<")), na.rm = TRUE)) |>
  pivot_longer(cols = c(median_value, median_less_than_value), names_to = "type", values_to = "median") |>
  ggplot(aes(x = reorder(Parameter_Name, median), y = median, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(end = 0.8) +
  theme_minimal() +
  labs(title = "Median reported value by group", x = NULL, y = "Median value (mg/L)", fill = NULL) +
  coord_flip()

# Ridge plot of those two distributions (the numeric and less-than values) for 4 parameters

