## -----------------------------------------------------------------------------
#| label: required_packages
#| message: false
#| warning: false

library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(knitr)


## -----------------------------------------------------------------------------
#| label: fig-inf
#| fig-cap: "Heat map of inflationary impact on key itens over 2023,
#| published by the
#|   Straits Times."
#| out-width: 60%

include_graphics("images/price_index.png")


## -----------------------------------------------------------------------------
#| label: read-data
#| message: false
#| warning: false


# Load the dataset
dataset <- read_csv("M212911.csv", skip = 10)
dataset <- dataset[1:152, ]
dataset


## -----------------------------------------------------------------------------
#| label: preprocess-data
#| message: false
#| warning: false

# Convert relevant columns to numeric
dataset <- dataset |>
  mutate(across(matches("^2019|^2020|^2021|^2022|^2023"),
                ~ as.numeric(as.character(.))))

month_data <- dataset |>
  filter(`Data Series` %in% c("Food", "Food Excl Food Serving Services",
                              "Clothing & Footwear", "Housing & Utilities",
                              "Household Durables & Services", "Health Care",
                              "Transport", "Communication",
                              "Recreation & Culture", "Education",
                              "Miscellaneous Goods & Services", "All Items")) |>
  select(`Data Series`, matches("^2019|^2020|^2021|^2022|^2023"))
month_data


## -----------------------------------------------------------------------------
#| label: pivot_longer
#| message: false
#| warning: false

# Pivot longer for heatmap generation
long_data <- month_data |>
  pivot_longer(cols = matches("^2019|^2020|^2021|^2022|^2023"),
               names_to = "Year_Month", values_to = "Percent_Change") |>
  separate(Year_Month, into = c("Year", "Month"), sep = " ") |>
  mutate(Year = as.integer(Year),
         # Add "Overall_Avg" as the 14th month
         Month = factor(Month, levels = c(month.abb, "Overall_Avg")),
         Month_Index = as.numeric(factor(Month, levels = c(month.abb,
                                                           "Overall_Avg"))),
         Year_Month_Index = paste(Year, sprintf("%02d",
                                                Month_Index), sep = "-"))
long_data


## -----------------------------------------------------------------------------
#| label: overall_averages
#| message: false
#| warning: false
# Calculate the overall average for each data series from 2019 to 2023
overall_averages <- long_data |>
  filter(Year >= 2019 & Year <= 2023) |>
  group_by(`Data Series`) |>
  summarize(Percent_Change = mean(Percent_Change, na.rm = TRUE),
            .groups = "drop") |>
  mutate(Year = 2023, Month = "Overall_Avg", Month_Index = 14,
         Year_Month_Index = "2023-14")  # Use month index 14 for "Overall_Avg"
overall_averages


## -----------------------------------------------------------------------------
#| label: add_gaps
#| message: false
#| warning: false


# Add a small gap before and after "Overall_Avg"
gap_data_before_avg <- overall_averages |>
  mutate(Month = "Gap", Month_Index = 13, Year_Month_Index = "2023-13",
         Percent_Change = NA)

gap_data_after_avg <- overall_averages |>
  mutate(Month = "Gap", Month_Index = 15, Year_Month_Index = "2023-15",
         Percent_Change = NA)
gap_data_after_avg


## -----------------------------------------------------------------------------
#| label: color_scale
#| message: false
#| warning: false


# Combine the original data with the overall averages and the gaps
combined_data <- bind_rows(long_data, overall_averages,
                           gap_data_before_avg, gap_data_after_avg) |>
  arrange(Year, Month_Index)


color_scale <- scale_fill_gradientn(
  # Use the RdYlBu palette with 9 colors in reverse order
  colors = rev(brewer.pal(9, "RdYlBu")),
  values = scales::rescale(c(min(combined_data$Percent_Change, na.rm = TRUE),
                             -3, 0, 3, 6, 9, 12,
                             max(combined_data$Percent_Change, na.rm = TRUE))),
  # Define breakpoints dynamically
  limits = c(min(combined_data$Percent_Change, na.rm = TRUE),
             max(combined_data$Percent_Change, na.rm = TRUE)),
  breaks = seq(floor(min(combined_data$Percent_Change, na.rm = TRUE)),
               ceiling(max(combined_data$Percent_Change, na.rm = TRUE)),
               by = 3),  # Adjust breaks dynamically
  labels = as.character(seq(floor(min(combined_data$Percent_Change,
                                      na.rm = TRUE)),
                            ceiling(max(combined_data$Percent_Change,
                                        na.rm = TRUE)),
                            by = 3)),  # Adjust labels dynamically
  na.value = "white" # Set color for NA values
)


## -----------------------------------------------------------------------------
#| label: adjust_series_name
#| message: false
#| warning: false


# Adjust the data series names for better readability
combined_data <- combined_data |>
  mutate(`Data Series` = case_when(
    `Data Series` ==
      "Miscellaneous Goods & Services" ~ "Miscellaneous\nGoods & Services",
    `Data Series` ==
      "Household Durables & Services" ~ "Household Durables\n& Services",
    `Data Series` ==
      "Food Excl Food Serving Services" ~ "Food Excl Food\nServing Services",
    TRUE ~ as.character(`Data Series`)
  ))
combined_data


## -----------------------------------------------------------------------------
#| label: breaks_labels
#| message: false
#| warning: false


# Generate the x-axis breaks and labels to include January, July, and
# "Overall_Avg" for each year
x_breaks <- combined_data |>
  filter(Month %in% c("Jan", "Jul", "Overall_Avg", "Gap")) |>
  mutate(Year_Month_Index = paste(Year, sprintf("%02d", Month_Index),
                                  sep = "-")) |>
  distinct(Year_Month_Index, .keep_all = TRUE) |>
  arrange(Year, Month_Index) |>
  pull(Year_Month_Index)

x_labels <- combined_data |>
  filter(Month %in% c("Jan", "Jul", "Overall_Avg", "Gap")) |>
  mutate(Year_Month_Index = paste(Year, sprintf("%02d", Month_Index),
                                  sep = "-")) |>
  distinct(Year_Month_Index, .keep_all = TRUE) |>
  arrange(Year, Month_Index) |>
  mutate(Label = case_when(
    Month == "Overall_Avg" ~ paste("Overall Avg"),
    Month == "Gap" ~ "",
    Month == "Jan" ~ paste("Jan", Year),
    Month == "Jul" ~ paste("Jul", Year),
    TRUE ~ month.abb[Month_Index]
  )) |>
  pull(Label)

