############################################################
### Loading libraries
############################################################

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats) 
library(treemapify)
library(ggplotify)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)
library(webshot2)
library(lubridate)
library(ggh4x)
library(tidycensus)
library(tigris)
library(ggiraph)
library(htmlwidgets)
library(sf)
library(stringr)

############################################################
### Setting up directories
############################################################

setwd("YOUR DIRECTORY GOES HERE")

############################################################
### Loading in and cleaning data
############################################################

# United States unemployment figures
us_ur <- read_csv("Data/bls_us_ur.csv") |> 
  pivot_longer(cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                           "Aug", "Sep", "Oct", "Nov", "Dec"),
               names_to = "month",
               values_to = "ur") |> 
  rename(year = Year) |> 
  mutate(date = ymd(paste(year, month, "01", sep = "-")),
         area = "us") |> 
  select(date, area, ur, -year, -month) |> 
  filter(date < "2025-06-30")

# United States labor force
us_lf <- read_csv("Data/bls_us_lf.csv") |> 
  rename(date = Month,
         lf_rate = Total) |> 
  mutate(date = ymd(date),
         area = "us")

# Colorado employment figures
co_data <- read_csv("Data/bls_colorado.csv") |> 
  mutate(`labor force participation rate` = if_else(str_sub(`labor force participation rate`, -3, -1) == "(R)" | 
                                         str_sub(`labor force participation rate`, -3, -1) == "(P)",
                                       str_sub(`labor force participation rate`, 1, -4),
                                       `labor force participation rate`),
         `employment-population ratio` = if_else(str_sub(`employment-population ratio`, -3, -1) == "(R)" | 
                                                      str_sub(`employment-population ratio`, -3, -1) == "(P)",
                                                    str_sub(`employment-population ratio`, 1, -4),
                                                    `employment-population ratio`),
         `labor force` = if_else(str_sub(`labor force`, -3, -1) == "(R)" | 
                                                   str_sub(`labor force`, -3, -1) == "(P)",
                                                 str_sub(`labor force`, 1, -4),
                                                 `labor force`),
         employment = if_else(str_sub(employment, -3, -1) == "(R)" | 
                                str_sub(employment, -3, -1) == "(P)",
                              str_sub(employment, 1, -4),
                              employment),
         unemployment = if_else(str_sub(unemployment, -3, -1) == "(R)" | 
                                str_sub(unemployment, -3, -1) == "(P)",
                              str_sub(unemployment, 1, -4),
                              unemployment),
         `unemployment rate` = if_else(str_sub(`unemployment rate`, -3, -1) == "(R)" | 
                                        str_sub(`unemployment rate`, -3, -1) == "(P)",
                                      str_sub(`unemployment rate`, 1, -4),
                                      `unemployment rate`),
         `labor force participation rate` = as.numeric(`labor force participation rate`),
         `employment-population ratio` = as.numeric(`employment-population ratio`), 
         `labor force` = as.numeric(`labor force`),
         employment = as.numeric(employment),
         unemployment = as.numeric(unemployment),
         `unemployment rate` = as.numeric(`unemployment rate`),
         date = ymd(paste(Year, Period, "01", sep = "-")),
         area = "co") |> 
  select(date, area, everything(), -Year, -Period) |> 
  rename(lf_rate = "labor force participation rate",
         emp_pop_ratio = "employment-population ratio",
         lf = "labor force",
         ur = "unemployment rate")

us_co_data <- bind_rows(co_data, us_ur)

### Regional employment figures

# Mutate function
mutate_function <- function(data) {
  
  mutate(data,
         `labor force` = if_else(str_sub(`labor force`, -3, -1) == "(T)" | 
                                   str_sub(`labor force`, -3, -1) == "(P)",
                                 str_sub(`labor force`, 1, -4),
                                 `labor force`),
         employment = if_else(str_sub(employment, -3, -1) == "(T)" | 
                                str_sub(employment, -3, -1) == "(P)",
                              str_sub(employment, 1, -4),
                              employment),
         unemployment = if_else(str_sub(unemployment, -3, -1) == "(T)" | 
                                  str_sub(unemployment, -3, -1) == "(P)",
                                str_sub(unemployment, 1, -4),
                                unemployment),
         `unemployment rate` = if_else(str_sub(`unemployment rate`, -3, -1) == "(T)" | 
                                         str_sub(`unemployment rate`, -3, -1) == "(P)",
                                       str_sub(`unemployment rate`, 1, -4),
                                       `unemployment rate`),
         `labor force` = as.numeric(`labor force`),
         employment = as.numeric(employment),
         unemployment = as.numeric(unemployment),
         `unemployment rate` = as.numeric(`unemployment rate`),
         date = ymd(paste(Year, Period, "01", sep = "-"))) |> 
    select(date, everything(), -Year, -Period) |> 
    rename(lf = "labor force",
           ur = "unemployment rate")
}

# Boulder
boulder_data_raw <- read_csv("Data/bls_boulder.csv")
boulder_data <- mutate_function(boulder_data_raw) |> 
  mutate(area = "boulder") |> 
  select(date, area, everything())

# Colorado Springs
cospgs_data_raw <- read_csv("Data/bls_cosprings.csv")
cospgs_data <- mutate_function(cospgs_data_raw) |> 
  mutate(area = "cospgs") |> 
  select(date, area, everything())

# Denver-Aurora-Centennial
den_data_raw <- read_csv("Data/bls_denaurcen.csv")
den_data <- mutate_function(den_data_raw) |> 
  mutate(area = "denver") |> 
  select(date, area, everything())

# Fort Collins
ftco_data_raw <- read_csv("Data/bls_ftcollins.csv")
ftco_data <- mutate_function(ftco_data_raw) |> 
  mutate(area = "ftco") |> 
  select(date, area, everything())

# Grand Junction
grand_data_raw <- read_csv("Data/bls_grandjunction.csv")
grand_data <- mutate_function(grand_data_raw) |> 
  mutate(area = "grand") |> 
  select(date, area, everything())

# Greeley
greeley_data_raw <- read_csv("Data/bls_greeley.csv")
greeley_data <- mutate_function(greeley_data_raw) |> 
  mutate(area = "greeley") |> 
  select(date, area, everything())

# Pueblo
pueblo_data_raw <- read_csv("Data/bls_pueblo.csv")
pueblo_data <- mutate_function(pueblo_data_raw) |> 
  mutate(area = "pueblo") |> 
  select(date, area, everything())

### Industries by employment figures
co_industry <- read_csv("Data/bls_industry.csv") |> 
  select(-"2025-06-01") |> 
  rename(employment = "2025-05-01") |> 
  mutate(industry = if_else(str_sub(industry, -3, -1) == "(3)",
                           str_sub(industry, 1, -4),
                           industry)) |> 
  filter(industry != "Total employment",
         industry != "Total Nonfarm")

### Merging datasets
co_areas_data <- boulder_data |> 
  rbind(cospgs_data, den_data, ftco_data, grand_data, greeley_data, pueblo_data)

### Cleaning environment
rm(boulder_data_raw, cospgs_data_raw, den_data_raw, ftco_data_raw, grand_data_raw,
   greeley_data_raw, pueblo_data_raw, boulder_data, cospgs_data, den_data, ftco_data, 
   grand_data, greeley_data, pueblo_data, co_data, us_ur)

############################################################
### Colorado vs national unemployment (2000 to today)
############################################################

fig1 <- us_co_data |> 
  mutate(area = recode(area, 
                       "us" = "United States",
                       "co" = "Colorado")) |> 
  ggplot() +
  geom_line(mapping = aes(x = date,
                          y = ur,
                          color = area),
            size = 1) + 
  scale_color_manual(values = c("Colorado" = "red2",
                                "United States" = "navyblue")) + 
  scale_y_continuous(limits = c(0, 15),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "1. Colorado's Unemployment Rate, Compared \nto National Average",
       subtitle = "Colorado has historically beaten national average.", 
       color = "") +
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig1.png", fig1, width = 10, height = 8, units = "in", dpi = 300)

us_co_data |> 
  pivot_wider(id_cols = date,
              names_from = area,
              values_from = ur) |> 
  mutate(co_over_us = if_else(co > us, 1, 0)) |> 
  filter(co_over_us == 1) |> 
  arrange(desc(date)) |> 
  print(n = 50)

############################################################
### Colorado vs national unemployment (2022 to today)
############################################################

fig2 <- us_co_data |> 
  filter(date > "2021-12-31") |> 
  mutate(area = recode(area, 
                       "us" = "United States",
                       "co" = "Colorado")) |> 
  ggplot() +
  geom_line(mapping = aes(x = date,
                          y = ur,
                          color = area),
            size = 1.5) + 
  scale_color_manual(values = c("Colorado" = "red2",
                                "United States" = "navyblue")) + 
  scale_y_continuous(limits = c(2.5, 5),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "2. Closeup of Post-pandemic Unemployment \nRate",
       subtitle = "Colorado eclipsed the national average on January 2024.", 
       color = "") +
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig2.png", fig2, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Colorado labor force participation rate
############################################################

fig3 <- us_co_data |> 
  filter(area == "co") |> 
  select(date, area, lf_rate) |>
  rbind(us_lf) |> 
  mutate(area = recode(area,
                       "co" = "Colorado",
                       "us" = "United States")) |> 
  ggplot() + 
  geom_line(mapping = aes(x = date,
                          y = lf_rate,
                          color = area),
            size = 1.25) + 
  scale_color_manual(values = c("Colorado" = "darkgoldenrod2",
                                "United States" = "grey50")) + 
  scale_y_continuous(limits = c(58, 75),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "3. Labor Force Participation Rate in Colorado, \nComparted to the National Average",
       subtitle = "Despite a decline, the labor for participation rate in Colorado \nhas been greater than the national average.",
       color = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig3.png", fig3, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Colorado employment growth
############################################################

# 2010 to today
us_co_data |> 
  filter(area == "co",
         date == "2010-01-01" | date == "2019-12-01") |> 
  arrange(date) |> 
  mutate(pct_change = 100 * (employment - lag(employment)) / lag(employment))

fig4 <- us_co_data |> 
  filter(area == "co") |> 
  arrange(date) |> 
  mutate(pct_change = 100 * (employment - lag(employment)) / lag(employment),
         fill_code = if_else(pct_change >= 0, "pos", "neg"))  |> 
  filter(date != "2020-04-01") |> 
  ggplot() + 
  geom_bar(mapping = aes(x = date,
                         y = pct_change,
                         fill = fill_code),
           stat = "identity") + 
  scale_fill_manual(values = c("pos" = "navyblue",
                               "neg" = "red2")) + 
  scale_y_continuous(limits = c(-0.5, 3),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "4. Employment Growth in Colorado, \n2000 to Today",
       subtitle = "Quick recovery immediately after pandemic, compared to the \nGreat Recession.",
       caption = "Note: April 2020 figure omitted due to extreme value.",
       fill = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "none")

ggsave("Graphs/fig4.png", fig4, width = 10, height = 8, units = "in", dpi = 300)

# 2010 - 2019
fig5 <- us_co_data |> 
  filter(area == "co",
         date > "2009-12-31",
         date < "2020-01-01") |> 
  arrange(date) |> 
  mutate(pct_change = 100 * (employment - lag(employment)) / lag(employment),
         fill_code = if_else(pct_change >= 0, "pos", "neg"))  |> 
  filter(date != "2020-04-01") |> 
  ggplot() + 
  geom_bar(mapping = aes(x = date,
                         y = pct_change,
                         fill = fill_code),
           stat = "identity") + 
  scale_fill_manual(values = c("pos" = "navyblue",
                               "neg" = "red2")) + 
  scale_y_continuous(limits = c(-0.1, 0.5),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "5. Pre-pandemic Employment Growth in \nColorado",
       subtitle = "A decade of growth.",
       fill = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "none")

ggsave("Graphs/fig5.png", fig5, width = 10, height = 8, units = "in", dpi = 300)
  
# 2022 to today
fig6 <- us_co_data |> 
  filter(area == "co",
         date > "2021-12-31") |> 
  arrange(date) |> 
  mutate(pct_change = 100 * (employment - lag(employment)) / lag(employment),
         fill_code = if_else(pct_change >= 0, "pos", "neg"))  |> 
  filter(date != "2020-04-01") |> 
  ggplot() + 
  geom_bar(mapping = aes(x = date,
                         y = pct_change,
                         fill = fill_code),
           stat = "identity") + 
  scale_fill_manual(values = c("pos" = "navyblue",
                               "neg" = "red2")) +
  scale_y_continuous(limits = c(-0.15, 0.55),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "6. Post-pandemic Employment Growth in \nColorado",
       subtitle = "A rocky recovery.",
       fill = "") + 
  theme_fivethirtyeight() + 
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "none")

ggsave("Graphs/fig6.png", fig6, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Metropolitan areas by unemployment rate
############################################################

co_areas_data |> 
  mutate(area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "grand" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  filter(date < "2010-01-01") |>
  group_by(area) |> 
  slice_max(order_by = ur,
            n = 1,
            with_ties = FALSE) |> 
  ungroup()

fig7 <- co_areas_data |> 
  mutate(area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "grand" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  filter(date < "2010-01-01") |> 
  ggplot() + 
  geom_line(mapping = aes(x = date,
                          y = ur,
                          color = area),
            size = 1.25,
            alpha = 0.7) + 
  scale_color_manual(values = c("Boulder" = "goldenrod2",
                                "Denver-Aurora-Centennial" = "red2",
                                "Grand Junction" = "navyblue",
                                "Colorado Springs" = "chartreuse4",
                                "Fort Collins-Loveland" = "deepskyblue",
                                "Greeley" = "darkorange2",
                                "Pueblo" = "grey20")) + 
  scale_y_continuous(limits = c(2, 13),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "7. Unemployment Rates in 2000-2010, \nby Metropolitan Area",
       subtitle = "",
       color = "",
       caption = "Grand Junction omitted due to unavailability of data.") + 
  theme_fivethirtyeight() +
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig7.png", fig7, width = 10, height = 8, units = "in", dpi = 300)

# Pre-pandemic
fig8 <- co_areas_data |> 
  mutate(area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "grand" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  filter(date > "2009-12-31", 
         date < "2020-01-01") |> 
  ggplot() + 
  geom_line(mapping = aes(x = date,
                          y = ur,
                          color = area),
            size = 1.25,
            alpha = 0.7) + 
  scale_color_manual(values = c("Boulder" = "goldenrod2",
                                "Denver-Aurora-Centennial" = "red2",
                                "Grand Junction" = "navyblue",
                                "Colorado Springs" = "chartreuse4",
                                "Fort Collins-Loveland" = "deepskyblue",
                                "Greeley" = "darkorange2",
                                "Pueblo" = "grey20")) + 
  scale_y_continuous(limits = c(2, 13),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "8. Pre-pandemic Unemployment Rates, \nby Metropolitan Area",
       subtitle = "Declining unemployment rates since the heights of the Great Recession.",
       color = "") + 
  theme_fivethirtyeight() +
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig8.png", fig8, width = 10, height = 8, units = "in", dpi = 300)

# Pandemic and post-pandemic
fig9 <- co_areas_data |> 
  mutate(area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "grand" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  filter(date > "2019-12-31") |> 
  ggplot() + 
  geom_line(mapping = aes(x = date,
                          y = ur,
                          color = area),
            size = 1.25,
            alpha = 0.7) + 
  scale_color_manual(values = c("Boulder" = "goldenrod2",
                                "Denver-Aurora-Centennial" = "red2",
                                "Grand Junction" = "navyblue",
                                "Colorado Springs" = "chartreuse4",
                                "Fort Collins-Loveland" = "deepskyblue",
                                "Greeley" = "darkorange2",
                                "Pueblo" = "grey20")) + 
  scale_y_continuous(limits = c(2, 13),
                     labels = scales::label_percent(scale = 1)) + 
  labs(title = "9. Post-pandemic Unemployment Rates, \nby Metropolitan Area",
       subtitle = "",
       color = "") + 
  theme_fivethirtyeight() +
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig9.png", fig9, width = 10, height = 8, units = "in", dpi = 300)

# Difference between Pueblo and the rest of the state
co_areas_data |> 
  mutate(area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "grand" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  select(date, area, ur) |> 
  filter(date == "2025-05-01") |>
  select(area, ur) |> 
  rename("Region" = "area",
         "Unemployment Rate" = "ur") |> 
  gt() |> 
  tab_header(title = md("9a. Metropolitan Areas by Unemployment Rate"),
             subtitle = md("As of May 2025")) |>   opt_vertical_padding(scale = 0) |> 
  opt_table_font(font = list(google_font("Lato"),
                             default_fonts())) |>
  opt_align_table_header(align = "left") |> 
  tab_style(style = cell_text(size = px(16),
                              weight = "bold"),
            locations = cells_column_labels(everything())) |> 
  tab_style(style = cell_text(size = px(26),
                              weight = "bold"),
            locations = cells_title(groups = "title")) |> 
  tab_style(style = cell_text(size = px(20)),
            locations = cells_title(groups = "subtitle")) |> 
  gtsave("Graphs/fig9a.png")

############################################################
### Metropolitan areas by employment percentage growth
############################################################

fig10 <- co_areas_data |> 
  filter(date == "2015-05-01" | date == "2025-05-01") |> 
  group_by(area) |> 
  arrange(date) |> 
  mutate(pct_change = 100 * (employment - lag(employment)) / lag(employment),
         label = paste0(round(pct_change, 1), "%")) |> 
  filter(date != "2010-05-01") |> 
  ungroup() |> 
  mutate(area = fct_reorder(area, pct_change),
         end = 0,
         area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "grand" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  ggplot() + 
  geom_segment(mapping = aes(x = area, 
                             xend = area,
                             y = end,
                             yend = pct_change,
                             color = area),
               size = 10) + 
  geom_point(mapping = aes(x = area,
                           y = pct_change,
                           color = area),
             size = 9) + 
  geom_text(mapping = aes(x = area,
                          y = pct_change,
                          label = label),
            color = "white",
            hjust = 0.9,
            fontface = "bold",
            size = 5) + 
  scale_color_manual(values = c("Boulder" = "goldenrod2",
                                "Denver-Aurora-Centennial" = "red2",
                                "Colorado Springs" = "chartreuse4",
                                "Fort Collins-Loveland" = "deepskyblue",
                                "Grand Junction" = "navyblue",
                                "Greeley" = "darkorange2",
                                "Pueblo" = "grey20")) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) + 
  coord_flip() + 
  theme_fivethirtyeight() +
  labs(title = "10. Employment Growth Over One Decade, \nby Metropolitan Area",
       subtitle = "Colorado Springs experienced the most growth, while Pueblo grew \nminusculely.",
       y = "Growth in employment",
       color = "") +
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = -10)),
        axis.text.y = element_text(margin = margin(r = -10)),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"))

us_co_data |> 
  filter(date == "2015-05-01" | date == "2025-05-01",
         area == "co") |> 
  mutate(pct_change = 100 * (employment - lag(employment)) / lag(employment)) # average CO = 15.5%

ggsave("Graphs/fig10.png", fig10, width = 12, height = 8, units = "in", dpi = 300)

############################################################
### Metropolitan areas by key industries
############################################################

fig11 <- co_industry |> 
  mutate(area = recode(area,
                       "boulder" = "Boulder",
                       "denver" = "Denver-Aurora-Centennial",
                       "gj" = "Grand Junction",
                       "cospgs" = "Colorado Springs",
                       "ftco" = "Fort Collins-Loveland",
                       "greeley" = "Greeley",
                       "pueblo" = "Pueblo")) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = factor(area),
                         y = employment,
                         fill = industry),
           stat = "identity",
           position = "fill") + 
  scale_fill_manual(values = c("Mining, Logging, and Construction" = "navyblue",
                               "Manufacturing" = "red2",
                               "Trade, Transportation, and Utilities" = "goldenrod2",
                               "Information" = "darkolivegreen3",
                               "Financial Activities" = "deepskyblue2",
                               "Professional and Business Services" = "darkorange2",
                               "Education and Health Services" = "darkgreen",
                               "Leisure and Hospitality" = "tan2",
                               "Other Services" = "tomato4",
                               "Government" = "gray40")) + 
  coord_flip() +
  theme_fivethirtyeight() + 
  labs(title = "11. Metropolitan Areas by Number of Non-farm Jobs per Industry",
       subtitle = "",
       fill = "") +
  theme(text = element_text(size = 20),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(margin = margin(r = -10)))

ggsave("Graphs/fig11.png", fig11, width = 18, height = 8, units = "in", dpi = 300)

# Function
industry_function <- function(code_num) {
  
  co_industry |> 
    mutate(code = case_when(
      area == "boulder" ~ 1,
      area == "Denver" ~ 2,
      area == "gj" ~ 3,
      area == "cospgs" ~ 4,
      area == "ftco" ~ 5,
      area == "greeley" ~ 6,
      area == "pueblo" ~ 7)) |> 
    filter(code == code_num) |> 
    mutate(area = recode(area,
                         "boulder" = "Boulder",
                         "denver" = "Denver-Aurora-Centennial",
                         "gj" = "Grand Junction",
                         "cospgs" = "Colorado Springs",
                         "ftco" = "Fort Collins-Loveland",
                         "greeley" = "Greeley",
                         "pueblo" = "Pueblo"),
           total_emp = sum(employment),
           pct = employment / total_emp,
           pct_label = paste0(round(pct * 100, 1), "%"),
           pct_label = if_else(pct > 0.07, pct_label, ""),
           industry = fct_reorder(industry, pct),
           total_emp_label = paste0(round(total_emp, 3), "k")) |> 
    arrange(desc(pct)) |> 
    ggplot() + 
    geom_bar(mapping = aes(x = factor(area),
                           y = employment,
                           fill = industry),
             stat = "identity",
             position = "fill") + 
    geom_text(mapping = aes(x = factor(area),
                            y = pct,
                            label = pct_label),
              position = position_stack(vjust = 0.5),
              size = 5,
              color = "white",
              fontface = "bold") + 
    geom_text(mapping = aes(x = 1.52,
                            y = 0.18,
                            label = "Total number of jobs:"),
              size = 6) + 
    geom_text(mapping = aes(x = 1.52,
                            y = 0.37,
                            label = total_emp_label),
              size = 6,
              fontface = "bold") + 
    scale_fill_manual(values = c("Mining, Logging, and Construction" = "navyblue",
                                 "Manufacturing" = "red2",
                                 "Trade, Transportation, and Utilities" = "goldenrod1",
                                 "Information" = "darkolivegreen3",
                                 "Financial Activities" = "deepskyblue2",
                                 "Professional and Business Services" = "darkorange3",
                                 "Education and Health Services" = "darkgreen",
                                 "Leisure and Hospitality" = "tan1",
                                 "Other Services" = "tomato4",
                                 "Government" = "gray40")) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip() +
    theme_fivethirtyeight() + 
    labs(fill = "") +
    theme(text = element_text(size = 20),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.background = element_rect(fill = "white", color = NA),
          legend.box.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")
}

# Boulder
fig11a <- industry_function(1)+ 
  labs(title = "11a. Boulder")

ggsave("Graphs/fig11a.png", fig11a, width = 10, height = 4, units = "in", dpi = 300)

# Denver
fig11c <- industry_function(2)+ 
  labs(title = "11c. Denver-Centennial-Aurora")

ggsave("Graphs/fig11c.png", fig11c, width = 10, height = 4, units = "in", dpi = 300)

# Grand Junction
fig11e <- industry_function(3)+ 
  labs(title = "11e. Grand Junction")

ggsave("Graphs/fig11c.png", fig11c, width = 10, height = 4, units = "in", dpi = 300)

# Colorado Springs
fig11b <- industry_function(4)+ 
  labs(title = "11b. Colorado Springs")

ggsave("Graphs/fig11b.png", fig11b, width = 10, height = 4, units = "in", dpi = 300)

# Fort Collins
fig11d <- industry_function(5)+ 
  labs(title = "11d. Fort Collins-Loveland")

ggsave("Graphs/fig11d.png", fig11d, width = 10, height = 4, units = "in", dpi = 300)

# Greeley
fig11f <- industry_function(6) + 
  labs(title = "11f. Greeley")

ggsave("Graphs/fig11f.png", fig11f, width = 10, height = 4, units = "in", dpi = 300)

# Pueblo
fig11g <- industry_function(7) + 
  labs(title = "11g. Pueblo")

ggsave("Graphs/fig11g.png", fig11g, width = 10, height = 4, units = "in", dpi = 300)



