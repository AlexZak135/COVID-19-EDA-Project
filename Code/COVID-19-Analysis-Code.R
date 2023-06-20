# Title: COVID-19 Data Analysis
# Author: Alexander Zakrzeski
# Date: June 19, 2023

# Import the necessary packages

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(gt)
library(scales)

# Load the CSV files

dc_data <- read_csv("DC-Covid.csv")

md_data <- read_csv("Maryland-Covid.csv")

va_data <- read_csv("Virginia-Covid.csv")

# Create a function that performs the necessary data preprocessing steps:
  # This includes cleaning, wrangling, and merging the data frames 

clean_merge <- function(df1, df2, df3) {  
  dfs <- list(df1, df2, df3)  
  
  cleaned_dfs <- list()  
  
  for (df in dfs) {   
    df <- df %>% 
      mutate(date = mdy(date)) %>% 
      filter(between(date, as.Date("2020-03-01"), as.Date("2021-02-28"))) %>%
      arrange(date) %>%
      select(date, state, positiveIncrease, positive, deathIncrease, death) %>%
      rename(daily_cases = positiveIncrease, 
             cum_cases = positive,
             daily_deaths = deathIncrease, 
             cum_deaths = death) %>%
      mutate(cum_cases = replace_na(cum_cases, 0),
             cum_deaths = replace_na(cum_deaths, 0), 
             date = factor(format(date, format = "%b %Y"), 
                           levels = c("Mar 2020", "Apr 2020", "May 2020",
                                      "Jun 2020", "Jul 2020", "Aug 2020", 
                                      "Sep 2020", "Oct 2020", "Nov 2020", 
                                      "Dec 2020", "Jan 2021", "Feb 2021"))) %>% 
      group_by(date, state) %>% 
      summarize(daily_cases = sum(daily_cases), 
                cum_cases = max(cum_cases), 
                daily_deaths = sum(daily_deaths), 
                cum_deaths = max(cum_deaths)) %>%  
      rename(monthly_cases = daily_cases, 
             monthly_deaths = daily_deaths) %>%
      ungroup() 
    
    cleaned_dfs <- append(cleaned_dfs, list(df))  
    }  
  merged_df <- do.call(rbind, cleaned_dfs) %>%
    mutate(state = factor(state, 
                          levels = c("VA", "MD", "DC"), 
                          labels = c("Virginia", 
                                     "Maryland", 
                                     "District of Columbia")), 
           omicron = if_else( 
             date %in% c("Dec 2020", "Jan 2021", "Feb 2021"),
             "Omicron", 
             "Pre-Omicron"))
  
  return(merged_df)  
}

# Use the function created above

dmv_data <- clean_merge(dc_data, md_data, va_data)

# Create a stacked bar chart:
  # This visualization displays the monthly COVID-19 cases in the DMV

ggplot(data = dmv_data, aes(x = date, y = monthly_cases, fill = state)) +  
  geom_col(position = "stack", width = 0.5) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  scale_x_discrete(labels = function(x) str_replace(x, " ", "\n")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#1E486A", "#4AA6ED", "#89CFEC")) +
  labs(title = "Monthly COVID-19 Cases in the DMV Region", 
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) + 
  theme_void() +
  theme(text = element_text(family = "Roboto"), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"), 
        axis.text = element_text(size = 13, color = "black"), 
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.45, "cm"),
        legend.position = "top")

# Create a line graph:
  # This visualization displays the cumulative COVID-19 cases in the DMV

ggplot(data = dmv_data, 
       aes(x = date, y = cum_cases, group = state, color = state)) +
  geom_line(size = 1.25) +
  geom_point(size = 2.25) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  scale_x_discrete(labels = function(x) str_replace(x, " ", "\n")) +
  scale_y_continuous(labels = comma,
                     limits = c(0, 625000), breaks = seq(0, 625000, 
                                                         by = 125000)) +
  scale_color_manual(values = c("#1E486A", "#4AA6ED", "#89CFEC")) +
  labs(title = "Cumulative COVID-19 Cases in the DMV Region", 
       x = "", y = "") +
  guides(color = guide_legend(title = "", reverse = TRUE)) + 
  theme_void() +
  theme(text = element_text(family = "Roboto"), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"), 
        axis.text = element_text(size = 13, color = "black"), 
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.65, "cm"),
        legend.position = "top")

# Create a stacked bar chart:
  # This visualization displays the monthly COVID-19 deaths in the DMV

ggplot(data = dmv_data, aes(x = date, y = monthly_deaths, fill = state)) +  
  geom_col(position = "stack", width = 0.5) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  scale_x_discrete(labels = function(x) str_replace(x, " ", "\n")) +
  scale_y_continuous(labels = comma, limits = c(0, 3000), 
                     breaks = seq(0, 3000, by = 600))  +
  scale_fill_manual(values = c("#1E486A", "#4AA6ED", "#89CFEC")) +
  labs(title = "Monthly COVID-19 Deaths in the DMV Region", 
       x = "", y = "") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) + 
  theme_void() +
  theme(text = element_text(family = "Roboto"), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"), 
        axis.text = element_text(size = 13, color = "black"), 
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.45, "cm"),
        legend.position = "top")

# Create a line graph:
  # This visualization displays the cumulative COVID-19 deaths in the DMV

ggplot(data = dmv_data, 
       aes(x = date, y = cum_deaths, group = state, color = state)) +
  geom_line(size = 1.25) +
  geom_point(size = 2.25) +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  scale_x_discrete(labels = function(x) str_replace(x, " ", "\n")) +
  scale_y_continuous(labels = comma,
                     limits = c(0, 10000), breaks = seq(0, 10000, by = 2500)) +
  scale_color_manual(values = c("#1E486A", "#4AA6ED", "#89CFEC")) +
  labs(title = "Cumulative COVID-19 Deaths in the DMV Region", 
       x = "", y = "") +
  guides(color = guide_legend(title = "", reverse = TRUE)) + 
  theme_void() +
  theme(text = element_text(family = "Roboto"), 
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.major.y = element_line(linetype = 1, size = 0.3, 
                                          color = "gray"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 16, face = "bold"), 
        axis.text = element_text(size = 13, color = "black"), 
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(0.65, "cm"),
        legend.position = "top")

# Group by the appropriate columns and use aggregation functions

table_data <- dmv_data %>%
  group_by(state, omicron) %>%
  summarize(mean1 = round(mean(monthly_cases)),
            mean2 = round(mean(monthly_deaths))) %>%
  arrange(desc(state), desc(omicron))

# Create a table displaying summary statistics:
  # The table displays the average monthly COVID cases and deaths before and 
  # during the Omicron variant

gt(table_data, rowname_col = "omicron") %>%
  tab_header( 
    title = md("**COVID-19 Metrics: DMV Region, March 2020 - February 2021**") 
    ) %>%
  cols_label(mean1 = md("**Avg. Monthly Cases**"),
             mean2 = md("**Avg. Monthly Deaths**")) %>%
  cols_align("center") %>%
  fmt_number(columns = c(mean1, mean2),
             sep_mark = ",",
             decimals = 0) %>%
  opt_stylize(style = 6, color = "blue") %>%
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_row_groups())  %>%
  tab_options(data_row.padding = px(2), 
              summary_row.padding = px(3), 
              row_group.padding = px(4),
              table.font.names = "Roboto",
              table.font.size = px(18))
