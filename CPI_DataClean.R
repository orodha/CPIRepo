# Remove all in the environment
rm(list = ls())

# Setting directory
setwd('C:/Users/ENVYLL/Desktop/Visualization_Pro/CPI/CPI_R_Project/data/raw')
getwd()

# Import needed libraries
library('tidyverse')
library('readxl')

# Checking the CPI data file and sheets name that will be used 
list.files() # All file in the directory
excel_sheets("CPI_time_series_November_2022.xls") # sheets of interested file

# Checking how data looks like
urban <- read_excel("CPI_time_series_November_2022.xls", sheet='Urban', skip=3, col_names=TRUE)

############## CLEANING & IMPORTING DATA FILE ###########

# Loading Urban cpi data
urban <- read_excel("CPI_time_series_November_2022.xls", sheet='Urban', skip=3, col_names=TRUE)%>%
          slice(-1, -20) %>% 
          rename(province = "...1",
                 U_R = "...2",
                 COICOP = "...3",
                 Weight = "Weights",
                 Products = "...4") %>% 
          pivot_longer(!c(province, U_R, COICOP, Products, Weight), names_to = "Date", values_to = "CPI") %>%
          glimpse() %>% 
          mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
                 Products = gsub("v", "", Products),
                 Products = str_squish(Products),
                 Source = "urban")

# Loading Rural cpi data
rural <- read_excel("CPI_time_series_November_2022.xls", sheet='Rural', skip=3, col_names=TRUE)%>%
  slice(-1,-20)%>%
  rename(province = "...1",
         U_R = "...2",
         COICOP = "...3",
         Weight = "Weights",
         Products = "...4")%>%
  pivot_longer(!c(province, U_R, COICOP, Products, Weight), names_to = "Date", values_to = "CPI")%>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         Products = gsub("v", "", Products),
         Products = str_squish(Products),
         Source = "rural")

# loading All Rwanda cpi data
rwanda <- read_excel("CPI_time_series_November_2022.xls", sheet='All Rwanda', skip=3, col_names=TRUE)%>%
  slice(-1,-20)%>%
  rename(province = "...1",
         U_R = "...2",
         COICOP = "...3",
         Weight = "Weights",
         Products = "...4")%>%
  pivot_longer(!c(province, U_R, COICOP, Products, Weight), names_to = "Date", values_to = "CPI")%>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         Products = gsub("v", "", Products),
         Products = str_squish(Products),
         Source = "rwanda")

# Combining all data file into main data set         
combined_cpi <- rwanda %>% 
            rbind(rural)%>%
            rbind(urban)

# Export the combined cpi data to csv
write_csv(combined_cpi, "cpi_main_dataset .csv")


###############    END   #######################