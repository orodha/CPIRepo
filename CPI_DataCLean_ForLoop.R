# Preparing CPI data for visualization
#-------------------------------------

# Remove all in the environment
rm(list = ls())

# Setting working directory
setwd('C:/Users/ENVYLL/Desktop/Visualization_Pro/CPI/CPI_R_Project/data/raw')

cpi_data <- function(path = "CPI_time_series_November_2022.xls") {
  
  # Import needed libraries
  library('tidyverse')
  library('readxl')
  
  # Cleaning Data
  Sources = c("Rural", "Urban", "All Rwanda")
  datalist = list()
  datalist = vector("list", length = length(Sources))
  
  for (i in Sources) {
    data <- read_excel(path, sheet=i, skip=3, col_names=TRUE)%>%
      slice(-1,-20)%>%
      rename(Province = "...1",
             U_R = "...2",
             COICOP = "...3",
             Products = "...4")%>%
      pivot_longer(!c(Province, U_R, COICOP, Products, Weights), names_to = "Date", values_to = "Index")%>%
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
             Products = gsub("v", "", Products),
             Products = str_squish(Products),
             Source = i)
    datalist[[i]] <- data
  }

    combined = do.call(rbind, datalist)
    return(combined)
}

# Calling cpi_data() function and use current month cpi
cpi_main_dataset <- cpi_data()

# Exporting CPI main data to csv
write_csv(cpi_main_dataset, "cpi_main_dataset .csv")

#---------------------------------END-------------------------------------------