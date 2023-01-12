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

getwd()
###############    END   ##############


########### CPI VISUALIZATION ##############

# Creating plots to visualize the trends of cpi
#----------------------------------------------

plot1 <- ggplot(combined_cpi%>%filter(Products=="GENERAL INDEX (CPI)"))+
          geom_line(aes(x=Date, y=CPI, colour=Source))+
          scale_x_date(date_labels = "%b %y", breaks = "18 months")+
          labs(x="", y="Index",
          title="General Index (CPI)",
          caption="Source: NISR")+
          theme_classic()
plot1

plot2 <- ggplot(combined)+
        geom_line(aes(x=Date, y=Index, colour=Source))+
        facet_wrap(~Products)
plot2


#________________________________________________________________________________________________________________
#________________________________________________________________________________________________________________

cpi <-  function(sheet_name) {
  data <- read_excel("CPI_time_series_November_2022.xls", sheet='sheet_name', skip=3, col_names=TRUE)%>%
    slice(-1,-20)%>%
    rename(Province = "...1",
           U_R = "...2",
           COICOP = "...3",
           Products = "...4")%>%
    pivot_longer(!c(Province, U_R, COICOP, Products, Weights), names_to = "Date", values_to = "Index")%>%
    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
           Products = gsub("v", "", Products),
           Products = str_squish(Products),
           Source = "sheet_name")
  return(data)
} 

urban <- cpi(1)

combined <- rwanda %>% 
  rbind(rural)%>%
  rbind(urban)

#_________________________________________________________________________________________________________
#_________________________________________________________________________________________________________


# Checking the CPI data file and sheets name that will be used 
list.files() # All file in the directory
excel_sheets("CPI_time_series_November_2022.xls") # sheets of interested file




data_cpi <- function(x) {
  
  Source = c("Rural", "Urban", "All Rwanda")
  
  for (x in Source) {
    data <- read_excel("CPI_time_series_November_2022.xls", sheet=x, skip=3, col_names=TRUE)%>%
      slice(-1,-20)%>%
      rename(province = "...1",
             U_R = "...2",
             COICOP = "...3",
             Weight = "Weights",
             Products = "...4")%>%
      pivot_longer(!c(province, U_R, COICOP, Products, Weight), names_to = "Date", values_to = "CPI")%>%
      mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
             Products = gsub("v", "", Products),
             Products = str_squish(Products))
    }
  
}
urban <- data_cpi(Urban)

