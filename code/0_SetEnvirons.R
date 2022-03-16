################################################################################
#
#           Code to set global environment variables for MP code
#               Developed by Sophia Bryson (sab159@duke.edu)
#               Masters Project for client Internet of Water
#                                 2021 - 2022
#
################################################################################

############################ LOAD LIBRARIES ####################################
#Specify the packages to be used
   packages = c("tidyverse", "readxl", "dataRetrieval", "sf", "lubridate",
                "leaflet", "nhdplusTools", "tigris", "s2", "tidycensus", 
                "arcpullr", "mapview") 

## Now load or install&load all
   package.check <- lapply(
      packages,
      FUN = function(x) {
         if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
         }
      }
   )
################################################################################
   
########################### SET GLOBAL VARIABLES ###############################

# clear out 
   rm(list=ls())
   
# Working directory setup 
   source_path = rstudioapi::getActiveDocumentContext()$path 
   setwd(dirname(source_path))
   mp_wd_data <- paste0("..\\data\\")

 #Load census API key: 
   # UNCOMMENT to add if none is installed
   # A census bureau api key can be obtained through https://api.census.gov/data/key_signup.html
   # census_api_key("<PASTE KEY HERE>") #, install = TRUE) 
   
   
# Dates & update variables
   currentYear = year(today())
   dataYears = 2  
   startYear = currentYear - dataYears
   startDate = paste0("01-01-", startYear) # 2019 corresponds with ACS survey of interest (2014-2019 aggregated data) 
   endDate = today() %>% format('%m-%d-%Y') 
   
# Useful function
   `%notin%` = function(x,y) !(x %in% y); #function to get what is not in the list
   
# State codes to use in a variety of functions & calls
   statenums <- seq(1,56) %>% str_pad(width = 2, side = "left", pad = "0") #add leading zeroes to get 2 digit codes for all states
   #remove invalid FIPS codes
   invalid_fips = c("03", "07", "14", "43", "52") #fips codes on reserve for territories, etc. 
   statenums <- statenums[statenums %notin% invalid_fips]
   statecodes <- paste0("US:", statenums, ";") 
   
# Make sure memory limit won't limit vector allocations
   memory.limit(36000)
   
################################################################################   