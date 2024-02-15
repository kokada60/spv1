library(tidyverse) 
library(odbc)
library(DBI)
library(glue)
library(tictoc)
library(lubridate)

## Version
# major          4                           
# minor          0.5                         
# year           2021 

#Sourcing helpers...
sdir <- "C:/Users/OKADAK/Documents/Spv1"
sfiles <- dir(sdir, pattern="OCR_Helpers.R")
for(f in sfiles) {
  source(file = file.path(sdir, sfiles))
}
load("schoolsList.RData")