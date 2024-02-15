library(readxl)
library(odbc)
library(DBI)
library(tidyverse)
con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server = "ORION.CIS.CCSD.NET", Database="SDM", Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
bar <- DBI::dbGetQuery(con, "
  SELECT * FROM DIF.mv_BehaviorIncidentsAllResolutions mv 
  WHERE schoolYear = '2022' AND 
  	NOT BehaviorEventDescription IN ( 'Truancy', 'Tardy/Lkout/swps' )                         
                       ")
fl <- list.files(path="C://users//okadak//desktop//", "Non-Attendance Yearly Total_Full Data.csv", include.dirs = TRUE, full.names = TRUE) 
dfo <- map(fl, read.csv) %>% as.data.frame()
dfo %>% is.data.frame()

summary(dfo)
summary(bar) 

nrow(dfo)
nrow(bar) 

map_int(bar, ~ length(unique(.x)))
bar$BehaviorResolutionType %>% unique()

bar %>% split(.$StudentCurrentGrade) %>% 
  map(~ length(unique(.x$BehaviorIncidentId))) %>% 
  as_tibble() %>% pivot_longer(cols=1:17, names_to="grade", values_to = "cnt") %>%
  mutate(cs = cumsum(cnt))

bar %>% filter(.$StudentCurrentGrade == '01')-> b
b$BehaviorIncidentId %>% unique() %>% length()

bar %>% filter(SchoolID == '555') %>% map_dbl(~length(unique(.x)))




a <- c(10, 2, 4, 9, 1, 5)
a %in% c(1,2,3,4) %>% as.integer()




bt <- data.table::data.table(bar) 






