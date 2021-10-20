
library(DBI) 
library(dplyr)
library(writexl)

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication = "ActiveDirectoryInteractive")
orig1 <- DBI::dbGetQuery (con, "
      SELECT DISTINCT School, REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(School, '&amp;', '&'), '  ', ' '), '''', ''), '.', ' '), ',', ' ') AS ModSchool, 
      NULL AS CCSDID, NULL AS schoolNumber, NULL personID
      FROM [dbo].[MBK_20211019]")

schools <- DBI::dbGetQuery(con, "
  SELECT DISTINCT 
    REPLACE(REPLACE(EducationOrganizationName, ',', ' '), '.', ' ') AS EducationOrganizationName,
    EducationOrganizationShortName, 
    EducationOrganizationID
  FROM [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization
  WHERE 
    EducationOrganizationCurrentRowIndicator= 'Current' 
    AND ISNUMERIC(EducationOrganizationID) = 1 
	AND CHARINDEX('Summ', EducationOrganizationName) = 0 
	AND NOT EducationOrganizationOperationalStatus = 'Closed'
")
writexl::write_xlsx(schools, "C:\\Users\\okadak\\documents\\schools.xlsx")
writexl::write_xlsx(modSchools, "C:\\Users\\okadak\\documents\\modschools.xlsx")


schools <- schools %>% mutate(schoolSplit=strsplit(EducationOrganizationName, ' ', fixed=TRUE))
modSchools <- orig1 %>% mutate(schoolSplit= strsplit(ModSchool, ' ', fixed=TRUE) ) 
jsonedit(schools)

library(purrr)
# combined <- cross2(schools %>% transmute(SDMSchoolSplit=schoolSplit), 
#               modSchools %>% transmute(ModSchoolSplit=schoolSplit))

# 
# rm(combined) %>% 
#   map2_dfr(combined$SDMSchoolSplit, combined$ModSchoolSplit, function(i, j) { intersect(i, j) }) 

regexpr("h hell100", "h hell100 Academy of Excellence, h hell100", ignore.case=TRUE )
regexpr("100.", "My Lady!!", "The theatre of joy and beauty!!", "oh hell100 Academy of Excellence", "Abston", "Adcokc", "Haha") 
grep("100.", c("My Lady!!", "The theatre of joy and beauty!!", "oh hell100 Academy of Excellence", "Abston", "Adcokc", "Haha") )
grep("100.", c("My Lady!!", "The theatre of joy and beauty!!", "oh hell100 Academy of Excellence", "Abston", "Adcokc", "Haha") )
modSchools$schoolSplit <-  modSchools$schoolSplit %>% map( ~ .[. %in% c("", ",", ".", " ") == FALSE])#  function(i) { i[i %in% c("", ",", ".") == FALSE]})
schools$schoolSplit <- schools$schoolSplit %>% map( ~ .[. %in% c("", ",", ".") == FALSE])



modSchools
x <- list(1:10, 11:20, 21:30)
l1 <- list(x=c("a", "b"), y=c("c", "d"))
purrr::map(l1, sort, decreasing=TRUE)

library(listviewer)
jsonedit(modSchools)

mods <- modSchools$schoolSplit 
sdms <- schools$schoolSplit
noIntersect <- function(.x, .y) {
  length(intersect(.x, .y)) <= 1 
}
cl <- cross2(mods, sdms, .filter=noIntersect)
length(intersect(cl[[1]][1], cl[[1]][2])) == 0 
jsonedit(cl)

map(cl, 1) %>% unique()

install.packages("tictoc")
library(tictoc)
library(furrr)

tic("serial")
matchedSplits <- map(mods, 
    function(ms) { 
      map(sdms, function(s) {
        scr <- intersect(ms, s) 
        if(
            # !( 
            #   is_empty(scr) |
            #   ( length(scr) == 1 & intersect(scr, list("ES", "HS", "MS")) %>% length() == 1 ) 
            # )
          length(scr) >= 2
        ) 
        scr
      })
  }) %>% map( ~ discard(., ~ is.null(.))) 
toc()

tic("parallel")
matchedSplits_Parallel <- future_map(mods, function(ms) { 
    future_map(sdms, function(s) { 
      scr <- intersect(ms, s)
      if( length(scr) >= 2 )
        scr
    })
  }) %>% future_map( ~ discard(., ~ is.null(.))) 
toc()

scored_Matches <- map(matchedSplits_Parallel, function(m) {
  map(m, ~ ( list(
                    matchedSegments=., 
                    SegmentMatchScore=length(.), 
                    MeanMatchedSegsScore=mean(stringr::str_length(.))
                  )))
}) 
scored_Matches <- map(scored_Matches, function(n) { 
    map(n, function(p) { 
        list(
          matchedSegments=p$matchedSegments, 
          SegmentMatchScore=p$SegmentMatchScore, 
          MeanMatchedSegsScore=p$MeanMatchedSegsScore, 
          TotalScore=p$SegmentMatchScore + p$MeanMatchedSegsScore
        )}) 
})

jsonedit(scored_Matches)
map(scored_Matches, function(m) { map(m, function(n) {n$TotalScore}) }) 







