install.packages("sqldf")
library(listviewer)
library(DBI) 
library(sqldf)
library(dplyr)
library(writexl)
library(tidyr) 
library(purrr)

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication = "ActiveDirectoryInteractive")
orig1 <- DBI::dbGetQuery (con, "
      SELECT DISTINCT School, REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(School, '&amp;', ' and '), '&', ' and '), '  ', ' '), '''', ''), '.', ' '), ',', ' '), '-', ' ') AS ModSchool, 
      NULL AS CCSDID, NULL AS schoolNumber, NULL personID
      FROM [dbo].[MBK_20211019]")

schools <- DBI::dbGetQuery(con, "
  SELECT DISTINCT 
    REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(EducationOrganizationName, ',', ' '), '.', ' '), '-', ' '), '&amp;', ' and '), '&', ' and ') AS EducationOrganizationName,
    EducationOrganizationShortName, 
    EducationOrganizationID
  FROM [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization
  WHERE 
    EducationOrganizationCurrentRowIndicator= 'Current' 
    AND ISNUMERIC(EducationOrganizationID) = 1 
	AND CHARINDEX('Summ', EducationOrganizationName) = 0 
	AND NOT EducationOrganizationOperationalStatus = 'Closed'
")
DBI::dbDisconnect(con)

schools <- schools %>% mutate(schoolSplit=strsplit(EducationOrganizationName, ' ', fixed=TRUE))
modSchools <- orig1 %>% mutate(schoolSplit= strsplit(ModSchool, ' ', fixed=TRUE) ) 

modSchools$schoolSplit <-  modSchools$schoolSplit %>% map( ~ .[. %in% c("", ",", ".", " ") == FALSE])#  function(i) { i[i %in% c("", ",", ".") == FALSE]})
schools$schoolSplit <- schools$schoolSplit %>% map( ~ .[. %in% c("", ",", ".") == FALSE])

mods <- modSchools$schoolSplit 
sdms <- schools$schoolSplit
msList <- modSchools %>% rowwise() %>% mutate(modSplit_Collapsed = paste0(schoolSplit, collapse=" ")) %>% select(modSchool=School, modSplit_Collapsed)



matchedSplits <- map(mods, function(ms) { 
  map(sdms, function(s) { 
    scr <- intersect(ms, s)
    if( length(scr) >= 2 ) {
      list(matches = scr, ModSegs = ms, SDMsegs = s)
    }
    # else {
    #   list(matches = list(), SDMsegs = s)
    # }
  })
}) %>% transpose()  

modSchools

matchList <- tibble(SDMID=schools$EducationOrganizationID, SDMName=schools$EducationOrganizationName,  matchedSplits=matchedSplits)
matchListExpanded <- matchList %>% unnest_longer(matchedSplits)

matchListExpandedCleared <- matchListExpanded %>% rowwise() %>% discard(~ any(is.null(matchedSplits)))
matchedNulls <- map(matchListExpandedCleared$matchedSplits, is.null) %>% unlist() 
matchedListFiltered <- matchListExpandedCleared[which(!matchedNulls), ]




matchComplete <- matchedListFiltered %>% unnest_wider(matchedSplits) %>% rowwise() %>% 
  mutate(matches_expanded = paste0(matches, collapse = " ")) %>% 
  mutate(ModSegs_expanded = paste0(ModSegs, collapse=" ")) %>% 
  mutate(SDMSegs_expanded = paste0(SDMsegs, collapse=" ")) %>% 
  mutate(MatchSegScore = length(matches)) %>% 
  mutate(MatchedLengthScore = mean(length(matches))) %>% 
  mutate(ModSegsCountScore = length(matches)/length(ModSegs)) %>% 
  mutate(SDMSegsCountScore = length(matches)/length(SDMsegs)) %>% 
  mutate(ModLengthScore = length(matches_expanded) / length(ModSegs_expanded)) %>% 
  mutate(SDMLengthScore = length(matches_expanded) / length(SDMSegs_expanded)) %>%
  mutate(TotalScore = MatchSegScore * 0.500 + MatchedLengthScore * 0.500 + ModSegsCountScore + SDMSegsCountScore + ModLengthScore + SDMLengthScore ) %>%
  select(MatchSegScore, MatchedLengthScore, ModSegsCountScore, SDMSegsCountScore, TotalScore, SDMID, SDMName, matches_expanded, ModSegs_expanded, SDMSegs_expanded) %>% 
  left_join(msList, by=c("ModSegs_expanded" = "modSplit_Collapsed")) %>% tibble()

matchComplete %>% is.data.frame()
typeof(matchComplete)
matchComplete %>% is.data.frame()
matchGrouped <- group_by(matchComplete, matchComplete$modSchool)
matchGrouped %>% is.data.frame()
matchGroupedFiltered <- matchGrouped %>% filter(TotalScore == max(TotalScore)) %>% select(modSchool, everything())

View(matchGrouped %>% select(modSchool, SDMName, TotalScore, everything()))
View(matchGroupedFiltered %>% select(modSchool, SDMName, TotalScore, everything()))

con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication = "ActiveDirectoryInteractive")
dbWriteTable(con, name="tmpMatchedSchools", value=matchGroupedFiltered)

dbDisconnect(con)

