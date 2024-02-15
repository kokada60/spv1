### Discipline modules... 

## Version
# major          4                           
# minor          0.5                         
# year           2021 

## Updated to Version 4.1.1


sdir <- getwd()
sfiles <- dir(sdir, pattern="OCR_Helpers")
for(f in sfiles) {
  source(file = file.path(sdir, sfiles))
}


## Student Aternative School Enrollments Year-round...
dfBehaviorSchoolsEnrollments <- qryHelper(glue::glue("EXEC [crdc_2021].generate_AdhocBehaviorSchoolsEnrollments_EntireYear @endYear='{year_id}'", year_id="2021")) 
## Student Behavior Incidents Year-round...
tic()
dfBehaviorIncAllYear <- qryHelper(glue::glue("EXEC [crdc_2021].generate_AdhocBehaviorIncidents_EntireYear @endYear='{year_id}'", year_id="2021")) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate="stateGrade", columnValues=gradesList) %>%
  feat_SwitchValues( colsToUpdate="raceEthnicity", columnValues=ethnicitiesList) %>% 
  feat_SwitchValues( colsToUpdate="gender", columnValues=genderList) %>%
  mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>%   
  rename(crdcGrade = stateGrade) %>% 
  group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup()
toc()
## creating schoolID index column...
dfBehaviorIncAllYear <- dfBehaviorIncAllYear %>% 
  mutate(schoolID = map(dfBehaviorIncAllYear$dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfBehaviorIncAllYear$dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) 


## Validation of schoolID alignment with dfs...
b2 <- dfBehaviorIncAllYear %>% mutate(schoolID = map(dfBehaviorIncAllYear$dfs, ~ .[1, "schoolID"]) %>% unlist())
b3 <- cbind(v1 = b2$schoolID, v2 = map(b2$dfs, ~.[1, "schoolID"]) %>% unlist())
as_tibble(b3) %>% filter(!(v1 == v2))


## DISC-1a 
crdcID <- "DISC_1a"
DISC_1a <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Preschool" )), .names="{.col}_PreSchoolfiltered")) %>% 
  mutate(across("dfs_PreSchoolfiltered", ~ map(.x, ~ filter(.x, stateResCode == "OS")), .names="{.col}_OSd")) %>% 
  mutate(across(contains("_OSd"),  ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  )

noPK_Enrolls <- which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(~ nrow(.x)) == 0)
withPK_Enrolls <- which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(~ nrow(.x)) > 0)

#coerce NAs to zeros for schools with PK enrollments...
DISC_1a[withPK_Enrolls, ] <- 
  DISC_1a[withPK_Enrolls, ] %>%
  mutate(across("dfs_PreSchoolfiltered_OSd_grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = ifelse(is.na(grpCount), as.integer(0), grpCount)))))
# map elements to proper fields... 
DISC_1a <- DISC_1a %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


## Validating count
# which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(nrow) == 0) %>% as.integer()
# which(DISC_1a$dfs_PreSchoolfiltered %>% map_int(nrow) > 0) %>% as.integer()
# 
# DISC_1a$dfs_PreSchoolfiltered[[108]] %>% filter(stateResCode == 'OS') %>% View()
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped[[108]]
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[108]]
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[70]]
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[109]]
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[1]]
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped_wd[[108]] %>% select_if(., ~ . > 0)
# DISC_1a$dfs_PreSchoolfiltered_OSd_grouped_mapped[[108]] %>% filter(grpCount > 0)
# DISC_1a$schoolID[[108]]


##DISC_1b 
crdcID <- "DISC_1b" 
DISC_1b <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Preschool" )), .names="{.col}_PreSchoolfiltered")) %>% 
  mutate(across("dfs_PreSchoolfiltered", ~ map(.x, ~ filter(.x, stateResCode == "E" | resolutionCode %in% c("EXP", "EXPOS"))), .names="{.col}_Ed")) %>% 
  mutate(across(contains("_Ed"),  ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  )

#coerce NAs to zeros for schools with PK enrollments...
DISC_1b[withPK_Enrolls, ] <- DISC_1b[withPK_Enrolls, ] %>%
  mutate(across("dfs_PreSchoolfiltered_Ed_grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount))))))
#map element to proper fields... 
DISC_1b <- DISC_1b %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

## validating counts...
# which(DISC_1b$dfs_PreSchoolfiltered_Ed %>% map_int(nrow) == 0 ) %>% as.integer() %>% length()
# which(DISC_1b$dfs_PreSchoolfiltered_Ed %>% map_int(nrow)>  0 ) %>% length()
# DISC_1b$dfs_PreSchoolfiltered_Ed_grouped_mapped_wd[[1]]



##DISC_2 
# in this case, just to be sure of how the distinct counts of "Incidents will be handled, helper function will not be used...
crdcID <- "DISC_2"   
DISC_2 <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Preschool" )), .names="{.col}_PreSchoolfiltered")) %>% 
  mutate(across("dfs_PreSchoolfiltered", ~ map(.x, ~ filter(.x, stateResCode == "OS" | resolutionCode %in% c("SUS", "SWI"))), .names="{.col}_Susp")) %>% 
  mutate(across(contains("_Susp"), ~map(.x, groupingByListOfcolumns, lc=list(c("schoolID"), c("inIDEA")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="{.col}_grouped")) %>% 
  mutate(across(contains("_grouped"), ~map(.x, ~ mutate(.x, dataElement1 = 
                                                          case_when(
                                                            dataElement1 == 0 ~ "0", 
                                                            dataElement1 == "Students with Disabilities (IDEA)" ~ "Preschool Children with Disabilities (IDEA)", 
                                                            TRUE ~ "All Preschool Children"))))) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  )
#Coerce NAs to zeros for schools with PK enrollments... 
DISC_2[withPK_Enrolls, ] <- DISC_2[withPK_Enrolls, ] %>% 
  mutate(across("dfs_PreSchoolfiltered_Susp_grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount))))))
#pivot measurement fields...
DISC_2 <- DISC_2 %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


##DISC_3    Corporal punishments...
DISC_3 <- select(dfBehaviorIncAllYear, c("schoolID")) %>% 
  mutate("SCH_CORPINSTANCES_IND" = "No") %>% 
  nest_by(schoolID) %>% 
  select(schoolID, dfs_adhc_wd = data)

##DISC_4
crdcID <- "DISC_4"
DISC_4 <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ .x[0, ]))) %>% 
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, 
                             lc=list(groupByColumns1, groupByColumns2, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>%   
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) 
#Coerce NAs for schools wih PK enrollments
DISC_4[withPK_Enrolls, ] <- DISC_4[withPK_Enrolls, ] %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount))))))
#pivot measurement fields...
DISC_4 <- DISC_4 %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

##DISC_5
DISC_5 <- select(dfBehaviorIncAllYear, c("schoolID")) %>% 
  mutate(
    SCH_PSCORPINSTANCES_ALL = as.integer(NA),
    SCH_PSCORPINSTANCES_IDEA = as.integer(NA)
  ) 
#Coerce NAs for schools wih PK enrollments & #pivot measurement fields...
DISC_5[withPK_Enrolls, ] <- DISC_5[withPK_Enrolls, ] %>%
  mutate(
          SCH_PSCORPINSTANCES_ALL = as.integer(0),
          SCH_PSCORPINSTANCES_IDEA = as.integer(0)
  ) 
DISC_5 <- DISC_5 %>% 
  nest_by(schoolID) %>% 
  select(schoolID, dfs_adhc_wd = data) 



##DISC_6    Instances of corporal punishments...
DISC_6 <- select(dfBehaviorIncAllYear, c("schoolID")) %>% 
  mutate(
    SCH_CORPINSTANCES_WODIS = as.integer(0),
    SCH_CORPINSTANCES_WDIS = as.integer(0)
  ) %>% 
  nest_by(schoolID) %>% 
  select(schoolID, dfs_adhc_wd = data) 

##DISC_7a
crdcID <- "DISC_7a"
DISC_7a <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ .x[0, ]))) %>% 
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, 
                             lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>%   
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>%
#Coerce NAs for schools wih PK enrollments
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
#pivot measurement fields...
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

         
##DISC_7b
crdcID <- "DISC_7b"
DISC_7b <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12") | 
                                       ( crdcGrade == "Ungraded"  & as.integer(AgeOnEOY) <= 19 )
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA)) )), 
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "IS")), .names="{.col}_ISd")) %>%
  mutate(across(contains("_ISd"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  )

#Coerce NAs to zeros for schools with + enrollments... 
DISC_7b <- DISC_7b %>% 
  mutate(across("dfs_FilteredByGradesIDEA_ISd_grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount))))))
#pivot measurement fields...
DISC_7b <- DISC_7b %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 



##DISC_7c
crdcID <- "DISC_7c"
DISC_7c <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA))
                )), 
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "OS")), .names="{.col}_OSd")) %>%
  mutate(across(contains("_OSd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, ~ filter(.x, IncidentsCount == 1)), .names="{.col}_IncCntFiltered")) %>% 
  mutate(across(contains("_IncCntFiltered"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_grouped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


##DISC_7d
crdcID <- "DISC_7d"
DISC_7d <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA)) )),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "OS")), .names="{.col}_OSd")) %>%
  mutate(across(contains("_OSd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, ~ filter(.x, IncidentsCount > 1)), .names="{.col}_IncCntFiltered")) %>% 
  mutate(across(contains("_IncCntFiltered"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_grouped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 



# Expulsion with Education Service...
##DISC_7e 
crdcID <- "DISC_7e"
DISC_7e <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "E")), .names="{.col}_EXPd")) %>%
  mutate(across(contains("_EXPd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


##DISC_7f
crdcID <- "DISC_7f"
DISC_7f <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, resolutionCode %in% c( "EXP", "EXPOS" ) & ( ! stateResCode == "E" | is.na(stateResCode)))), .names="{.col}_EXPd")) %>%
  mutate(across(contains("_EXPd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 
  


# Expulsion under Zero-Tolerance policy... 
# Some event types are conditional ( eg. Assaults are counted as Urgent Behavior Incidents resolved with Mandatory Expulsion Recommendation if victim sustained injury, Sexual Assault will be 
# resolved with MER if police "acknowledges" the event.
# Expulsion without Education Service...
##DISC_7g 
crdcID <- "DISC_7g"
DISC_7g <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), 
                ~ map(.x, 
                      ~ filter(.x, 
                                (
                                  ( code == "DSA" ) | 
                                  ( code %in% c( "DWH2", "DWI", "DWT" ) ) | 
                                  ( code %in% c( "DBE", "DBF" ) & ! replace_na(role_Injury, 1) == 1 )
                                ) & ! replace_na(stateResCode, "") == "E" 
                               )), .names="{.col}_UrgentIncidents_EXPd")) %>% 
  mutate(across(contains("_EXPd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>%   
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 



##DISC_8a 
crdcID <- "DISC_8a"
DISC_8a <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( ! inIDEA == "Students with Disabilities (IDEA)" | is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>%
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "E")), .names="{.col}_EXPd")) %>%
  mutate(across(contains("_EXPd"), 
                ~ map(.x, 
                  ~ inner_join(.x, dfBehaviorSchoolsEnrollments, by="personID") %>% 
                    mutate(dd = difftime(as.Date(parse_date_time(startDate.y, "b d Y HM")), 
                                         as.Date(parse_date_time(IncidentDate, "y-m-d H:M:s")), "day") 
                           %>% as.integer() %>% abs() %>% between(1, 40)
                           )), .names="{.col}_AltSchool")) %>% 
  mutate(across(contains("_AltSchool"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender.x", "raceEthnicity.x", "inEL", "incidentID")) %>% group_by(across(all_of(c("personID", "gender.x", "raceEthnicity.x", "inEL")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>%  
  mutate(across(contains("_IncCount"), ~ map(.x, ~ rename_with(.x, ~ c("gender", "raceEthnicity"), .cols=c("gender.x", "raceEthnicity.x"))))) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


## Corporal Punishments... Zero Across the board...
##DISC_9a
crdcID <- "DISC_9a"
DISC_9a <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ .x[0, ]))) %>% 
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, 
                             lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>%   
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>%
  #Coerce NAs for schools wih PK enrollments
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  #pivot measurement fields...
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )



##DISC_9b
crdcID <- "DISC_9b"
DISC_9b <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12") | 
                                       ( crdcGrade == "Ungraded"  & as.integer(AgeOnEOY) <= 19 )
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA)) )), 
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "IS")), .names="{.col}_ISd")) %>%
  mutate(across(contains("_ISd"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  )

#Coerce NAs to zeros for schools with + enrollments... 
DISC_9b <- DISC_9b %>% 
  mutate(across("dfs_FilteredByGradesIDEA_ISd_grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount))))))
#pivot measurement fields...
DISC_9b <- DISC_9b %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


dfBehaviorIncAllYear <- qryHelper(glue::glue("EXEC [crdc_2021].generate_AdhocBehaviorIncidents_EntireYear @endYear='{year_id}'", year_id="2021")) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate="stateGrade", columnValues=gradesList) %>%
  feat_SwitchValues( colsToUpdate="raceEthnicity", columnValues=ethnicitiesList) %>% 
  feat_SwitchValues( colsToUpdate="gender", columnValues=genderList) %>%
  mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>%   
  rename(crdcGrade = stateGrade) 
  
aa <- dfBehaviorIncAllYear %>%
  filter(ncesSchoolID =="00033")

tic()
  bb <- aa %>% 
    group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup()
  toc()
  ## creating schoolID index column...
  bb <- bb %>% 
    mutate(schoolID = map(bb$dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
    mutate(ncesSchoolID = map(bb$dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) 
  
  
  
  
dfBehaviorIncAllYear %>% filter(ncesSchoolID == "00007" & stateResCode == "OS" ) %>% View()

dfBehaviorIncAllYear %>% filter(inIDEA=="Students with Disabilities (IDEA)" & in504=="Students with Disabilities (Section 504 Only)") %>% View()
  
dfBehaviorIncAllYear %>% View()

##DISC_9c
crdcID <- "DISC_9c"
##DISC_9c <- dfBehaviorIncAllYear %>%
DISC_9c <- bb %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA))
                                                   
                )),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "OS")), .names="{.col}_OSd")) %>%
  mutate(across(contains("_OSd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "in504", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL", "in504")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, ~ filter(.x, IncidentsCount == 1)), .names="{.col}_IncCntFiltered")) %>% 
  mutate(across(contains("_IncCntFiltered"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_grouped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


##DISC_9d
crdcID <- "DISC_9d"
#DISC_9d <- dfBehaviorIncAllYear %>%
DISC_9d <- bb %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA)) )),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "OS")), .names="{.col}_OSd")) %>%
  mutate(across(contains("_OSd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "in504", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL", "in504")))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, ~ filter(.x, IncidentsCount > 1)), .names="{.col}_IncCntFiltered")) %>% 
  mutate(across(contains("_IncCntFiltered"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_grouped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 

DISC_9d$dfs_FilteredByGradesIDEA[[1]] %>% View()



# Expulsion with Education Service...
##DISC_9e 
crdcID <- "DISC_9e"
DISC_9e <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "E")), .names="{.col}_EXPd")) %>%
  mutate(across(contains("_EXPd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "in504", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL", "in504" )))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


##DISC_9f
crdcID <- "DISC_9f"
DISC_9f <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, resolutionCode %in% c( "EXP", "EXPOS" ) & ( ! stateResCode == "E" | is.na(stateResCode)))), .names="{.col}_EXPd")) %>%
  mutate(across(contains("_EXPd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "in504", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL", "in504" )))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 



# Expulsion under Zero-Tolerance policy... 
# Some event types are conditional ( eg. Assaults are counted as Urgent Behavior Incidents resolved with Mandatory Expulsion Recommendation if victim sustained injury, Sexual Assault will be 
# resolved with MER if police "acknowledges" the event.
# Expulsion without Education Service...
##DISC_9g 
crdcID <- "DISC_9g"
DISC_9g <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>% 
  mutate(across(contains("_FilteredByGradesIDEA"), 
                ~ map(.x, 
                      ~ filter(.x, 
                               (
                                 ( code == "DSA" ) | 
                                   ( code %in% c( "DWH2", "DWI", "DWT" ) ) | 
                                   ( code %in% c( "DBE", "DBF" ) & ! replace_na(role_Injury, 1) == 1 )
                               ) & ! replace_na(stateResCode, "") == "E" 
                      )), .names="{.col}_UrgentIncidents_EXPd")) %>% 
  mutate(across(contains("_EXPd"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender", "raceEthnicity", "inEL", "in504", "incidentID")) %>% group_by(across(all_of(c("personID", "gender", "raceEthnicity", "inEL", "in504" )))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>%   
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 



##DISC_10 
crdcID <- "DISC_10"
DISC_10 <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   ) & ( inIDEA == "Students with Disabilities (IDEA)" & ! is.na(inIDEA) )) ),
                .names="{.col}_FilteredByGradesIDEA") ) %>%
  mutate(across(contains("_FilteredByGradesIDEA"), ~ map(.x, ~ filter(.x, stateResCode == "E")), .names="{.col}_EXPd")) %>%
  mutate(across(contains("_EXPd"), 
                ~ map(.x, 
                      ~ inner_join(.x, dfBehaviorSchoolsEnrollments, by="personID") %>% 
                        mutate(dd = difftime(as.Date(parse_date_time(startDate.y, "b d Y HM")), 
                                             as.Date(parse_date_time(IncidentDate, "y-m-d H:M:s")), "day") 
                               %>% as.integer() %>% abs() %>% between(1, 40)
                        )), .names="{.col}_AltSchool")) %>% 
  mutate(across(contains("_AltSchool"), ~ map(.x, ~ { 
    .x %>% distinct_at(., c("personID", "gender.x", "raceEthnicity.x", "inEL", "in504", "incidentID")) %>% group_by(across(all_of(c("personID", "gender.x", "raceEthnicity.x", "inEL", "in504" )))) %>% count(name="IncidentsCount") %>% ungroup()
  }), .names="{.col}_IncCount")) %>%  
  mutate(across(contains("_IncCount"), ~ map(.x, ~ rename_with(.x, ~ c("gender", "raceEthnicity"), .cols=c("gender.x", "raceEthnicity.x"))))) %>% 
  mutate(across(contains("_IncCount"), ~ map(.x, groupingByListOfcolumns, lc=list(groupByColumns1, groupByColumns3, groupByColumns4),  distinctCnt = TRUE, cntColumnName="personID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


## DISC_11 
crdcID <- "DISC_11"
#DISC_11 <- dfBehaviorIncAllYear %>%
DISC_11 <- bb %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   )) ),
                .names="FilteredByGrades") ) %>%
  mutate(across("FilteredByGrades", ~ map(.x, ~ filter(.x, stateResCode == "OS" & ! is.na(stateResCode))), .names="filteredfor_OSd")) %>%
  mutate(across("filteredfor_OSd", 
                ~ map(.x, ~ mutate(.x, across("inIDEA", ~ modify_if(., isEqualToZeroOrNA, ~ c("Students without Disabilities"))))), .names="updatedIDEAfield")) %>%  
  mutate(across("updatedIDEAfield", ~ map(.x, groupingByListOfcolumns, lc=list(c("inIDEA"), c("in504")),  distinctCnt = TRUE, cntColumnName="resolutionID"), .names="{.col}_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 


## DISC_12
crdcID <- "DISC_12"
#DISC_12 <- dfBehaviorIncAllYear %>% 
DISC_12 <- bb %>% 
  mutate(across("dfs", 
                ~ map(.x, ~ filter(.x, 
                                   (
                                     crdcGrade %in% c(
                                       "Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", 
                                       "Grade 6", "Grade 7", "Grade 8", 
                                       "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded" ) & as.integer(AgeOnEOY) <= 19
                                   )) ),
                .names="{.col}_FilteredByGrades") ) %>%
  mutate(across(contains("_FilteredByGrades"), ~ map(.x, ~ filter(.x, stateResCode == "OS" & ! is.na(stateResCode))), .names="{.col}_OSd")) %>%
  mutate(
    across(contains("_OSd"), ~ map(.x, feat_ChangeType, colsToUpdate=indicatorColumns1), .names="indicatorChanged")
  ) %>% 
  mutate(
    across("indicatorChanged", ~ map(.x, ~ mutate(.x, ceilSchoolDaysDuration = ceiling(as.numeric(resolutionSchoolDaysDuration)))), .names="chg_Ceiling")
  ) %>% 
  mutate(
    across("chg_Ceiling", ~ map(.x, groupingByListOfcolumnsSum, lc=columnGroupList1,  distinctCnt = FALSE, cntColumnName="ceilSchoolDaysDuration"), .names="cntgrouped")
  ) %>% 
  mutate(
    across("cntgrouped",
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% 
  mutate(across(contains("_mapped"), ~ map(.x, ~ mutate(.x, grpCount = if_else(is.na(grpCount), as.integer(0), as.integer(grpCount)))))) %>%
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", starts_with("grpCount"))))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) 

DISC_12$dfs_FilteredByGrades[[1]] %>% View()

  
DISC_1a_Bound <- cbind(ncesSchoolID = DISC_1a$ncesSchoolID, schoolID = DISC_1a$schoolID, DISC_1a$dfs_adhc_wd %>% reduce(rbind))
DISC_1b_Bound <- cbind(schoolID = DISC_1b$schoolID, DISC_1b$dfs_adhc_wd %>% reduce(rbind))
DISC_2_Bound <- cbind(schoolID = DISC_2$schoolID, DISC_2$dfs_adhc_wd %>% reduce(rbind))
DISC_3_Bound <- cbind(schoolID = DISC_3$schoolID, DISC_3$dfs_adhc_wd %>% reduce(rbind))
DISC_4_Bound <- cbind(schoolID = DISC_4$schoolID, DISC_4$dfs_adhc_wd %>% reduce(rbind))
DISC_5_Bound <- cbind(schoolID = DISC_5$schoolID, DISC_5$dfs_adhc_wd %>% reduce(rbind))
DISC_6_Bound <- cbind(schoolID = DISC_6$schoolID, DISC_6$dfs_adhc_wd %>% reduce(rbind))
DISC_7a_Bound <- cbind(schoolID = DISC_7a$schoolID, DISC_7a$dfs_adhc_wd %>% reduce(rbind))
DISC_7b_Bound <- cbind(schoolID = DISC_7b$schoolID, DISC_7b$dfs_adhc_wd %>% reduce(rbind))
DISC_7c_Bound <- cbind(schoolID = DISC_7c$schoolID, DISC_7c$dfs_adhc_wd %>% reduce(rbind))
DISC_7d_Bound <- cbind(schoolID = DISC_7d$schoolID, DISC_7d$dfs_adhc_wd %>% reduce(rbind))
DISC_7e_Bound <- cbind(schoolID = DISC_7e$schoolID, DISC_7e$dfs_adhc_wd %>% reduce(rbind))
DISC_7f_Bound <- cbind(schoolID = DISC_7f$schoolID, DISC_7f$dfs_adhc_wd %>% reduce(rbind))
DISC_7g_Bound <- cbind(schoolID = DISC_7g$schoolID, DISC_7g$dfs_adhc_wd %>% reduce(rbind))
DISC_8a_Bound <- cbind(schoolID = DISC_8a$schoolID, DISC_8a$dfs_adhc_wd %>% reduce(rbind))
DISC_9a_Bound <- cbind(schoolID = DISC_9a$schoolID, DISC_9a$dfs_adhc_wd %>% reduce(rbind))
DISC_9b_Bound <- cbind(schoolID = DISC_9b$schoolID, DISC_9b$dfs_adhc_wd %>% reduce(rbind))
DISC_9c_Bound <- cbind(schoolID = DISC_9c$schoolID, DISC_9c$dfs_adhc_wd %>% reduce(rbind))
DISC_9d_Bound <- cbind(schoolID = DISC_9d$schoolID, DISC_9d$dfs_adhc_wd %>% reduce(rbind))
DISC_9e_Bound <- cbind(schoolID = DISC_9e$schoolID, DISC_9e$dfs_adhc_wd %>% reduce(rbind))
DISC_9f_Bound <- cbind(schoolID = DISC_9f$schoolID, DISC_9f$dfs_adhc_wd %>% reduce(rbind))
DISC_9g_Bound <- cbind(schoolID = DISC_9g$schoolID, DISC_9g$dfs_adhc_wd %>% reduce(rbind))
DISC_10_Bound <- cbind(schoolID = DISC_10$schoolID, DISC_10$dfs_adhc_wd %>% reduce(rbind))
DISC_11_Bound <- cbind(schoolID = DISC_11$schoolID, DISC_11$dfs_adhc_wd %>% reduce(rbind))
DISC_12_Bound <- cbind(schoolID = DISC_12$schoolID, DISC_12$dfs_adhc_wd %>% reduce(rbind))


DISC_Bound <- bind_cols(
  DISC_1a_Bound,
  select(DISC_1b_Bound, -schoolID), 
  select(DISC_2_Bound, -schoolID), 
  select(DISC_3_Bound, -schoolID), 
  select(DISC_4_Bound, -schoolID), 
  select(DISC_5_Bound, -schoolID), 
  select(DISC_6_Bound, -schoolID), 
  select(DISC_7a_Bound, -schoolID), 
  select(DISC_7b_Bound, -schoolID), 
  select(DISC_7c_Bound, -schoolID), 
  select(DISC_7d_Bound, -schoolID), 
  select(DISC_7e_Bound, -schoolID), 
  select(DISC_7f_Bound, -schoolID), 
  select(DISC_7g_Bound, -schoolID), 
  select(DISC_8a_Bound, -schoolID), 
  select(DISC_9a_Bound, -schoolID), 
  select(DISC_9b_Bound, -schoolID), 
  select(DISC_9c_Bound, -schoolID), 
  select(DISC_9d_Bound, -schoolID), 
  select(DISC_9e_Bound, -schoolID), 
  select(DISC_9f_Bound, -schoolID), 
  select(DISC_9g_Bound, -schoolID), 
  select(DISC_10_Bound, -schoolID), 
  select(DISC_11_Bound, -schoolID), 
  select(DISC_12_Bound, -schoolID)
)

DISC_Bound %>% writexl::write_xlsx("DISC.xlsx")



## HIBS: Harassment or Bullying...
tic()
dfBehaviorAllegAllYear <- qryHelper(glue::glue("EXEC [crdc_2021].generate_BehaviorAllegations_EntireYear @endYear='{year_id}'", year_id="2021")) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate="stateGrade", columnValues=gradesList ) %>%
  feat_SwitchValues( colsToUpdate="raceEthnicity", columnValues=ethnicitiesList ) %>% 
  feat_SwitchValues( colsToUpdate="gender", columnValues=genderList ) %>%
  mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>%   
  rename(crdcGrade = stateGrade) %>% 
  group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup()
toc()
## creating schoolID index column...
dfBehaviorAllegAllYear <- dfBehaviorAllegAllYear %>% 
  mutate(schoolID = map(dfBehaviorAllegAllYear$dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfBehaviorAllegAllYear$dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) 


## Validation of schoolID alignment with dfs...
b2 <- dfBehaviorAllegAllYear %>% mutate(schoolID = map(dfBehaviorAllegAllYear$dfs, ~ .[1, "schoolID"]) %>% unlist())
b3 <- cbind(v1 = b2$schoolID, v2 = map(b2$dfs, ~.[1, "schoolID"]) %>% unlist())
as_tibble(b3) %>% filter(!(v1 == v2))


## HIBS_1a...
crdcID <- "HIBS_1a"
HIBS_1a <- dfBehaviorAllegAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>%
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgTypeGroup = case_when(
                            AllgEventCode %in% c("GEN", "GID", "SHR") ~ "Allegations of harassment or bullying on the basis of sex",
                            AllgEventCode %in% c("RCE") ~ "Allegations of harassment or bullying on the basis of race, color, or national origin",
                            AllgEventCode %in% c("DIS") ~ "Allegations of harassment or bullying on the basis of disability",
                                      TRUE ~ ""))))) %>%
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, lc=list("AllgTypeGroup"), distinctCnt=TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(
    across("grouped", 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
           .names="{.col}_mapped"
    ) 
  ) 
    ## This list has been verified... There are two schools with a single row, a test entries. Not valid students... 
noEnrolls <- which(HIBS_1a$dfs %>% map_int(~ nrow(.x)) == 1) %>% as.integer()
withEnrolls <- which(HIBS_1a$dfs %>% map_int(~ nrow(.x)) > 1) %>% as.integer()

HIBS_1a[noEnrolls, ] <- HIBS_1a[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_1a[withEnrolls, ] <- HIBS_1a[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_1a <- HIBS_1a %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

## HIBS_1b...
crdcID <- "HIBS_1b"
HIBS_1b <- dfBehaviorAllegAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>%
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgTypeGroup = case_when(
    AllgEventCode %in% c("GID" ) ~ "Allegations of harassment or bullying on the basis of sexual orientation",
    AllgEventCode %in% c("REL") ~ "Allegations of harassment or bullying on the basis of religion",
    TRUE ~ ""))))) %>%
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, lc=list("AllgTypeGroup"), distinctCnt=TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(
    across("grouped", 
           ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                 byMatchColumns = c("dataElement1" = "dataElement1"),
                 selectColumns = c("elementName", "grpCount", "dataElement1Seq")), 
           .names="{.col}_mapped"))

HIBS_1b[noEnrolls, ] <- HIBS_1b[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_1b[withEnrolls, ] <- HIBS_1b[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_1b <- HIBS_1b %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


## HIBS_1c...
crdcID <- "HIBS_1c"
elementNameList <- filter(subQs, mod_crdcIdentifier == crdcID) %>% arrange(dataElement1Seq) %>% select(elementName) %>% unlist() %>% as.character()
HIBS_1c <- dfBehaviorAllegAllYear
HIBS_1c[elementNameList] <- NA
HIBS_1c <- HIBS_1c %>% select(-dfs) %>% nest_by(schoolID, ncesSchoolID) %>% dplyr::rename("dfs_adhc_wd" = "data")


## HIBS_2a...
crdcID <- "HIBS_2a"
HIBS_2a <- dfBehaviorAllegAllYear %>% 
  #mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>%
  mutate(across("dfs", ~ map(.x, ~ filter(.x, AllgEventCode  %in% c("GEN", "GID", "SHR"))), .names="filtered")) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, lc = columnGroupList1, distinctCnt = TRUE, cntColumnName = "personID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier == crdcID), 
                                 byMatchColumns = c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"),
                                 selectColumns = c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")),
                .names="{.col}_mapped"))

HIBS_2a[noEnrolls, ] <- HIBS_2a[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_2a[withEnrolls, ] <- HIBS_2a[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_2a <- HIBS_2a %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )



## hibs_2b
crdcID <- "HIBS_2b"
HIBS_2b <- dfBehaviorAllegAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, AllgEventCode  %in% c("RCE"))), .names="filtered")) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, lc = columnGroupList1, distinctCnt = TRUE, cntColumnName = "personID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier == crdcID), 
                                 byMatchColumns = c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"),
                                 selectColumns = c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")),
                .names="{.col}_mapped"))

HIBS_2b[noEnrolls, ] <- HIBS_2b[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_2b[withEnrolls, ] <- HIBS_2b[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_2b <- HIBS_2b %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


## HIBS_2c
crdcID <- "HIBS_2c"
HIBS_2c <- dfBehaviorAllegAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, AllgEventCode  %in% c("DIS"))), .names="filtered")) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, lc = columnGroupList1, distinctCnt = TRUE, cntColumnName = "personID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier == crdcID), 
                                 byMatchColumns = c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"),
                                 selectColumns = c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")),
                .names="{.col}_mapped"))

HIBS_2c[noEnrolls, ] <- HIBS_2c[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_2c[withEnrolls, ] <- HIBS_2c[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_2c <- HIBS_2c %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


## HIBS_3a
disciplineCodes <- c("")
crdcID <- "HIBS_3a"
HIBS_3a <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, AllgEventCode %in% c("GEN", "GID", "SHR") & ! is.na(resolutionCode) )), .names="filtered")) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, lc = columnGroupList1, distinctCnt = TRUE, cntColumnName = "personID"), .names="grouped")) %>%
  mutate(across("grouped", ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier == crdcID), 
                                 byMatchColumns = c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"),
                                 selectColumns = c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")),
                .names="{.col}_mapped"))

HIBS_3a[noEnrolls, ] <- HIBS_3a[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_3a[withEnrolls, ] <- HIBS_3a[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_3a <- HIBS_3a %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

# HIBS_3a$filtered[[140]] %>% View()
# HIBS_3a$grouped[[140]] %>% View()
# HIBS_3a$grouped_mapped[[140]] %>% View()
# HIBS_3a$dfs_adhc_wd[[140]] %>% View()


## HIBS_3b
crdcID <- "HIBS_3b"
HIBS_3b <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, AllgEventCode %in% c("RCE") & ! is.na(resolutionCode) )), .names="filtered")) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, lc = columnGroupList1, distinctCnt = TRUE, cntColumnName = "personID"), .names="grouped")) %>%
  mutate(across("grouped", ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier == crdcID), 
                                 byMatchColumns = c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"),
                                 selectColumns = c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")),
                .names="{.col}_mapped"))

HIBS_3b[noEnrolls, ] <- HIBS_3b[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_3b[withEnrolls, ] <- HIBS_3b[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_3b <- HIBS_3b %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


## HIBS_3c
crdcID <- "HIBS_3c"
HIBS_3c <- dfBehaviorIncAllYear %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, AllgEventCode %in% c("DIS") & ! is.na(resolutionCode) )), .names="filtered")) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, lc = columnGroupList1, distinctCnt = TRUE, cntColumnName = "personID"), .names="grouped")) %>%
  mutate(across("grouped", ~ map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier == crdcID), 
                                 byMatchColumns = c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"),
                                 selectColumns = c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")),
                .names="{.col}_mapped"))

HIBS_3c[noEnrolls, ] <- HIBS_3c[noEnrolls, ] %>%
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = NA))))
HIBS_3c[withEnrolls, ] <- HIBS_3c[withEnrolls, ] %>% 
  mutate(across("grouped_mapped", ~ map(.x, ~ mutate(.x, grpCount = replace_na(grpCount, as.integer(0))))))

HIBS_3c <- HIBS_3c %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

HIBS_1a_Bound <- cbind(ncesSchoolID = HIBS_1a$ncesSchoolID, schoolID = HIBS_1a$schoolID, HIBS_1a$dfs_adhc_wd %>% reduce(rbind))
HIBS_1b_Bound <- cbind(schoolID = HIBS_1b$schoolID, HIBS_1b$dfs_adhc_wd %>% reduce(rbind))
HIBS_1c_Bound <- cbind(schoolID = HIBS_1c$schoolID, HIBS_1c$dfs_adhc_wd %>% reduce(rbind))
HIBS_2a_Bound <- cbind(schoolID = HIBS_2a$schoolID, HIBS_2a$dfs_adhc_wd %>% reduce(rbind))
HIBS_2b_Bound <- cbind(schoolID = HIBS_2b$schoolID, HIBS_2b$dfs_adhc_wd %>% reduce(rbind))
HIBS_2c_Bound <- cbind(schoolID = HIBS_2c$schoolID, HIBS_2c$dfs_adhc_wd %>% reduce(rbind))
HIBS_3a_Bound <- cbind(schoolID = HIBS_3a$schoolID, HIBS_3a$dfs_adhc_wd %>% reduce(rbind))
HIBS_3b_Bound <- cbind(schoolID = HIBS_3b$schoolID, HIBS_3b$dfs_adhc_wd %>% reduce(rbind))
HIBS_3c_Bound <- cbind(schoolID = HIBS_3c$schoolID, HIBS_3c$dfs_adhc_wd %>% reduce(rbind))

HIBS_Bound <- bind_cols(
  HIBS_1a_Bound, 
  select(HIBS_1b_Bound, -schoolID), 
  select(HIBS_1c_Bound, -schoolID), 
  select(HIBS_2a_Bound, -schoolID), 
  select(HIBS_2b_Bound, -schoolID), 
  select(HIBS_2c_Bound, -schoolID), 
  select(HIBS_3a_Bound, -schoolID), 
  select(HIBS_3b_Bound, -schoolID), 
  select(HIBS_3c_Bound, -schoolID)
) 
HIBS_Bound %>% writexl::write_xlsx("HIBS.xlsx")









