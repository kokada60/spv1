sdir <- getwd()
sfiles <- dir(path=sdir, pattern="OCR_Helpers.R")
for ( f in sfiles ) {
  source(file = file.path(sdir, f))
}


tic() 
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{specific_date}', @EOYDay='{eoy_date}'", 
                     year_id=str_SchoolYear, mapYear_id=str_SchoolYear, specific_date=dt_CountDay, eoy_date=dt_EOY)
  dfStudentsList <- qryHelper2(strQ, str_ServerName, str_DBName) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate=all_of(c("crdcGrade")), columnValues=gradesList ) %>%
  feat_SwitchValues( colsToUpdate=all_of(c("raceEthnicity")), columnValues=ethnicitiesList ) %>% 
  feat_SwitchValues( colsToUpdate=all_of(c("gender")), columnValues=genderList ) %>% 
  mutate(birthDate = lubridate::mdy_hm(birthDate) ) 
  # %>%
  # group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup() %>% 
  # mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  # mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist())   
toc()
save(dfStudentsList, file="dfStudentsList_OnCountDay.RData")
#load("dfStudentsList_OnCountDay.RData")

tic()
  strQ <- glue::glue("EXEC crdc_2021.generate_AdHoc_AdvancedCourseEnrollmentTable_District @SchoolYear='{year_id}', @CountDay='{countDate_ID}'", year_id="2021", countDate_ID=as.character(dt_CountDay))
  dfAdvCoursesEnrollsCountDay <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfAdvCoursesEnrollsCountDay, "CountDay_AdvCoursesEnrollsWholeList.RData")
  load("CountDay_AdvCoursesEnrollsWholeList.RData")
toc()

tic()
  dfAdvCoursesEnrollsCountDay <-
  dfAdvCoursesEnrollsCountDay %>%
  filter(dt_CountDay %between% list(EntryFullDate, ExitFullDate)) %>% 
  filter(DualCreditIndicator == 1) %>% 
  # group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup() %>% 
  # mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  # #mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist())   
toc()  

dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay <- 
  joinWithExternalData2(dfStudentsList, dfAdvCoursesEnrollsCountDay, 
                        byMatchColumns = c("schoolID"="schoolID", "studentNumber"="StudentNumber"), 
                        selectColumns = expr(c("schoolID.x", "ncesSchoolID", "stateSchoolID", "schoolName", 
                                               "studentNumber", "personID", "gender", "raceEthnicity", "crdcGrade", 
                                               "AgeOnCountDay", "AgeOnEOY", "inIDEA", "inEL", "in504", "inGate", 
                                               "CourseCode", "CourseTitle", "CourseLevelCode", "ActivelyEnrolled", 
                                               "APIndicator", "IBIndicator", "CTEIndicator", "DualCreditIndicator")), 
                        joinType="L") %>% rename(schoolID = schoolID.x)

dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay_Split <-
  dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay %>% 
  # 
  # group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup() %>%
  # mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>%
  # mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist())
  # 
  group_by(schoolID) %>% 
  split(.$schoolID) %>% 
  tibble(dfs = .)  %>% ungroup() %>% 
  mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist())


# Loading Enrolled Students list... 
load("schoolsList.RData")
schoolsWithEnrolls_Index <- schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% map_lgl(~ nrow(.) > 0) %>% which()
schoolsWithEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[schoolsWithEnrolls_Index] %>% unlist()



##PENR_1
crdcID= "PENR_1"
PENR_1 <- dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(
    across("dfs", ~ map(.x, ~ filter(.x, inGate == "1")), .names="{.col}_filtered")
  ) %>% 
  mutate(
    across(contains("_filtered"), ~ map(.x, ~ { tibble(SCH_GT_IND = if_else(nrow(.x) > 0, "YES", "NO")) }), .names="dfs_adhc_wd")
  ) %>% fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE)


##PENR_2
crdcID= "PENR_2"
PENR_2 <- dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(
    across("dfs", ~ map(.x, ~ filter(.x, inGate == "1")), .names="{.col}_filtered")
  ) %>% 
  mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt=TRUE, cntColumnName="personID"), .names="{.col}_grouped")
  ) %>% 
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
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_fillInWithAValue, 0, FALSE)



##PENR-3
crdcID= "PENR_3"
PENR_3 <- dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(
    across("dfs", ~ map(.x, ~ filter(.x, DualCreditIndicator == 1 & ( crdcGrade %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeOnCountDay %in% c(14, 15, 16, 17, 18, 19) )) )),
           .names="{.col}_filtered")
  ) %>% 
  mutate(
    across(contains("_filtered"), ~ map(.x, ~ { tibble(SCH_DUAL_IND = if_else(nrow(.x) > 0, "YES", "NO")) }), .names="dfs_adhc_wd")
  ) %>% fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE)


##PENR-4
crdcID= "PENR_4"
PENR_4 <- dfAdvCourseesEnrolls_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(
    across("dfs", ~ map(.x, ~ filter(.x, DualCreditIndicator == 1 & ( crdcGrade %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeOnCountDay %in% c(14, 15, 16, 17, 18, 19) )) )),
           .names="{.col}_filtered")
  ) %>% 
  mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt=TRUE, cntColumnName="personID"), .names="{.col}_grouped")
  ) %>% 
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
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>%
  mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_fillInWithAValue, 0, FALSE)



PENR_1_Bound <- cbind(ncesSchoolID = PENR_1$ncesSchoolID, schoolID = PENR_1$schoolID, PENR_1$dfs_adhc_wd %>% reduce(rbind))
PENR_2_Bound <- cbind(schoolID = PENR_2$schoolID, PENR_2$dfs_adhc_wd %>% reduce(rbind))
PENR_3_Bound <- cbind(schoolID = PENR_3$schoolID, PENR_3$dfs_adhc_wd %>% reduce(rbind))
PENR_4_Bound <- cbind(schoolID = PENR_4$schoolID, PENR_4$dfs_adhc_wd %>% reduce(rbind))


PENR_Bound <- bind_cols(
  PENR_1_Bound, 
  select(PENR_2_Bound, -schoolID), 
  select(PENR_3_Bound, -schoolID),
  select(PENR_4_Bound, -schoolID)
) 

PENR_Bound %>% writexl::write_xlsx("PENR.xlsx")




  