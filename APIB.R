sDir <- getwd()
sFiles <- dir(sDir, pattern="OCR_Helpers.R")
for( f in sFiles ) {
  source(file = file.path(sDir, f))
}

# Data...
## Student Profiles...
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}',  @mapYear='{mapYear_id}', @SetDay='{specific_date}', @EOYDay='{eoy_date}'", 
                     year_id=str_SchoolYear, mapYear_id=str_SchoolYear, specific_date=dt_CountDay, eoy_date=dt_EOY)
  dfStudentProfilesOnCountDay <- qryHelper2(strQ, str_ServerName, str_DBName) %>%
    #dfStudentProfilesOnCountDay <- dfStudentProfilesOnCountDay %>%
    feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("crdcGrade")), columnValues=gradesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("raceEthnicity")), columnValues=ethnicitiesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("gender")), columnValues=genderList ) %>%
    #mutate(birthDate = lubridate::mdy_hm(birthDate) ) %>% 
    mutate(AgeOnEOY = as.integer(AgeOnEOY), AgeOnCountDay = as.integer(AgeOnCountDay)) %>%
    mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19)))
  save(dfStudentProfilesOnCountDay, file = "studentProfiles_CountDay.RData")
  #load(file = "studentProfiles_CountDay.RData")
toc()
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}',  @mapYear='{mapYear_id}', @SetDay='{specific_date}', @EOYDay='{eoy_date}'", 
                     year_id=str_SchoolYear, mapYear_id=str_SchoolYear, specific_date=dt_CountDay_2ndSemester, eoy_date=dt_EOY)
  dfStudentProfilesOnCountDay_2ndSemester <- qryHelper2(strQ, str_ServerName, str_DBName) %>%
    #dfStudentProfilesOnCountDay_2ndSemester <- dfStudentProfilesOnCountDay_2ndSemester %>% 
    feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("crdcGrade")), columnValues=gradesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("raceEthnicity")), columnValues=ethnicitiesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("gender")), columnValues=genderList ) %>%
    #mutate(birthDate = lubridate::mdy_hm(birthDate) ) %>% 
    mutate(AgeOnEOY = as.integer(AgeOnEOY), AgeOnCountDay = as.integer(AgeOnCountDay)) %>%
    mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) 
  save(dfStudentProfilesOnCountDay_2ndSemester, file = "studentProfiles_CountDay_2ndSemester.RData")
  #load(file = "studentProfiles_CountDay_2ndSemester.RData")
toc()

## Advance Level Course Rosters...
tic()
  strQ <- glue::glue("EXEC crdc_2021.generate_AdHoc_AdvancedCourseEnrollmentTable_District @SchoolYear='{year_id}', @CountDay='{countDate_ID}'", 
                     year_id="2021", countDate_ID=as.character(dt_CountDay))
  dfAdvCoursesEnrolls_CountDay <- qryHelper2(strQ, str_ServerName, str_DBName) %>% 
  filter(dt_CountDay %between% list(EntryFullDate, ExitFullDate)) 
  save(dfAdvCoursesEnrolls_CountDay, file="CountDay_AdvCoursesEnrolls_CountDay.RData")
  #load("CountDay_AdvCoursesEnrolls_CountDay.RData")
toc()
tic()
  strQ <- glue::glue("EXEC crdc_2021.generate_AdHoc_AdvancedCourseEnrollmentTable_District @SchoolYear='{year_id}', @CountDay='{countDate_ID}'", 
                     year_id="2021", countDate_ID=as.character(dt_CountDay_2ndSemester))
  dfAdvCoursesEnrolls_2ndTermCountDay <- qryHelper2(strQ, str_ServerName, str_DBName) %>%
  filter(dt_CountDay_2ndSemester %between% list(EntryFullDate, ExitFullDate)) 
  save(dfAdvCoursesEnrolls_2ndTermCountDay, file="CountDay_AdvCoursesEnrolls_2ndTermCountDay.RData")
  #load("CountDay_AdvCoursesEnrolls_2ndTermCountDay.RData")
toc()

## List of Block Scheduling schools...
tic() 
  strQ <- glue::glue("EXEC [crdc_2021].generate_BlockSchedulingCalendarList @endYear='{year_id}', @CountDay='{count_day}'", year_id="2021", count_day="")
  dfCalendars_BlockSchedulingSchools <- qryHelper2(strQ, str_ServerName, str_DBName)
  save(dfCalendars_BlockSchedulingSchools, file = "Calendars_BlockSchedulingSchools.RData")
  #load(file = "Calendars_BlockSchedulingSchools.RData")
toc()

## Course Types...
tic()
  strQ <- "EXEC crdc_2021.generate_AdHoc_AdvancedCourseTypes"
  dfCourseCategories <- qryHelper2(strQ, str_ServerName, str_DBName)
toc()


tic()
  dfStudentProfiles_AdvCoursesEnrolls_CountDay <- 
  joinWithExternalData2(dfStudentProfilesOnCountDay, dfAdvCoursesEnrolls_CountDay, 
                        byMatchColumns = c("schoolID"="schoolID", "studentNumber"="StudentNumber"), 
                        selectColumns = expr(c("schoolID.x", "ncesSchoolID", "stateSchoolID", "schoolName", 
                                               "studentNumber", "personID", "gender", "raceEthnicity", "crdcGrade", 
                                               "AgeOnCountDay", "AgeOnEOY", "AgeGroup", "inIDEA", "inEL", "in504", "inGate", 
                                               "CourseCode", "CourseTitle", "CourseLevelCode", "ActivelyEnrolled", 
                                               "APIndicator", "IBIndicator", "CTEIndicator", "DualCreditIndicator")), 
                        joinType="L") %>% #rename(schoolID = schoolID.x) %>% 
  rename_with(~ str_remove(., "\\.x$"), .cols = ends_with(".x")) %>% 
  joinWithExternalData2(dfCourseCategories, 
                        byMatchColumns = c("CourseCode" = "cNB"), 
                        joinType = "L") %>% 
  mutate(CourseCategory = courseType %>% map(., ~ map(predicatesList_CourseTypes_APIB, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                           unlist() %>% .[which(.)] %>% names() %>%
                                           head(1) %||% NA )) %>% 
  mutate(CourseCategory = map_chr(CourseCategory, ~ ifelse(length(CourseCategory) == 0, "_", .)))
toc()
tic()
  dfStudentProfiles_AdvCoursesEnrolls_2ndTermCountDay <- 
  joinWithExternalData2(dfStudentProfilesOnCountDay_2ndSemester, dfCalendars_BlockSchedulingSchools, 
                        byMatchColumns = c("schoolID"="schoolID"),
                        selectColumns = expr(c("schoolID.x", "ncesSchoolID.x", "stateSchoolID", "schoolName.x", 
                                               "studentNumber", "personID", "gender", "raceEthnicity", "crdcGrade", 
                                               "AgeOnCountDay", "AgeOnEOY", "AgeGroup", "inIDEA", "inEL", "in504", "inGate")), 
                        joinType ="I") %>% #rename(schoolID = schoolID.x) %>% 
  rename_with(~ str_remove(., "\\.x$"), .cols = ends_with(".x")) %>% 
  joinWithExternalData2(dfAdvCoursesEnrolls_2ndTermCountDay, 
                        byMatchColumns = c("schoolID"="schoolID", "studentNumber"="StudentNumber"), 
                        selectColumns = expr(c("schoolID.x", "ncesSchoolID", "stateSchoolID", "schoolName", 
                                               "studentNumber", "personID", "gender", "raceEthnicity", "crdcGrade", 
                                               "AgeOnCountDay", "AgeOnEOY", "AgeGroup", "inIDEA", "inEL", "in504", "inGate", 
                                               "CourseCode", "CourseTitle", "CourseLevelCode", "ActivelyEnrolled", 
                                               "APIndicator", "IBIndicator", "CTEIndicator", "DualCreditIndicator")), 
                        joinType="L") %>% #rename(schoolID = schoolID.x) %>% 
  rename_with(~ str_remove(., "\\.x$"), .cols = ends_with(".x")) %>% 
  joinWithExternalData2(dfCourseCategories, 
                        byMatchColumns = c("CourseCode" = "cNB"), 
                        joinType = "L") %>% 
  mutate(CourseCategory = courseType %>% map(., ~ map(predicatesList_CourseTypes_APIB, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                               unlist() %>% .[which(.)] %>% names() %>%
                                               head(1) %||% NA )) %>% 
  mutate(CourseCategory = map_chr(CourseCategory, ~ ifelse(length(CourseCategory) == 0, "_", .)))
toc()

tic()
  dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split <-
  dfStudentProfiles_AdvCoursesEnrolls_CountDay %>% 
  group_by(schoolID) %>% 
  split(.$schoolID) %>% 
  tibble(dfs = .)  %>% ungroup() %>%  
  mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) %>%
  mutate(schoolName = map(dfs, ~ .[1, "schoolName"]) %>% unlist())
toc()
tic()
  dfStudentProfiles_AdvCoursesEnrolls_2ndTermCountDay_Split <-
  dfStudentProfiles_AdvCoursesEnrolls_2ndTermCountDay %>% 
  group_by(schoolID) %>% 
  split(.$schoolID) %>% 
  tibble(dfs = .)  %>% ungroup() %>%  
  mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) %>% 
  mutate(schoolName = map(dfs, ~ .[1, "schoolName"]) %>% unlist())
toc()
tic()
  dfStudentProfiles_AdvCoursesEnrolls_Combined_Split <- 
  union(dfStudentProfiles_AdvCoursesEnrolls_CountDay, dfStudentProfiles_AdvCoursesEnrolls_2ndTermCountDay) %>% 
  group_by(schoolID) %>% 
  split(.$schoolID) %>% 
  tibble(dfs = .)  %>% ungroup() %>%  
  mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) %>% 
    mutate(schoolName = map(dfs, ~ .[1, "schoolName"]) %>% unlist())
toc()


# Loading Enrolled Students list, to check on
load(file="schoolsList.RData")
schoolsFromCRDCMapping <- schoolsFromCRDCMapping %>%
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, AgeGroup = cut(as.integer(AgeOnCountDay), c(2, 10, 13, 19)))))) %>%
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="HSEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% MS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))), .names="MSEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% ES_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(2,10]"))), .names="ESEnrollsFiltered"))
  # mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% c("Grade 7", "Grade 8") | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))), .names="7th8thGradesEnrollsFiltered")) %>% 
  # mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 7" )), .names="7thGradesEnrollsFiltered")) %>% 
  # mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 8" )), .names="8thGradesEnrollsFiltered")) AllGradeLevelsEnrolls_Index <- schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% map_lgl(~ nrow(.) > 0) %>% which()
AllGradeLevelsEnrolls_Index <- schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% map_lgl(~ nrow(.) > 0) %>% which()
AllGradeLevelsEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[AllGradeLevelsEnrolls_Index] %>% unlist()
HSEnrolls_Index <- schoolsFromCRDCMapping$HSEnrollsFiltered %>% map_lgl(~ nrow(.) > 0) %>% which()
HSEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[HSEnrolls_Index] %>% unlist()
MSEnrolls_Index <- schoolsFromCRDCMapping$MSEnrollsFiltered %>% map_lgl(~ nrow(.) > 0) %>% which()
MSEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[MSEnrolls_Index] %>% unlist()
ESEnrolls_Index <- schoolsFromCRDCMapping$ESEnrollsFiltered %>% map_lgl(~ nrow(.) > 0) %>% which()
ESEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[ESEnrolls_Index] %>% unlist()
# Grades7th8th_Enrolls_Index <- schoolsFromCRDCMapping$`7th8thGradesEnrollsFiltered` %>% map_lgl(~ nrow(.) > 0) %>% which()
# Grades7th8th_Enrolls_IDs <- schoolsFromCRDCMapping$schoolID[Grades7th8th_Enrolls_Index] %>% unlist()
# Grades7th_Enrolls_Index <- schoolsFromCRDCMapping$`7thGradesEnrollsFiltered` %>% map_lgl(~ nrow(.) > 0) %>% which()
# Grades7th_Enrolls_IDs <- schoolsFromCRDCMapping$schoolID[Grades7th_Enrolls_Index] %>% unlist()
# Grades8th_Enrolls_Index <- schoolsFromCRDCMapping$`8thGradesEnrollsFiltered` %>% map_lgl(~ nrow(.) > 0) %>% which()
# Grades8th_Enrolls_IDs <- schoolsFromCRDCMapping$schoolID[Grades8th_Enrolls_Index] %>% unlist()
rm(schoolsFromCRDCMapping)


##APIB_1
crdcID= "APIB_1"
APIB_1 <- dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, IBIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ { tibble(SCH_IBENR_IND = if_else(nrow(.x) > 0, "YES", "NO")) }), .names="dfs_adhc_wd")
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE) 


##APIB_2
crdcID= "APIB_2"
APIB_2 <- dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, IBIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(
    across("filtered_for_Courses", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="_grouped") 
  ) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


##APIB_3
crdcID= "APIB_3"
APIB_3 <- dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ { tibble(SCH_APENR_IND = if_else(nrow(.x) > 0, "YES", "NO")) }), .names="dfs_adhc_wd")
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE) 



##APIB_4
crdcID= "APIB_4"
APIB_4 <- dfStudentProfiles_AdvCoursesEnrolls_Combined_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(across("filtered_for_Courses", ~ map(.x, ~ distinct(.x, CourseTitle )), .names="CoursesList")) %>% 
  #mutate(across("CoursesList", ~ map(.x, ~ { tibble(count(.x, name="SCH_APCOURSES")), .names="dfs_adhc_wd")) %>% 
  mutate(across("CoursesList", ~ map(.x, ~ { tibble(SCH_APCOURSES = nrow(.x)) }), .names="dfs_adhc_wd")) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE)


crdcID= "APIB_5"
APIB_5 <- dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(dfs_adhc_wd = map(schoolID, ~ build_EmptyResultSet(crdcID, "No"))) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


crdcID= "APIB_6"
APIB_6 <- dfStudentProfiles_AdvCoursesEnrolls_Combined_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(across("filtered_for_Courses", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)
  

crdcID= "APIB_7"
APIB_7 <- dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>%
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ filter(.x, CourseCategory == APIB_Course_Categories[1])), .names="filtered_for_Category")
  ) %>% 
  mutate(across("filtered_for_Category", ~ map(.x, ~ { tibble( SCH_APMATHENR_IND = if_else(nrow(.x) > 0, "Yes", "No")) }), .names="dfs_adhc_wd")) %>%
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, "No"))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)
  

crdcID <- "APIB_8"
APIB_8 <- dfStudentProfiles_AdvCoursesEnrolls_Combined_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>%
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ filter(.x, replace_na(CourseCategory, "") == APIB_Course_Categories[1])), .names="filtered_for_Category")
  ) %>% 
  mutate(
    across("filtered_for_Category", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="_grouped") 
  ) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE)


crdcID <- "APIB_9"
APIB_9 <- dfStudentProfiles_AdvCoursesEnrolls_CountDay_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>%
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ filter(.x, CourseCategory == APIB_Course_Categories[2])), .names="filtered_for_Category")
  ) %>% 
  mutate(
    across("filtered_for_Category", ~ map(.x, ~ { tibble(SCH_APSCIENR_IND = if_else(nrow(.x) > 0, "Yes", "No")) }), .names="dfs_adhc_wd")
  ) %>%
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID <- "APIB_10"
APIB_10 <- dfStudentProfiles_AdvCoursesEnrolls_Combined_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ filter(.x, replace_na(CourseCategory, "") == APIB_Course_Categories[2])), .names="filtered_for_Category")
  ) %>% 
  mutate(
    across("filtered_for_Category", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="_grouped") 
  ) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE)
  

crdcID <- "APIB_11"
APIB_11 <- dfStudentProfiles_AdvCoursesEnrolls_Combined_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ filter(.x, CourseCategory == APIB_Course_Categories[3])), .names="filtered_for_Category") 
  ) %>% 
  mutate(
    across("filtered_for_Category", ~ map(.x, ~ { tibble(CH_APCOMPENR_IND = if_else(nrow(.x) > 0, "Yes", "No")) }), .names="dfs_adhc_wd")
  ) %>%
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


crdcID <- "APIB_12"
APIB_12 <- dfStudentProfiles_AdvCoursesEnrolls_Combined_Split %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]")) ), .names="filtered_For_Grades")) %>%
  mutate(
    across("filtered_For_Grades", ~ map(.x, ~ filter(.x, APIndicator == "1")), .names="filtered_for_Courses")
  ) %>% 
  mutate(
    across("filtered_for_Courses", ~ map(.x, ~ filter(.x, replace_na(CourseCategory, "") == APIB_Course_Categories[3])), .names="filtered_for_Category")
  ) %>% 
  mutate(
    across("filtered_for_Category", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="_grouped") 
  ) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, NA, TRUE)



APIB_1_Bound <- cbind(ncesSchoolID = APIB_1$ncesSchoolID, schoolID = APIB_1$schoolID, APIB_1$dfs_adhc_wd %>% reduce(rbind))
APIB_2_Bound <- cbind(schoolID = APIB_2$schoolID, APIB_2$dfs_adhc_wd %>% reduce(rbind))
APIB_3_Bound <- cbind(schoolID = APIB_3$schoolID, APIB_3$dfs_adhc_wd %>% reduce(rbind))
APIB_4_Bound <- cbind(schoolID = APIB_4$schoolID, APIB_4$dfs_adhc_wd %>% reduce(rbind))
APIB_5_Bound <- cbind(schoolID = APIB_5$schoolID, APIB_5$dfs_adhc_wd %>% reduce(rbind))
APIB_6_Bound <- cbind(schoolID = APIB_6$schoolID, APIB_6$dfs_adhc_wd %>% reduce(rbind))
APIB_7_Bound <- cbind(schoolID = APIB_7$schoolID, APIB_7$dfs_adhc_wd %>% reduce(rbind))
APIB_8_Bound <- cbind(schoolID = APIB_8$schoolID, APIB_8$dfs_adhc_wd %>% reduce(rbind))
APIB_9_Bound <- cbind(schoolID = APIB_9$schoolID, APIB_9$dfs_adhc_wd %>% reduce(rbind))
APIB_10_Bound <- cbind(schoolID = APIB_10$schoolID, APIB_10$dfs_adhc_wd %>% reduce(rbind))
APIB_11_Bound <- cbind(schoolID = APIB_11$schoolID, APIB_11$dfs_adhc_wd %>% reduce(rbind))
APIB_12_Bound <- cbind(schoolID = APIB_12$schoolID, APIB_12$dfs_adhc_wd %>% reduce(rbind))

APIB_Bound <- bind_cols(
  APIB_1_Bound,
  select(APIB_2_Bound, -schoolID),
  select(APIB_3_Bound, -schoolID),
  select(APIB_4_Bound, -schoolID),
  select(APIB_5_Bound, -schoolID),
  select(APIB_6_Bound, -schoolID),
  select(APIB_7_Bound, -schoolID),
  select(APIB_8_Bound, -schoolID),
  select(APIB_9_Bound, -schoolID),
  select(APIB_10_Bound, -schoolID),
  select(APIB_11_Bound, -schoolID),
  select(APIB_12_Bound, -schoolID)
) 

APIB_Bound %>% writexl::write_xlsx("APIB.xlsx")


