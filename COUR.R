## COUR
sdir <- getwd() 
sfiles <- dir(sdir, pattern="OCR_Helpers.R")
for(f in sfiles) {
  source(file=file.path(sdir, f))
}

## Data on Course Roster data
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_CourseRosterTeacherInfo_CountDay @endYear='{year_id}', @CountDay='{count_day}'", year_id="2021", count_day=dt_CountDay)
  dfCourses_CountDay <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfCourses_CountDay, file = "CoursesCountDay.RData")
  #load(file = "CoursesCountDay.RData")
toc()  
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_CourseRosterTeacherInfo_CountDay @endYear='{year_id}', @CountDay='{count_day}'", year_id="2021", count_day=dt_CountDay_2ndSemester)
  dfCourses_CountDay_2ndSemester <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfCourses_CountDay_2ndSemester, file = "CoursesCountDay_2ndSemester.RData")
  #load(file = "CoursesCountDay_2ndSemester.RData")
toc()
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_CourseRosterTeacherInfo_CountDay @endYear='{year_id}', @CountDay='{count_day}'", year_id="2021", count_day=dt_EOY)
  dfCourses_EOYDay <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfCourses_EOYDay, file = "CoursesEOYDay.RData")
  #load(file = "CoursesEOYDay.RData")
toc() 
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_CourseRosterTeacherInfo_CountDay @endYear='{year_id}', @CountDay='{count_day}'", year_id="2021", count_day=dt_BlockSchedule_Term1_EndDate)
  dfCourses_Term1EndDay <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfCourses_Term1EndDay, file = "CoursesTerm1EndDay.RData")
  #load(file = "CoursesTerm1EndDay.RData")
toc() 

## List of Block Scheduling schools...
tic() 
  strQ <- glue::glue("EXEC [crdc_2021].generate_BlockSchedulingCalendarList @endYear='{year_id}', @CountDay='{count_day}'", year_id="2021", count_day="")
  dfCalendars_BlockSchedulingSchools <- qryHelper2(strQ, str_ServerName, str_DBName)
  save(dfCalendars_BlockSchedulingSchools, file = "Calendars_BlockSchedulingSchools.RData")
  #load(file = "Calendars_BlockSchedulingSchools.RData")
toc()

## Student Profile Data...
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{count_day}', @EOYDay='{eoy_day}'", 
                     year_id=str_SchoolYear, mapYear_id=str_SchoolYear, count_day=dt_CountDay, eoy_day=dt_EOY)  
  dfStudentProfilesOnCountDay <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfStudentProfilesOnCountDay, file = "studentProfiles_CountDay.RData")
  #load(file = "studentProfiles_CountDay.RData")
toc()
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{count_day}', @EOYDay='{eoy_day}'", 
                     year_id=str_SchoolYear, mapYear=str_SchoolYear, count_day=dt_CountDay_2ndSemester, eoy_day=dt_EOY)  
  dfStudentProfilesOnCountDay_2ndSemester <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfStudentProfilesOnCountDay_2ndSemester, file = "studentProfiles_CountDay_2ndSemester.RData")
  #load(file = "studentProfiles_CountDay_2ndSemester.RData")
toc()
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{count_day}', @EOYDay='{eoy_day}'", 
                     year_id=str_SchoolYear, mapYear=str_SchoolYear, count_day=dt_EOY, eoy_day=dt_EOY)
  dfStudentProfilesOnEOY <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfStudentProfilesOnEOY, file = "studentProfiles_EOYDay.RData")
  #load(file = "studentProfiles_EOYDay.RData")
toc()
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{count_day}', @EOYDay='{eoy_day}'", 
                     year_id=str_SchoolYear, mapYear=str_SchoolYear, count_day=dt_BlockSchedule_Term1_EndDate, eoy_day=dt_EOY)
  dfStudentProfilesOn_Term1EndDay <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfStudentProfilesOn_Term1EndDay, file = "studentProfiles_Term1EndDay.RData")
  #load(file = "studentProfiles_Term1EndDay.RData")
toc()

## Data on Grades earned for Math ( Algebra I ) courses... 
tic()
  CurriculumStandardType <- "Mathematics"
  strQ <- glue::glue("EXEC [crdc_2021].generate_SelectedCourseGroupGrades_OnDate @endYear='{year_id}', @SetDay='{count_day}', @CourseType='{Course_Type}', @GradingTaskType='{GradingTask_Type}', @CurriculumStandardType='{CurriculumStandard_Type}'", 
                     year_id=str_SchoolYear, count_day=dt_EOY, Course_Type = CourseType, GradingTask_Type = GradingTaskType, CurriculumStandard_Type = CurriculumStandardType)
  dfStudent_Math_GradesEarnedOnEOY <- qryHelper2(strQ, str_ServerName, str_DBName) %>% 
  filter(trim(csNumber) %>% stringr::str_ends(pattern="2") & as.double(creditsEarned) > 0.000 & transcriptGrade %in% passingGrade )
  save(dfStudent_Math_GradesEarnedOnEOY, file="student_Math_Grades_EOYDay.RData")
  #load(file="student_Math_Grades_EOYDay.RData")
toc()
tic()
  CurriculumStandardType <- "Mathematics"
  strQ <- glue::glue("EXEC [crdc_2021].generate_SelectedCourseGroupGrades_OnDate @endYear='{year_id}', @SetDay='{count_day}', @CourseType='{Course_Type}', @GradingTaskType='{GradingTask_Type}', @CurriculumStandardType='{CurriculumStandard_Type}'", 
                     year_id=str_SchoolYear, count_day=dt_BlockSchedule_Term1_EndDate, Course_Type = CourseType, GradingTask_Type = GradingTaskType, CurriculumStandard_Type = CurriculumStandardType)
  dfStudent_Math_GradesEarnedOn_Term1EndDate <- qryHelper2(strQ, str_ServerName, str_DBName) %>% 
    filter(trim(csNumber) %>% stringr::str_ends(pattern="2") & as.double(creditsEarned) > 0.000 & transcriptGrade %in% passingGrade )
  save(dfStudent_Math_GradesEarnedOn_Term1EndDate, file="student_Math_Grades_Term1EndDate.RData")
  #load(file="student_Math_Grades_Term1EndDate.RData")
toc()


dfCertifiedTeachers <- 
  readxl::read_xlsx(paste(getwd(), "data", "testsheet.xlsx", sep="/")) %>% 
  filter(!is.na(Endorsements)) %>%
  mutate(ID = as.character(ID)) %>% 
  mutate(Endorsements_splits = 
           map(.$Endorsements, ~ str_split(., pattern=",") %>% unlist() %>% map_chr(., ~ str_trim(.)))) %>% 
  strListToIndicatorColumns("Endorsements_splits", Endorsements_CategoryName, EndorsementsCertification_Tests) 


dfCertifiedTeachers_Charters <- 
  readxl::read_xlsx(paste(getwd(), "data", "Charter School Teacher Endrosements.xlsx", sep="/")) %>% 
  filter(!is.na(Endorsements)) %>%
  mutate(teacherPersonID = trim(as.character(teacherPersonID))) %>% 
  group_by(ccsdID, teacherName, teacherPersonID) %>% 
  group_split(.keep=TRUE) %>% tibble(dfs=.) %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, summarise(.x, Endorsements = paste0(Endorsements, collapse=",")) %>% ungroup())))) %>%
  ##mutate(across("dfs", ~ map(.x, ~ mutate(.x, summarise(.x, Endorsements = list(Endorsements)) %>% ungroup())))) %>%    ### aggr endorsement column into a string vector...
  unnest(cols=dfs) %>% distinct() %>%
  mutate(Endorsements_splits = 
           map(.$Endorsements, ~ str_split(., pattern=",") %>% unlist() %>% map_chr(., ~ str_trim(.)))) %>%
  strListToIndicatorColumns("Endorsements_splits", Endorsements_CategoryName, EndorsementsCertification_Tests)

tic()
  dfCombined_CountDay_Union <- 
  union(
    left_join(dfCourses_CountDay, select(dfStudentProfilesOnCountDay, "personID", "gender", "raceEthnicity", "grade", "crdcGrade", "birthDate", "AgeOnCountDay", "AgeOnEOY", "inIDEA", "inEL", "in504", "inGate"), by="personID"), 
    inner_join(dfCourses_CountDay_2ndSemester, distinct(dfCalendars_BlockSchedulingSchools, schoolID), by="schoolID", keep=FALSE, na_matches="never") %>% 
    left_join(select(dfStudentProfilesOnCountDay_2ndSemester, "personID", "gender", "raceEthnicity", "grade", "crdcGrade", "birthDate", "AgeOnCountDay", "AgeOnEOY", "inIDEA", "inEL", "in504", "inGate"), by="personID") 
  ) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate="crdcGrade", columnValues=gradesList) %>%
  feat_SwitchValues( colsToUpdate="raceEthnicity", columnValues=ethnicitiesList) %>% 
  feat_SwitchValues( colsToUpdate="gender", columnValues=genderList) %>%
  mutate(AgeOnEOY = as.integer(AgeOnEOY), AgeOnCountDay = as.integer(AgeOnCountDay)) %>%
  mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>%  
    # now join with teachers's endorsement info... 
  left_join( select(dfCertifiedTeachers, "ID", "Job Title", "Math_Endorsed", "Science_Endorsed", "CompSci_Endorsed"), by=c("staffNumber" = "ID"), na_matches = "never") %>%
  left_join( select(dfCertifiedTeachers_Charters, "teacherPersonID", "Math_Endorsed", "Science_Endorsed", "CompSci_Endorsed"), by=c("teacherPersonID" = "teacherPersonID"), na_matches = "never") %>%
    ### !!! Safe to bluntly coalesce .x with .y in this line, because it is assured that there are no overwrapping instructors between reg schools and charter schools found in EDA...
  mutate(across(ends_with("_Endorsed.x"), ~ coalesce(., get(str_replace(cur_column(), "\\.x$", "\\.y"))))) %>%  
  rename_with(.fn = ~ str_remove(., '\\.x'), .cols=ends_with('.x')) %>% 
  group_by(schoolID) %>% dplyr::group_split(.keep=TRUE) %>% tibble(dfs=.) %>% ungroup() %>% 
  mutate(schoolID = map(.$dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(.$dfs, ~.[1, "ncesSchoolID"]) %>% unlist()) %>%
  mutate(schoolName = map(.$dfs, ~.[1, "schoolName"]) %>% unlist())
  save(dfCombined_CountDay_Union, file="dfCombined_CountDay_Union.RData")
  #load("dfCombined_CountDay_Union.RData")
toc()



# Courses rostered on EOY, also attached is the completion of 02 section of Algebra I Courses...
tic()
  dfCombined_EndDay_Union <- 
  union(
    left_join(dfCourses_EOYDay, select(dfStudentProfilesOnEOY, "personID", "gender", "raceEthnicity", "grade", "crdcGrade", "birthDate", "AgeOnCountDay", "AgeOnEOY", "inIDEA", "inEL", "in504", "inGate"), by="personID", na_matches="never") %>%
      left_join( select(dfStudent_Math_GradesEarnedOnEOY, "transcriptGrade", "creditsEarned", "pid" = "personID" ), by=c("personID" = "pid"), na_matches = "never"),
    inner_join(dfCourses_Term1EndDay, distinct(dfCalendars_BlockSchedulingSchools, schoolID), by="schoolID", keep=FALSE, na_matches="never") %>% 
      left_join(select(dfStudentProfilesOn_Term1EndDay, "personID", "gender", "raceEthnicity", "grade", "crdcGrade", "birthDate", "AgeOnCountDay", "AgeOnEOY", "inIDEA", "inEL", "in504", "inGate"), by="personID", na_matches="never") %>%
      left_join(select(dfStudent_Math_GradesEarnedOn_Term1EndDate, "transcriptGrade", "creditsEarned", "pid" = "personID" ), by=c("personID" = "pid"), na_matches = "never")
  )  %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate="crdcGrade", columnValues=gradesList) %>%
  feat_SwitchValues( colsToUpdate="raceEthnicity", columnValues=ethnicitiesList) %>% 
  feat_SwitchValues( colsToUpdate="gender", columnValues=genderList) %>%
  mutate(AgeOnEOY = as.integer(AgeOnEOY), AgeOnCountDay = as.integer(AgeOnCountDay)) %>%
  mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>%  
  # now join with teachers's endorsement info... 
  left_join( select(dfCertifiedTeachers, "ID", "Job Title", "Math_Endorsed", "Science_Endorsed", "CompSci_Endorsed"), by=c("staffNumber" = "ID"), na_matches = "never") %>%
  
  #rename(crdcGrade = stateGrade) %>% 
  group_by(schoolID) %>% dplyr::group_split(.keep=TRUE) %>% tibble(dfs=.) %>% ungroup() %>% 
  mutate(schoolID = map(.$dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(.$dfs, ~.[1, "ncesSchoolID"]) %>% unlist()) %>% 
  mutate(schoolName = map(.$dfs, ~.[1, "schoolName"]) %>% unlist())
  save(dfCombined_EndDay_Union, file="dfCombined_EndDay_Union.RData")
  #load("dfCombined_EndDay_Union.RData")
toc()

schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% reduce(rbind) %>% View()



load("schoolsList.RData")
schoolsFromCRDCMapping <- schoolsFromCRDCMapping %>%
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, AgeGroup = cut(as.integer(AgeOnCountDay), c(2, 10, 13, 19)))))) %>%
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% HS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="HSEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% MS_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))), .names="MSEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% ES_GradesList | ( crdcGrade == "Ungraded" & AgeGroup == "(2,10]"))), .names="ESEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% c("Grade 7", "Grade 8") | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))), .names="7th8thGradesEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 7" )), .names="7thGradesEnrollsFiltered")) %>% 
  mutate(across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 8" )), .names="8thGradesEnrollsFiltered")) 
AllGradeLevelsEnrolls_Index <- schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% map_lgl(~ nrow(.) > 0) %>% which()
AllGradeLevelsEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[AllGradeLevelsEnrolls_Index] %>% unlist()
HSEnrolls_Index <- schoolsFromCRDCMapping$HSEnrollsFiltered %>% map_lgl(~ nrow(.) > 0) %>% which()
HSEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[HSEnrolls_Index] %>% unlist()
MSEnrolls_Index <- schoolsFromCRDCMapping$MSEnrollsFiltered %>% map_lgl(~ nrow(.) > 0) %>% which()
MSEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[MSEnrolls_Index] %>% unlist()
ESEnrolls_Index <- schoolsFromCRDCMapping$ESEnrollsFiltered %>% map_lgl(~ nrow(.) > 0) %>% which()
ESEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[ESEnrolls_Index] %>% unlist()
Grades7th8th_Enrolls_Index <- schoolsFromCRDCMapping$`7th8thGradesEnrollsFiltered` %>% map_lgl(~ nrow(.) > 0) %>% which()
Grades7th8th_Enrolls_IDs <- schoolsFromCRDCMapping$schoolID[Grades7th8th_Enrolls_Index] %>% unlist()
Grades7th_Enrolls_Index <- schoolsFromCRDCMapping$`7thGradesEnrollsFiltered` %>% map_lgl(~ nrow(.) > 0) %>% which()
Grades7th_Enrolls_IDs <- schoolsFromCRDCMapping$schoolID[Grades7th_Enrolls_Index] %>% unlist()
Grades8th_Enrolls_Index <- schoolsFromCRDCMapping$`8thGradesEnrollsFiltered` %>% map_lgl(~ nrow(.) > 0) %>% which()
Grades8th_Enrolls_IDs <- schoolsFromCRDCMapping$schoolID[Grades8th_Enrolls_Index] %>% unlist()
rm(schoolsFromCRDCMapping)



## COUR_1a...
crdcID = "COUR_1a"
COUR_1a <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade %in% c("Grade 7", "Grade 8") | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]")) ), 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ distinct(.x, csName, csStateCode, csNumber, courseID, sectionID, crdcGrade)), .names="Algebra1_Sections")) %>% 
  mutate(across("Algebra1_Sections", ~ map(.x, ~ count(.x, name="SCH_ALGCLASSES_GS0708")), .names="dfs_adhc_wd")) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


## COUR_1b...
crdcID = "COUR_1b"
COUR_1b <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade %in% c("Grade 7", "Grade 8") | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]")) ), 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ filter(.x, Math_Endorsed == 1)), .names="filtered_for_EndorsedTeacher")) %>%
  mutate(across("filtered_for_EndorsedTeacher", ~ map(.x, ~ distinct(.x, csName, csStateCode, csNumber, courseID, sectionID)), .names="Algebra1_Sections")) %>%
  mutate(across("Algebra1_Sections", ~ map(.x, ~ count(.x, name="SCH_ALGCERT_GS0708")), .names="dfs_adhc_wd")) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


## COUR_2a...
crdcID = "COUR_2a"
COUR_2a <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade == "Grade 7") ), 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ distinct(.x, csName, csStateCode, csNumber, courseID, sectionID)), .names="Algebra1_Sections")) %>% 
  mutate(across("Algebra1_Sections", ~ map(.x, ~ count(.x, name="SCH_ALGENR_G07_IND")), .names="dfs_adhc_wd")) %>%
  mutate(across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, SCH_ALGENR_G07_IND = if_else(SCH_ALGENR_G07_IND > 0, "Yes", "No"))))) %>%
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


## COUR_2b...
crdcID = "COUR_2b"
COUR_2b <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade =="Grade 8" | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))) , 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ distinct(.x, csName, csStateCode, csNumber, courseID, sectionID)), .names="Algebra1_Sections")) %>% 
  mutate(across("Algebra1_Sections", ~ map(.x, ~ count(.x, name="SCH_ALGENR_G08_IND")), .names="dfs_adhc_wd")) %>%
  mutate(across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, SCH_ALGENR_G08_IND = if_else(SCH_ALGENR_G08_IND > 0, "Yes", "No"))))) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


# No_8thG_Enrolled <- which(COUR_2b$filtered_for_MS_Grades %>% map(nrow) == 0)
# COUR_2b[No_8thG_Enrolled, ] <- COUR_2b[No_8thG_Enrolled, ] %>%
#   mutate(across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, "SCH_ALGENR_G08_IND" = NA))))  
# cbind(COUR_2a$schoolID, COUR_2a$schoolName, COUR_2a$dfs_adhc_wd %>% reduce(rbind)) -> a
# a %>% View()
# cbind(COUR_1b$schoolID, COUR_1b$schoolName, COUR_1b$dfs_adhc_wd %>% reduce(rbind)) -> a
# a %>% View()


## COUR_3a...
crdcID = "COUR_3a"
COUR_3a <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade == "Grade 7") ), 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ distinct(.x, personID)), .names="Algebra1_Students")) %>% 
  mutate(across("Algebra1_Students", ~ map(.x, ~ count(.x, name="SCH_ALGENR_G07")), .names="dfs_adhc_wd")) %>%
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


## COUR_3b...
crdcID = "COUR_3b"
COUR_3b <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade =="Grade 8" | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))) , 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Algebra1_Students_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_4a 
crdcID = "COUR_4a"
COUR_4a <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 7") ), .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("passed_Algebra1", ~ map(.x, ~ distinct(.x, personID)), .names="Algebra1_Students")) %>% 
  mutate(across("Algebra1_Students", ~ map(.x, ~ count(.x, name="SCH_ALGPASS_G07")), .names="dfs_adhc_wd")) %>%
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>%
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_4b
crdcID = "COUR_4b"
COUR_4b <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade =="Grade 8" | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))), .names="filtered_for_MS_Grades")) %>%
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("passed_Algebra1", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Algebra1_Students_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>%
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)
  

## COUR_5a...
crdcID = "COUR_5a"
COUR_5a <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade =="Grade 8" | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))) , 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Geometry")), .names="filtered_for_Geometry")) %>% 
  mutate(across("filtered_for_Geometry", ~ map(.x, ~ distinct(.x, csName, csStateCode, csNumber, courseID, sectionID)), .names="Geometry_Sections")) %>% 
  mutate(across("Geometry_Sections", ~ map(.x, ~ count(.x, name="SCH_GEOMENR_G08_IND")), .names="dfs_adhc_wd")) %>% # 
  mutate(across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, SCH_GEOMENR_G08_IND = if_else(SCH_GEOMENR_G08_IND > 0, "Yes", "No"))))) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, "No"))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


## COUR_5b...
crdcID = "COUR_5b"
COUR_5b <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", 
                ~ map(.x, 
                      ~ filter(.x, crdcGrade =="Grade 8" | ( crdcGrade == "Ungraded" & AgeGroup == "(10,13]"))) , 
                .names="filtered_for_MS_Grades")) %>% 
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Geometry")), .names="filtered_for_Geometry")) %>% 
  mutate(across("filtered_for_Geometry", ~ map(.x, ~ distinct(.x, personID)), .names="Geometry_Enrollees")) %>% 
  mutate(across("Geometry_Enrollees", ~ map(.x, ~ count(.x, name="SCH_GEOMENR_G08")), .names="dfs_adhc_wd")) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, "0"))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", MSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


##COUR_6a
crdcID = "COUR_6a"
COUR_6a <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Algebra1_Students_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


##COUR_6b
crdcID = "COUR_6b"
COUR_6b <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_MS_Grades")) %>%
  mutate(across("filtered_for_MS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Algebra1_Students_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_7a
crdcID = "COUR_7a"
COUR_7a <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("passed_Algebra1", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Algebra1_Students_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_7b
crdcID = "COUR_7b"
COUR_7b <- dfCombined_EndDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType == "Algebra I")), .names="filtered_for_Algebra1")) %>% 
  mutate(across("filtered_for_Algebra1", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("passed_Algebra1", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Algebra1_Students_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq", "dataElement2Seq")), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq, dataElement2Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>%
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_8
crdcID = "COUR_8"
COUR_8 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% mathCourseGroup )), .names="filtered_for_MathCourses")) %>% 
  mutate(across("filtered_for_MathCourses", ~ map(.x, 
                                                  ~ mutate(.x, courseCategory = courseType %>% 
                                                             map(., ~ map(predicatesList_MathCourseTypes, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                   unlist() %>% .[which(.)] %>% names() %>%
                                                   head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  mutate(across("filtered_for_MathCourses", ~map(.x, groupingByListOfcolumns, lc=list(c("courseCategory")), distinctCnt = TRUE, cntColumnName="sectionID"), .names="Math_Sections_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)
  


##COUR_9
crdcID = "COUR_9"
COUR_9 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% mathCourseGroup )), .names="filtered_for_MathCourses")) %>% 
  mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_endorsements", ~ map(.x, 
                                                  ~ mutate(.x, courseCategory = courseType %>% 
                                                             map(., ~ map(predicatesList_MathCourseTypes_COUR9, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                   unlist() %>% .[which(.)] %>% names() %>%
                                                                   head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_endorsements", ~map(.x, groupingByListOfcolumns, lc=list(c("courseCategory")), distinctCnt = TRUE, cntColumnName="sectionID"), .names="Math_Sections_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_9a
crdcID = "COUR_9a"
COUR_9a <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% mathCourseGroup )), .names="filtered_for_MathCourses")) %>% 
  mutate(across("filtered_for_MathCourses", ~ map(.x, 
                                                   ~ mutate(.x, courseCategory = courseType %>% 
                                                              map(., ~ map(predicatesList_MathCourseTypes_COUR9, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                    unlist() %>% .[which(.)] %>% names() %>%
                                                                    head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  mutate(across("filtered_for_MathCourses", ~map(.x, ~ filter(.x, courseCategory == "Algebra II")), .names="filtered_for_AlgebraII")) %>%
  mutate(across("filtered_for_AlgebraII", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Math_Enrollments_grouped")) %>% 
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



##COUR_9b
crdcID = "COUR_9b"
COUR_9b <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% mathCourseGroup )), .names="filtered_for_MathCourses")) %>% 
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_MathCourses", ~ map(.x, 
                                                  ~ mutate(.x, courseCategory = courseType %>% 
                                                             map(., ~ map(predicatesList_MathCourseTypes_COUR9, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                   unlist() %>% .[which(.)] %>% names() %>%
                                                                   head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_MathCourses", ~map(.x, ~ filter(.x, courseCategory == "Advanced Mathematics")), .names="filtered_for_AlgebraII")) %>%
  mutate(across("filtered_for_AlgebraII", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Math_Enrollments_grouped")) %>% 
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


##COUR_9c
crdcID = "COUR_9c"
COUR_9c <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% mathCourseGroup )), .names="filtered_for_MathCourses")) %>% 
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_MathCourses", ~ map(.x, 
                                                  ~ mutate(.x, courseCategory = courseType %>% 
                                                             map(., ~ map(predicatesList_MathCourseTypes_COUR9, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                   unlist() %>% .[which(.)] %>% names() %>%
                                                                   head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_MathCourses", ~map(.x, ~ filter(.x, courseCategory == "Calculus")), .names="filtered_for_Calculus")) %>%
  mutate(across("filtered_for_Calculus", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Math_Enrollments_grouped")) %>% 
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


##COUR_9d
crdcID = "COUR_9d"
COUR_9d <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% mathCourseGroup )), .names="filtered_for_MathCourses")) %>% 
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_MathCourses", ~ map(.x, 
                                                  ~ mutate(.x, courseCategory = courseType %>% 
                                                             map(., ~ map(predicatesList_MathCourseTypes_COUR9, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                   unlist() %>% .[which(.)] %>% names() %>%
                                                                   head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_MathCourses", ~map(.x, ~ filter(.x, courseCategory == "Geometry")), .names="filtered_for_Geometry")) %>%
  mutate(across("filtered_for_Geometry", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Math_Enrollments_grouped")) %>% 
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


##COUR_10
crdcID = "COUR_10"
COUR_10 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% COUR_10_ScienceCourse_Category )), .names="filtered_for_ScienceCourses")) %>% 
  #mutate(across("filtered_for_ScienceCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_ScienceCourses", ~ map(.x, 
                                                   ~ mutate(.x, courseCategory = courseType %>% 
                                                              map(., ~ map(predicatesList_ScienceCourseTypes_COUR10, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                    unlist() %>% .[which(.)] %>% names() %>%
                                                                    head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_ScienceCourses", ~map(.x, groupingByListOfcolumns, lc=list(c("courseCategory")), distinctCnt = TRUE, cntColumnName="sectionID"), .names="Science_Sections_grouped")) %>% 
  mutate(across(contains("_grouped"), ~ map(.x, ~ mutate(.x, feat_SwitchValues(.x, colsToUpdate="dataElement1", columnValues=dataElement1_Sciences_COUR_10)  )))) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_11a
crdcID = "COUR_11a"
COUR_11a <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% COUR_10_ScienceCourse_Category )), .names="filtered_for_ScienceCourses")) %>% 
  #mutate(across("filtered_for_ScienceCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_ScienceCourses", ~ map(.x, 
                                                     ~ mutate(.x, courseCategory = courseType %>% 
                                                                map(., ~ map(predicatesList_ScienceCourseTypes_COUR10, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                      unlist() %>% .[which(.)] %>% names() %>%
                                                                      head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_ScienceCourses", ~map(.x, ~ filter(.x, courseCategory == "Biology")), .names="filtered_for_Biology")) %>%
  mutate(across("filtered_for_Biology", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Science_Sections_grouped")) %>% 
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


##COUR_11b
crdcID = "COUR_11b"
COUR_11b <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% COUR_10_ScienceCourse_Category )), .names="filtered_for_ScienceCourses")) %>% 
  #mutate(across("filtered_for_ScienceCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_ScienceCourses", ~ map(.x, 
                                                     ~ mutate(.x, courseCategory = courseType %>% 
                                                                map(., ~ map(predicatesList_ScienceCourseTypes_COUR10, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                      unlist() %>% .[which(.)] %>% names() %>%
                                                                      head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_ScienceCourses", ~map(.x, ~ filter(.x, courseCategory == "Chemistry")), .names="filtered_for_Chemistry")) %>%
  mutate(across("filtered_for_Chemistry", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Science_Sections_grouped")) %>% 
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



##COUR_11c
crdcID = "COUR_11c"
COUR_11c <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% COUR_10_ScienceCourse_Category )), .names="filtered_for_ScienceCourses")) %>% 
  #mutate(across("filtered_for_ScienceCourses", ~ map(.x, ~ filter(.x, Math_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_ScienceCourses", ~ map(.x, 
                                                     ~ mutate(.x, courseCategory = courseType %>% 
                                                                map(., ~ map(predicatesList_ScienceCourseTypes_COUR10, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                      unlist() %>% .[which(.)] %>% names() %>%
                                                                      head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_MathCourses", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_ScienceCourses", ~map(.x, ~ filter(.x, courseCategory == "Physics")), .names="filtered_for_Physics")) %>%
  mutate(across("filtered_for_Physics", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="Science_Sections_grouped")) %>% 
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


##COUR_12
crdcID = "COUR_12"
COUR_12 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(dfs_adhc_wd = map(schoolID, ~ build_EmptyResultSet(crdcID, "No"))) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)


##COUR_13
crdcID = "COUR_13"
COUR_13 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(dfs_adhc_wd = map(schoolID, ~ build_EmptyResultSet(crdcID, "No"))) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_14
crdcID = "COUR_14"
COUR_14 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType %in% COUR_10_ScienceCourse_Category )), .names="filtered_for_ScienceCourses")) %>% 
  mutate(across("filtered_for_ScienceCourses", ~ map(.x, ~ filter(.x, Science_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_endorsements", ~ map(.x, 
                                                     ~ mutate(.x, courseCategory = courseType %>% 
                                                                map(., ~ map(predicatesList_ScienceCourseTypes_COUR10, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                      unlist() %>% .[which(.)] %>% names() %>%
                                                                      head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_endorsements", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_endorsements", ~map(.x, groupingByListOfcolumns, lc=list(c("courseCategory")), distinctCnt = TRUE, cntColumnName="sectionID"), .names="Science_Sections_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_15
crdcID = "COUR_15"
COUR_15 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType == "Computer Science" )), .names="filtered_for_CSCourses")) %>% 
  mutate(across("filtered_for_CSCourses", ~ map(.x, 
                                                   ~ mutate(.x, courseCategory = courseType %>% 
                                                              map(., ~ map(predicatesList_CSCourseTypes_COUR15, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                    unlist() %>% .[which(.)] %>% names() %>%
                                                                    head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_endorsements", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_CSCourses", ~map(.x, groupingByListOfcolumns, lc=list(c("schoolID")), distinctCnt = TRUE, cntColumnName="sectionID"), .names="cs_Sections_grouped")) %>% 
  mutate(across("cs_Sections_grouped", ~ map(.x, ~ mutate(.x, dataElement1 = "Number of Computer Science Classes" )))) %>%
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_16
crdcID = "COUR_16"
COUR_16 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType == "Computer Science" )), .names="filtered_for_CSCourses")) %>% 
  mutate(across("filtered_for_CSCourses", ~ map(.x, ~ filter(.x, CompSci_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_endorsements", ~ map(.x, 
                                                ~ mutate(.x, courseCategory = courseType %>% 
                                                           map(., ~ map(predicatesList_CSCourseTypes_COUR15, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                 unlist() %>% .[which(.)] %>% names() %>%
                                                                 head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  #mutate(across("filtered_for_endorsements", ~ map(.x, ~ filter(.x, !is.na(transcriptGrade) & !is.na(creditsEarned))), .names="passed_Algebra1")) %>%
  mutate(across("filtered_for_endorsements", ~map(.x, groupingByListOfcolumns, lc=list(c("courseCategory")), distinctCnt = TRUE, cntColumnName="sectionID"), .names="cs_Sections_grouped")) %>% 
  mutate(
    across(contains("_grouped"), 
           ~map(.x, 
                mapToElementName, 
                yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                byMatchColumns=c("dataElement1" = "dataElement1"), 
                selectColumns=c("elementName", "grpCount", "dataElement1Seq" )), 
           .names="{.col}_mapped"
    ) 
  ) %>% mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", HSEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



##COUR_17
crdcID = "COUR_17"
COUR_17 <- dfCombined_CountDay_Union %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade  %in% c("Grade 9", "Grade 10", "Grade 11", "Grade 12") | ( crdcGrade == "Ungraded" & AgeGroup == "(13,19]"))), .names="filtered_for_HS_Grades")) %>%
  mutate(across("filtered_for_HS_Grades", ~ map(.x, ~ filter(.x, courseType == "Computer Science" )), .names="filtered_for_CSCourses")) %>% 
  #mutate(across("filtered_for_CSCourses", ~ map(.x, ~ filter(.x, CompSci_Endorsed == "1")), .names="filtered_for_endorsements")) %>% 
  mutate(across("filtered_for_CSCourses", ~ map(.x, 
                                                   ~ mutate(.x, courseCategory = courseType %>% 
                                                              map(., ~ map(predicatesList_CSCourseTypes_COUR15, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                                    unlist() %>% .[which(.)] %>% names() %>%
                                                                    head(1) %||% NA )) %>% mutate(courseCategory = map_chr(courseCategory, ~ ifelse(length(courseCategory) == 0, "_", .)))))) %>%
  mutate(across("filtered_for_CSCourses", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="cs_Sections_grouped")) %>% 
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




COUR_1a_Bound <- cbind(ncesSchoolID = COUR_1a$ncesSchoolID, schoolID = COUR_1a$schoolID, COUR_1a$dfs_adhc_wd %>% reduce(rbind))
COUR_1b_Bound <- cbind(schoolID = COUR_1b$schoolID, COUR_1b$dfs_adhc_wd %>% reduce(rbind))
COUR_2a_Bound <- cbind(schoolID = COUR_2a$schoolID, COUR_2a$dfs_adhc_wd %>% reduce(rbind))
COUR_2b_Bound <- cbind(schoolID = COUR_2b$schoolID, COUR_2b$dfs_adhc_wd %>% reduce(rbind))
COUR_3a_Bound <- cbind(schoolID = COUR_3a$schoolID, COUR_3a$dfs_adhc_wd %>% reduce(rbind))
COUR_3b_Bound <- cbind(schoolID = COUR_3b$schoolID, COUR_3b$dfs_adhc_wd %>% reduce(rbind))
COUR_4a_Bound <- cbind(schoolID = COUR_4a$schoolID, COUR_4a$dfs_adhc_wd %>% reduce(rbind))
COUR_4b_Bound <- cbind(schoolID = COUR_4b$schoolID, COUR_4b$dfs_adhc_wd %>% reduce(rbind))
COUR_5a_Bound <- cbind(schoolID = COUR_5a$schoolID, COUR_5a$dfs_adhc_wd %>% reduce(rbind))
COUR_5b_Bound <- cbind(schoolID = COUR_5b$schoolID, COUR_5b$dfs_adhc_wd %>% reduce(rbind))
COUR_6a_Bound <- cbind(schoolID = COUR_6a$schoolID, COUR_6a$dfs_adhc_wd %>% reduce(rbind))
COUR_6b_Bound <- cbind(schoolID = COUR_6b$schoolID, COUR_6b$dfs_adhc_wd %>% reduce(rbind))
COUR_7a_Bound <- cbind(schoolID = COUR_7a$schoolID, COUR_7a$dfs_adhc_wd %>% reduce(rbind))
COUR_7b_Bound <- cbind(schoolID = COUR_7b$schoolID, COUR_7b$dfs_adhc_wd %>% reduce(rbind))
COUR_8_Bound <- cbind(schoolID = COUR_8$schoolID, COUR_8$dfs_adhc_wd %>% reduce(rbind))
COUR_9_Bound <- cbind(schoolID = COUR_9$schoolID, COUR_9$dfs_adhc_wd %>% reduce(rbind))
COUR_9a_Bound <- cbind(schoolID = COUR_9a$schoolID, COUR_9a$dfs_adhc_wd %>% reduce(rbind))
COUR_9b_Bound <- cbind(schoolID = COUR_9b$schoolID, COUR_9b$dfs_adhc_wd %>% reduce(rbind))
COUR_9c_Bound <- cbind(schoolID = COUR_9c$schoolID, COUR_9c$dfs_adhc_wd %>% reduce(rbind))
COUR_9d_Bound <- cbind(schoolID = COUR_9d$schoolID, COUR_9d$dfs_adhc_wd %>% reduce(rbind))
COUR_10_Bound <- cbind(schoolID = COUR_10$schoolID, COUR_10$dfs_adhc_wd %>% reduce(rbind))
COUR_11a_Bound <- cbind(schoolID = COUR_11a$schoolID, COUR_11a$dfs_adhc_wd %>% reduce(rbind))
COUR_11b_Bound <- cbind(schoolID = COUR_11b$schoolID, COUR_11b$dfs_adhc_wd %>% reduce(rbind))
COUR_11c_Bound <- cbind(schoolID = COUR_11c$schoolID, COUR_11c$dfs_adhc_wd %>% reduce(rbind))
COUR_12_Bound <- cbind(schoolID = COUR_12$schoolID, COUR_12$dfs_adhc_wd %>% reduce(rbind))
COUR_13_Bound <- cbind(schoolID = COUR_13$schoolID, COUR_13$dfs_adhc_wd %>% reduce(rbind))
COUR_14_Bound <- cbind(schoolID = COUR_14$schoolID, COUR_14$dfs_adhc_wd %>% reduce(rbind))
COUR_15_Bound <- cbind(schoolID = COUR_15$schoolID, COUR_15$dfs_adhc_wd %>% reduce(rbind))
COUR_16_Bound <- cbind(schoolID = COUR_16$schoolID, COUR_16$dfs_adhc_wd %>% reduce(rbind))
COUR_17_Bound <- cbind(schoolID = COUR_17$schoolID, COUR_17$dfs_adhc_wd %>% reduce(rbind))


COUR_Bound <- bind_cols(
  COUR_1a_Bound,
  select(COUR_1b_Bound, -schoolID),
  select(COUR_2a_Bound, -schoolID),
  select(COUR_2b_Bound, -schoolID),
  select(COUR_3a_Bound, -schoolID),
  select(COUR_3b_Bound, -schoolID),
  select(COUR_4a_Bound, -schoolID),
  select(COUR_4b_Bound, -schoolID),
  select(COUR_5a_Bound, -schoolID),
  select(COUR_5b_Bound, -schoolID),
  select(COUR_6a_Bound, -schoolID),
  select(COUR_6b_Bound, -schoolID),
  select(COUR_7a_Bound, -schoolID),
  select(COUR_7b_Bound, -schoolID),
  select(COUR_8_Bound, -schoolID),
  select(COUR_9_Bound, -schoolID),
  select(COUR_9a_Bound, -schoolID),
  select(COUR_9b_Bound, -schoolID),
  select(COUR_9c_Bound, -schoolID),
  select(COUR_9d_Bound, -schoolID),
  select(COUR_10_Bound, -schoolID),
  select(COUR_11a_Bound, -schoolID),
  select(COUR_11b_Bound, -schoolID),
  select(COUR_11c_Bound, -schoolID),
  select(COUR_12_Bound, -schoolID),
  select(COUR_13_Bound, -schoolID),
  select(COUR_14_Bound, -schoolID),
  select(COUR_15_Bound, -schoolID),
  select(COUR_16_Bound, -schoolID),
  select(COUR_17_Bound, -schoolID)
) 

COUR_Bound %>% writexl::write_xlsx("COUR.xlsx")




