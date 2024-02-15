sDir <- getwd() 
sFiles <- dir(sDir, pattern = "OCR_Helpers.R") 
for ( f in sFiles ) {
  source(file = file.path(sDir, f))
}

# Data...
## Student Profiles... At EOY...
tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{count_day}', @EOYDay='{eoy_day}'", 
                     year_id=str_SchoolYear, mapYear_id=str_SchoolYear, count_day=dt_EOY, eoy_day=dt_EOY)
  dfStudentProfilesOnEOY <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfStudentProfilesOnEOY, file = "savedData/studentProfiles_EOYDay.RData")
  #load(file = "savedData/studentProfiles_EOYDay.RData")
  dfStudentProfilesOnEOY <- dfStudentProfilesOnEOY %>%
    feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("crdcGrade")), columnValues=gradesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("raceEthnicity")), columnValues=ethnicitiesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("gender")), columnValues=genderList ) %>%
    #mutate(birthDate = lubridate::mdy_hm(birthDate) ) %>% 
    mutate(AgeOnEOY = as.integer(AgeOnEOY), AgeOnCountDay = as.integer(AgeOnCountDay)) %>%
    mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>% 
    mutate(pid = personID)
      ### creating a key column...
toc()

tic()
  strQ <- glue::glue("EXEC [crdc_2021].generate_StudentProfiles_SetDay @endYear='{year_id}', @mapYear='{mapYear_id}', @SetDay='{count_day}', @EOYDay='{eoy_day}'", 
                     year_id=str_FollowingSchoolYear, mapYear_id=str_SchoolYear, count_day=dt_FollowingYearStartDate + 14, eoy_day=dt_FollowingYearEOYDate)
  dfStudentProfiles_OnStartDay_2022 <- qryHelper2(strQ, str_ServerName, str_DBName) 
  save(dfStudentProfiles_OnStartDay_2022, file = "savedData/studentProfiles_StartDay_2022.RData")
  #load(file = "savedData/studentProfiles_StartDay_2022.RData")
  dfStudentProfiles_OnStartDay_2022 <- dfStudentProfiles_OnStartDay_2022 %>%
    feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("crdcGrade")), columnValues=gradesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("raceEthnicity")), columnValues=ethnicitiesList ) %>%
    feat_SwitchValues( colsToUpdate=all_of(c("gender")), columnValues=genderList ) %>%
    #mutate(birthDate = lubridate::mdy_hm(birthDate) ) %>% 
    mutate(AgeOnEOY = as.integer(AgeOnEOY), AgeOnCountDay = as.integer(AgeOnCountDay)) %>%
    mutate(AgeGroup = cut(AgeOnEOY, c(2, 10, 13, 19))) %>% 
    mutate(pid = personID, crdcGrade_FollowingYear = crdcGrade)
      ### creating key column...
toc()  

## Data on Grades and credits earned... Includes summer credits earned in the school year summer...
tic()
  CurriculumStandardType <- ""
  strQ <- glue::glue("EXEC [crdc_2021].generate_TranscriptGrades_UpToDate @endYear='{year_id}', @SetDay='{count_day}', @Include_SchYearSummer='{IncludeSummerSchool}'",
                   year_id=str_SchoolYear, count_day=dt_EOY,  IncludeSummerSchool = "Y")
  dfStudent_GradesEarned_IncludingSummer <- 
    qryHelper2(strQ, str_ServerName, str_DBName) %>%
    mutate(pid = personID)
  save(dfStudent_GradesEarned_IncludingSummer, file="savedData/student_GradesEarned_IncludeSummer.RData")
  #load(file="savedData/student_GradesEarned_IncludeSummer.RData")
toc()


dfStudent_GradesEarned_IncludingSummer_filtered <- dfStudent_GradesEarned_IncludingSummer %>% 
  feat_SwitchValues( colsToUpdate=all_of(c("grade")), columnValues=gradesList ) %>%
  feat_SwitchValues( colsToUpdate=all_of(c("transcriptGradeLevel")), columnValues=gradesList ) %>%
  filter(
    ! (
        (
          ( csNumber == "82200001" & csName == 	"8th Grade Science/IAFNR:A" ) |
          ( csNumber == "82200002" & csName == 	"8th Grade Science/IAFNR:B" ) |
          ( csNumber == "27100002S" & csName == 	"Pre-Algebra 8" ) |
          ( csNumber == "27105001S" & csName == 	"Pre-Algebra 8" ) |
          ( csNumber == "27109002" & csName == 	"Pre-Algebra 8" ) |
          ( csNumber == "27100001" & csName == 	"Pre-Algebra 8" ) |
          ( csNumber == "27109001" & csName == 	"Pre-Algebra 8" ) |
          ( csNumber == "57560001" & csName == 	"PE 8" ) |
          ( csNumber == "37700001" & csName == 	"Science 8" ) |
          ( csNumber == "8695002" & csName == 	"Exploration 7" ) |
          ( csNumber == "47410001" & csName == 	"History & Geography 7" ) |
          ( csNumber == "57350002" & csName == 	"PE 7" ) |
          ( csNumber == "57350001" & csName == 	"PE 7" )
        ) & parentStdID == "2" & transcriptGradeLevel %in% c( "Grade 7", "Grade 8" ) 
    ) & ( transcriptGradeLevel %in% HS_GradesList | parentStdID == "2" )
  ) %>% 
  mutate(pid = personID)

df_GradesEarned_Aggr <- dfStudent_GradesEarned_IncludingSummer_filtered %>%
  group_by(pid) %>% 
  summarise(total_creditsEarned = sum(as.double(creditsEarned)))


dfStudentProfiles_OnStartDay_2022 %>% View()
dfStudentProfilesOnEOY %>% View()

dfStudentProfilesOnEOY$pid
dfStudentProfiles_OnStartDay_2022$pid


aa <- 
  left_join(dfStudentProfilesOnEOY, distinct(dfStudentProfiles_OnStartDay_2022, pid, crdcGrade_FollowingYear), by="pid", keep=TRUE, na_matches="never") %>%
  left_join(df_GradesEarned_Aggr, by=c("pid.x"="pid"), keep=FALSE, na_matches="never" ) %>%
  mutate(retained = 
           if_else(
             ( crdcGrade %in% c(ES_GradesList, MS_GradesList) & crdcGrade == crdcGrade_FollowingYear ) | 
               ( crdcGrade == "Grade 9" & total_creditsEarned < 5.00 ) | 
               ( crdcGrade == "Grade 10" & total_creditsEarned < 11.00 ) | 
               ( crdcGrade == "Grade 11" & total_creditsEarned < 17.00 ) | 
               ( crdcGrade == "Grade 12" & total_creditsEarned < 22.50 ), 
             "Y", "N"))


tic()
  dfStudent_Retained_Combined <- 
    left_join(dfStudentProfilesOnEOY, distinct(dfStudentProfiles_OnStartDay_2022, pid, crdcGrade_FollowingYear), by="pid", keep=TRUE, na_matches="never") %>%
    left_join(df_GradesEarned_Aggr, by=c("pid.x"="pid"), keep=FALSE, na_matches="never" ) %>%
    mutate(retained = 
             if_else(
                      ( crdcGrade %in% c(ES_GradesList, MS_GradesList) & crdcGrade == crdcGrade_FollowingYear ) | 
                      ( crdcGrade == "Grade 9" & total_creditsEarned < 5.00 ) | 
                      ( crdcGrade == "Grade 10" & total_creditsEarned < 11.00 ) | 
                      ( crdcGrade == "Grade 11" & total_creditsEarned < 17.00 ) | 
                      ( crdcGrade == "Grade 12" & total_creditsEarned < 22.50 ), 
                     "Y", "N")) %>% 
    group_by(schoolID) %>% 
    group_split(.keep=TRUE) %>% tibble(dfs = .) %>% ungroup() %>% 
    mutate(schoolID = map(.$dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
    mutate(ncesSchoolID = map(.$dfs, ~ .[1, "ncesSchoolID"]) %>% unlist()) %>% 
    mutate(schoolName = map(.$dfs, ~ .[1, "schoolName"]) %>% unlist()) 
  save(dfStudent_Retained_Combined, file="savedData/dfStudent_Retained_Combined.RData")
toc()




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

#HS based on credit sufficiency.
# this is what the state uses to report:
# Grade 9 less than 5 credits
# Grade 10 less than 11 credits
# Grade 11 less than 17 credits
# the state doesn't report it for grade 12 so whatever the high school graduation require is, I think 22
# according to the DOE its 22.5 https://doe.nv.gov/High_School_Graduation/



aa1 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% All_GradesList[!All_GradesList %in% c("Preschool", "UG")])) , .names="filtered_for_Grades"))

aa2 <- aa1$filtered_for_Grades %>% reduce(rbind) %>% filter(retained=="Y" & crdcGrade=="Kindergarten") 
aa2 %>% View()
aa2 %>% distinct(schoolName, schoolID)  %>% View()



which(aa1$schoolID =="133")  39
which(aa1$schoolID =="71")  365
which(aa1$schoolID =="463")  331 
which(aa1$schoolID =="223")  136 
which(aa1$schoolID =="100")  3
which(aa1$schoolID =="101")  4
aa1$schoolName[[136]]
aa1$filtered_for_Grades[[39]] %>% filter(crdcGrade=="Kindergarten" & retained=="Y") %>% View()
aa1$filtered_for_Grades[[365]] %>% filter(crdcGrade=="Kindergarten" & retained=="Y") %>% View()
aa1$filtered_for_Grades[[331]] %>% filter(crdcGrade=="Kindergarten" & retained=="Y") %>% View()
aa1$filtered_for_Grades[[136]] %>% filter(crdcGrade=="Kindergarten" & retained=="Y") %>% View()
aa1$filtered_for_Grades[[3]] %>% filter(crdcGrade=="Kindergarten" & retained=="Y") %>% View()
aa1$filtered_for_Grades[[4]] %>% filter(crdcGrade=="Kindergarten" & retained=="Y") %>% View()

## Validation... 
dfStudent_Retained_Combined %>% filter(gsub(",", " ", schoolName) == "Clark  Ed W HS")
dfStudent_Retained_Combined %>% distinct(schoolName) %>% arrange(schoolName) %>% View()
dfStudent_Retained_Combined %>% filter(gsub(",", " ", schoolName) == "McDoniel  Estes M ES")
dfStudent_Retained_Combined %>% filter(gsub(",", " ", schoolName) == "Coronado HS")


## RETN_1...
crdcID = "RETN_1"
RETN_1 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade %in% All_GradesList[!All_GradesList %in% c("Preschool", "UG")])) , .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=list(c("crdcGrade")), distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
  mutate(across("retentions_grouped", ~ map(.x, ~ mutate(.x, ind_retained = if_else(grpCount > 0, "Yes", "No"))))) %>%
  mutate(
      across(contains("_grouped"), 
             ~map(.x, 
                  mapToElementName, 
                  yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                  byMatchColumns=c("dataElement1" = "dataElement1"), 
                  selectColumns=c("elementName", "ind_retained", "dataElement1Seq" )), 
             .names="{.col}_mapped"
      ) 
    ) %>% mutate(
      across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq ) %>% select("elementName", "ind_retained")))
    ) %>% 
    mutate(
      across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("ind_retained")), .names="dfs_adhc_wd")
    ) %>% 
    mutate(
      across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, "No"))))
    ) %>% 
    fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)
  
    
crdcID = "RETN_2 GRK"
RETN_2_GRK <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Kindergarten")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR1"
RETN_2_GR1 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 1")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)




crdcID = "RETN_2 GR2"
RETN_2_GR2 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 2")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR3"
RETN_2_GR3 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 3")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR4"
RETN_2_GR4 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 4")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR5"
RETN_2_GR5 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 5")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR6"
RETN_2_GR6 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 6")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR7"
RETN_2_GR7 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 7")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR8"
RETN_2_GR8 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 8")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR9"
RETN_2_GR9 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 9")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR10"
RETN_2_GR10 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 10")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)




crdcID = "RETN_2 GR11"
RETN_2_GR11 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 11")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



crdcID = "RETN_2 GR12"
RETN_2_GR12 <- dfStudent_Retained_Combined %>% 
  mutate(across("dfs", ~ map(.x, ~ filter(.x, crdcGrade == "Grade 12")), .names="filtered_for_Grades")) %>% 
  mutate(across("filtered_for_Grades", ~ map(.x, ~ filter(.x, !is.na(retained) & retained == "Y" )), .names="filtered_for_retention")) %>%
  mutate(across("filtered_for_retention", ~map(.x, groupingByListOfcolumns, lc=columnGroupList1, distinctCnt = TRUE, cntColumnName="personID"), .names="retentions_grouped")) %>% 
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
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of("grpCount")), .names="dfs_adhc_wd")
  ) %>% 
  mutate(
    across("dfs_adhc_wd", ~ map(.x, ~ mutate(.x, across(everything(), replace_na, 0))))
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", AllGradeLevelsEnrolls_IDs, hf_WriteOverWithAValue, as.character(NA), TRUE)



RETN_1_Bound <- cbind(ncesSchoolID = RETN_1$ncesSchoolID, schoolID = RETN_1$schoolID, RETN_1$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GRK_Bound <- cbind(schoolID = RETN_2_GRK$schoolID, RETN_2_GRK$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR1_Bound <- cbind(schoolID = RETN_2_GR1$schoolID, RETN_2_GR1$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR2_Bound <- cbind(schoolID = RETN_2_GR2$schoolID, RETN_2_GR2$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR3_Bound <- cbind(schoolID = RETN_2_GR3$schoolID, RETN_2_GR3$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR4_Bound <- cbind(schoolID = RETN_2_GR4$schoolID, RETN_2_GR4$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR5_Bound <- cbind(schoolID = RETN_2_GR5$schoolID, RETN_2_GR5$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR6_Bound <- cbind(schoolID = RETN_2_GR6$schoolID, RETN_2_GR6$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR7_Bound <- cbind(schoolID = RETN_2_GR7$schoolID, RETN_2_GR7$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR8_Bound <- cbind(schoolID = RETN_2_GR8$schoolID, RETN_2_GR8$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR9_Bound <- cbind(schoolID = RETN_2_GR9$schoolID, RETN_2_GR9$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR10_Bound <- cbind(schoolID = RETN_2_GR10$schoolID, RETN_2_GR10$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR11_Bound <- cbind(schoolID = RETN_2_GR11$schoolID, RETN_2_GR11$dfs_adhc_wd %>% reduce(rbind))
RETN_2_GR12_Bound <- cbind(schoolID = RETN_2_GR12$schoolID, RETN_2_GR12$dfs_adhc_wd %>% reduce(rbind))



RETN_Bound <- bind_cols(
  RETN_1_Bound,
  select(RETN_2_GRK_Bound, -schoolID),
  select(RETN_2_GR1_Bound, -schoolID),
  select(RETN_2_GR2_Bound, -schoolID),
  select(RETN_2_GR3_Bound, -schoolID),
  select(RETN_2_GR4_Bound, -schoolID),
  select(RETN_2_GR5_Bound, -schoolID),
  select(RETN_2_GR6_Bound, -schoolID),
  select(RETN_2_GR7_Bound, -schoolID),
  select(RETN_2_GR8_Bound, -schoolID),
  select(RETN_2_GR9_Bound, -schoolID),
  select(RETN_2_GR10_Bound, -schoolID),
  select(RETN_2_GR11_Bound, -schoolID),
  select(RETN_2_GR12_Bound, -schoolID)
) 


RETN_Bound %>% writexl::write_xlsx("RETN.xlsx")
