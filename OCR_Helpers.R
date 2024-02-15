library(tidyverse) 
library(odbc)
library(DBI)
library(glue)
library(tictoc)
library(lubridate)
library(data.table)
library(datapasta)

## Version
# major          4                           
# minor          1.1                         
# year           2021 


###OCR_helper_functions

## Helper functions...

qryHelper2 <- function(strQuery, srvName, dbName) {
  con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server=srvName, Database=dbName, Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
  op <- DBI::dbGetQuery(conn = con, strQuery)
  DBI::dbDisconnect(con)
  op
}

qryHelper <- function(strQuery) {
  con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
  op <- DBI::dbGetQuery(conn = con, strQuery)
  DBI::dbDisconnect(con)
  op
}

generateSchoolsList <- function(endYearID, versionID) {
  qryHelper( glue::glue("EXEC crdc_2021.get_SchoolList {yearPar}, {versionPar} ", yearPar = endYearID, versionPar = versionID))
} 
checkAllPredicates <- function(x, cmbPredicates) {
  sapply(cmbPredicates, mapply, x) %>% reduce(`&`)
}
checkAnyPredicates <- function(x, cmbPredicates) {
  sapply(cmbPredicates, mapply, x) %>% reduce(`|`)
}


isEqualToOne <- function(x) {
  r1 <- ( as.character(x)=="1" & !is.na(as.numeric(x)) )
  # if(is.na(r1))
  #   FALSE
  # else
  r1
}
isEqualToZeroOrNA <- function(x) {
  r1 <- ( as.character(x) == "0" | is.na(x))
  r1
}


isEqualToValue <- function(x, V1) {
  r1 <- ( as.character(x)==v1 & !is.na(as.numeric(x)) )
  if(is.na(r1))
    FALSE
  else
    r1
}

isEqualToValue2 <- function(value) {
  function(data) {
    r1 <- ( as.character(data)==value & !is.na(as.character(data)) )
    if(is.na(r1))
      FALSE
    else
      r1
  }
}

isStringIncludedInTheList <- function(valList) {
  function(data) {
    if(is.na(data))
      FALSE
    else
      ( as.character(data) %>% intersect(valList) %>% length() > 0 )
  }
}

isEqualtoY <- function(x) {
  isEqualToValue(x, "Y")
}


isEqualOrMoreOne <- function(x) {
  r1 <- ( as.character(x) >= "1" & is.integer(x) )
  if(is.na(r1))
    FALSE
  else
    r1
}
isNotNA <- function(x) {
  !is.na(x)
}
isNA <- function(x) {
  is.na(x)
}

feat_ChangeType <- function(x, colsToUpdate) {
  x %>% 
    mutate(across(names(colsToUpdate), as.character)) 
}
feat_SwitchValuesIfTrue_o <- function(x, colsToUpdate, predicateFuncs) {
  x %>% 
    mutate(across(names(colsToUpdate), ~ modify_if(.x, ~ map_lgl(.x, checkAllPredicates, predicateFuncs), ~ colsToUpdate[cur_column()])))
}
feat_SwitchValuesIfTrue <- function(x, colsToUpdate, predicateFuncs) {
  colNameList <- names(colsToUpdate)
  x %>% 
    mutate(across(all_of(colNameList), ~ modify_if(.x, ~ map_lgl(.x, checkAllPredicates, predicateFuncs), ~ colsToUpdate[cur_column()])))
}

feat_SwitchValues <- function(x, colsToUpdate, columnValues) {
  x %>% 
    mutate(across(colsToUpdate, ~ columnValues[.]))
}
feat_SwitchValuesIfTrueAtomic <- function(x, colsToUpdate, predicateFuncs, trueValue, falseValue) {
  x %>% 
    mutate(across(colsToUpdate, ~ if_else(map_lgl(.x, checkAllPredicates, predicateFuncs), trueValue, falseValue))) 
}

groupingByColumns <- function(c, df0, renameColumns, distinctCnt=FALSE, cntColumnName="") {
  if (distinctCnt)
    df1 <- df0 %>% group_by(across(all_of(c))) %>% distinct_at(., cntColumnName) %>% count(name="grpCount")
  else
    df1 <- df0 %>% group_by(across(all_of(c))) %>% count(name="grpCount") 
  
  if (renameColumns) {
    df1 <- rename_with(df1, ~ head(c("dataElement1", "dataElement2"), length(c)), .cols=all_of(c))
  }  
  ####rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
  df1 %>% ungroup() %>% filter(if_all(head(c("dataElement1", "dataElement2"), length(c)), ~!is.na(.)))
}
groupingByListOfcolumns <- function(x, lc, distinctCnt=FALSE, cntColumnName="") {
  map(lc, groupingByColumns, df0=x, renameColumns = TRUE, distinctCnt, cntColumnName) %>% reduce(rbind) 
}

groupingByColumnsSum <- function(c, df0, renameColumns, distinctCnt=FALSE, cntColumnName="") {
  if (distinctCnt)
    df1 <- df0 %>% group_by(across(all_of(c))) %>% distinct_at(., cntColumnName) %>% 
      summarise(grpCount = sum(!!sym(cntColumnName)))
  #count(name="grpCount")
  else
    df1 <- df0 %>% group_by(across(all_of(c))) %>% 
      summarise(grpCount = sum(!!sym(cntColumnName)))
  
  if (renameColumns) {
    df1 <- rename_with(df1, ~ head(c("dataElement1", "dataElement2"), length(c)), .cols=all_of(c))
  }  
  ####rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
  df1 %>% ungroup() %>% filter(if_all(head(c("dataElement1", "dataElement2"), length(c)), ~!is.na(.)))
}
groupingByListOfcolumnsSum <- function(x, lc, distinctCnt=FALSE, cntColumnName="") {
  map(lc, groupingByColumnsSum, df0=x, renameColumns = TRUE, distinctCnt, cntColumnName) %>% reduce(rbind) 
}


# 
# groupingByColumns <- function(c, df0, renameColumns) {
#   df1 <- df0 %>% group_by(across(all_of(c))) %>% count(name="grpCount") 
#   if (renameColumns) {
#     df1 <- rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1))
#   }
#   ####rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
#   df1 %>% ungroup() %>% filter(if_all(c("dataElement1", "dataElement2"), ~!is.na(.)))
# }
# groupingByListOfcolumns <- function(x, lc) {
#   map(lc, groupingByColumns, df0=x, renameColumns = TRUE) %>% reduce(rbind) 
# }
mapToElementName <- function(xf, yf, byMatchColumns, selectColumns) {
  right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% 
    select(all_of(selectColumns))
}
joinWithExternalData <- function(xf, yf, byMatchColumns, selectColumns, joinType, returnUniqueSet) {
  fn <- if(joinType=="R") right_join #~ right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  else if(joinType=="I") inner_join #~ inner_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  else if(joinType=="L") left_join #~ left_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  
  r1 <- fn(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% select(all_of(selectColumns))
  if(returnUniqueSet == "Y")
    distinct(r1)
  else 
    r1
}
joinWithExternalData2 <- function(xf, yf, byMatchColumns, selectColumns = parse(text="everything()"), joinType, returnUniqueSet=FALSE) {
  fn <- if(joinType=="R") right_join #~ right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  else if(joinType=="I") inner_join #~ inner_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  else if(joinType=="L") left_join #~ left_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE)
  
  r1 <- fn(xf, yf, by=byMatchColumns, na_matches="never", keep=TRUE) %>% select(all_of(eval(selectColumns)))
  if(returnUniqueSet == "Y")
    distinct(r1)
  else 
    r1
}

widerbyColumns <- function(x, nameCols, valueCols) {
  x %>% pivot_wider(names_from=all_of(nameCols), values_from=all_of(valueCols))
}
imputeAndSumAcrossColumns <- function(x, columnsToAdd, aggrColName) {
  x %>% replace(is.na(.), 0) %>% mutate(!!quo_name(aggrColName) := rowSums(across(columnsToAdd)))
}


# 1. list of atomic values... 
# 1. list of logical functions... 
# 2. use exec to return the list of results. One-to-One pairing between arg and function lists. 
# 2. The exec will output a logical vector. This could be reduced to a single boolean. 
# function1 -- an refactored anonymous function to run exec() for a single pair of arg and function...
testSingle_Predicate_Value_Pair <- function(fn, arg) { exec(fn, arg) }

# vectorized testOneTest... 
# This is GOOD, except Step2 needs to be incorporated... 
testSinglePlus <- function(fns, args) { map2(fns, args, ~ testSingle_Predicate_Value_Pair(.x, .y)) %>% reduce(`&`) } 

##GOOD!! This works perfect with ability to trim argument vectors to the size of tests vector at each iteration... 
testSingle_Predicate_Value_Pair_Vectorized <- function(fnsSet, argsSet, TrimToPredicatesSize=TRUE) 
{
  if(TrimToPredicatesSize)
    trimmed_argsSet <- head(argsSet, length(fnsSet))
  else
    trimmed_argsSet <- argsSet
  map2_lgl(fnsSet, trimmed_argsSet, testSingle_Predicate_Value_Pair) %>% reduce(`&`)
}
# testing helper functions above...
#map(a22$c1, ~ map(predicatesList_OffensesTypes, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% unlist())

packageColumns <- function(x, columnsList, newColName) {
  x %>% mutate(!!newColName := list(c_across(all_of(columnsList))))
}


## helper function to insert zeros for schools with enrollments...
hf_fillInWithAValue <- function(df, v1) { 
  df %>% mutate(across(everything(), ~ replace_na(.x, v1))) 
}
hf_WriteOverWithAValue <- function(df, v1) {
  df %>% mutate(across(everything(), ~ v1))
}
fillInZeroForIndexdSchools <- function(df, colName_schoolID, colName_df, schoolIDs, f, newValue, ids_Except) {
  if (!ids_Except)
    schoolIDsIndex <- df[, colName_schoolID] %>% unlist() %in% schoolIDs
  else
    schoolIDsIndex <- !(df[, colName_schoolID] %>% unlist() %in% schoolIDs)
  df[schoolIDsIndex, ] <- df[schoolIDsIndex, ] %>%
    mutate(across(all_of(colName_df), ~ map(.x, ~ f(.x, newValue))))
  df
}



### To build blank empty result list... 
build_EmptyResultSet <- function(moduleID, empStr) {
  namedColumns <- subQs %>% filter(mod_crdcIdentifier == moduleID) %>% arrange(dataElement1Seq, dataElement2Seq) %>% distinct(elementName) %>% flatten_chr() %>% unlist()
  as_tibble_row(rep(as.character(empStr), length(namedColumns)), .name_repair = ~ namedColumns)
}

#### Parameters  ####

str_ServerName = "ORIONTEST.CIS.CCSD.NET"
str_DBName = "ACCOUNTABILITY"


dt_EOY <- lubridate::as_date("2021-05-26")
dt_BlockSchedule_Term1_EndDate = lubridate::as_date("2020-12-18")
dt_CountDay <- lubridate::as_date("2020-10-01")
dt_CountDay_2ndSemester <- lubridate::as_date("2021-03-01")
str_SchoolYear <- "2021"

dt_FollowingYearStartDate <- lubridate::as_date("2021-08-09")
dt_FollowingYearEOYDate <- lubridate::as_date("2022-05-25") 
str_FollowingSchoolYear <- "2022"

## Execute... 
## AND q.crdcQuestionID = 159 
# subQs <- qryHelper("SELECT * FROM OPENQUERY(campus, 'SELECT DISTINCT q.crdcQuestionID, REPLACE(q.crdcIdentifier, ''-'', ''_'') AS mod_crdcIdentifier, q.crdcIdentifier, sq.elementName, sq.dataElement1, sq.dataElement2, sq.dataElement1Seq, sq.dataElement2Seq FROM clark.dbo.CRDCQuestion q INNER JOIN clark.dbo.CRDCSubQuestion sq ON q.crdcQuestionID = sq.crdcQuestionID 
#                     WHERE q.endYear = ''2021'' ')")

subQs <- qryHelper2("SELECT * FROM OPENQUERY(campus, 'SELECT DISTINCT q.crdcQuestionID, REPLACE(q.crdcIdentifier, ''-'', ''_'') AS mod_crdcIdentifier, q.crdcIdentifier, sq.elementName, sq.dataElement1, sq.dataElement2, sq.dataElement1Seq, sq.dataElement2Seq FROM clark.dbo.CRDCQuestion q INNER JOIN clark.dbo.CRDCSubQuestion sq ON q.crdcQuestionID = sq.crdcQuestionID 
                    WHERE q.endYear = ''2021'' ')", "ORION.CIS.CCSD.NET", "SDM")
#schoolTypes <- qryHelper2("EXEC crdc_2021.schools_Characteristics '2020-10-01'	", "ORION.CIS.CCSD.NET", "SDM")


groupByColumns1 <- c("gender", "raceEthnicity")
groupByColumns2 <- c("gender", "inIDEA") #
groupByColumns3 <- c("gender", "inEL")
groupByColumns4 <- c("gender", "in504")
groupByColumns5 <- c("gender", "inELProg")
groupByColumns6 <- c("crdcGrade")
countValueColumns <- c("grpCount")
widenByColumns <- c("elementName")

columnGroupList1 <- list(
  groupByColumns1, 
  groupByColumns2, 
  groupByColumns3, 
  groupByColumns4
)

predicateFuncs1 <- list(isNotNA, isEqualToOne)
predicateFuncs2 <- list(~ TRUE, ~ TRUE)
predicateFuncs3 <- list(isEqualOrMoreOne)



indicatorColumns1 <- set_names(
  c("English Learners (EL)", "Students with Disabilities (IDEA)", "Students with Disabilities (Section 504 Only)"),
  nm = c("inEL", "inIDEA", "in504")
)
gradesList <- set_names(
  c("Preschool", "Kindergarten","Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Ungraded"), 
  nm = c("PK", "0K","KG", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "UG")
)
genderList <- set_names(
  c("Male", "Female"), 
  nm = c("M", "F")
)
ethnicitiesList <- set_names(
  c("Hispanic or Latino of any race", "American Indian or Alaska Native", "Asian", "Native Hawaiian or Other Pacific Islander", "Black or African American", "White", "Two or more races" ), 
  nm = c("H", "I", "A", "P", "B", "C", "M")
)
ageGroupList <- set_names(
  c("School has mainly elementary school age students?",
    "School has mainly high school age students?",
    "School has mainly middle school age students?"),
  nm = c("(2,10]", "(10,13]", "(13,19]")
)

HS_GradesList <- c("Grade 9", "Grade 10", "Grade 11", "Grade 12")
MS_GradesList <- c("Grade 6", "Grade 7", "Grade 8")
ES_GradesList <- c("Preschool", "Kindergarten","Kindergarten", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5")
All_GradesList <- c(ES_GradesList, MS_GradesList, HS_GradesList, "UG") %>% unique()

ENRL_AddColumns1 <- c( "SCH_ENR_AM_F", "SCH_ENR_AS_F", "SCH_ENR_BL_F", "SCH_ENR_HI_F", "SCH_ENR_HP_F", "SCH_ENR_TR_F", "SCH_ENR_WH_F", "SCH_ENR_AM_M", "SCH_ENR_AS_M", "SCH_ENR_BL_M", "SCH_ENR_HI_M", "SCH_ENR_HP_M", "SCH_ENR_TR_M", "SCH_ENR_WH_M" )

OFFN_1_OffensesCategoryNames <- c(
  "Sexual assault (other than rape)", 
  "Rape or attempted rape", 
  "Robbery without a weapon", 
  "Robbery with a weapon", 
  "Physical attack or fight without a weapon",
  "Physical attack or fight with a weapon", 
  "Threats of physical attack without a weapon",
  "Threats of physical attack with a weapon", 
  "Possession of a firearm or explosive device"
)

# isNotNA to specify weaponType is switched to comparison of the column value to a list of distinct weaponType values... 
lstWeaponType = c("H: Handgun", "R: Rifle/Shot guns") 
predicatesList_OffensesTypes <- list(    ## Each test group will be reduced to a single boolean... The function is to return the index of the first TRUE. 
  list(isStringIncludedInTheList(c("DIN", "SHR"))),    #Inappropriate touching and SH. ?SA other than rape...
  list(isStringIncludedInTheList(c("DSA"))),           #SA. ? Equivalent of rape?
  list(isStringIncludedInTheList(c("THR")), purrr::negate(isStringIncludedInTheList(lstWeaponType))),     #Theft/Robbery without weapon
  list(isStringIncludedInTheList(c("THR")), isStringIncludedInTheList(lstWeaponType)),  #Theft/Robbery with weapon
  list(isStringIncludedInTheList(c("DFH", "DHG", "DBC", "DBD", "DAJ")), purrr::negate(isStringIncludedInTheList(lstWeaponType))),     # Physical Attack wo a weapon.
  list(isStringIncludedInTheList(c("DFH", "DHG", "DBC", "DBD", "DAJ")), isStringIncludedInTheList(lstWeaponType)),  # Physical Attack with a weapon.
  list(isStringIncludedInTheList(c("DTS", "TTT"))),    # Threats wo weapon
  list(isStringIncludedInTheList(c("DWT"))),          # Threats with weapon... Question!! Is Threat w/weapong more egregious than simply possessing a weapon?  
  list(isStringIncludedInTheList(c("DWH2", "DWI")), isStringIncludedInTheList(lstWeaponType))   # Possession of Weapon...
)
names(predicatesList_OffensesTypes) <- OFFN_1_OffensesCategoryNames
offenseColumns_To_Be_Packed <- c("code", "weaponType")


#COURs
#COUR endorsements list column to indicators... 
## RegEx patterns to determine endorsement subject type...
## RegEx patterns cleaned up...
Math_TestExpStr <- "(\\b((?i)Math|Mathematics(?-i)).*\\b(\\w)?)"
Science_TestExpStr <- "(^((?!.*(?i)computer(?-i))(?!.*(?i)Political(?-i)))(?!.*(?i)Health(?-i))(?!.*(?i)Library(?-i))(?!.*(?i)Forensic(?-i))(.*(?i)science(?-i)))|([\\*.]?\\b((?i)Biology(?-i)|(?i)Chemistry(?-i)|(?i)Botany(?-i)|(?i)Geology(?-i)|(?i)Physics(?-i)|(?i)Physiology(?-i)|(?i)zoology(?-i))\\b(\\w)?)"
ComputerScience_TestExpStr <-  "(?i)(computer.*\\s*(programming|application|Electronics|Literacy|application|system|science|Software))(?-i)|((?i)(digital\\s*game\\s*development)|(Information\\s*(technology|technologies))|(CTE\\s*Technology\\s*Education)(?-i))"

Math_TestExp <- regex(Math_TestExpStr, comments=TRUE, ignore_case=TRUE)
Science_TestExp <- regex(Science_TestExpStr, comments=TRUE, ignore_case=TRUE)
ComputerScience_TestExp <- regex(ComputerScience_TestExpStr, comments=TRUE, ignore_case=TRUE)

Endorsements_CategoryName <- c(
  "Math_Endorsed", 
  "Science_Endorsed", 
  "CompSci_Endorsed"
)
EndorsementsCertification_Tests <- list(
  Math_TestExpStr, 
  Science_TestExpStr, 
  ComputerScience_TestExpStr
)

CourseType <- 'Algebra I';
GradingTaskType <- 'Semester Grade';
passingGrade <- c("A", "B", "C", "D")
mathCourseGroup <- c("Algebra I", "Algebra II", "Geometry", "Precalculus", "Calculus", "Trigonometry", "Statistics", "Math Analysis" )

mathCourses <- c("Algebra I", "Algebra II", "Geometry", "Precalculus", "Calculus", "Trigonometry", "Statistics", "Math Analysis" )


COUR_5_MathCourse_Category <- c(
  "Number of Advanced Mathematics Classes",
  "Number of Algebra I Classes",
  "Number of Algebra II Classes",
  "Number of Calculus Classes",
  "Number of Geometry Classes"
)
predicatesList_MathCourseTypes <- list(    ## Each test group will be reduced to a single boolean... The function is to return the index of the first TRUE. 
  list(isStringIncludedInTheList(c("Math Analysis", "Statistics", "Precalculus", "Trigonometry"))),    
  list(isStringIncludedInTheList(c("Algebra I"))), 
  list(isStringIncludedInTheList(c("Algebra II"))), 
  list(isStringIncludedInTheList(c("Calculus"))), 
  list(isStringIncludedInTheList(c("Geometry")))
)
names(predicatesList_MathCourseTypes) <- COUR_5_MathCourse_Category

COUR_9_MathCourse_Category <- c(
  "Advanced Mathematics",
  "Algebra I",
  "Algebra II",
  "Calculus",
  "Geometry"
)
predicatesList_MathCourseTypes_COUR9 <- list(    ## Each test group will be reduced to a single boolean... The function is to return the index of the first TRUE. 
  list(isStringIncludedInTheList(c("Math Analysis", "Statistics", "Precalculus", "Trigonometry"))),    
  list(isStringIncludedInTheList(c("Algebra I"))), 
  list(isStringIncludedInTheList(c("Algebra II"))), 
  list(isStringIncludedInTheList(c("Calculus"))), 
  list(isStringIncludedInTheList(c("Geometry")))
)
names(predicatesList_MathCourseTypes_COUR9) <- COUR_9_MathCourse_Category

COUR_10_ScienceCourse_Category <- c(
  "Biology",
  "Chemistry",
  "Physics"
)
predicatesList_ScienceCourseTypes_COUR10 <- list(    ## Each test group will be reduced to a single boolean... The function is to return the index of the first TRUE. 
  list(isStringIncludedInTheList(c("Biology"))),    
  list(isStringIncludedInTheList(c("Chemistry"))), 
  list(isStringIncludedInTheList(c("Physics")))
)
names(predicatesList_ScienceCourseTypes_COUR10) <- COUR_10_ScienceCourse_Category

dataElement1_Sciences_COUR_10 <- set_names(
  subQs %>% filter(mod_crdcIdentifier == "COUR_10") %>% select(dataElement1) %>% arrange(dataElement1) %>% unlist(),
  nm = c("Biology", "Chemistry", "Physics")
)


COUR_15_CSCourse_Category <- c(
  "Computer Science"
)
predicatesList_CSCourseTypes_COUR15 <- list(    ## Each test group will be reduced to a single boolean... The function is to return the index of the first TRUE. 
  list(isStringIncludedInTheList(c("Computer Science")))
)
names(predicatesList_CSCourseTypes_COUR15) <- COUR_15_CSCourse_Category

APIB_Course_Categories <- c(
  "Does this school have any students enrolled in Advanced Placement (AP) mathematics courses?", 
  "Does this school have any students enrolled in Advanced Placement (AP) science courses?",
  "Does this school have any students enrolled in Advanced Placement (AP) computer science courses?"
)
APIB_Math_Category <- c(
  "Statistics", "Calculus"
)
APIB_Science_Category <- c(
  "Biology", "Chemistry", "Physics", "Environmental Science"
)
APIB_CompusterScience_Category <- c(
  "Computer Science"
)
predicatesList_CourseTypes_APIB <- list(    ## Each test group will be reduced to a single boolean... The function is to return the index of the first TRUE. 
  list(isStringIncludedInTheList(APIB_Math_Category)), 
  list(isStringIncludedInTheList(APIB_Science_Category)),
  list(isStringIncludedInTheList(APIB_CompusterScience_Category))
)
names(predicatesList_CourseTypes_APIB) <- APIB_Course_Categories



# df with list column of string vectors will be tested for indicator values with list of regular expressions. 
# parameters: 
#     df: dataframe
#     colname: list column
#     indicatorNames : name of indicator columns to be generated... Count must match with count of regular expressions packed in the list.
#     testsList: list of regular expressions. 
strListToIndicatorColumns <- function(df, colName, indicatorNames, testsList) {
  vecCol <- select(df, colName) %>% flatten()  # flatten to a simple list. 
  testsList_Named <- setNames(testsList, indicatorNames)
  for(i in indicatorNames) { 
    df[[i]] <- vecCol %>% map_int(., ~ str_detect(., testsList_Named[[i]]) %>% any()) 
  }
  df
}


