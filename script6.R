library(tidyverse) 
library(odbc)
library(DBI)
library(glue)
library(tictoc)

## Helper functions...
qryHelper <- function(strQuery) {
  con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
  op <- DBI::dbGetQuery(conn = con, strQuery)
  DBI::dbDisconnect(con)
  op
}
generateSchoolsList <- function(endYearID, versionID) {
  qryHelper( glue::glue("EXEC crdc_2021.get_SchoolList {yearPar}, {versionPar} ", yearPar = endYearID, versionPar = versionID))
} 
feat_ChangeType_SwitchNonNAValues <- function(x, colsToUpdate) {
  x %>% 
    mutate(across(names(colsToUpdate), as.character)) %>% 
    mutate(across(names(colsToUpdate), ~ ifelse((!is.na(.) & .== "1"), colsToUpdate[cur_column()], .)))
  # mutate(across(names(colsToUpdate), ~ ifelse(!is.na(.), colsToUpdate[cur_column()], .))) 
}
groupingByColumns <- function(c, df0, renameColumns) {
  df1 <- df0 %>% group_by(across(all_of(c))) %>% count(name="grpCount") 
  if (renameColumns) {
    df1 <- rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1))
  }
  #rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
  df1 %>% ungroup() %>% filter(if_all(c("dataElement1", "dataElement2"), ~!is.na(.)))
}
groupingByListOfcolumns <- function(x, lc) {
  map(lc, groupingByColumns, df0=x, renameColumns = TRUE) %>% reduce(rbind) 
}
mapToElementName <- function(xf, yf, byMatchColumns, selectColumns) {
  right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% 
    select(all_of(selectColumns))
}
widerbyColumns <- function(x, nameCols, valueCols) {
  x %>% pivot_wider(names_from=nameCols, values_from=valueCols)
}
imputeAndSumAcrossColumns <- function(x, columnsToAdd, aggrColName) {
  x %>% replace(is.na(.), 0) %>% mutate(!!quo_name(aggrColName) := rowSums(across(columnsToAdd)))
}


#### Parameters  ####
groupByColumns1 <- c("gender", "raceEthnicity")
groupByColumns2 <- c("gender", "inIDEA") #
groupByColumns3 <- c("gender", "inEL")
groupByColumns4 <- c("gender", "in504")
groupByColumns5 <- c("gender", "inELProg")
groupByColumns6 <- c("stateGrade")
countValueColumns <- c("grpCount")
widenByColumns <- c("elementName")

columnGroupList1 <- list(
  groupByColumns1, 
  groupByColumns2, 
  groupByColumns3, 
  groupByColumns4
)


indicatorColumns1 <- set_names(
  c("English Learners (EL)", "Students with Disabilities (IDEA)", "Students with Disabilities (Section 504 Only)"),
  nm = c("inEL", "inIDEA", "in504")
)
gradesList <- set_names(
  c("Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10", "Grade 11", "Grade 12", "Kindergarten", "Preschool", "Ungraded"), 
  nm = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "0K", "PK", "UG")
)

#skipColumns1 <- c( "SCH_ENR_LEP_F", "SCH_ENR_LEP_M", "SCH_ENR_504_F", "SCH_ENR_504_M", "SCH_ENR_IDEA_F", "SCH_ENR_IDEA_M" )
addColumns1 <- c( "SCH_ENR_AM_F", "SCH_ENR_AS_F", "SCH_ENR_BL_F", "SCH_ENR_HI_F", "SCH_ENR_HP_F", "SCH_ENR_TR_F", "SCH_ENR_WH_F", "SCH_ENR_AM_M", "SCH_ENR_AS_M", "SCH_ENR_BL_M", "SCH_ENR_HI_M", "SCH_ENR_HP_M", "SCH_ENR_TR_M", "SCH_ENR_WH_M" )


## Execute... 
## AND q.crdcQuestionID = 159 
subQs <- qryHelper("SELECT * FROM OPENQUERY(campus, 'SELECT DISTINCT q.crdcQuestionID, REPLACE(q.crdcIdentifier, ''-'', ''_'') AS mod_crdcIdentifier, q.crdcIdentifier, sq.elementName, sq.dataElement1, dataElement2 FROM clark.dbo.CRDCQuestion q INNER JOIN clark.dbo.CRDCSubQuestion sq ON q.crdcQuestionID = sq.crdcQuestionID 
                    WHERE q.endYear = ''2021'' ')")


initializeCRDCEnrollsTable <- 1
initializeADHCEnrollsTable <- 1
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_CRDC_OverallEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeCRDCEnrollsTable))
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_AdHoc_OverallEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeADHCEnrollsTable))
schoolsFromCRDCMapping <- generateSchoolsList('2021', '2021') %>% head(5) %>%
  mutate(
    crdc_EnrollsQry = map_chr(schoolID, ~ glue::glue("EXEC crdc_2021.generate_crdcOverallEnrollment @schoolID='{.}', @endYear='{year_id}', @CountDay='{countDate_ID}'", year_id="2021", countDate_ID="2020-10-01")), 
    adhc_EnrollsQry = map_chr(schoolID, ~ glue::glue("EXEC crdc_2021.generate_AdhocOverallEnrollment @schoolID='{.}', @endYear='{year_id}', @CountDay='{countDate_ID}'", year_id="2021", countDate_ID="2020-10-01")), 
  ) %>% 
  mutate(
    across(contains("_EnrollsQry"), ~map(.x, qryHelper), .names="{.col}_dfs")
  ) %>% 
  mutate(
    across(contains("_dfs"), ~map(.x, feat_ChangeType_SwitchNonNAValues, colsToUpdate=indicatorColumns1), .names="{.col}_fed")
  )  

## Saving dataframe.
save(schoolsFromCRDCMapping, file="schoolsList.RData")


## ENRL_1
crdcID <- "ENRL_1"
ENRL_1 <- schoolsFromCRDCMapping %>% 
  #head(10) %>% 
  mutate(
    across(contains("_fed"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount")),
           .names="{.col}_mapped"
    )
  ) %>% mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) %>% mutate(
    across(contains("_wd"), ~map(.x, imputeAndSumAcrossColumns, columnsToAdd=all_of(addColumns1), aggrColName="totalCntEthnicities"), .names="{.col}_colsummed")
  ) %>% mutate(
    ethnicityCntEqual = map2(crdc_EnrollsQry_dfs_fed_grouped_mapped_wd_colsummed, adhc_EnrollsQry_dfs_fed_grouped_mapped_wd_colsummed, ~ .x$totalCntEthnicities == .y$totalCntEthnicities)
    #ethnicityCntEqual = map2(crdc_dfs_fed_grouped_mapped_wd_colsummed, adhc_dfs_fed_grouped_mapped_wd_colsummed, ~ .x$totalCntEthnicities == .y$totalCntEthnicities)
  ) 



## ENRL_2a
crdcID = "ENRL_2a"
ENRL_2a <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_fed"), ~map(.x, filter(inEL == 1 )), .names="{.col}_filtered") 
  ) %>%
  mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1[1]), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount")),
           .names="{.col}_mapped"
    )
  ) %>% mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))

## ENRL_2a
ENRL_2b <- ENRL_2a

## ENRL_3a
crdcID = "ENRL_3a"
ENRL_3a <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_fed"), ~map(.x, filter(inIDEA == 1 )), .names="{.col}_filtered") 
  ) %>%
  mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1[1]), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount")),
           .names="{.col}_mapped"
    )
  ) %>% mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))

## ENRL_3b
crdcID = "ENRL_3b"
ENRL_3b <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_fed"), ~map(.x, filter(in504 == 1 )), .names="{.col}_filtered") 
  ) %>% mutate(
    across(contains("_filtered"), ~map(.x, groupingByListOfcolumns, lc=columnGroupList1[1]), .names="{.col}_grouped")
  ) %>% mutate(
    across(contains("_grouped"), 
           ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1", "dataElement2" = "dataElement2"), selectColumns=c("elementName", "grpCount")),
           .names="{.col}_mapped"
    )
  ) %>% mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) #%>% mutate(allEqual = map2(crdc_enrolls_fed_grouped_mapped_wd, adhc_enrolls_fed_grouped_mapped_wd, all_equal))


## SCHR_1
crdcID = "SCHR_1" 
SCHR_1 <- schoolsFromCRDCMapping %>%
  mutate(
    across(contains("_fed"), ~map(.x, feat_ChangeType_SwitchNonNAValues, colsToUpdate=gradesList), .names="{.col}_gradesfed")
  ) %>%
  mutate(
    across(contains("_gradesfed"), ~map(.x, groupingByListOfcolumns, lc=groupByColumns6), .names="{.col}_grouped")
  ) %>% 
  mutate(
    across(contains("_grouped"), ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1"), selectColumns=c("elementName", "grpCount")),
           .names="{.col}_mapped"
    )           
  ) %>% mutate(
    across(contains("_mapped"), ~map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="{.col}_wd")
  ) 

# (about ages 3-10)
# (about ages 11-13)
# (about ages 14 or older)

## SCHR_2
crdcID = "SCHR_2" 
SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_fed"), ~map(.x, filter(stateGrade == "UG")), .names="{.col}_UG")
  ) %>% 
  mutate(
    across(contains("_UG"), ~map(.x, ))    
  )













