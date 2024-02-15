#### OFFN module.
## version 4.1.1 

sdir <- getwd()
sfiles <- dir(path=sdir, pattern="OCR_Helpers.R")
for ( f in sfiles ) {
  source(file = file.path(sdir, f))
}

tic() 
  strQ <- glue::glue("EXEC [crdc_2021].generate_OffensesIncidents_EntireYear @endYear='{year_id}'", year_id=str_SchoolYear)
  dfBehaviorEventsOffense <- qryHelper2(strQ, str_ServerName, str_DBName) %>%
  feat_SwitchValuesIfTrue(indicatorColumns1, predicateFuncs1) %>% 
  feat_SwitchValues( colsToUpdate=all_of(c("stateGrade")), columnValues=gradesList ) %>%
  feat_SwitchValues( colsToUpdate=all_of(c("raceEthnicity")), columnValues=ethnicitiesList ) %>% 
  feat_SwitchValues( colsToUpdate=all_of(c("gender")), columnValues=genderList ) %>%
  rowwise() %>% packageColumns(all_of(offenseColumns_To_Be_Packed), "packedColumns") %>% ungroup() %>% 
  mutate(offenseCategory = packedColumns %>% map(., ~ map(predicatesList_OffensesTypes, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% 
                                                   unlist() %>% .[which(.)] %>% names() %>%
                                                   head(1) %||% NA )) %>% 
  mutate(offenseCategory = map_chr(offenseCategory, ~ ifelse(length(offenseCategory) == 0, "_", .))) %>%
  group_by(schoolID) %>% split(.$schoolID) %>% tibble(dfs=.) %>% ungroup() %>% 
  mutate(schoolID = map(dfs, ~ .[1, "schoolID"]) %>% unlist()) %>% 
  mutate(ncesSchoolID = map(dfs, ~ .[1, "ncesSchoolID"]) %>% unlist())   
toc()
save(dfBehaviorEventsOffense, file="dfBehaviorEventsOffense.RData")
load(file="dfBehaviorEventsOffense.RData")
#schoolsFromCRDCMapping
load("schoolsList.RData")

schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% map_lgl(~ nrow(.)<10 & nrow(.) > 0)  %>% which()
schoolsWithEnrolls_Index <- schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% map_lgl(~ nrow(.) > 0) %>% which()
schoolsWithEnrolls_IDs <- schoolsFromCRDCMapping$schoolID[schoolsWithEnrolls_Index] %>% unlist()



### building out a test dataframe...
# a2 <- tibble(code=as.character(NA), weaponType=as.character(NA))
# a2[1, ] <- tibble("DFH", NA)
# a2[2, ] <- tibble("A", "O")
# a2[3, ] <- tibble("DHG", "O")
# a2[4, ] <- tibble(code="DBD", weaponType=NA)
# a2[5, ] <- tibble(code=NA, weaponType=NA) 
# a2[6, ] <- tibble(code=NA, weaponType="O")
# a2[7, ] <- tibble(code="DBC", "O")
# a2[1, ] <- tibble(code = "DSA", weaponType = NA)
# cc <- c("code", "weaponType")
# a22 <- a2 %>% rowwise() %>% packageColumns(all_of(cc), "testCol")
# 
# aa22 <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")
# names(aa22) <- c("first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth")
# 
# ## GOOD for a single set of functions... The only thing is argument list has to be the second list... Is it possible to bring arg list to the front???
# map(a22$c1, ~ map2(list(isStringIncludedInTheList(c("DHG", "DFH", "DBC", "DBD", "DAJ")), isNotNA), ., function(fn = .x, args = .y) { exec(fn, args) }) %>% reduce(`&`))
# ## GOOD with args list brought to the front. 
# a22$c1 %>% map(~ map2(.x=., .y=list(isStringIncludedInTheList(c("DHG", "DFH", "DBC", "DBD", "DAJ")), isNotNA), function(args = .x, fn = .y) { exec(fn, args) }) %>% reduce(`&`))
# 
# ## GOOD for a single set of binary tests... With args brought to the front... 
# a22$c1 %>% map(~ map2(.x=.x, .y=.y, function(args = .x, fn = .y) { exec(fn, args) }) %>% reduce(`&`), .y=predicatesList_OffensesTypes[[5]])
# 
# ## Turn this into a vectorized function... 
# a22$c1 %>% map(~ map2(.x=.x, .y=.y, function(args = .x, fn = .y) { exec(fn, args) }) %>% reduce(`&`), .y=predicatesList_OffensesTypes[[5]])
# 
# # 1. list of atomic values... 
# # 1. list of logical functions... 
# # 2. use exec to return the list of results. One-to-One pairing between arg and function lists. 
# # 2. The exec will output a logical vector. This could be reduced to a single boolean. 
# 
# # testing helper functions above...
# map2(list(isStringIncludedInTheList(c("DFH", "DHG", "DBC", "DBD", "DAJ")), isNotNA), list("DFH", NA), function(fn = .x, args = .y) { exec(fn, !!!args) })
# #map2(
# # list(isStringIncludedInTheList(c("DFH", "DHG", "DBC", "DBD", "DAJ")), isNotNA), 
# # list(list(a2$code), list(a2$weaponType)), 
# # function(fn = .x, args = .y) { exec(fn, !!!args) }) 
# a22$testCol
# ro1 <- 
#   map(a22$testCol, ~ map(predicatesList_OffensesTypes, testSingle_Predicate_Value_Pair_Vectorized, ., TRUE) %>% unlist()) %>% 
#   map(., ~ which(.) %>% names(.)) %>% 
#   map(tail) %>% 
#   map(~ if_else( length(.) == 0, as.character(NA), `[`(., 1)))








## OFFENSES 
## OFFN_1... 
crdcID = "OFFN_1"
OFFN_1 <- dfBehaviorEventsOffense %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, code = replace_na(code, ""))))) %>% 
  #mutate(across("dfs", ~ map(.x, ~ mutate(.x, offenseCategory = unlist(offenseCategory))))) %>%
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_fillInWithAValue, 0, FALSE)

testWeaponType <- function(x) {
  isStringIncludedInTheList(lstWeaponType)(x)
} 

crdcID <- "OFFN_2" 
OFFN_2 <- dfBehaviorEventsOffense %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, code = replace_na(code, ""))))) %>% 
  mutate(dfs = map(.$dfs, ~ filter(.x, code == "DWI" & ( isStringIncludedInTheList(lstWeaponType)("H: Handgun") )  ))) %>% 
  mutate(across("dfs", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(dfs_adhc_wd = map(.$grouped, ~ if_else(nrow(.x) == 0, as.character(NA), "Y") %>% tibble(`SCH_FIREARM_IND` = .))) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_fillInWithAValue, "N", FALSE)
  

crdcID <- "OFFN_3"
OFFN_3 <- dfBehaviorEventsOffense %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, AllgEventCode = replace_na(AllgEventCode, ""))))) %>% 
  mutate(across("dfs", ~ map(.x, ~ mutate(.x, code = replace_na(code, ""))))) %>% 
  mutate(dfs_adhc_wd = list(tibble(`SCH_HOMICIDE_IND` = as.character(NA)))) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_fillInWithAValue, "N", FALSE)


crdcID <- "OFFN_4" 
OFFN_4 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(personID) & offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape")))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  ) %>% 
  fillInZeroForIndexdSchools("schoolID", "dfs_adhc_wd", schoolsWithEnrolls_IDs, hf_fillInWithAValue, "N", FALSE)


crdcID <- "OFFN_5" 
OFFN_5 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(stfPersonID) & offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape")))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )


crdcID <- "OFFN_6" 
OFFN_6 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(stfPersonID) & 
                                          offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape") & 
                                          isNotNA(stateResCode)
                                          ))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

crdcID <- "OFFN_7" 
OFFN_7 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(stfPersonID) & 
                                          offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape") & 
                                          isNotNA(AllgEventCode)
  ))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )




crdcID <- "OFFN_8" 
OFFN_8 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(stfPersonID) & 
                                          offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape") & 
                                          isNotNA(resolutionCode) & isNotNA(AllgEventCode)
  ))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

  
crdcID <- "OFFN_9" 
OFFN_9 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(stfPersonID) & 
                                          offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape") & 
                                          isNotNA(resolutionCode) & isNotNA(AllgEventCode)
  ))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )

crdcID <- "OFFN_10" 
OFFN_10 <- dfBehaviorEventsOffense %>% 
  mutate(filtered = map(.$dfs, ~ filter(.x, role == "Offender" & isNotNA(stfPersonID) & 
                                          offenseCategory %in% c("Sexual assault (other than rape)", "Rape or attempted rape") & 
                                          isNotNA(resolutionCode) & isNotNA(AllgEventCode)
  ))) %>% 
  mutate(across("filtered", ~ map(.x, groupingByListOfcolumns, list(c("offenseCategory")), distinctCnt = TRUE, cntColumnName="incidentID"), .names="grouped")) %>% 
  mutate(across("grouped", ~ map(.x, mapToElementName, 
                                 yf=filter(subQs, mod_crdcIdentifier==crdcID), 
                                 byMatchColumns=c("dataElement1" = "dataElement1"), 
                                 selectColumns=c("elementName", "grpCount", "dataElement1Seq")), 
                .names="{.col}_mapped"
  )) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, ~ arrange(.x, dataElement1Seq) %>% select("elementName", "grpCount")))
  ) %>% 
  mutate(
    across(contains("_mapped"), ~ map(.x, widerbyColumns, nameCols=all_of(widenByColumns), valueCols=all_of(countValueColumns)), .names="dfs_adhc_wd")
  )



OFFN_1_Bound <- cbind(ncesSchoolID = OFFN_1$ncesSchoolID, schoolID = OFFN_1$schoolID, OFFN_1$dfs_adhc_wd %>% reduce(rbind))
OFFN_2_Bound <- cbind(schoolID = OFFN_2$schoolID, OFFN_2$dfs_adhc_wd %>% reduce(rbind))
OFFN_3_Bound <- cbind(schoolID = OFFN_3$schoolID, OFFN_3$dfs_adhc_wd %>% reduce(rbind))
OFFN_4_Bound <- cbind(schoolID = OFFN_4$schoolID, OFFN_4$dfs_adhc_wd %>% reduce(rbind))
OFFN_5_Bound <- cbind(schoolID = OFFN_5$schoolID, OFFN_5$dfs_adhc_wd %>% reduce(rbind))
OFFN_6_Bound <- cbind(schoolID = OFFN_6$schoolID, OFFN_6$dfs_adhc_wd %>% reduce(rbind))
OFFN_7_Bound <- cbind(schoolID = OFFN_7$schoolID, OFFN_7$dfs_adhc_wd %>% reduce(rbind))
OFFN_8_Bound <- cbind(schoolID = OFFN_8$schoolID, OFFN_8$dfs_adhc_wd %>% reduce(rbind))
OFFN_9_Bound <- cbind(schoolID = OFFN_9$schoolID, OFFN_9$dfs_adhc_wd %>% reduce(rbind))


OFFN_Bound <- bind_cols(
  OFFN_1_Bound, 
  select(OFFN_2_Bound, -schoolID), 
  select(OFFN_3_Bound, -schoolID)
  #select(OFFN_4_Bound, -schoolID)
  # select(OFFN_5_Bound, -schoolID), 
  # select(OFFN_6_Bound, -schoolID), 
  # select(OFFN_7_Bound, -schoolID), 
  # select(OFFN_8_Bound, -schoolID), 
  # select(OFFN_9_Bound, -schoolID)
) 

OFFN_Bound %>% writexl::write_xlsx("OFFN.xlsx")

