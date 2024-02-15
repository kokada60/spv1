sDir <- getwd() 
sFiles <- dir(sDir, pattern = "OCR_Helpers.R") 
for ( f in sFiles ) {
  source(file = file.path(sDir, f))
}

schs <- readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\schoolID.xlsx") %>% distinct(ncesSchoolID) %>% unique()
load("schoolsList.RData")
schoolsFromCRDCMapping$
load("savedData/dfStudent_Retained_Combined.RData")


ENRL_1_Valid <- cbind(ccsdID = ENRL_1$ccsdID, schoolNumber = ENRL_1$schoolNumber, ncesSchoolID = ENRL_1$ncesSchoolID, schoolID = ENRL_1$schoolID %>% reduce(rbind), schoolName = ENRL_1$schoolName %>% reduce(rbind), ENRL_1$adhc_EnrollsQry_dfs_grouped_wd %>% reduce(rbind)) %>%
  filter(schoolName %in% schNames)


ENRL_Valid <- ENRL_1_Valid %>% mutate(Asians = SCH_ENR_AS_M + SCH_ENR_AS_F, 
                                      Blacks = SCH_ENR_BL_M + SCH_ENR_BL_F, 
                                      Hispanics = SCH_ENR_HI_M + SCH_ENR_HI_F, 
                                      PacIslanders = SCH_ENR_HP_M + SCH_ENR_HP_F, 
                                      MultiEthnic = SCH_ENR_TR_M + SCH_ENR_TR_F, 
                                      White = SCH_ENR_WH_M + SCH_ENR_WH_F, 
                                      Ideas=SCH_ENR_IDEA_M + SCH_ENR_IDEA_F, 
                                      EL = SCH_ENR_LEP_M + SCH_ENR_LEP_F, 
                                      S504=SCH_ENR_504_M + SCH_ENR_504_F ) %>% 
  select(ccsdID, schoolNumber, ncesSchoolID, schoolID, schoolName, Asians, Blacks, Hispanics, PacIslanders, MultiEthnic, White, Ideas, EL, S504)
View(ENRL_Valid)

# schTmp <- c("320006000011", "320006000018", "320006000276", "320006000420", "320006000539")
# dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% distinct(schoolName) %>% arrange(schoolName) %>% View() 
#schTmp <- c("320006000950", "320006000661", "320006000007", "320006000011", "320006000567", "320006000493", "320006000018", "320006000405", "320006000406", "320006000030", "320006000540", "320006000571", "320006000754", "320006000495", "320006000767", "320006000276", "320006000748", "320006000032", "320006000494", "320006000407", "320006000065", "320006000714", "320006000604", "320006000342", "320006000445", "320006000446", "320006000089", "320006000595", "320006000539", "320006000420", "320006000616", "320006000777", "320006000745", "320006000107", "320006000114")
#dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% distinct(schoolName) %>% arrange(schoolName) %>% View() 
#schTmp <- c("320006000007", "320006000011", "320006000012", "320006000013", "320006000018", "320006000030", "320006000032", "320006000065", "320006000089", "320006000107", "320006000111", "320006000114", "320006000276", "320006000328", "320006000405", "320006000406", "320006000407", "320006000420", "320006000445", "320006000446", "320006000493", "320006000494", "320006000495", "320006000539", "320006000540", "320006000567", "320006000571", "320006000595", "320006000604", "320006000616", "320006000661", "320006000714", "320006000745", "320006000754", "320006000767", "320006000777", "320006000950")
#dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schTmp) %>% distinct(schoolName) %>% arrange(schoolName) %>% View() 
# schTmp <- c("320006000007", "320006000276", "320006000407", "320006000420", "320006000446", "320006000539", "320006000595", "320006000604", "320006000714", "320006000754")
# dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schTmp) %>% distinct(schoolName) %>% arrange(schoolName) %>% View() 


grds <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "0K", "10", "11", "12")
schsTmp <- c("320006000011", "320006000018", "320006000276", "320006000420", "320006000539")
re1 <- "Asian"
dfRETN1911B <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & raceEthnicity == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=raceEthnicity, RETNAsian = RETNCnt) %>% arrange(schoolName) 
#dfRETN1911B = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1911S_B.xlsx") %>% group_by(ncesSchoolID, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=raceEthnicity, RETNAsian = RETNCnt)
schsTmp <- c("320006000950", "320006000661", "320006000007", "320006000011", "320006000567", "320006000493", "320006000018", "320006000405", "320006000406", "320006000030", "320006000540", "320006000571", "320006000754", "320006000495", "320006000767", "320006000276", "320006000748", "320006000032", "320006000494", "320006000407", "320006000065", "320006000714", "320006000604", "320006000342", "320006000445", "320006000446", "320006000089", "320006000595", "320006000539", "320006000420", "320006000616", "320006000777", "320006000745", "320006000107", "320006000114")
re1 <- "Black or African American"
dfRETN1911C <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & raceEthnicity == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=raceEthnicity, RETNBlack = RETNCnt) %>% arrange(schoolName) 
#dfRETN1911C = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1911S_C.xlsx") %>% group_by(ncesSchoolID, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=raceEthnicity, RETNBlack = RETNCnt)
schsTmp <- c("320006000007", "320006000011", "320006000012", "320006000013", "320006000018", "320006000030", "320006000032", "320006000065", "320006000089", "320006000107", "320006000111", "320006000114", "320006000276", "320006000328", "320006000405", "320006000406", "320006000407", "320006000420", "320006000445", "320006000446", "320006000493", "320006000494", "320006000495", "320006000539", "320006000540", "320006000567", "320006000571", "320006000595", "320006000604", "320006000616", "320006000661", "320006000714", "320006000745", "320006000754", "320006000767", "320006000777", "320006000950")
re1 <- "Hispanic or Latino of any race"
dfRETN1911D <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & raceEthnicity == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=raceEthnicity, RETNHisp = RETNCnt) %>% arrange(schoolName) 
#dfRETN1911D = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1911S_D.xlsx") %>% group_by(ncesSchoolID, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=raceEthnicity, RETNHisp = RETNCnt)
schsTmp <- c("320006000007", "320006000276", "320006000407", "320006000420", "320006000446", "320006000539", "320006000595", "320006000604", "320006000714", "320006000754")
re1 <- "Native Hawaiian or Other Pacific Islander"
dfRETN1911E <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & raceEthnicity == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=raceEthnicity, RETNPacIsn = RETNCnt) %>% arrange(schoolName) 
#dfRETN1911E = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1911S_E.xlsx") %>% group_by(ncesSchoolID, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=raceEthnicity, RETNPacIsn = RETNCnt)
schsTmp <- c("320006000007", "320006000011", "320006000018", "320006000030", "320006000107", "320006000114", "320006000276", "320006000405", "320006000406", "320006000407", "320006000420", "320006000445", "320006000493", "320006000494", "320006000495", "320006000539", "320006000540", "320006000567", "320006000571", "320006000595", "320006000604", "320006000616", "320006000661", "320006000714", "320006000754", "320006000777")
re1 <- "Two or more races"
dfRETN1911F <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & raceEthnicity == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=raceEthnicity, RETNMulti = RETNCnt) %>% arrange(schoolName) 
#dfRETN1911F = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1911S_F.xlsx") %>% group_by(ncesSchoolID, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=raceEthnicity, RETNMulti = RETNCnt)
schsTmp <- c("320006000007", "320006000011", "320006000013", "320006000018", "320006000030", "320006000032", "320006000107", "320006000114", "320006000276", "320006000405", "320006000408", "320006000420", "320006000445", "320006000493", "320006000494", "320006000495", "320006000539", "320006000567", "320006000595", "320006000714", "320006000745", "320006000777", "320006000950")
re1 <- "White" 
dfRETN1911G <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & raceEthnicity == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=raceEthnicity, RETNWhite = RETNCnt) %>% arrange(schoolName) 
#dfRETN1911G = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1911S_G.xlsx") %>% group_by(ncesSchoolID, raceEthnicity) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=raceEthnicity, RETNWhite = RETNCnt)
schsTmp <- c("320006000007", "320006000011", "320006000018", "320006000026", "320006000030", "320006000032", "320006000065", "320006000089", "320006000107", "320006000111", "320006000114", "320006000276", "320006000328", "320006000405", "320006000406", "320006000407", "320006000420", "320006000445", "320006000446", "320006000493", "320006000494", "320006000495", "320006000530", "320006000536", "320006000539", "320006000540", "320006000567", "320006000571", "320006000587", "320006000595", "320006000604", "320006000609", "320006000616", "320006000661", "320006000714", "320006000745", "320006000754", "320006000777", "320006000950")
re1 <- "English Learners (EL)" 
dfRETN1912A <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & inEL == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, inEL) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=inEL, RETNEL = RETNCnt) %>% arrange(schoolName) 
#dfRETN1912A = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1912S_A_EL.xlsx") %>% group_by(ncesSchoolID, inEL) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=inEL, RETNEL = RETNCnt)
schsTmp <- c("320006000007", "320006000011", "320006000013", "320006000018", "320006000030", "320006000032", "320006000065", "320006000089", "320006000107", "320006000114", "320006000272", "320006000276", "320006000405", "320006000406", "320006000407", "320006000408", "320006000420", "320006000445", "320006000493", "320006000494", "320006000495", "320006000539", "320006000540", "320006000567", "320006000571", "320006000595", "320006000604", "320006000616", "320006000661", "320006000694", "320006000714", "320006000745", "320006000749", "320006000754", "320006000777")
re1 <- "Students with Disabilities (IDEA)"
dfRETN1912B <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & inIDEA == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, inIDEA) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=inIDEA, RETNIDEA = RETNCnt) %>% arrange(schoolName) 
#dfRETN1912B = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1912S_B_IDEA.xlsx") %>% group_by(ncesSchoolID, inIDEA) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=inIDEA, RETNIDEA = RETNCnt)
schsTmp <- c("320006000007", "320006000406", "320006000407", "320006000420", "320006000446", "320006000493", "320006000494", "320006000539", "320006000540", "320006000571", "320006000595", "320006000604", "320006000661", "320006000754")
re1 <- "Students with Disabilities (Section 504 Only)"
dfRETN1912C <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & in504 == re1 & retained == "Y") %>% group_by(ncesSchoolID, schoolName, in504) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=in504, RETN504 = RETNCnt) %>% arrange(schoolName) 
#dfRETN1912C = readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\df1912S_C_504.xlsx")  %>% group_by(ncesSchoolID, in504) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, group=in504, RETN504 = RETNCnt)

schsTmp <- c("320006000446", "320006000616", "320006000755")
re1 <- "Students with Disabilities (IDEA)"
dfRETN1917J_IDEA <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(inIDEA == re1 & retained == "Y" & grade == "09") %>% group_by(ncesSchoolID, schoolName, inIDEA) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=inIDEA, RETNIDEA = RETNCnt) %>% arrange(schoolName) 
re1 <- "Students with Disabilities (Section 504 Only)"
dfRETN1917J_504 <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(in504 == re1 & retained == "Y" & grade == "09") %>% group_by(ncesSchoolID, schoolName, in504) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=in504, RETN504 = RETNCnt) %>% arrange(schoolName) 
schsTmp <- c("320006000408", "320006000796")
re1 <- "Students with Disabilities (IDEA)"
dfRETN1917K_IDEA <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(inIDEA == re1 & retained == "Y" & grade == "10") %>% group_by(ncesSchoolID, schoolName, inIDEA, grade) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=inIDEA, grade, RETNIDEA = RETNCnt) %>% arrange(schoolName) 
re1 <- "Students with Disabilities (Section 504 Only)"
dfRETN1917K_504 <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(in504 == re1 & retained == "Y" & grade == "10") %>% group_by(ncesSchoolID, schoolName, in504, grade) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=in504, grade, RETN504 = RETNCnt) %>% arrange(schoolName) 
schsTmp <- c("320006000446")
re1 <- "Students with Disabilities (IDEA)"
dfRETN1917M_IDEA <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(inIDEA == re1 & retained == "Y" & grade == "12") %>% group_by(ncesSchoolID, schoolName, inIDEA, grade) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=inIDEA, grade, RETNIDEA = RETNCnt) %>% arrange(schoolName) 
re1 <- "Students with Disabilities (Section 504 Only)"
dfRETN1917M_504 <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(in504 == re1 & retained == "Y" & grade == "12") %>% group_by(ncesSchoolID, schoolName, in504, grade) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, group=in504, grade, RETN504 = RETNCnt) %>% arrange(schoolName) 



# 1919S
schsTmp <- c("320006000007", "320006000011", "320006000013", "320006000018", "320006000030", "320006000032", "320006000065", "320006000089", "320006000107", "320006000114", "320006000276", "320006000405", "320006000420", "320006000445", "320006000446", "320006000493", "320006000494", "320006000495", "320006000539", "320006000540", "320006000553", "320006000567", "320006000571", "320006000595", "320006000604", "320006000616", "320006000661", "320006000714", "320006000745", "320006000754", "320006000767", "320006000777")
dfRETN1919_2020_2021 <- dfStudent_Retained_Combined %>% filter(paste("3200060", ncesSchoolID, sep="") %in% schsTmp) %>% select(dfs) %>% reduce(rbind) %>% reduce(rbind) %>% filter(grade %in% grds & retained == "Y") %>% group_by(ncesSchoolID, schoolName) %>% summarize(RETNCnt = n(), .groups="keep") %>% select(id = ncesSchoolID, schoolName, "20-21" = RETNCnt) %>% arrange(id) 

dfRETN1911B_2017_2018 <- readxl::read_excel("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\2017_2018_Schools_3200060.xlsx") %>% 
  select(SCH_ID, starts_with("SCH_RET") & !ends_with("_IND"))

dfRETN1911B_2017_2018_Aggr <- dfRETN1911B_2017_2018 %>% mutate(totalRETN = rowSums(across(where(is.numeric) & !c("SCH_ID")), na.rm = TRUE)) %>% filter(SCH_ID %in% schsTmp) 
dfRETN1911B_2017_2018_Aggr <- dfRETN1911B_2017_2018_Aggr %>% select(SCH_ID, "17-18" = totalRETN) %>% arrange(SCH_ID)

cbind(dfRETN1919_2020_2021 %>% ungroup, dfRETN1911B_2017_2018_Aggr %>% ungroup) %>% mutate(diff=`20-21` - `17-18`) %>% 
  select(schoolName, id, SCH_ID, `17-18`, `20-21`, diff) %>% #View()
  writexl::write_xlsx("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\RETN_Validation_1919S.xlsx")





#   White Ideas  EL  S504
RETN_1911S_B <- ENRL_Valid %>% inner_join(dfRETN1911B, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1911S_B") %>% select(Issue, 1:5, group, Enrolled=Asians, RETN=RETNAsian)
RETN_1911S_C <- ENRL_Valid %>% inner_join(dfRETN1911C, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1911S_C") %>% select(Issue, 1:5, group, Enrolled=Blacks, RETN=RETNBlack)
RETN_1911S_D <- ENRL_Valid %>% inner_join(dfRETN1911D, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1911S_D") %>% select(Issue, 1:5, group, Enrolled=Hispanics, RETN=RETNHisp)
RETN_1911S_E <- ENRL_Valid %>% inner_join(dfRETN1911E, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1911S_E") %>% select(Issue, 1:5, group, Enrolled=PacIslanders, RETN=RETNPacIsn)
RETN_1911S_F <- ENRL_Valid %>% inner_join(dfRETN1911F, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1911S_F") %>% select(Issue, 1:5, group, Enrolled=MultiEthnic, RETN=RETNMulti)
RETN_1911S_G <- ENRL_Valid %>% inner_join(dfRETN1911G, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1911S_G") %>% select(Issue, 1:5, group, Enrolled=White, RETN=RETNWhite)

RETN_1912S_A <- ENRL_Valid %>% inner_join(dfRETN1912A, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1912S_A") %>% select(Issue, 1:5, group, Enrolled=EL, RETN=RETNEL)
RETN_1912S_B <- ENRL_Valid %>% inner_join(dfRETN1912B, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1912S_B") %>% select(Issue, 1:5, group, Enrolled=Ideas, RETN=RETNIDEA)
RETN_1912S_C <- ENRL_Valid %>% inner_join(dfRETN1912C, by=c("ncesSchoolID" = "id"), keep=FALSE) %>% mutate( Issue="RETN_1912S_C") %>% select(Issue, 1:5, group, Enrolled=S504, RETN=RETN504)

rbind(RETN_1911S_B, RETN_1911S_C, RETN_1911S_D, RETN_1911S_E, RETN_1911S_F, RETN_1911S_G, RETN_1912S_A, RETN_1912S_B, RETN_1912S_C) %>% mutate("RETN%" = RETN / Enrolled) %>% 
  writexl::write_xlsx("D:\\Users\\OKADAK\\Documents\\Tasks\\Named Tasks\\OCR_2021\\validation\\RETN_Validation.xlsx")






