library(readxl)
library(tidyverse) 
library(odbc)
library(DBI)
library(lubridate)

con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
distanceEds <- DBI::dbGetQuery(con, "
  SELECT * 
  FROM OPENQUERY(campus, '
  	SELECT DISTINCT
  		--GETDATE() AS DataAsof
  		c.schoolID 
  		, sch.number AS stateSchoolID, sch.standardCode AS ccsdID
  		,s.studentNumber
  		,s.grade
  		,s.serviceType
  		,s.activeToday
  		,c.name
  		,cc.value AS CalendarType
  		,b.groupName
  		,b.virtualToday
  
  	FROM clark.dbo.v_AdHocStudent s with (nolock)
  	INNER JOIN clark.dbo.Calendar c with (nolock) ON s.calendarID = c.calendarID
  	INNER JOIN clark.dbo.school sch ON c.schoolID = sch.schoolID
  	--CCSD School Type
  	INNER JOIN [clark].[dbo].[CustomCalendar] cc WITH (NOLOCK)	ON c.calendarID = cc.calendarID AND cc.attributeID = 872	
  	INNER  JOIN clark.dbo.v_BlendedLearningAssignmentActive b with (nolock) ON s.personID = b.personID
  	--LEFT OUTER JOIN clark.dbo.v_BlendedLearningAssignmentHistory b with (nolock) ON s.personID = b.personID
  
  	Where 1=1
  		AND s.activeToday = 1
  		AND s.serviceType = ''P''
  		AND s.grade IN (''PK'', ''TP'', ''0K'', ''01'', ''02'', ''03'', ''04'', ''05'', ''06'', ''07'', ''08'', ''09'', ''10'', ''11'', ''12'', ''UN'')
  		AND s.startDate >= ''2021-08-09''
  		AND b.assignmentEndDate >= GETDATE()
  		AND b.groupStartDate > ''2021-08-01''
  		AND b.groupName = ''Distance Learning Group''
  		AND b.virtualToday = 1
  		--AND s.schoolID  IN ( 
  
  		--						''217''	--Bennett, William G. ES		10
  		--						,''252''	--Becker Sr., Ernest A. MS		29
  		--						,''232''	--Bridger, Jim MS				82
  		--						,''266''	--Cram, Brian and Teri MS		111
  		--						,''285''	--Faiss, Wilbur and Theresa MS	0
  		--						,''242''	--Guinn, Kenny C. MS			120
  		--						,''282''	--Johnston, Carroll M. MS		0
  		--						,''256''	--Lied STEM Academy MS			0 
  		--						,''279''	--Mack, Jerome MS				0
  		--						,''258''	--Molasky, Irwin and Susan JHS	19
  		--						,''246''	--Laughlin JHS/HS				23
  		--						,''329''	--Desert Pines HS				0
  
  		--					)
  
  	--ORDER BY
  	--	s.calendarName
  ')
");

currentEnrolls <- DBI::dbGetQuery(con, "
  SELECT DISTINCT 
  	CASE WHEN GradeLevelCode IN ('0K', 'TP', 'PK', '01', '02', '03', '04', '05' ) THEN 'ES' WHEN GradeLevelCode IN ('06', '07', '08' ) THEN 'MS' WHEN GradeLevelCode IN ( '09', '10', '11', '12', 'AD', 'UN' ) THEN 'HS' END AS schoolType,
  	e.EducationOrganizationID AS ccsdID, 
  	s.EducationOrganizationStateID AS stateID,
  	s.EducationOrganizationShortName As schoolName,
  	StudentEthnicityCode AS cntType, 
  	e.GradeLevelCode AS grade,
  	e.StudentDateOfBirth AS dob,
  	StudentNumber	
  FROM [ORION.CIS.CCSD.NET].SDM.Dif.mv_Enrollment e
  INNER JOIN [ORION.CIS.CCSD.NET].SDM.dbo.DimEducationOrganization s 
  	ON e.EducationOrganizationID = s.EducationOrganizationID
  WHERE 
  	s.EducationOrganizationCurrentRowIndicator = 'Current' 
  	AND e.SchoolYear = '2022'
  	AND SchoolYearActiveIndicator = 'Active' 
  	AND CurrentEnrollment = 1	
" )

currentEnrolls[which(is.na(map_dbl(currentEnrolls$stateID, as.numeric))), "stateID"]
currentEnrolls[which(is.na(map_dbl(currentEnrolls$stateID, as.numeric))), ]

modIfNumeric <- function(x, modOperand) {
  ifelse(suppressWarnings(!is.na(as.numeric(x))), as.numeric(x) %% modOperand %>% as.character(), x)
}
getCurrentAge <- function(dt_Of_Birth, curr, dr = lubridate::duration(num = 1, units="years")) {
  floor(lubridate::interval(lubridate::as_date(dt_Of_Birth), curr) / dr )
}

dr <- duration(num = 1, units = "years")
currentDate = Sys.Date() 
sampledGradesGroup <- c(4, 8) 
sampledGradesGroup2Dig <- c("04", "08")


currentEnrolls <- currentEnrolls %>% mutate(stateSchoolID = map_chr(stateID, modIfNumeric, 2000))

origFiles <- readxl::read_xlsx(path="C:\\users\\okadak\\downloads\\TUDA_PSI_Follow-Up_Submission.xlsx", sheet=1, col_names=TRUE) %>% as_tibble()
origFileStateIDAdded <- origFiles %>% mutate(stateSchoolID = map_chr(`State School ID`, modIfNumeric, 2000))


# Take age group... Only for four schools 213, 192, 256, 144...
currentEnrolls_ByAge <- list(origFileStateIDAdded[which(!origFileStateIDAdded$`Sampled Grade/Age` %in% sampledGradesGroup), ], currentEnrolls) %>% 
  reduce(inner_join, by=c("stateSchoolID" = "stateSchoolID")) %>% 
  mutate(currentAge = getCurrentAge(dob, currentDate, dr))

currentEnrolls_ByGrade <- list(origFileStateIDAdded[which(origFileStateIDAdded$`Sampled Grade/Age` %in% sampledGradesGroup), ], currentEnrolls) %>% 
  reduce(inner_join, by=c("stateSchoolID" = "stateSchoolID")) %>%
  filter(grade %in% sampledGradesGroup2Dig) %>%
  mutate(grade = as.integer(grade))


distanceEds_Cnt <- distanceEds %>%
  #filter(suppressWarnings(!is.na(as.numeric(grade))))
  filter(.$grade %in% sampledGradesGroup2Dig) %>% 
  group_by(stateSchoolID, grade) %>% 
  summarise(`PSI Follow-up Question 2: Full-time remote/virtual` = n()) ##%>% tidyr::pivot_wider(id_cols = "stateSchoolID", values_from= "cnt", names_from="grade") %>% 

enrolled_Cnt <- rbind(
  list(origFileStateIDAdded[which(origFileStateIDAdded$`Sampled Grade/Age` %in% sampledGradesGroup), ], currentEnrolls_ByGrade) %>%  
    reduce(inner_join, by=c("stateSchoolID" = "stateSchoolID", "Sampled Grade/Age" = "grade")) %>% select(-c("currentAge")),
  list(origFileStateIDAdded[which(!origFileStateIDAdded$`Sampled Grade/Age` %in% sampledGradesGroup), ], currentEnrolls_ByAge) %>%  
    reduce(inner_join, by=c("stateSchoolID" = "stateSchoolID", "Sampled Grade/Age" = "currentAge")) %>% select(-c("grade"))
) %>% group_by(stateSchoolID, `Sampled Grade/Age`) %>% 
  summarise(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age` = n())

list( select(origFileStateIDAdded,c(1:7, 12, 13)), enrolled_Cnt ) %>%
  reduce(inner_join, by=c("stateSchoolID" = "stateSchoolID", "Sampled Grade/Age" = "Sampled Grade/Age")) %>% 
  mutate(twoDigsSampledGrade = paste("0", `Sampled Grade/Age`, sep="")) %>% 
list(., distanceEds_Cnt) %>% 
  reduce(left_join, by=c("stateSchoolID" = "stateSchoolID", "twoDigsSampledGrade" = "grade"), keep=TRUE) %>%
  mutate(`PSI Follow-up Question 3: Full-time in person` = `PSI Follow-up Question 1: Total enrollment for the sampled grade/age` - `PSI Follow-up Question 2: Full-time remote/virtual`, 
         `PSI Follow-up Question 4: Part-time in person` = NA) %>% 
  select(colnames(origFiles)) %>% 
  writexl::write_xlsx(path="C:\\Users\\OKADAK\\Documents\\Tasks\\2021_11_09\\TUDA_FollowUp\\result\\TUDA_PSI_Follow-Up_Submission.xlsx")
#####################################################




intersect(currentEnrolls_Cnt$stateSchoolID %>% unique(), origFileStateIDAdded$stateSchoolID %>% unique()) %>% length() # 118
intersect(currentEnrolls_Cnt$stateSchoolID %>% unique(), distanceEds_Cnt$stateSchoolID %>% unique())%>% length() # 10
intersect(origFileStateIDAdded$stateSchoolID %>% unique(), distanceEds_Cnt$stateSchoolID %>% unique())%>% length() # 6


currentEnrolls_Cnt$stateSchoolID %>% unique() %>% length() #380
origFileStateIDAdded$stateSchoolID %>% unique() %>% length() #118
distanceEds_Cnt$stateSchoolID %>% unique() %>% length() #10

setdiff(origFileStateIDAdded$stateSchoolID %>% unique(), distanceEds_Cnt$stateSchoolID %>% unique())%>% length() # 112
distanceEds %>% filter(stateSchoolID %in% (setdiff(distanceEds_Cnt$stateSchoolID %>% unique(), origFileStateIDAdded$stateSchoolID %>% unique()))) %>% select(stateSchoolID, name) %>% unique()


origFiles %>% summary()
map(origFiles, ~ unique(.) %>% summary()) 
uniqueCountsOrigFile <- map_dbl(origFiles, function(.x) { unique(.x) %>% length() })
uniqueListsOrigFile <- map(origFiles, function(.x) { unique(.x) })
uniqueCountsOrigFile["NAEP ID"]
uniqueListsOrigFile[c("NAEP ID", "School Name") ] %>% map(function(`School Name`) { strsplit(., "") })
uniqueListsOrigFile[c("School Name") ] %>% map(function(.x) { grepl("Ber", .x) })

uniqueCountsDE <- distanceEds %>% select(stateSchoolID, ccsdID, schoolID, grade, serviceType, name, CalendarType, groupName, virtualToday) %>% 
  map_dbl(function(.x) { unique(.x) %>% length() })



origFileStateIDAdded[which(!origFileStateIDAdded$`Sampled Grade/Age` %in% c(4, 8)), c("stateSchoolID", "Sampled Grade/Age")]

origFileStateIDAdded$stateSchoolID %>% which(!origFileStateIDAdded$`Sampled Grade/Age` %in% c(4, 8)) %>% 
origFileStateIDAdded$`Sampled Grade/Age`


unique(origFiles$`State School ID`)
unique(distanceEds_Cnt$stateSchoolID)


origFileStateIDAdded
currentEnrolls_Cnt
distanceEds_Cnt

currentEnrolls_Cnt
origFileStateIDAdded %>% 
origFileStateIDAdded[, c(8, 9, 10, 11)]
origFileStateIDAdded[, -c(8, 9, 10, 11)] %>% 
  mutate(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age` = )


create_df <- function(x) {
  df <- data.frame(A = letters[1:5],
                   B = runif(5),
                   C = x)
  names(df)[-1] <- sample(LETTERS[-1], 2) #To ensure different column names after "A"
  return(df)
}
df_list <- list()
for (i in 1:3) {
  df_list[[i]] <- create_df(i) #Yes, you could also use lapply(1:3, create_df), but I went for maximum ugliness
}


df_list[[1]] <- origFileStateIDAdded
df_list[[2]] <- currentEnrolls_Cnt
df_list[[3]] <- distanceEds_Cnt 

dj1 <- list(origFileStateIDAdded, currentEnrolls_Cnt) %>% 
  reduce(left_join, by="stateSchoolID") 

sampledGrades <- dj1[, 7] %>% unique() #4, 8, 9
sampledGrades
sampledGradesColName <- dj1 %>% colnames() %>% pluck(7)
sampledGradesColName
dj1 %>% colnames()
dj1 %>% filter(dj1[, sampledGradesColName] == 4) %>% nrow() #61
dj11 <- dj1 %>% filter(dj1[, sampledGradesColName] == 4) %>% 
  mutate(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age` = `04`) %>% 
  select(stateSchoolID, `04`, `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`) #%>% filter(`04` == `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`)
dj11 %>% nrow()  
dj12 <- dj1 %>% filter(dj1[, sampledGradesColName] == 8) %>% 
  mutate(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age` = `08`) %>% 
  select(stateSchoolID, `08`, `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`) #%>% filter(`08` == `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`)
dj12 
dj13 <- dj1 %>% filter(dj1[, sampledGradesColName] == 9) %>% 
  mutate(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age` = `09`) %>% 
  select(stateSchoolID, `09`, `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`) #%>% filter(`09` == `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`)
dj13

  #column H
rbind(dj11[, c(1, 3)], dj12[, c(1, 3)], dj13[, c(1, 3)])


dj2 <- list(origFileStateIDAdded, distanceEds_Cnt) %>% 
  reduce(left_join, by="stateSchoolID") 

dj21 <- dj2 %>% filter(dj1[, sampledGradesColName] == 4) %>% 
  mutate(`PSI Follow-up Question 2: Full-time remote/virtual` = `04`) %>% 
  select(stateSchoolID, `04`, `PSI Follow-up Question 2: Full-time remote/virtual`) #%>% filter(`04` == `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`)
dj21 %>% nrow()  
dj22 <- dj2 %>% filter(dj1[, sampledGradesColName] == 8) %>% 
  mutate(`PSI Follow-up Question 2: Full-time remote/virtual` = `08`) %>% 
  select(stateSchoolID, `08`, `PSI Follow-up Question 2: Full-time remote/virtual`) #%>% filter(`08` == `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`)
dj22 
dj23 <- dj2 %>% filter(dj1[, sampledGradesColName] == 9) %>% 
  mutate(`PSI Follow-up Question 2: Full-time remote/virtual` = `09`) %>% 
  select(stateSchoolID, `09`, `PSI Follow-up Question 2: Full-time remote/virtual`) #%>% filter(`09` == `PSI Follow-up Question 1: Total enrollment for the sampled grade/age`)
dj23


head(dj21 )
head(dj22)
head(dj23)

d1 <- rbind(dj11[, c(1, 3)], dj12[, c(1, 3)], dj13[, c(1, 3)]) 
d2 <- rbind(dj21[, c(1, 3)], dj22[, c(1, 3)], dj23[, c(1, 3)]) 

d1 %>% filter(is.na(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age`))
d1 %>% filter(!is.na(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age`))
d2 %>% filter(!is.na(`PSI Follow-up Question 2: Full-time remote/virtual`))




dj12 # 53
dj11 %>% View() #61 
dj13 %>% View()


currentEnrolls %>% filter(stateSchoolID == "213") %>% select("grade") %>% unique()
currentEnrolls %>% filter(stateSchoolID == "256") %>% select("grade") %>% unique()
currentEnrolls %>% filter(stateSchoolID == "144") %>% select("grade") %>% unique()





ij <- function(x, y, key_name, sample_key_1) {
  
  reduce(x, inner_join, by=key_name) pull(select_all(x[[1]], y[[x[, sample_key_1] ]]))
  
    
}
list(origFileStateIDAdded, currentEnrolls_Cnt) %>% reduce(inner_join, by="stateSchoolID") 

d4 <- reduce(
  list(origFileStateIDAdded %>% select(c(1,2,3,4,5,6,7,11,12,13)), d1, d2), 
  inner_join,
  by="stateSchoolID", 
  keep=FALSE
) 
origFileStateIDAdded
View(d4)
d4 %>% nrow()

d5 <- d4 %>% mutate(`PSI Follow-up Question 3: Full-time in person` = na_if(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age`, 0) - na_if(`PSI Follow-up Question 2: Full-time remote/virtual`, 0))
d5 %>% nrow()
d5 %>% filter(!is.na(`PSI Follow-up Question 3: Full-time in person`))

d6 <- d5 %>% relocate(c(9,10), .after=13) %>% select(-13)
d6[!is.na(8), 8] %>%
d6%>% colnames()

d6 %>% filter(is.na(`PSI Follow-up Question 2: Full-time remote/virtual`))
d6 %>% filter(is.na(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age`))
d6 <- d6 %>% mutate(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age` = na_if(`PSI Follow-up Question 1: Total enrollment for the sampled grade/age`, 0), 
                    `PSI Follow-up Question 2: Full-time remote/virtual` = na_if(`PSI Follow-up Question 2: Full-time remote/virtual`, 0), 
                    `PSI Follow-up Question 3: Full-time in person` = na_if(`PSI Follow-up Question 3: Full-time in person`, 0)
                    )

d7 <- d6 %>% relocate(8, .after=11) 
d7 %>% class()

writexl::write_xlsx(d7, path="C:\\Users\\OKADAK\\Documents\\Tasks\\2021_11_09\\TUDA_FollowUp\\result\\TUDA_PSI_Follow-Up_Submission.xlsx")


