library(tidyverse)

load("schoolsList.RData")


SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("adhc_EnrollsQry_dfs"), ~map(.x, ~ filter(.x, crdcGrade == "Ungraded")) , .names="{.col}_ug")
  ) 




SCHR_22 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("adhc_EnrollsQry_dfs"), ~map(.x, ~ filter(., crdcGrade == "Ungraded")) , .names="{.col}_ug")
  ) %>% 
  mutate(
    across(contains("adhc_EnrollsQry_dfs_ug"), ~map(.x, ~ nrow(.)), .names = "{.col}_ugCount")
  )


SCHR_2C <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
      ~ { filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) })
    )
  )
SCHR_2C$adhc_EnrollsQry_dfs %>% 
  map_lgl(~ nrow(.x) > 0) %>% which(.)

# Filtering for UG students... 
SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
      ~ { 
        filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) %>% 
        mutate(ageGrp = cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19)), 
               totalCnt = count(.)) %>% 
        group_by(totalCnt, ageGrp) %>% count(name="ageGrpCount") %>% 
        ungroup() %>% 
        mutate(ageGrpPerc = as.double(ageGrpCount) / as.double(totalCnt))
      }), .names="{.col}_ug") 
  ) %>% 
  mutate(
    map_lgl
  )
aa <- SCHR_2$adhc_EnrollsQry_dfs_ug %>% 
  map_lgl(~ nrow(.) > 0) %>% which(.)
SCHR_2$adhc_EnrollsQry_dfs_ug[aa]

SCHR_2$adhc_EnrollsQry_dfs_ug[[1]]


feat_SwitchValues <- function(x, colsToUpdate, columnValues) {
  x %>% 
    mutate(across(colsToUpdate, ~ columnValues[.]))
}
widerbyColumns <- function(x, nameCols, valueCols) {
  x %>% pivot_wider(names_from=nameCols, values_from=valueCols)
}

ageGroupList <- set_names(
  c("School has mainly elementary school age students?", "School has mainly high school age students?", "School has mainly middle school age students?"),
  nm = c("(2,10]", "(10,13]", "(13,19]")
)
# checkForMax <- function(x) {
#   x %>% mutate(maxValue=if_else(.$perc == max(.$perc, na.rm=TRUE), "Y", "N"))
# }



library(DBI)
library(odbc)
qryHelper <- function(strQuery) {
  con <- DBI::dbConnect(odbc::odbc(), Driver="SQL Server", Server="ORIONTEST.CIS.CCSD.NET", Database="ACCOUNTABILITY", 
                        Trusted_Connection="TRUE", Authentication="ActiveDirectoryInteractive")
  op <- DBI::dbGetQuery(conn=con, strQuery)
  DBI::dbDisconnect(con)
  op 
}

subQs <- qryHelper("SELECT * FROM OPENQUERY(campus, 'SELECT DISTINCT q.crdcQuestionID, REPLACE(q.crdcIdentifier, ''-'', ''_'') AS mod_crdcIdentifier, q.crdcIdentifier, sq.elementName, sq.dataElement1, dataElement2 FROM clark.dbo.CRDCQuestion q INNER JOIN clark.dbo.CRDCSubQuestion sq ON q.crdcQuestionID = sq.crdcQuestionID 
                    WHERE q.endYear = ''2021'' ')")

SCHR_2$adhc_EnrollsQry_dfs[[154]] %>% checkForMax()

crdcID= "SCHR_2"
SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
      ~ { 
          filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) %>% 
          mutate(AgeGrp = cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19)))
      })) 
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", 
           ~ map(.x, ~ .$AgeGrp %>% 
                   table() %>% prop.table() %>% round(1) %>% as_tibble() %>% rename(c("AgeGroup" = 1, "perc" = 2)) %>%
                   feat_SwitchValues(c("AgeGroup"), ageGroupList))
    )
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
      ~ {
          mutate(., majorityGroup = if_else(.$perc == max(.$perc, na.rm = FALSE), "Y", "N", "N")) %>% 
          mapToElementName(yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("AgeGroup" = "dataElement1"), 
                           selectColumns=c("elementName", "majorityGroup")) %>%
          widerbyColumns(nameCols=all_of(c("elementName")), valueCols=all_of(c("majorityGroup")))
       }))
  )


SCHR_2$adhc_EnrollsQry_dfs[[154]]
SCHR_2$adhc_EnrollsQry_dfs[[1]]

x <- c(1:4, 11, 0:5, 11)
which.max(x)

SCHR_2$adhc_EnrollsQry_dfs[[1]]

SCHR_2$adhc_EnrollsQry_dfs[[154]] %>% 
SCHR_2$adhc_EnrollsQry_dfs_ug[[154]] %>% table() %>% prop.table() %>% round(1) %>% as_tibble() %>% rename(c("AgeGroup" = ".", "cnt"="n"))


SCHR_2$adhc_EnrollsQry_dfs_ug[[154]]

SCHR_2$adhc_EnrollsQry_dfs_ug %>% map()                                      

bb <- SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug %>%
  map_lgl(~ nrow(.) > 0) %>% which(.)
bb

cbind(SCHR_2OnlyUG$schoolName[bb], SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug[bb] %>% reduce(rbind) )

  
SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
    ~ { 
      filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) %>% 
        mutate(ageGrp = cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19)), 
               ) %>% 
        group_by(totalCnt, ageGrp) %>% count(name="ageGrpCount") %>% 
        ungroup() %>% 
        mutate(ageGrpPerc = as.double(ageGrpCount) / as.double(totalCnt))
    }), .names="{.col}_ug")
  ) 



  

x <- c(
    c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
    c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),
    c(2,2)
)

c <- prop.table(table(x)) %>% round(2)

c%>% class()%>%as_tibble()
c%>%as_tibble()


schoolTypes <- qryHelper("EXEC crdc_2021.schools_Characteristics '2020-10-01'	")
schoolTypes %>% glimpse()
schoolTypes %>% class()
SCHR_3$adhc_EnrollsQry_dfs[[1]] %>% class()

# mapToElementName <- function(xf, yf, byMatchColumns, selectColumns) {
#   right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% 
#     select(all_of(selectColumns))
# }

SCHR_3 <- schoolsFromCRDCMapping %>%
  mutate(
    across("adhc_EnrollsQry_dfs", 
           ~map(.x, inner_join, y=schoolTypes, by=c("schoolID" = "schID"), na_matches="never") 
    )
  )

inner_join(SCHR_3$adhc_EnrollsQry_dfs[[1]], schoolTypes, by=c("schoolID" = "schID"))
inner_join(SCHR_3$adhc_EnrollsQry_dfs[[6]], schoolTypes, by=c("schoolID" = "schID"))

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

SCHR_2 <- schoolsFromCRDCMapping %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
                                        ~ { 
                                          filter(., all(crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19)) %>% 
                                            mutate(AgeGrp = cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19)))
                                        })) 
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", 
           ~ map(.x, ~ .$AgeGrp %>% 
                   table() %>% prop.table() %>% round(1) %>% as_tibble() %>% rename(c("AgeGroup" = 1, "perc" = 2)) %>%
                   feat_SwitchValues(c("AgeGroup"), ageGroupList))
    )
  ) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, 
                                        ~ {
                                          mutate(., majorityGroup = if_else(.$perc == max(.$perc, na.rm = FALSE), "Y", "N", "N")) %>% 
                                            mapToElementName(yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("AgeGroup" = "dataElement1"), 
                                                             selectColumns=c("elementName", "majorityGroup")) %>%
                                            widerbyColumns(nameCols=all_of(c("elementName")), valueCols=all_of(c("majorityGroup")))
                                        }))
  )
SCHR_2 %>% nrow()
SCHR_3_OnlySpecialSchools %>% nrow()

schoolTypes
SCHR_3_OnlySpecialSchools <- schoolsFromCRDCMapping %>%
  mutate(
    across("adhc_EnrollsQry_dfs", 
           ~map(.x, joinWithExternalData, yf=schoolTypes, byMatchColumns=c("schoolID" = "schID"), 
                selectColumns=all_of(c("schoolID", "schName", "schCategory", "schAnswer")), 
                joinType="L", 
                returnUniqueSet="Y")
    )
  ) %>% pull(adhc_EnrollsQry_dfs) %>% reduce(rbind) %>% widerbyColumns(nameCols=all_of(c("schCategory")), valueCols=all_of(c("schAnswer")))

SCHR_3_OnlySpecialSchools <- schoolsFromCRDCMapping %>%
  

sf1 <- filter(SCHR_3, map_int(SCHR_3$adhc_EnrollsQry_dfs, ~nrow(.)) > 0)
sf1 %>% nrow()
sf1$adhc_EnrollsQry_dfs[[1]]
sf1$schoolName[[1]]
subQs$

sf1$adhc_EnrollsQry_dfs %>% reduce(rbind) 

SCHR_3_OnlySpecialSchools$schoolID %>% unique() %>% length()
a22 <- schoolsFromCRDCMapping %>% filter(map(.$adhc_EnrollsQry_dfs, ~ nrow(.x)) > 0)
a22 %>% nrow() 

schoolsFromCRDCMapping$schoolID %>% unique() %>% length()

  
PSCH_1 <- schoolsFromCRDCMapping %>%
PSCH_1$adhc_EnrollsQry_dfs[[1]]$crdcGrade %>% map(.x, pull, crdcGrde)  


cbind( SCHR_22B$schoolName[aa], SCHR_22B$adhc_EnrollsQry_dfs_ug[aa] %>% reduce(rbind)) %>% View()


# 154 257 266 301 302 303 304 305 306 308 309 310 311 312 313 314 315 316 317 318 321 322 324 325 326 327 328 330 331 332 333 334 335 337 338 339 340 341
# [39] 345 346 347 349 350 356 357 360 362 364 377 379 393
SCHR_22B$adhc_EnrollsQry_dfs_ug[[154]]
SCHR_22B$adhc_EnrollsQry_dfs_ug[[257]]
SCHR_22B$adhc_EnrollsQry_dfs_ug[[266]]
SCHR_22B$adhc_EnrollsQry_dfs_ug[[302]]
SCHR_22B$adhc_EnrollsQry_dfs_ug[[393]]

SCHR_2$adhc_EnrollsQry_dfs_ug[[154]] %>% table() %>% prop.table() %>% round(1) %>% as_tibble() %>% rename(c("AgeGroup" = ".", "cnt"="n"))


SCHR_2$adhc_EnrollsQry_dfs_ug[[154]] %>% table() %>% prop.table() %>% as_tibble() %>% rownames_to_column("AgeGroupName")
SCHR_2$adhc_EnrollsQry_dfs_ug[[257]]
SCHR_2$adhc_EnrollsQry_dfs_ug[[266]]
SCHR_2$adhc_EnrollsQry_dfs_ug[[302]]
SCHR_2$adhc_EnrollsQry_dfs_ug[[393]]

SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug[[154]]
SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug[[257]]
SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug[[266]]
SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug[[302]]
SCHR_2OnlyUG$adhc_EnrollsQry_dfs_ug[[393]]

SCHR_22B %>% filter(., nrow(.$adhc_EnrollsQry_dfs_ug) > 0)


SCHR_22A <- SCHR_22 %>% filter(adhc_EnrollsQry_dfs_ug_ugCount > 0) %>% 
  mutate(
    across("adhc_EnrollsQry_dfs_ug", ~ map(.x, grpByAge))
  )

SCHR_22A$adhc_EnrollsQry_dfs_ug[[1]] %>% View()

SCHR_22A$adhc_EnrollsQry_dfs_ug[[1]] %>%  grpByAge()



grpByAge <- function(x) {
  x %>% 
    mutate(AgeGrp = cut(as.integer(AgeOnCountDay), c(2, 10, 13, 19)))
}

feat_SwitchValuesIfTrue <- function(x, colsToUpdate, predicateFuncs) {
  x %>% 
    mutate(across(names(colsToUpdate), ~ modify_if(.x, ~ map_lgl(.x, , predicateFuncs1), ~ colsToUpdate[cur_column()])))
}
mutate(
  across(contains("_dfs"), ~ map(.x, feat_SwitchValuesIfTrue, colsToUpdate=indicatorColumns1, predicateFuncs=predicateFuncs1))
) %>% 
  
  
SCHR_22 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("adhc_EnrollsQry_dfs"), ~map(.x, ~ filter(., crdcGrade == "Ungraded")) , .names="{.col}_ug")
  ) %>% filter(nrow(.$adhc_EnrollsQry_dfs_ug) > 0)



SCHR_2$adhc_EnrollsQry_dfs[[1]]$AgeOnCountDay
SCHR_2$adhc_EnrollsQry_dfs %>% map_lgl(., ~ any((.$crdcGrade == "Ungraded" & as.integer(.$AgeOnCountDay) <= 19), na.rm=TRUE)) %>% which(.)

SCHR_2$adhc_EnrollsQry_dfs %>% map(., ~ filter(., (crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <=19))) %>% 
  map_lgl(~ nrow(.) > 0) %>% which(.) %>% length()


SCHR_2$adhc_EnrollsQry_dfs %>% is.data.frame()
SCHR_2$adhc_EnrollsQry_dfs[[1]] %>% is.data.frame()

SCHR_2$adhc_EnrollsQry_dfs %>% map(~ filter(., any( (crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <=19), na.rm=TRUE))) %>% 
  map_lgl(~ nrow(.) > 0) %>% length()

SCHR_2$adhc_EnrollsQry_dfs %>% map(~ filter(., any( (crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <=19)), na.rm=TRUE)) %>% 
  map_lgl(~ nrow(.) > 0) %>% which(.) %>% length()



schoolsFromCRDCMapping[225, ] %>% glimpse()
schoolsFromCRDCMapping[393, ] %>% glimpse()

schoolsFromCRDCMapping[393, ]$adhc_EnrollsQry_dfs[[1]] %>% View()

schoolsFromCRDCMapping[393, ]$adhc_EnrollsQry_dfs %>% glimpse()


SCHR_2 <- schoolsFromCRDCMapping[393, ] %>% 
mutate(
  across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(., (crdcGrade == "Ungraded" & as.integer(AgeOnCountDay) <= 19 ))) ) 
) %>% 
mutate(
  across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, 
    ageGroup=cut(as.integer(.$AgeOnCountDay), c(2, 10, 13, 19))
  )))
)

SCHR_2$adhc_EnrollsQry_dfs[[1]] %>% glimpse()





SCHR_2$adhc_EnrollsQry_dfs[[1]]
bb$adhc_EnrollsQry_dfs[[1]]


SCHR_2$adhc_EnrollsQry_dfs[[1]]$crdcGrade

SCHR_2$adhc_EnrollsQry_dfs[[301]]$AgeOnCountDay
cut(SCHR_2$adhc_EnrollsQry_dfs[[393]]$AgeOnCountDay %>% as.integer(), c(2, 10, 13, 19)) -> aa
aa
aa[1]
aa[2]
aa[3] 
aa2 <- SCHR_2$adhc_EnrollsQry_dfs[[393]]$AgeOnCountDay
which(aa== "(13,19]")

aa <- SCHR_2$adhc_EnrollsQry_dfs[[154]]

aa %>% filter(crdcGrade == "Ungraded") %>% mutate(AgeGrp = cut(AgeOnCountDay %>% as.integer(), c(2, 10, 13, 19)))

aa <- SCHR_2$adhc_EnrollsQry_dfs %>% reduce(rbind)                                                   



which(levels(aa) == "(13,19]")
aa %>% filter(crdcGrade == "Ungraded") %>% 

aa[1] 
aa %>% class() 
aa %>% attributes() -> aa2 
class(aa2)
aa2[["levels"]]



SCHR_2$adhc_EnrollsQry_dfs[[393]]$AgeOnCountDay %>% as.integer() 

cut(as.integer(AgeOnCountDay), c(2, 10, 13, 19))

cut(SCHR_2$adhc_EnrollsQry_dfs[[394]]$AgeOnCountDay %>% as.integer(), c(2, 10, 13, 19)) 

SCHR_2$adhc_EnrollsQry_dfs[[393]]$AgeOnCountDay

empties_df <- schoolsFromCRDCMapping %>% filter(map(adhc_EnrollsQry_dfs, nrow) == 0 )
nrow(testEmpty)

schoolsFromCRDCMapping %>% which(map(adhc_EnrollsQry_dfs, nrow) == 0 )
which(schoolsFromCRDCMapping$adhc_EnrollsQry_dfs %>% nrow() == 0)

which(schoolsFromCRDCMapping, nrow(schoolsFromCRDCMapping$adhc_EnrollsQry_dfs) == 0)

emptySchools <- which(map(schoolsFromCRDCMapping$adhc_EnrollsQry_dfs, nrow) == 0)
schoolsFromCRDCMapping %>% map(.$adhc_EnrollsQry_dfs, nrow) 
schoolsFromCRDCMapping %>% map(pull(., "adhc_EnrollsQry_dfs"), nrow)
map_int(aa$adhc_EnrollsQry_dfs, nrow) 
map_int(pull(aa, "adhc_EnrollsQry_dfs"), nrow)

aa %>% map(., ~ nrow(.$adhc_EnrollsQry_dfs))

fdn <- "adhc_EnrollsQry_dfs"

first(c(1,2,3))

aa <- schoolsFromCRDCMapping

fillInNamedEmptyDFs <- function(df, dfcol_Name) {
  df = aa
  dfcol_Name = "adhc_EnrollsQry_dfs"
  idx_Empty <- which(unlist(map(pull(df, dfcol_Name), nrow)) == 0)
  
  schoolIDs <- df[idx_Empty, "schoolID"]
  rm(naVals) <- rep(NA, length(idx_Empty))
  

  rowEmpty <- df[first(idx_Empty), ]
    
    ## Constructing a dummy variable...
  tmpEmpty <- pull(rowEmpty, dfcol_Name)[[1]]
  namedEmpty <- as.list(rep(NA, length(tmpEmpty)))
  names(namedEmpty) <- names(tmpEmpty)
  emptyDF <- as_tibble(namedEmpty)
  emptyDFs <- map_dfr(1:length(idx_Empty), ~ emptyDF) %>% mutate(across(everything(), ~ as.character(.))) %>% 
    mutate(schoolID = schoolIDs) %>% split(.$schoolID, )
  aa$adhc_EnrollsQry_dfs[idx_Empty] <- emptyDFs[as.character(schoolIDs)]
  aa[idx_Empty, c('schoolID', 'adhc_EnrollsQry_dfs')]
  aa[-idx_Empty, ] %>% select(c(schoolID, schoolName, adhc_EnrollsQry_dfs)) %>% head(1) %>% View()
  
}

aa$adhc_EnrollsQry_dfs[idx_Empty][[1]]
aa$schoolID[idx_Empty][[1]]

cc <- fillInNamedEmptyDFs(aa, "adhc_EnrollsQry_dfs")
cc %>% length()

cc %>% glimpse()




class(cc)
cc$
dd <- map(aa$adhc_EnrollsQry_dfs, nrow) %>% unlist() %>% `==`(., 0) %>% which()
ee <- rep("aaa", length(dd)) %>% as.list()
ee
aa[dd, "schoolID"] <- ee
aa[dd, "schoolID"]
aa$schoolID %>% class() 

aa[dd, ] %>% mutate_at("schoolID", ~ ee)

map(aa$adhc_EnrollsQry_dfs, nrow) %>% unlist() 
aa %>% pull("adhc_EnrollsQry_dfs") %>% map_

pull(aa, "adhc_EnrollsQry_dfs") %>% map(., nrow) %>% unlist()

aa[[]]


idx_Empty <- which(pull(aa, "adhc_EnrollsQry_dfs") %>% map_int(., ~ nrow(.x)) == 0)
idx_Empty  





tic("step1")
cc2 <- fillInNamedEmptyDFs(aa, "adhc_EnrollsQry_dfs")

toc()
aa[1, "adhc_EnrollsQry_dfs"]

library(tictoc)
tic("step1")
aa <- schoolsFromCRDCMapping
toc()



aa %>% map_int(.$adhc_EnrollsQry_dfs, nrow)
aa %>% map(., pull, "adhc_EnrollsQry_dfs")
aa$adhc_EnrollsQry_dfs %>% map_int(., nrow) 

head(aa, 3) %>% map_df(as_tibble(pull(., "adhc_EnrollsQry_dfs")), ~ .)







tic("step1")
aa%>% fillInNamedEmptyDFs("adhc_EnrollsQry_dfs")
toc()

aa %>% filter(map(pull(., "adhc_EnrollsQry_dfs"), nrow) == 0)
aa$adhc_EnrollsQry_dfs[[1]] %>% class() 


schoolsFromCRDCMapping$schoolID
schoolsFromCRDCMapping$adhc_EnrollsQry_dfs[[1]]$stateSchoolID

l2 <- schoolsFromCRDCMapping %>% filter(map(pull(., fdn), nrow) == 0)

emptyDF <- testEmpty$adhc_EnrollsQry_dfs[[1]]
l1 <- as.list(rep(NA, 14)) 
names(l1) <- names(emptyDF)
t1 <- as_tibble(l1) 
rbind(emptyDF, as_tibble(l1))

pull(testEmpty, adhc_EnrollsQry_dfs)[[1]]

rowEmpty <- df %>% filter(map(pull(., colName), nrow) == 0) %>% first()

testEmpty %>% nrow()


testEmpty %>% nrow()
testEmpty[c(1,2), ]




groupByColumns1 <- c("gender", "raceEthnicity")
groupByColumns2 <- c("gender", "inIDEA") #
groupByColumns3 <- c("gender", "inEL")
groupByColumns4 <- c("gender", "in504")
groupByColumns5 <- c("gender", "inELProg")
groupByColumns6 <- c("crdcGrade")




groupingByColumns <- function(c, df0, renameColumns) {
  df1 <- df0 %>% group_by(across(all_of(c))) %>% count(name="grpCount") 
  if (renameColumns) {
    df1 <- rename_with(df1, ~ head(c("dataElement1", "dataElement2"), length(c)), .cols=all_of(c))
  }
  ####rename_with(df1, ~ c("dataElement1", "dataElement2"), .cols=group_cols(df1)) %>%
  
  df1 %>% ungroup() %>% filter(if_all(head(c("dataElement1", "dataElement2"), length(c)), ~!is.na(.)))
}
groupingByListOfcolumns <- function(x, lc) {
  map(lc, groupingByColumns, df0=x, renameColumns = TRUE) %>% reduce(rbind) 
}

map(list(groupByColumns6), groupingByColumns, df0=SCHR_1$adhc_EnrollsQry_dfs[[1]], renameColumns=TRUE) 
groupingByColumns(groupByColumns6, SCHR_1$adhc_EnrollsQry_dfs[[1]], TRUE)

mapToElementName <- function(xf, yf, byMatchColumns, selectColumns) {
  right_join(xf, yf, by=byMatchColumns, na_matches="never", keep=FALSE) %>% 
    select(all_of(selectColumns))
}
 <- function(x, cmbPredicates) {
  sapply(cmbPredicates, mapply, x) %>% reduce(`&`)
}
isEqualToOne <- function(x) {
  r1 <- ( as.character(x)=="1" & is.numeric(x) )
  if(is.na(r1))
    FALSE
  else
    r1
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

feat_SwitchValuesIfTrue <- function(x, colsToUpdate, predicateFuncs) {
  x %>% 
    mutate(across(names(colsToUpdate), ~ modify_if(.x, ~ map_lgl(.x, , predicateFuncs), ~ colsToUpdate[cur_column()])))
}
feat_SwitchValues <- function(x, colsToUpdate, columnValues) {
  x %>% 
    mutate(across(colsToUpdate, ~ columnValues[.]))
}
feat_SwitchValuesIfTrueAtomic <- function(x, colsToUpdate, predicateFuncs, trueValue, falseValue) {
  x %>% 
    mutate(across(colsToUpdate, ~ map_lgl(.x, , predicateFuncs))) %>% 
    mutate(gradeExists = across(colsToUpdate, ~ if_else(.x, trueValue, falseValue)))
}

feat_SwitchValuesIfTrueAtomic <- function(x, colsToUpdate, predicateFuncs, trueValue, falseValue) {
  x %>% 
    mutate(across(colsToUpdate, ~ if_else(map_lgl(.x, , predicateFuncs), trueValue, falseValue))) 
}



### GOOD
feat_SwitchValuesIfTrueAtomic <- function(x, colsToUpdate, predicateFuncs, newValue) {
  x %>% 
    mutate(across(colsToUpdate, ~ map_lgl(.x, , predicateFuncs)))
}

SCHR_1$adhc_EnrollsQry_dfs_grouped[[1]] %>% 
  #mutate(grpCount = as.character(grpCount)) %>% 
  feat_SwitchValuesIfTrueAtomic("grpCount", list(isEqualOrMoreOne), "YES", "NO")

# mutate(
#   across(contains("_dfs"), ~ map(.x, feat_SwitchValuesIfTrue, colsToUpdate=indicatorColumns1, predicateFuncs=predicateFuncs1))
# ) %>% 

crdcID = "SCHR_1" 
SCHR_1 <- schoolsFromCRDCMapping %>% head(3) %>%
  mutate(
    across(contains("_dfs"), ~map(.x, groupingByListOfcolumns, lc=groupByColumns6), .names="{.col}_grouped")
  ) %>% 
  mutate(
    across(contains("_grouped"), ~map(.x, mapToElementName, yf=filter(subQs, mod_crdcIdentifier==crdcID), byMatchColumns=c("dataElement1" = "dataElement1"), selectColumns=c("elementName", "grpCount")))
  ) %>% 
  
  mutate(
    across(contains("_grouped"), ~ map(.x, feat_SwitchValuesIfTrueAtomic))
  )
  
SCHR_1$adhc_EnrollsQry_dfs_grouped[[1]] %>% feat_SwitchValuesIfTrueAtomic("grpCount", isEqualOrMoreOne, "YES")

SCHR_1$adhc_EnrollsQry_dfs_grouped[[1]]
  aa <- SCHR_1$adhc_EnrollsQry_dfs_grouped[[1]]

(SCHR_1$adhc_EnrollsQry_dfs_grouped[[1]]$grpCount[[1]], isEqualOrMoreOne)

aa %>% mutate(
  grpCount22 = modify_if(.$grpCount, (grpCount, isEqualOrMoreOne), "YES", "NO")
)






crdcID= "PENR-1"
PENR-1 <- schoolsFromCRDCMapping %>% 
  mutate(
    across(contains("_dfs"), ~ map(.x, ~ mutate(.x, schoolID = as.character(schoolID))))
  ) %>% 
  ## reformat student birthdate
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ mutate(.x, birthDate = lubridate::mdy_hm(birthDate) )))
  ) %>%
  mutate(
    across("adhc_EnrollsQry_dfs", ~ map(.x, ~ filter(.x, inGate == "1")))
  ) 



rep(NA, length(bb)) %>% as.matrix() %>% t() -> b 
colnames(b) <- names(aa)
b <- b %>% as_tibble()
map_dfr(1:3, ~ b)

b %>% as.matrix() %>% as_tibble(rownames=NA)

b %>% as.matrix() %>% tibble::rownames_to_column()






tic()
initializeCRDCEnrollsTable <- 1
initializeADHCEnrollsTable <- 1
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_CRDC_OverallEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeCRDCEnrollsTable))
qryHelper(glue::glue("EXEC [crdc_2021].Initialize_AdHoc_OverallEnrollmentTable @Rebuild_Table = {initialize_ID}", initialize_ID=initializeADHCEnrollsTable))
schoolsFromCRDCMapping <- 
  generateSchoolsList('2021', '2021') %>% #head(1) %>%
  mutate(
    crdc_EnrollsQry = map_chr(schoolID, ~ glue::glue("EXEC crdc_2021.generate_crdcOverallEnrollment @schoolID='{.}', @endYear='{year_id}', @CountDay='{countDate_ID}'", year_id="2021", countDate_ID="2020-10-01")), 
    adhc_EnrollsQry = 
      map_chr(schoolID, 
              ~ glue::glue("EXEC crdc_2021.generate_AdhocOverallEnrollment @schoolID='{.}', @endYear='{year_id}', @CountDay='{countDate_ID}', @EOYDay='{EOYDate_ID}'", 
                           year_id="2021", countDate_ID=as.character(dt_CountDay), EOYDate_ID=as.character(dt_EOY)))
  ) %>% 
  mutate(
    across(contains("_EnrollsQry"), ~map(.x, qryHelper), .names="{.col}_dfs")
  ) 
toc()

