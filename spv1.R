library(sparklyr)
library(dplyr)
library(dbplot) 
library(ggplot2) 
library(sparklyr.nested)
library(FactoMineR)

sc <- spark_connect(master="local")
spark_available_versions()

