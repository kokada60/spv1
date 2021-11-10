boots <- rsample::bootstraps(mtcars, times=2000, apparent=TRUE) 

library(tidyverse) 
library(repurrrsive)
install.packages("repurrrsive")

mtcars %>% mutate(df = mtcars %>% split(cyl) )
list(mtcars, mtcars %>% nest_by(key=cyl)) %>%
  reduce(inner_join, by=c("cyl" = "key"))

library(stringr)
install.packages("tidygraph", "ggraph")
library(tidygraph)
library(ggraph)


repurrrsive::got_chars

m <- mtcars %>% as_tibble()
m[, 2]

pickCol <- function(x, ref, cList) {
  ifelse(x$ref==6, eval(cList))
}


dat <- repurrrsive::got_chars 
dat_m <- dat %>% {
  tibble(
    name = map_chr(., "name"),
    gender = map_chr(., "gender"),
    culture = map_chr(., "culture"),
    aliases = map(., "aliases"),
    allegiances = map(., "allegiances")
  )}


d_or_a <- function(x) {
  ifelse(x$alive, 
         paste(x$name, "is alive!!"), 
         paste(x$name, "has been long dead..."))
}


map_chr(dat, d_or_a)


#Find character with empty "alias" list...
map(dat, "aliases") %>% map_dbl( ~ length(.)) %>% { . == 0 } %>% which()

  
  {. -1} %>% 
  {. == 0 } %>% which(.)








