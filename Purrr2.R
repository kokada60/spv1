boots <- rsample::bootstraps(mtcars, times=2000, apparent=TRUE) 

library(tidyverse) 
library(repurrrsive)
install.packages("repurrrsive")

mtcars %>% mutate(df = mtcars %>% split(cyl) )
list(mtcars, mtcars %>% nest_by(key=cyl)) %>%
  reduce(inner_join, by=c("cyl" = "key"))

library(stringr)
install.packages(c("tidygraph", "ggraph"))
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
dat[map(dat, "name") %>% {. == "Jon Snow"} %>% which()]


dat[[18]]$aliases

g <- dat
g <- map(dat, compact)

also_known_as <- function(x) {
  if("aliases" %in% names(x)) {
    g <- tibble(
      from = x$name,
      to = x$aliases
    )
    g <- as_tbl_graph(g) 
  }
}

g <- purrr::map(g, also_known_as) 
g

ggraph(g[[23]], layout="graphopt") + 
  geom_edge_link() + 
  geom_node_label(aes(label= name),
                  label.padding=unit(1, "lines"), 
                  label.size=0) +
  theme_graph()

ggraph(g[[23]], layout = "graphopt") + 
  geom_edge_link() + 
  geom_node_label(aes(label = name), 
                  label.padding = unit(1, "lines"), 
                  label.size = 0) +
  theme_graph()


intersect(dat_m$allegiances[16] %>% unlist(), c("House Stark of Winterfell")) %>% length()> 0

dat_m$allegiances[16:18]
dat_m$allegiances[16]
dat_m$allegiances[17]
dat_m$allegiances[18]

dat_p <- dat_m %>% mutate(stark_or_lanister = map(allegiances, ~ str_extract(.x, "Lannister|Stark") %>% 
                                                    discard(is.na))) # str_extract "extracts" the specified segments from the string. 
dat_p %>% unnest(stark_or_lanister) # revealing the filtered allegiance list. Possible that a house could have allegiance to both Stark and Lanister.
dat_p %>% mutate(Cnt_Of_houses_Allged_to = map_int(stark_or_lanister, ~ length(.))) %>% 
  filter(Cnt_Of_houses_Allged_to > 0)

dat_p %>% mutate(ssa = map(allegiances, paste, collapse="#")  %>% unlist())







dat_p$stark_or_lanister[[2]] %>% length()













