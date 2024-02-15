library(tidyverse)
library(repurrrsive)
a <- sw_people %>% tibble(name= map_chr(sw_people, "name"), 
                     films = map(sw_people, "films"), 
                     height = map_chr(sw_people, "height") %>% readr::parse_number(na="unknown"), 
                     species = map_chr(sw_people, "species", .null=NA_character_)
)

sw_people %>% transpose() %>% as_tibble() %>% rowwise() %>% mutate_all(length) %>% View()

sw_films %>% map("url")
sw_films %>% map("title")

filmLookups <- set_names(sw_films %>% map("title"), sw_films %>% map("url"))
a <- tibble(
  name = map_chr(sw_people, "name"), 
  filmUrls = map(sw_people, "films") ## Original film fields...
) %>% mutate(
  filmTitles = map(filmUrls, ~ filmLookups[.x] %>% unlist() %>% unname())  ## film title from the filmLookups! 
)

a$filmTitles[[1]]