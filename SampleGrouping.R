library(tidyverse)
library(tidymodels)
library(listviewer)
library(broom)

read.csv()
df_Train <- read.csv("C:\\users\\okadak\\downloads\\train.csv", header=TRUE, sep="," )
df_Train %>% summary()
df_Train %>% glimpse()

df_Train$Heating %>% unique()
df_TrainSplit <- split(df_Train, df_Train$Heating)
df_TrainSplit2 <- df_Train %>% split(.$Heating) %>% enframe()

a <- df_TrainSplit %>% enframe()
a %>% mutate(n = map_lgl(value, is.data.frame))
b <- a %>% mutate(n = map_dbl(value, ~ unlist(count(.))))
b <- a[2, ] %>% mutate(n = map_dbl(value, ~ count(.) %>% unlist()))

b <- a  %>% mutate(ByMoSold = map(value, ~ tibble(cn = colnames(.), )))
b$ByMoSold[[1]]
b2 <- a %>% mutate(ByMoSold = map(value, ~ cntByMoSold(.)))

b3 <- a %>% mutate(ByMoSold = map(value, ~ cntByMoSold(.x, .y), "MoSold"))


b3 <- b2 %>% select(name, ByMoSold)  %>% unnest(cols=c(ByMoSold)) %>% pivot_wider(id_cols="name", names_from=MoSold, values_from=n)
b3



b
cntByMoSold <- function(x, c) {
  x %>% group_by(across(all_of(c))) %>% count()
}



cntByMoSold(b$value[[1]], "MoSold")




b %>% map(value, )



b$value[2] %>% map(., "MoSold")

map(b$value, "MoSold")
map_df(b$value, "MoSold")

b$value[[2]]->c
map(c, "MoSold")
c$MoSold
map_dbl(c, "MoSold")






library(repurrrsive)
library(httr)
library(here) 

got_chars
pov <- set_names(map_int(got_chars, "id"), 
          map_chr(got_chars, "name")
)
ice <- enframe(pov, value="id")
ice_and_fire_url <- "https://anapioficeandfire.com/"
ice <- ice %>%
  mutate(
    response = map(id,
                   ~ GET(ice_and_fire_url,
                         path = c("api", "characters", .x))),
    stuff = map(response, ~ content(.x, as = "parsed",
                                    simplifyVector = TRUE))
  ) %>% 
  select(-id, -response)

iceBooks <- 

ice2 <- tibble(name = map_chr(got_chars, "name"), stuff = got_chars) 


