library(tidymodels)
library(listviewer)
library(listviewer)
library(modeldata)
data("crickets")
crickets

crickets$species %>% unique()

calc_model_PValue <- function(model) { 
  fst <- summary(model)
  pf(fst$fstatistic[1], fst$fstatistic[2], fst$fstatistic[3], lower.tail=FALSE) %>% unlist()
}


split_by_species <- split(crickets, f="species")
split_by_species <- crickets %>% group_nest(species)
model_by_species <- split_by_species %>% 
  mutate(model = map(data, ~ lm(rate ~ temp, data=.))) %>% 
  mutate(coef = map(model, tidy)) %>% 
  mutate(fstats = unlist(map(model, calc_model_PValue)))
RMSE_model <- function(y, f) {
  `^`(y - f, 2) %>% mean() %>% `^`(0.5)
}

### Housing Data 
data(ames) 
dim(ames)

tidymodels_prefer()
ggplot(ames, aes(x=Sale_Price)) + geom_histogram(bins=50)
summary(ames$)
summary(ames$Sale_Price) -> aaa
names(aaa[grepl("Qu.", names(aaa)) | names(aaa) =="Median"])
quantile(ames$Sale_Price)

ggplot(ames, aes(x=Sale_Price)) + geom_histogram(bins=50) + scale_x_log10()
ggplot(ames, aes(x=log(Sale_Price, base=5))) + geom_histogram(bins=30)
       

ames <- ames %>% mutate(Sale_Price_Log10 = log10(Sale_Price)) 
glimpse(ames)

plot_usmap(data=ames, values=)
mapdata <- map_data("city")
ggplot(data=ames, aes(x=Longitude, y=Latitude)) + 
  geom_point(color="red") + 
  guides(fill=FALSE) 


set.seed(123)
ames_split <- initial_split(ames, prop=0.80)
ames_split
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

ames_split_strata <- initial_split(ames, prop=0.80, strata=Sale_Price) 
ames_train <- training(ames_split)
ames_test <- testing(ames_split)
ames_train$Sale_Price %>% summary()
ames_test$Sale_Price %>% summary()




