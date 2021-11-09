library(tidymodels) 
data("Orange") 
Orange <- as_tibble(Orange) 
Orange %>% typeof()

Orange %>% is_tibble()
map(Orange %>% transpose(), "Tree") %>% unlist()
Orange$Tree
head(Orange) 
Orange %>% map( function(.x, .y, .z) { .x})
Orange %>% rowwise() %>% map( function(.x, .y, .z) { .x})
Orange[1]
Orange[2]

ggplot(Orange, aes(x=age, y=circumference, color=Tree)) + geom_line()

cor(Orange$age, Orange$circumference) 
Orange %>% group_by(Tree) %>% summarize(Correlation_Btw_Age_Circumference=cor(age, circumference))
group_by()

Orange %>% group_by(Tree) %>% summarise(Cor_BTW_AgeVCirc = cor(age, circumference))
Orange %>% group_by(Tree) %>% summarise(mean(age), mean(circumference))
corTest <- cor.test(Orange$age, Orange$circumference) %>% tidy() 
corTest$conf.high
count(Orange)

Orange %>% nest(data=c(age, circumference)) %>%
  mutate(test = map(data, ~ cor.test(.x$age, .x$circumference)), 
         tidied = map(test, tidy)) %>% unnest(cols = tidied) %>%
  select(- data, -test)

# Now try modeling a linear regression based on Tree-split data. 
Orange %>% nest(data=c(-Tree)) %>%
  mutate(fitted = map(data, ~ lm(.x$age ~ .x$circumference, data=.x)), 
         tidied = map(fitted, tidy)) %>% 
  unnest(cols = tidied) %>% 
  select(-c(data, fitted)) %>% 
  tidyr::pivot_wider(id_cols=Tree, names_from=term, names_glue= "{term}_{.value}", values_from=c(estimate, std.error, statistic, p.value)) 


ggplot(mtcars, aes(x=wt)) + geom_bar(stats="identity")
ggplot(mtcars, aes(x=wt)) + geom_boxplot() + coord_flip()

data(mtcars)
mtcars <- as_tibble(mtcars)
mtcars %>% nest(data = c(-am)) %>% 
  mutate(fitted = map(data, ~ lm(wt ~ mpg + qsec + gear, data=.x)), 
         tidied = map(fitted, tidy)) %>% 
  unnest(tidied) %>% pivot_wider(id_cols=am, names_from=term, values_from=c(estimate, std.error, statistic, p.value), names_glue="{term}_{.value}")

# Preserving the fitted model, retrieve glanced data, augments of the fitted models ... 

mtcars %>% as_tibble() %>% 
  nest(data=c(-am)) %>% 
  mutate(fitted = map(data, ~ lm(wt ~ mpg + qsec + gear, data=.x)), 
         tidied = map(fitted, tidy), 
         glanced = map(fitted, glance), 
         augmented = map(fitted, augment) 
  ) %>% unnest(glanced) 


mtcars %>% as_tibble() %>% 
  nest(data=c(-am)) %>% 
  mutate(fitted = map(data, ~ lm(wt ~ mpg + qsec + gear, data=.x)), 
         tidied = map(fitted, tidy), 
         glanced = map(fitted, glance), 
         augmented = map(fitted, augment) 
  ) %>% unnest(augmented) 













