library(tidymodels) 
library(dplyr) 
library(purrr) 
library(listviewer) 


data("attrition", package="modeldata")
attrition

table(attrition$Attrition)
# Assessing the dataset by fitting a logistic regression model with job-satisfaction, gender and monthly income 
# stacked up against a attrition variable. 

mod_form <- as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)
set.seed(4622) 
rs_obj <- vfold_cv(data=attrition, v=10, repeats=10) 
rs_obj$splits[[1]] %>% analysis() 
# In order to get the indices of records, better to use %in% to intersect the two sets of rownames.
which(row.names(attrition) %in% row.names(analysis(rs_obj$splits[[1]])))

head(attrition)
head(analysis(rs_obj$splits[[1]]))

row.names(analysis(rs_obj$splits[[1]]))
row.names(attrition)

# helper function to fit a model to the analysis splits ( the 90% of the bootstraps resamples ) 
holdout_results <- function(splits, frm) {
  mod <- glm(formula=frm, data = analysis(splits), family = binomial) 
  # running "prediction" on assessment splits ( 10% remainder ) 
  holdouts <- assessment(splits)
  res <- broom::augment(mod, newdata=holdouts) 
  lvls <- levels(holdouts$Attrition)
  predictions <- factor(if_else( (res$.fitted > 0), true= lvls[2], false=lvls[1], missing=NULL), levels=lvls)
  res$correct <- predictions == holdouts$Attrition
  res$predictions <- predictions
  res
}
mod_form # formula for the model. 

#attrition$Attrition  No - Yes

library(data.table)
# A test-run for one split... 
example <-   holdout_results(rs_obj$splits[[1]], mod_form)
class(example)
names(example) 
example[, setdiff(colnames(example), colnames(attrition))]
example[1:10, setdiff(names(example), names(attrition))]
example %>% data.table() %>% .[, mean(correct)] 


# Now fit the model for all splits... 
rs_obj <- rs_obj %>% mutate(results = map(rs_obj$splits, holdout_results, mod_form))

# Now the goal is to determine how accurate the fitted model, how it stacks up vs the true values...
# Statistics of each split could be aggregated to generate a single measure...
rs_obj$accuracy <- map_dbl(rs_obj$results, ~ mean(.$correct))
rs_obj$accuracy %>% summary() 





# Attrition ~ JobSatisfaction + Gender + MonthlyIncome formula did not really produce a good prediction. 


rsObj <- rsample::vfold_cv(data=attrition, v=10, repeats=10) 


# splits are created. Now build the formula 
names(attrition) 
frml <- formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome + Age)
mdl <- glm(formula=frml, data=analysis(rsObj$splits[[1]]), family="binomial")

# Assess the model with a sample split. 
holdouts <- rsObj$splits[[1]] %>% assessment() 

rs <- broom::augment(mdl, newdata=holdouts) 
lvls <- levels(holdouts$Attrition)
( if_else( (rs$.fitted > 0 ), true=lvls[2], false=lvls[1], missing=NULL) == rs$Attrition) %>% mean()
( if_else( (example$.fitted > 0), true=lvls[2], false=lvls[1], missing=NULL)==example$Attrition) %>% mean()

# Will build model on the fly based on passed split.
predict_On_holdouts <- function(splits, frml) {
  mdl <- glm(formula=frml, data=analysis(splits), family=binomial) 
  holdouts <- splits %>% assessment()
  lvls <- levels(holdouts$Attrition)
  tmpRS <- broom::augment(mdl, newdata=holdouts) %>% as.data.frame()
  predicted <- factor(if_else(tmpRS$.fitted > 0, lvls[2], lvls[1], NULL), levels = lvls)  #tmpRS$predictions <- predicted
  #tmpRS$correct <- (predicted == holdouts$Attrition)
  tmpRS <- tmpRS %>% mutate(predictions = factor(if_else(tmpRS$.fitted > 0, lvls[2], lvls[1], NULL), levels = lvls))
  tmpRS <- tmpRS %>% mutate(correct = (.$predictions == holdouts$Attrition))
  #correctPrediction <- (tmpRS$predictions == holdouts$Attrition)
  #tmpRS
  tmpRS
}

rsObj <- rsObj %>% mutate(results = map(rsObj$splits, predict_On_holdouts, frml))

rsObj$results[[1]]$correct %>% mean()

rsObj$results %>% map_dbl( ~ mean(.x$correct)) %>% mean() 














