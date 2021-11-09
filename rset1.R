
library(tidymodels) 
#install.packages(c("nlstools", "GGally"))
require("nlstools")
library(nlstools) 
library(GGally) 
library(ggplot2)
library(listviewer)

data(O2K)
O2K
ggplot2::ggplot(O2K, aes(t, VO2)) + geom_point() 
nonlin_form <- 
  as.formula(
    VO2 ~ ( t <= 5.883 ) * VO2rest + 
      ( t > 5.883 ) * 
      (VO2rest + ( VO2peak - VO2rest ) * ( 1 - exp(-(t-5.883) / mu)))
  )

nonlin_form <-  
  as.formula(
    VO2 ~ (t <= 5.883) * VO2rest + 
      (t > 5.883) * 
      (VO2rest + (VO2peak - VO2rest) * (1 - exp(-(t - 5.883) / mu)))
  )


start_vals <- list(VO2rest = 400, VO2peak = 1600, mu = 1) 
res <- nls(nonlin_form, start = start_vals, data = O2K) 
res

tidy(res) 
fit_fun <- function(split, ...) {
  nls(nonlin_form, data=analysis(split), ...) %>% 
    tidy()
}

library(rsample) 
set.seed(8584) 
bt_resamples <- bootstraps(mtcars, times=3) 
bt_resamples %>% class()
bt_resamples %>% str()
library(listviewer)
jsonedit(first_resample)

bt_resamples$splits[[1]]
first_resample <- bt_resamples$splits[[1]]


first_resample %>% as.data.frame() 
first_resample
jsonedit(first_resample)
analysis(first_resample)
assessment(first_resample)

data("attrition", package="modeldata")
names(attrition)
nrow(attrition)
glm(formula = Attrition ~ JobSatisfaction + Gender + MonthlyIncome, data = attrition, family = binomial) 

mod_form <- as.formula( Attrition ~ JobSatisfaction + Gender + MonthlyIncome)
set.seed(4622)
rs_obj <- vfold_cv(attrition, v=10, repeats=10)
rs_obj 

analysis(rs_obj$splits[[1]])
assessment(rs_obj$splits[[1]])

holdout_results <- function(splits, ...) { 
  mod <- glm(..., data=analysis(splits), family=binomial) 
  holdout <- assessment(splits) 
  res <- broom::augment(mod, newdata = holdout) 
  lvls <- levels(holdout$Attrition)
  predictions <- factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]), levels= lvls)
  res$correct <- predictions == holdout$Attrition
  res
}


holdout_results2 <- function(splits, frm) { 
  mod <- glm(frm, data=analysis(splits), family=binomial) 
  holdout <- assessment(splits) 
  res <- broom::augment(mod, newdata = holdout) 
  lvls <- levels(holdout$Attrition)
  predictions <- factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]), levels= lvls)
  res$correct <- predictions == holdout$Attrition
  res
}

example <- holdout_results(rs_obj$splits[[1]], mod_form)
example <- holdout_results2(rs_obj$splits[[1]], mod_form)
example[1:10, setdiff(names(example), names(attrition))]
dim(example) 
dim(assessment(rs_obj$splits[[1]]))


jsonedit(example)

# fit formula on all splits.
rs_obj$results <- map(rs_obj$splits, holdout_results2, mod_form) 
rs_obj

map(rs_obj$results, ~.x$correct)


rs_obj$accuracy <- map_dbl(rs_obj$results, function(x) mean(x$correct)) 
summary(rs_obj$accuracy)
rs_obj$accuracy

# mean is 0.839 



head(as.data.frame(first_resample))
as.data.frame(first_resample, data="assessment")

rsample::analysis(first_resample)
analysis(bt_resamples)
class(first_resample)

spa <- attrition %>% group_split(Gender)
spa[[1]]$MonthlyIncome
t.test(spa[[1]]$MonthlyIncome, spa[[2]]$MonthlyIncome)


ggplot(attrition, aes(x=Gender, y=MonthlyIncome)) + geom_boxplot() + scale_y_log10()

# The t.test reveals there statistically significant difference in Monthly Income between genders. 

median_diff <- function(splits) {
  x <- analysis(splits)
  median(x$MonthlyIncome[x$Gender=="Female"]) - median(x$MonthlyIncome[x$Gender=="Male"]) 
}

bt_resamples$splits[[1]]


map_dbl(bt_resamples$splits, function(.x) { 
  analysis(.x) %>% filter(Gender=="Female") %>% .$MonthlyIncome %>% median -
    analysis(.x) %>% filter(Gender=="Male") %>% .$MonthlyIncome %>% median
})


    

# We'll create a large bootstrap samples. 
set.seed(353)
bt_resamples <- bootstraps(attrition, times=500) 

# Now apply the difference of mean function to the samples. 
bt_resamples$wage_diff <- map_dbl(bt_resamples$splits, median_diff) 
jsonedit(head(bt_resamples))

ggplot(bt_resamples, aes(x= wage_diff)) + geom_line(stat="density", adjust=1.25)  
ggplot(bt_resamples, aes(x= wage_diff)) + geom_line(stat="density")  
quantile(bt_resamples$wage_diff, c(0.025, 0.975)) 

glm_coefs <- function(splits, ...) {
  mod <- glm(..., data=analysis(splits), family=binomial)
  as.data.frame(t(coef(mod)))
}
bt_resamples$betas <- map(bt_resamples$splits, glm_coefs, mod_form)
bt_resamples$betas 
bt_resamples$betas[[1]]

first_resample <- bt_resamples$splits[[1]]
analysis(first_resample)

which(rownames(attrition)=="1162")
slice(attrition, 834)
head(analysis(first_resample))

class(bt_resamples) 
tidy(bt_resamples) 
bt_resamples












