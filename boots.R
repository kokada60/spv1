library(tidymodels) 
data(mtcars)
ggplot(mtcars, aes(wt, mpg)) + geom_point() 

tmt <- mtcars %>% as_tibble() 
map(tmt, ~ as_tibble(.x) %>% ggplot(aes(.x)) + geom_histogram())

ggplot(tmt, aes(x=mpg, y=wt)) + geom_point() + geom_smooth(method="loess") + geom_smooth(method="lm", aes(col="red")) + 
geom_smooth(method="gam", aes(col="green"))
frm1 <- formula(mpg ~ wt)

nlsfit <- nls(formula=mpg ~ k / wt + b, mtcars, start = list(k=1, b=0))
summary(nlsfit) 
ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_line(aes(y=predict(nlsfit))) 
nlsfit %>% tidy()

install.packages("investr")
library(investr)

# Displaying confidence intervals ... 
interval <- as_tibble(predFit(nlsfit, newdata = mtcars, interval = "confidence", level=0.95)) %>% 
  mutate(wt = mtcars$wt)

tidy(nlsfit, conf.int=TRUE, conf.level=0.95)

interval
ggplot() + geom_point(data=mtcars, aes(x=wt,  y=mpg), size=1.5, col="darkgray", alpha=0.75) + #theme(legend.position = "none")
  guides(size="none") + 
  geom_line(data=interval, aes(x=wt, y=fit)) + 
  geom_ribbon(data=interval, aes(x=wt, ymin=lwr, ymax=upr), alpha=0.5, inherit.aes=F, fill="orange")

set.seed(21) 
boots <- bootstraps(mtcars, times = 2000, apparent=TRUE)
boots %>% nrow()

fit_nls <- function(split) {
  nls(mpg ~ k / wt + b, data = analysis(split), start=list(k=1, b=0)) #%>% tidy() 
}

boot_models <- boots %>% 
  #mutate(coef_info = map(splits, fit_nls)) %>% unnest(coef_info)
  mutate(model = map(splits, fit_nls), 
         coef_info = map(model, tidy)) 
  
  


map(boots$splits, fit_nls) 
boot_models$coef_info



boot_coefs <- boot_models %>% 
  unnest(coef_info)

rsample::int_pctl(boot_models, coef_info, alpha=0.95) %>% arrange(term)
rsample::int_pctl(boot_models, coef_info, alpha=0.05) %>% arrange(term)
rsample::int_pctl(boot_models, coef_info, alpha=0.95) %>% arrange(term)
tidy(nlsfit, conf.int = TRUE, conf.level = 0.05) %>% arrange(term)

rsample::int_pctl(boot_models, coef_info, alpha=0.95) %>% arrange(term)
rsample::int_t(boot_models, coef_info, alpha=0.95) %>% arrange(term)
rsample::int_t(boot_models, coef_info, alpha=0.99) %>% arrange(term)
tidy(nlsfit, conf.int = TRUE, conf.level = 0.95) %>% arrange(term)

percentile_intervals <- int_pctl(boot_models, coef_info, alpha=0.05)
percentile_intervals <- int_pctl(boot_models, coef_info, alpha=0.95)
ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")
  
boot_aug <- boot_models %>% sample_n(200) %>% 
  mutate(augmented = map(model, augment)) %>%
  unnest(augmented) 
boot_coefs
boot_aug

ggplot(boot_aug, aes(wt, mpg)) + 
  geom_line(aes(y = .fitted, group = id), alpha=.2, col="blue") + 
  geom_point()


fit_spline_on_bootstrap <- function(split) {
  data <- analysis(split)
  smooth.spline(data$wt, data$mpg, df=4) 
}

boot_aug <- boot_models %>% sample_n(200) %>% 
  mutate(augmented = map(model, function(model) { augment(model, se_fit=TRUE, interval=c("confidence")) })) %>%
  unnest(augmented) 
boot_aug$coef_info[[1]] %>% tidy()

boot_aug2 <- boot_aug %>% head(10) %>% 
  mutate(cf = map(model, function(model) { tidy(model, conf.int=TRUE) })) %>% 
  unnest(cf) 


boot_models_spline <- boots %>% sample_n(200) %>% 
  mutate(model = map(splits, fit_spline_on_bootstrap), 
         aug_train = map(model, function(model) { augment(model, interval="confidence") }))

splines_aug <- boot_models_spline %>% unnest(aug_train) 
splines_aug 
splines_aug %>% ggplot(aes(x, y)) + geom_line(aes(y=.fitted, group=id), alpha=0.2, col="green") + 
  geom_point() 

splines_aug[1, ]
splines_aug$splits[[1]] %>% assessment()

splines_aug$x %>% unique()
splines_aug %>% filter(x==2.2 & y==32.4) %>% View()


ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() + 
  geom_line(aes(y=predict(nlsfit, level=0.95, type="response", interval="confidence")))


# First bootstrap the samples 

predict(nlsfit, level=0.95, type="response",  interval = "confidence")

x <- rnorm(15)
y <- x + rnorm(15)
new <- data.frame(x = seq(-3, 3, 0.5))
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
new <- mtcars %>% sample_n(size=20) %>% select("wt")



boot_aug$model[[1]] %>% predict(new, interval="confidence")


# 
# ggplot() + geom_point(data=mtcars, aes(x=wt,  y=mpg), size=1.5, col="darkgray", alpha=0.75) + #theme(legend.position = "none")
#   guides(size="none") + 
#   geom_line(data=interval, aes(x=wt, y=fit)) + 
#   geom_ribbon(data=interval, aes(x=wt, ymin=lwr, ymax=upr), alpha=0.5, inherit.aes=F, fill="orange")



# no strata 
boots <- rsample::bootstraps(mtcars, times=2000, apparent=TRUE) 
# Build a helper function that'll train models using these splits and produce breakdown of coefficients info.

nls_fit <- function(split) {
  nls(mpg ~ k / wt + b, analysis(split), start=list(k = 1, b=0))
}


boot_models2 <- boots %>% 
  mutate(models = map(splits, nls_fit), 
         coefs = map(models, tidy))
boot_coefs <- boot_models %>% unnest(coefs) 
boot_coefs
boot_aug <- boot_models2 %>% 
  mutate(aug = map2(models, splits, function(models, splits) { augment(models, newdata=assessment(splits), interval="confidence") })) %>% 
  unnest(aug) %>% select(splits, id, models, coefs, .rownames, mpg, wt, .fitted) %>% mutate(.resid = mpg - .fitted)

boot_aug 

# Plot the augmented models...
ggplot(boot_aug, aes(x=wt, y=mpg)) + 
  geom_line(aes(y=.fitted, group=id), alpha=.2, col="blue") + 
  geom_point() 


#One more time. 
#1. Build bootstrap data splits. Repeat for 2000 times. Consider building stratified splits later. 
boots <- rsample::bootstraps(mtcars, times=2000, apparent=TRUE)
nls_helper <- function(splits) {
  nls(mpg~k/wt + b, data=analysis(splits), start=list(k=1, b=0))
}
boot_models <- boots %>% 
  mutate(models = map(splits, nls_helper), 
         aug = map2(splits, models, function(splits, models) { augment(models, newdata=assessment(splits), interval="confidence") }),
)
boot_aug <- boot_models %>% 
  unnest(aug) %>% 
  mutate(.resid=mpg - .fitted)

ggplot(boot_aug, aes(x=wt, y=mpg)) + geom_point() + 
  geom_line(aes(y=.fitted), alpha=0.2, col="blue")

# In order to include the ribbons to illustrate cf, group the residue by id, and take the cf of the group by applying int_pctl. 
# This could be done by first nesting the set by id, and mapping int_pctl to each nested set. 
# rsample::int_pctl(boot_models, coef_info, alpha=0.95) %>% arrange(term)
int_pctl(mtcars, )
boot_aug %>% nest_by(id) %>% 
  rsample::int_pctl(data, alpha=0.95) %>% arrange(term)


mtcars

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() + 
  stat_smooth(method="lm", formula="y ~ log(x)", se=FALSE)

fmrLog <- formula(mpg ~ log(wt))

fitLog <- lm(formula = fmrLog, data=mtcars)
beta <- coef(fitLog) 
beta

predict_y_nonLine <- function(beta, x) {
  beta[1] + beta[2] * exp(-beta[3] * x) 
}

# predict the y, and take the residuals...
a_non_linear_MOdel <- function(beta, x, y) {
  y_hat <- predict_y_nonLine(beta, x)
  # And now thake the mean of tll residuals...
  residuals_sq <- (y - y_hat) ^ 2
  sum_residuals <- sum(residuals_sq) 
}

f <- function(x) {
  x ^ 2
}

# limited numbering Bounded 
optim(4, f, method="L-BFGS-B")$oar


fit <- lm(mpg ~ wt, data=mtcars) 
beta <- coef(fit) 
beta
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() + 
  stat_smooth(formula="y ~ x", method="lm", se=FALSE)

linearModel1 <- function(beta, X, y) {
  yhat <- apply(X, 1, function(row) sum(beta * row))
  sum((y - yhat) ^ 2)
}

X <- model.matrix(~ wt, data=mtcars) 
y <- mtcars$mpg 
optim(rnorm(2), )
X

linearModel1(rnorm(2), X, mtcars$mpg) 

