library(mlr)
library(tidyverse) 
data(diabetes, package="mclust")
diabetesTib <- as_tibble(diabetes)
summary(diabetesTib)

# Now analyze the correlation of variables visually... t
nm <- diabetesTib %>% names()

normalize <- function(x) {
  mn <- mean(x)
  sd <- sd(x) 
  (x - mn) / sd
}


map( select(diabetesTib, -class), ~ mean(.x))
map( select(diabetesTib, -class), ~ mean(.x))$insulin
map( select(diabetesTib, -class), ~ sd(.x))$insulin
diabetes_Normalized$insulin %>% head()
diabetesTib$insulin %>% head()
rm(diabetes_Normailized)
diabetes_Normalized <- map( select(diabetesTib, -class), ~ normalize(.x))

diabetes_Normalized <- as_tibble(diabetes_Normalized)
ggplot(diabetes_Normalized, aes(x="glucose", y="insulin")) + geom_point() + scale_x_discrete() + scale_y_discrete()
ggplot(diabetes, aes(x=glucose, y=insulin, col=class)) + geom_point() + scale_x_continuous()

diabetes_Normalized$glucose
diabetes_Normalized$insulin
diabetes_Normalized %>% transpose() %>% as.data.frame() is.data.frame()
a <- data.frame(class=diabetes$class, glucose=diabetes_Normalized$glucose, insulin=diabetes_Normalized$insulin, sspg=diabetes_Normalized$sspg) 

ggplot(a, aes(x=glucose,y=insulin)) + geom_point()
ggplot(a, aes(x=sspg, y=insulin, color=class)) + geom_point()
ggplot(a, aes(x=sspg, y=glucose, shape=class, color=class)) + geom_point()

diabetesTask <- makeClassifTask(data=a, target="class")
diabetesTask
diabetesLearner <- makeLearner("classif.knn", par.vals=list("k"=1)) 
diabetesModel <- train(diabetesLearner, diabetesTask) 
prd1 <- predict(diabetesModel, newdata=a)
str(prd1)
map(prd1$data %>% transpose(), function(.x) { .x$truth == .x$response }) %>% unlist() %>% imap_int( ~ if (.x==FALSE) .y else 0L) 
sum(!b[b==FALSE])
which(b==FALSE)

holdout <- makeResampleDesc(method="Holdout", split=2/3, stratify=TRUE)
holdoutCV <- resample(diabetesLearner, task=diabetesTask, resampling=holdout, measures=list(mmce, acc))
#holdoutCV$aggr
holdoutCV_10Test <- resample(learner=diabetesLearner, task=diabetesTask, 
                             resampling = makeResampleDesc(method="Holdout", split=1/10, stratify=TRUE), 
                             measures=list(mmce, acc)) 
holdoutCV_10Test$aggr
holdoutCV$aggr

calculateConfusionMatrix(holdoutCV$pred, relative=TRUE)
calculateConfusionMatrix(holdoutCV_10Test$pred, relative=TRUE)

# Now CV the diabetes 
kFold <- makeResampleDesc(method="RepCV", folds=10, reps=50, stratify=TRUE)
kFoldCV <- resample(diabetesLearner, task=diabetesTask, resampling = kFold, measures=list(mmce, acc))

kFoldCV
calculateConfusionMatrix(kFoldCV$pred, relative=TRUE)

# Paramspace... Just k-neighbors...
knnParamSpace <- makeParamSet(
  makeDiscreteParam("k", values=1:10) 
) 

outer <- makeResampleDesc(method="RepCV", folds=10, reps=50, stratify=TRUE)
wrapper <- makeTuneWrapper(learner="classif.knn", 
                           resampling=makeResampleDesc(method="CV"),
                           par.set=knnParamSpace, 
                           control=makeTuneControlGrid()
                           )
library(parallelMap)
library(parallel)
parallelStartSocket(cpus=detectCores())
cvWTune <- resample(wrapper, diabetesTask, resampling=outer, models=TRUE) 
parallelStop()
calculateConfusionMatrix(cvWTune$pred, relative=TRUE)

LOO < -makeResampleDesc(method="LOO") 
LOOCV <- resample(learner="classif.knn", 
                  task=diabetesTask, 
                  resampling=LOO,
                  measures=list(mmce, acc))


inner <- makeResampleDesc(method="CV")
outer <- makeRessampleDesc("RepCV", folds=10, reps=5)

knnWrapper <- makeTuneWrapper("classif.knn", resampling=inner, par.set=knnParamSpace, control=makeTuneControlGrid())
cvWithTuning <- resample(knnWrapper, diabetesTask, resampling=outer)

