library(mlr)
library(mlr3)
library(tidyverse)


data(spam, package="kernlab")
spamTib <- as_tibble(spam)
spamTib$make %>% is.list()
spamTib[[1]]
length(spamTib)
count(spamTib)


summary(spamTib)
#ggplot(, aes(x=spamTib$make, y=n)) + geom_bar(stat="identity") + scale_y_log10()
a <- select(spamTib, make) %>% group_by(make) %>% count()
ggplot(a, aes(x=make, y=log10(n))) + geom_bar(stat="identity")

#Building SVM learner

glimpse(spamTib)
spamTask <- makeClassifTask(data=spamTib, target="type")
svm <- makeLearner("classif.svm") 
install.packages("e1071")
getParamSet("classif.svm")
getParamSet("classif.svm")$pars$kernel$values ## Possible Values for the picked parameter. 
## Note that Linear Kernel is identical to 1st-degree polynomial, so it will be omitted from the list of possible 
## when tuning... Waste of Computation is not a good thing. 

#make Parameter Set...
# kernel a discrete set of polynomial, radial, sigmoid.
# degree a discrete set of integer values in min-max range. A Integerparam
# cost a continuous values in min-max range. 
# gamma a continuous values in min-max range of 0,1 to 10

kernels <- getParamSet("classif.svm")$pars$kernel$values %>% discard( ~ .x=="linear") %>% unlist() 
attributes(kernels)$names <- NULL

svmParamSpace <- makeParamSet(
  makeDiscreteParam("kernel", kernels), # 3 possible values. 
  makeIntegerParam("degree", lower=1, upper=4), # 3 possible values, 
  makeNumericParam("cost", lower=0.01, upper=15), # In 0,1 incremetn, 100 possible values, 
  makeNumericParam("gamma", lower=0.01, upper=15) # In 0.1 increment, 100 possible values
) # 100 * 100 * 3 * 3= 90000 combinations to tune!!! Grid Search will be wasteful...
# For this size of value space, try random search instead. 
# Is not a guaranteed optimal tuning, but with enough iterations, will perform well enough. 


spamTask <- makeClassifTask(data=spamTib, target="type")
svm <- makeLearner("classif.svm")


randSearch <- makeTuneControlRandom(maxit = 40)
cvForTuning_HoldOut <- makeResampleDesc(method="Holdout", split=2/3)
cvForTuning_kFold <- makeResampleDesc(method="RepCV", folds=10, reps=5, stratify=TRUE)

#parallel::detectCores()

## Parallelize the tuning... 
## Iteration has been reset to 40 from original 20. The suggestion from the example selected linear kernel. 
## See if more iteration will select different kernel type...
library(parallelMap)
library(parallel) 

parallelStartSocket(cpus=detectCores())
tunedSvmPars <- tuneParams("classif.svm", 
                           task=spamTask,
                           resampling=cvForTuning_HoldOut, 
                           #resampling=cvForTuning_kFold, 
                           par.set=svmParamSpace,
                           control=randSearch)
parallelStop()
tunedSvmPars$x

## Since polynomial is the kernel being selected with no change, the learner will be assigned the tuned Param values...
tunedSvm <- setHyperPars(svm, par.vals = tunedSvmPars$x)
## Now train a SVM model using the task created earlier and the tuned learner. The task object contains the training
## data. 
#  Learner object holds the parameter collection ( tuned hyper params ), task object holds the data and 
# target variable, in the case of supervised learning... So for this spam detecting task, the target will be [type]
# variable that marks an incoming mail as spam/non-spam, and data is spam tribble. A model is trained by train method, 
# taking in both the tuned learner and task object. 
tunedSVMModel <- train(learner = tunedSvm, task = spamTask) 

# Now the model is built, it will  be cross-validated to estimate how it will perform on new, never-seen-yet data. 
# Remember, since cross-validation will encompass all deta-dependent steps, including pre-model train steps such as 
# imputation, hyperparameter tuning, we will be redoing the previous step as a part of process here...
# Cross-Validation will include any steps that are not data-deterministic. 
# First step is to build nested cross-validation strategy that mlr package uses...
## Previously built ResampleDescriptor cvForTuning with 2/3 split will be reused here. 
outer <- makeResampleDesc(method = "CV", iters=3) 
# a new learner ( non-tuned learner will be used here in cv because we are cross-validating from scratch...)
svmWrapper <- makeTuneWrapper(
                      learner="classif.svm", 
                      #resampling=cvForTuning_HoldOut, 
                      resampling=cvForTuning_kFold, 
                      par.set=svmParamSpace, 
                      control=randSearch
)
parallelStartSocket(detectCores())
cvWithTuning <- resample(learner=svmWrapper, task = spamTask, resampling=outer) 
parallelStop()

# Now lets see the result 
cvWithTuning
map(cvWithTuning, "Aggr perf") 
1-0.0689



