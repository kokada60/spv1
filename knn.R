library(tidymodels) 
library(kknn) 


show_engines("nearest_neighbor")

tidymodels_prefer()
data("two_class_dat")
data_train <- two_class_dat[-(1:10), ]
data_train
data_test <- two_class_dat[1:10, ]

knn_cls_spec <- 
  nearest_neighbor(neighbors = 11, weight_func = "triangular") %>% 
  set_mode("classification") %>% 
  set_engine("kknn")
knn_cls_spec

knn_cls_fit <- knn_cls_spec %>% fit(Class ~ ., data=data_train)
knn_cls_fit 

p1 <- bind_cols( 
    predict(knn_cls_fit, new_data=data_test), 
    predict(knn_cls_fit, new_data=data_test, type="prob")
  )
nrow(p1)
nrow(data_test)
data_test

p2 <- bind_cols ( truthClass = data_test$Class, predClass = p1$.pred_class )
table(p2)

p2%>% filter(predClass=="Class2" & truthClass=="Class1")
p2%>% filter(predClass=="Class2" & truthClass=="Class2")
p2%>% filter(predClass=="Class1" & truthClass=="Class1")
p2%>% filter(predClass=="Class1" & truthClass=="Class2")

