

# Model Building
#========================================================================

# =================Penalized Logistic Model==================================

# trying to build a simple penalized logistic model
# One vs all model
# -----------------------high-----------------
t_t<- which(logisctic_data$t_t=="test")

x_high<- model.matrix(high~. - low - medium -interest_level -listing_id -t_t, logisctic_data)[,-c(1,5, 73:76)]
y_high = logisctic_data$high


logistic_high<- glmnet(x_high[-t_t,], y_high[-t_t], family = "binomial" )
logistic_high_cv<- cv.glmnet(x_high[-t_t,], y_high[-t_t], family = "binomial" , 
                             type.measure = "auc" )

plot(logistic_high_cv)
best_lambda <- logistic_high_cv$lambda.min

# Predict
#high_prediction<- as.data.frame(predict(logistic_high_cv, newx = x_high[-t_t,],s = "lambda.min", type = "response"))
#high_prediction$pred<- predict(logistic_high_cv, newx = x_high[-t_t,],s = "lambda.min", type = "class")
#high_prediction$act<- logisctic_data$high[-t_t]

high_prediction<- as.data.frame(predict(logistic_high_cv, newx = x_high[t_t,],s = "lambda.min", type = "response"))
high_prediction$Listing_id<- logisctic_data$listing_id[t_t]
colnames(high_prediction)<- c("high", "listing_id")
high_prediction$high<- 1-high_prediction$high

rm(x_high)
rm(y_high)


# -----------------------low-----------------
x_low<- model.matrix(low~. - high - medium -interest_level -listing_id -t_t, logisctic_data)[,-c(1,5, 73:76)]
y_low = logisctic_data$low


logistic_low<- glmnet(x_low[-t_t,], y_low[-t_t], family = "binomial" )
logistic_low_cv<- cv.glmnet(x_low[-t_t,], y_low[-t_t], family = "binomial" , 
                             type.measure = "class" )
plot(logistic_low_cv)

best_lambda <- logistic_low_cv$lambda.min

# Predict
dm_pred<-as.data.frame(predict(logistic_low_cv, newx = x_low[-t_t,],s = "lambda.min", type = "response"))
dm_pred$act<- logisctic_data$low[-t_t]
dm_pred$pred<-  predict(logistic_low_cv, newx = x_low[-t_t,],s = "lambda.min", type = "class")
rm(dm_pred)

low_prediction<- as.data.frame(predict(logistic_low_cv, newx = x_low[t_t,],s = "lambda.min", type = "response"))
low_prediction$Listing_id<- logisctic_data$listing_id[t_t]
colnames(low_prediction)<- c("low", "listing_id")
low_prediction$low<- 1-low_prediction$low

rm(x_low)
rm(y_low)


# -----------------------medium-----------------
x_medium<- model.matrix(medium~. - high -low -interest_level -listing_id -t_t, logisctic_data)[,-c(1,5, 73:76)]
y_medium = logisctic_data$medium


logistic_medium<- glmnet(x_medium[-t_t,], y_medium[-t_t], family = "binomial" )
logistic_medium_cv<- cv.glmnet(x_medium[-t_t,], y_medium[-t_t], family = "binomial" , 
                            type.measure = "class" )
plot(logistic_medium_cv)

best_lambda <- logistic_medium_cv$lambda.min

# Predict
dm_pred<-as.data.frame(predict(logistic_medium_cv, newx = x_medium[-t_t,],s = "lambda.min", type = "response"))
dm_pred$act<- logisctic_data$medium[-t_t]
dm_pred$pred<-  predict(logistic_medium_cv, newx = x_medium[-t_t,],s = "lambda.min", type = "class")
rm(dm_pred)


medium_prediction<- as.data.frame(predict(logistic_medium_cv, newx = x_medium[t_t,],s = "lambda.min", type = "response"))
medium_prediction$Listing_id<- logisctic_data$listing_id[t_t]
colnames(medium_prediction)<- c("medium", "listing_id")
medium_prediction$medium<- 1-medium_prediction$medium

rm(x_medium)
rm(y_medium)

#----
final_submisson<- merge(high_prediction, medium_prediction, by.x = "listing_id", by.y ="listing_id")
final_submisson<- merge(final_submisson, low_prediction,by.x = "listing_id", by.y ="listing_id")
write.csv(final_submisson, "logistic.csv")
# Pred Score is .61

###################################################################
# Trying Random Forest
# Trying out Random Forest

rdf<- data_df
test_rdf<- test_data_df
test_rdf$interest_level<- "test"

l<- list(rdf, test_rdf)
rdf<- do.call(rbind, lapply(l, function(x) x[match(names(l[[1]]), names(x))]))

rdf<- select(rdf, -manager_id, - building_id, -description, -created, -display_address, -latitude,
             - longitude, - street_address, -features_list, - photos, - features)

#Tip1: Make sure that there is no NA or Inf
# Write a function to check for NA or Inf

#Tip2: If there is a column name containing "&", then better change it
#washer&dryer
names(rdf)[64]<- "washer_dryer"

#Tip3: Convert all the categorical predictors to factors or create dummy columns
x1<- summarise_each(rdf, funs(class)) # checking the class
# Converting to factor
rdf$fit.cluster<- as.factor(rdf$fit.cluster)
rdf<- mutate_if(rdf, is.character, as.factor) # EASY NO!!
x1<- summarise_each(rdf, funs(class))

# Separating test and Train
rdf$interest_level<- as.character(rdf$interest_level)

train<- rdf[which(rdf$interest_level!= "test"),]
test<- rdf[which(rdf$interest_level== "test"),]
rm(rdf)

# Running Basic RF without tuning
rf1<- randomForest(interest_level~., data = train[,-1])

# Prediction
x<-predict( rf1, train[,-c(1,5)])
# Confusion Matrix
prop.table(table(x, train$interest_level))

# Or to predict probablity use
x<- predict(rf1, train[,-c(1,5)], type = "prob")

# Prediction on test
rf_p1<- predict(rf1, test[,-c(1,5)], type = "prob")
rf_p1<- as.data.frame(rf_p1)
rf_p1$listing_id<- test$listing_id

write.csv(rf_p1, "rf_p1.csv")

# Accurace iproved to 0.58

# Tuning random forest
# two importtant tune parameters are mtry and ntree
# Random Search approach

#Random Search
mt<-seq(4,12, by = 0.5)
cv_loss<- NA
cv_oob<- NA
for (i in 1)
{
  s<- sample(seq(1:49352),4935)
  cv_train<- train[-s,] 
  m<- randomForest(interest_level~., data = cv_train[,-1], mtry = mt[i])
  cv_test<- train[s,]
  cv_pred<- data.frame(predict(rf1, cv_test[,-c(1,5)], type = "prob"))
  cv_pred$int_level<- cv_test$interest_level
  cv_pred$h<- ifelse(cv_pred$int_level == "high",1,0)
  cv_pred$m<- ifelse(cv_pred$int_level == "medium",1,0)
  cv_pred$l<- ifelse(cv_pred$int_level == "low",1,0)
  cv_pred$log_loss<- (cv_pred$high*cv_pred$h)+(cv_pred$low*cv_pred$l)+(cv_pred$medium*cv_pred$m)
  cv_loss[i]<- mean(log(cv_pred$log_loss))
  oob<- data.frame(m$err.rate)
  cv_oob[i]<- mean(oob$OOB)
}

best_mtry = mt[which(oob$err.rate)==min(which(oob$err.rate))]

# Prediction
rf2<- randomForest(interest_level~., data = train[,-1], mtry = best_mtry)
rf_p2<- predict(rf2, test[,-c(1,5)], type = "prob")
rf_p2<- as.data.frame(rf_p2)
rf_p2$listing_id<- test$listing_id

write.csv(rf_p2, "rf_p1.csv")

# Accuracy  - 0.57. Better than untuned RF

#### Model 3
# Trying Random Forest after oversampling the minor classes
# Data prep - Use the train file created for random forest for SMOTEing
library("DMwR")

# Separating train into two - one for high and another for medium
train_high<- train[train$interest_level == "high" | train$interest_level== "low",]
prop.table(table(train_high$interest_level))*100
train_high<- select(train_high, - listing_id)
train_high$interest_level<- as.factor(train_high$interest_level)
train_high<- SMOTE(interest_level~., train_high, prec.over = 120, prec.under = 200)
prop.table(table(train_high$interest_level))*100
length(which(train_high$interest_level=="low"))


train_medium<- train[train$interest_level == "medium" | train$interest_level== "low",]
prop.table(table(train_medium$interest_level))*100
train_medium$interest_level<- as.character(train_medium$interest_level)
train_medium<- select(train_medium, - listing_id)
train_medium$interest_level<- as.factor(train_medium$interest_level)
train_medium<- SMOTE(interest_level~., train_medium, prec.over = 70, prec.under = 200)
prop.table(table(train_medium$interest_level))*100
length(which(train_medium$interest_level=="low"))
medium<- train_medium[train_medium$interest_level=="medium",]

smote_train<- rbind(train_high, medium)
rm(train)
rm(train_medium)

# run RF
smote_rf<- randomForest(interest_level~., data = smote_train)


# Prediction on test
smote_rf_p1<- predict(smote_rf, test[,-c(1,5)], type = "prob")
smote_rf_p1<- as.data.frame(smote_rf_p1)
smote_rf_p1$listing_id<- test$listing_id

write.csv(smote_rf_p1, "smote_rf_p2.csv")

# Accuray - 0.61. It is worse than normal random forest so I am investing into tuning it

# ==================Model 4 - XGBOOST=========================
# XGB tuning using MLE
#create tasks
traintask <- makeClassifTask (data = train_xgb[,-1],target = "interest_level")
test_xgb$interest_level<- as.factor(as.character(test_xgb$interest_level))
testtask <- makeClassifTask (data = test_xgb[,-1],target = "interest_level")

#do one hot encoding
traintask <- createDummyFeatures (obj = traintask)

testtask <- createDummyFeatures (obj = testtask)

#Now, we'll set the learner and fix the number of rounds and eta as discussed above.

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="multi:softprob", eval_metric="mlogloss", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- makePrediction(xgmodel,testtask,type = "prob")

mytune$x
mytune$learner

# Manullay entered the best tuning parameters just to make sure that results match
xgb_params_1 = list(
  objective = "multi:softprob",
  eta = 0.1,                             
  max.depth = 10,                                                     
  eval_metric = "mlogloss",
  num_class = 4,
  subsample = 0.9922376,
  colsample_bytree = 0.6543029,
  min_child_weight = 2.823928
)

xgb_1 = xgboost(data = as.matrix(train_xgb %>%
                                   select(-listing_id,-interest_level)),
                label = train_xgb$interest_level,
                params = xgb_params_1,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                        
                print.every.n = 1,
                early.stop.round = 10                                          # stop if no improvement within 10 trees
)


x<- predict(xgb_1,newdata = as.matrix(test_xgb %>%
                                        select(-listing_id,-interest_level)))

test_prediction <- matrix(x, nrow = 4,
                          ncol=length(x)/4) %>%
  t() %>%
  data.frame()


test_prediction$X1<- test_xgb$listing_id
write.csv(test_prediction, file = "xgb_3.csv")

