# setting the working directory
rm(list=ls(all=TRUE))

setwd("C:/RLab/Hacathon/Data&Description")

# Reading from train data.
intrusion_train = read.table(file = "train_final.csv",header=T,sep=',')
summary(intrusion_train)

# Reading from test data.
intrusion_test = read.table(file = "test_final.csv",header=T,sep=',')
summary(intrusion_test)
dim(intrusion_test)
# reading from evaluation data.
intrusion_eval = read.table(file = "eval_final.csv",header=T,sep=',')
summary(intrusion_eval)

intrusion_data = rbind(intrusion_train, intrusion_test)# conbining test and train
#intrusion_data = intrusion_data[,-28] # removing the target variable
#intrusion_data = rbind(intrusion_data, intrusion_eval) #combining test train and eval for data preprosessing
str(intrusion_data)
head(intrusion_data)

# NA manipulation
sum(is.na(intrusion_data))
intrusion_data = na.omit(intrusion_data) # only 257 missing values so delete those rows
summary(intrusion_data)


table(intrusion_data$duration)
plot(intrusion_data$duration,intrusion_data$target)
intrusion_data = intrusion_data[intrusion_data$duration != 58329, ] # removing the outlier
head(intrusion_data)

plot(intrusion_data$land,intrusion_data$Target)
table(intrusion_data$land) # only 2 rows have  1 as value

plot(intrusion_data$wrong_fragment,intrusion_data$Target)
table(intrusion_data$wrong_fragment) # only 0's

plot(intrusion_data$urgent,intrusion_data$Target)
table(intrusion_data$urgent) # only 5 rowsa have the values from 0 t0 5

table(intrusion_data$hot)
plot(intrusion_data$hot,intrusion_data$Target)

table(intrusion_data$num_compromised)
plot(intrusion_data$num_compromised,intrusion_data$Target) # 1739 is an outlier
intrusion_data = intrusion_data[intrusion_data$num_compromised != 1739, ]

table(intrusion_data$root_shell)

table(intrusion_data$su_attempted)
# converting su_attempted data value from 2 to 1
intrusion_data$su_attempted = ifelse(intrusion_data$su_attempted ==2,1,
                                     ifelse(intrusion_data$su_attempted ==0,0,1))

intrusion_eval$su_attempted = ifelse(intrusion_eval$su_attempted ==2,1,
                                     ifelse(intrusion_eval$su_attempted ==0,0,1))
names(intrusion_data)
table(intrusion_data$num_root)
plot(intrusion_data$num_root,intrusion_data$Target) # 1045 is an outlier
intrusion_data = intrusion_data[intrusion_data$num_root != 1045, ]

table(intrusion_data$num_file_creations)
table(intrusion_data$num_shells) # only one datapoint having value 2

table(intrusion_data$num_access_files)
table(intrusion_data$num_outbound_cmds) # only 0's
table(intrusion_data$is_host_login) # only 0's
table(intrusion_data$is_guest_login)
table(intrusion_eval$rerror_rate) # 0 to 1
table(intrusion_eval$serror_rate) # 0 to 1
table(intrusion_eval$diff_srv_rate) # 0 to 1
table(intrusion_eval$srv_diff_host_rate) # 0 to 1

plot(intrusion_data$dst_host_count,intrusion_data$Target)
table(intrusion_data$dst_host_srv_diff_host_rate) # 0 to 1
table(intrusion_data$dst_host_rerror_rate)  # 0 to 1
table(intrusion_data$dst_host_srv_rerror_rate) # 0 to 1
table(intrusion_data$serror_rate) # 0 to 1
table(intrusion_data$Target)

plot(intrusion_data$X, intrusion_data$Target)


# dropping attributes
head(intrusion_data)
intrusion_data = intrusion_data[,-1] # deleting the  "X" attribute
intrusion_eval = intrusion_data[,-1]

# converting to appropriate type
names(intrusion_data)
intrusion_data = data.frame(sapply(intrusion_data, as.character))
cat_attr = c("land","logged_in","root_shell","su_attempted","is_host_login","is_guest_login","Target")
num_attr = setdiff(names(intrusion_data),cat_attr)
intrusion_catData = data.frame(sapply(intrusion_data[,cat_attr], as.factor))
intrusion_numdata = data.frame(sapply(intrusion_data[,num_attr], as.numeric))
intrusion_data = cbind(intrusion_numdata, intrusion_catData)
str(intrusion_data)

rm("cat_attr","num_attr")

# converting to appropriate type in evaluation data

intrusion_eval = data.frame(sapply(intrusion_eval, as.character))
cat_attr = c("land","logged_in","root_shell","su_attempted","is_host_login","is_guest_login")
num_attr = setdiff(names(intrusion_eval),cat_attr)
intrusion_evalcatData = data.frame(sapply(intrusion_eval[,cat_attr], as.factor))
str(intrusion_evaldata)
intrusion_evalnumdata = data.frame(sapply(intrusion_eval[,num_attr], as.numeric))
intrusion_evaldata = cbind(intrusion_evalnumdata, intrusion_evalcatData)

rm("cat_attr","num_attr")

# splitting data into test train and eval

dim(intrusion_data)
names(intrusion_data)
dim(intrusion_train)
intrusion_test = intrusion_test[,-1]
dim(intrusion_test)
head(intrusion_test)
train_data = intrusion_data[1:98000,]
test_data = intrusion_data[-c(1:98000),]
head(test_data)
dim(test_data)

# check how data is distributed in train and test

table(test_data$Target)
table(train_data$Target)

# Modelling using Random forest
library(randomForest)

randomForest_model = randomForest(Target~.,train_data, ntree=75, keep.forest = T)
print(randomForest_model)
randomForest_model$importance

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(randomForest_model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

# plot (directly prints the important attributes) 
varImpPlot(randomForest_model)


# Build randorm forest using top 9 important attributes. 
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:12])

# Build the classification model using randomForest (to generalise the model always keep the minimum no of attributes
rfmodel_Imp1 = randomForest(Target~.,
                         data=train_data[,c(top_Imp_Attr,"Target")], 
                         keep.forest=TRUE,ntree=50) 

# Print and understand the model
print(rfmodel_Imp1)

# Important attributes
rfmodel_Imp1$importance  

# Predict on Train data 
pred_Train = predict(rfmodel_Imp1, train_data[,top_Imp_Attr],
                     type="response", norm.votes=TRUE)


# Build confusion matrix and find accuracy   
cm_Train = table("actual" = train_data$Target, 
                 "predicted" = pred_Train);
recall_train = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Train, cm_Train)

# Predicton Test Data
pred_Test = predict(rfmodel_Imp1, test_data[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)
table(pred_Test)
# Build confusion matrix and find accuracy   
cm_Test = table("actual" = test_data$Target, 
                "predicted" = pred_Test);
recall_test = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Test, cm_Test)
table(test_data$Target)
recall_train# 61.8
recall_test # 61

###################################################

#predict on evaluation data

str(intrusion_evaldata)

pred_Test = predict(rfmodel_Imp1, intrusion_evaldata[,top_Imp_Attr],
                    type="response", norm.votes=TRUE)
table(pred_Test)


write.csv(pred_Test,file = "Random_Forest_Prediction.csv")

############################################
top_Imp_Attr
summary(train_data)
names(glm_train)

glm_train = train_data[,-c(2,11,25)]
glm_model = glm(Target~ diff_srv_rate+ serror_rate+logged_in+
                  dst_host_count+dst_host_srv_diff_host_rate+
                  dst_host_rerror_rate+rerror_rate+srv_rerror_rate+
                  dst_host_srv_rerror_rate+num_compromised+hot, 
                data = train_data,family = binomial)

#num_compromised is not significant so it is removed
glm_model = glm(Target~ diff_srv_rate+ serror_rate+logged_in+
                  dst_host_count+dst_host_srv_diff_host_rate+
                  dst_host_rerror_rate+rerror_rate+srv_rerror_rate+
                  dst_host_srv_rerror_rate+hot, 
                data = train_data,family = binomial)
summary(glm_model)

prob <- predict(glm_model, type="response")
head(prob)


library(ROCR)
library(pROC)
ROCRpred = prediction(prob,train_data$Target)
ROCRperf  = performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7))

#plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1))
#to get AUC
attributes(performance(ROCRpred, 'auc'))$y.values[[1]]


pred_class <- ifelse(prob> 0.9, 1, 0)

ConfusionMatrix <- table(train_data$Target, pred_class)
ConfusionMatrix
LRrec = ConfusionMatrix[2,2]/sum(ConfusionMatrix[2,]) # 

# Predictions on the test set
predictTest = predict(glm_model, type="response", newdata=test_data)
ConfusionMatrix <- table(test_data$Target, predictTest)
LRrec = ConfusionMatrix[2,2]/sum(ConfusionMatrix[2,]) # 


############################################################################################

#             ADA boost



library(vegan)
# standardise the numeric data

standard_testnumdata = decostand(test_data[,num_attr],"range")
standard_trainnumdata = decostand(train_data[,num_attr],"range")
head(standard_data)
# convert the categorical in to as.numeric
cat_attr = setdiff(cat_attr,"Target")
standard_testcatdata = cbind(standard_testdata, sapply(test_data[,cat_attr],as.numeric))
standard_traincatdata = cbind(standard_traindata, sapply(train_data[,cat_attr],as.numeric))

stand_train_data = cbind(standard_traincatdata, standard_trainnumdata)
stand_test_data = cbind(standard_testcatdata,standard_testnumdata)
dim(standard_traindata)
dim(standard_testdata)


#ind_Attr = append(cat_attr,num_attr)
ind_Attr = setdiff(names(stand_train_data),top_Imp_Attr[1:12]) # Top 12 ATTRIBUTES

## Build best ada boost model 
library(ada)
library (rpart)
str(stand_train_data[,ind_Attr])
ada_model = ada(x = stand_train_data[,ind_Attr], 
            y = train_data$Target, 
            iter=25, loss="logistic")



summary(ada_model)

# Predict on train data  
pred_Train  =  predict(ada_model, stand_train_data[,ind_Attr])  

# Build confusion matrix and find accuracy   
cm_Train = table(train_data$Target, pred_Train)
ada_recall = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Train, cm_Train)

# Predict on test data
pred_Test = predict(model, test_Data[,ind_Attr]) 

# Build confusion matrix and find accuracy   
cm_Test = table(test_Data$loan, pred_Test)
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test

#################################################################################

# Build best KNN model 
library(FNN)
library(class)
# k = 1
head(train_data)
pred_Train = knn(stand_train_data[,ind_Attr], 
                 stand_test_data[,ind_Attr], 
                 train_data$Target, k = 1)

cm_Train = table(pred_Train, train_Data$loan)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)

pred_Test = knn(train_Data[,reg_Ind_Attr], 
                test_Data[,reg_Ind_Attr], 
                train_Data$loan, k = 1)

cm_Test = table(pred_Test, test_Data$loan)
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

accu_Train
accu_Test

# Building svm with the top 13 attributes
library(e1071)
ind_Attr =setdiff(ind_Attr,c("wrong_fragment","num_outbound_cmds","is_host_login"))
svm_model = svm(x = stand_train_data[,ind_Attr], 
            y = train_data$Target, 
            type = "C-classification", 
            kernel = "linear", cost = 10, gamma = 0.1) 

# Look at the model summary
summary(svm_model)

#svm_model$index

# Predict on train data  
pred_Train  =  predict(svm_model, stand_train_data[,ind_Attr])  

# Build confusion matrix and find accuracy   
cm_Train = table(train_data$Target, pred_Train)
table(train_data$Target)
recall_train_svm = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Train, cm_Train)

# Predict on test data
pred_Test = predict(svm_model, test_data[,ind_Attr]) 

# Build confusion matrix and find accuracy   
cm_Test = table(test_data$Target, pred_Test)
recall_test_svm = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Test, cm_Test)
recall_train_svm
recall_test_svm

rm(model, accu_Test, accu_Train, ind_Attr, train_Data, test_Data)

