#Clearing the environment
rm(list=ls())

#Setting working directory
setwd("F:\\Attrition_final_G")

# Loading libraries
library(corrplot)
library(caTools)
library(glmnet)
library(class)
library(e1071)
library(rpart)
library(randomForest)
library(h2o)

#Reading the data
attrition <- read.csv("Attrition Case Study - Copy.CSV")

#---------------------------------Exploratory Data Analysis------------------------------------------------------
# Shape of the data
dim(attrition)
# Viewing data
View(attrition)
# Structure of the data
str(attrition)
# Variable names of the data
colnames(attrition)

# Checking for missing value
sum(is.na(attrition))

#Converting categorical variables as numeric
attrition$BusinessTravel <- as.numeric(factor(attrition$BusinessTravel,
                                              levels = c('Non-Travel','Travel_Rarely','Travel_Frequently'),
                                              labels = c(0,1,2)))
attrition$Department <- as.numeric(factor(attrition$Department,
                                          levels = c('Sales','Research & Development','Human Resources'),
                                          labels = c(0,1,2)))
attrition$Gender <- as.numeric(factor(attrition$Gender,
                                      levels = c('Male','Female'),
                                      labels = c(0,1)))
attrition$EducationField <- as.numeric(factor(attrition$EducationField,
                                              levels = c('Life Sciences','Marketing',
                                                         'Medical','Human Resources',
                                                         'Technical Degree','Other'),
                                              labels = c(1,2,3,4,5,6)))
attrition$JobRole <- as.numeric(factor(attrition$JobRole,
                                       levels = c('Manager','Manufacturing Director',
                                                  'Research Director','Research Scientist',
                                                  'Sales Executive','Sales Representative',
                                                  'Healthcare Representative','Human Resources',
                                                  'Laboratory Technician'),
                                       labels = c(1,2,3,4,5,6,7,8,9)))
attrition$MaritalStatus <- as.numeric(factor(attrition$MaritalStatus,
                                             levels = c('Single','Married','Divorced'),
                                             labels = c(1,2,3)))
attrition$OverTime <- as.numeric(factor(attrition$OverTime,
                                        levels = c('No','Yes'),
                                        labels = c(0,1)))
attrition$Over18 <- as.numeric(factor(attrition$Over18,
                                      levels = 'Y',
                                      labels = 1))

#-----------------------------------Feature Selection------------------------------------------#

## Correlation Plot 
attrition_cor <- cor(attrition)
corrplot(attrition_cor, method="circle")

# Kendal test for input as Categorical variable and output as numerical variable
res12 <- cor.test(attrition$Attrition,attrition$Age,method = "kendall") # correlated
res12 <- cor.test(attrition$Attrition,attrition$DailyRate,method = "kendall") #correlated
res12 <- cor.test(attrition$Attrition,attrition$DistanceFromHome,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$HourlyRate,method = "kendall")
res12 <- cor.test(attrition$Attrition,attrition$MonthlyIncome,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$MonthlyRate,method = "kendall")
res12 <- cor.test(attrition$Attrition,attrition$PercentSalaryHike,method = "kendall")
res12 <- cor.test(attrition$Attrition,attrition$TotalWorkingYears,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$TrainingTimesLastYear,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$YearsAtCompany,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$YearsInCurrentRole,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$YearsSinceLastPromotion,method = "kendall")#correlated
res12 <- cor.test(attrition$Attrition,attrition$YearsWithCurrManager,method = "kendall")#correlated
# Chi-Square test for input as categorical variables with output as categorical variables
chisq.test(attrition$Attrition,attritin$BusinessTravel)
chisq.test(attrition$Attrition,attritin$Attrition)
chisq.test(attrition$Attrition,attritin$MaritalStatus)
chisq.test(attrition$Attrition,attritin$OverTime)
chisq.test(attrition$Attrition,attritin$EducationField)
chisq.test(attrition$Attrition,attritin$Gender)

# Pearson,Spearman test for input as numerical variables 
res <- cor.test(attrition$MonthlyIncome,attrition$Attrition,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$Age,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$DailyRate,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$DistanceFromHome,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$Education,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$EnvironmentSatisfaction,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$HourlyRate,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$JobInvolvement,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$JobLevel,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$JobSatisfaction,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$MonthlyIncome,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$MonthlyRate,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$NumCompaniesWorked,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$PercentSalaryHike,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$PerformanceRating,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$RelationshipSatisfaction,method = "kendall")
res <- cor.test(attrition$MonthlyIncome,attrition$StockOptionLevel,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$TotalWorkingYears,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$TrainingTimesLastYear,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$WorkLifeBalance,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$YearsAtCompany,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$YearsInCurrentRole,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$YearsSinceLastPromotion,method = "spearman",exact = FALSE)
res <- cor.test(attrition$MonthlyIncome,attrition$YearsWithCurrManager,method = "spearman",exact = FALSE)


#------------------------------------------Model Development--------------------------------------------#
#Encoding the target variable as factor
attrition$Attrition = factor(attrition$Attrition,levels = c(0,1))

set.seed(123)
spl <- sample.split(attrition$Attrition,SplitRatio = 0.75)
training_set <- subset(attrition,spl == TRUE)
testing_set <- subset(attrition,spl == FALSE)

#Feature Scaling
training_set[,c(-1,-22)] <- scale(training_set[,c(-1,-22)])
testing_set[,c(-1,-22)] <- scale(testing_set[,c(-1,-22)])

#--------------------------------------------Logistics Regression---------------------------------------#
#Building logistic model on traing set
classifier <- glm(Attrition~TotalWorkingYears+
                    TrainingTimesLastYear+YearsInCurrentRole+YearsSinceLastPromotion+
                    JobRole+MaritalStatus+OverTime+Department+BusinessTravel+
                    EnvironmentSatisfaction+JobInvolvement+
                    JobSatisfaction+WorkLifeBalance,data=training_set,family = binomial)

#Summary of logistic regression on testing set
summary(classifier)

#Predicting logistic model on testing set
lg_pred<- predict(classifier,type = "response",newdata = testing_set[-1])
lg_pred <- round(lg_pred >0.50,1)

# Creating cofussion matrix of the model
lg_cm <- table(testing_set[,1],lg_pred)

#Calculating accuracy of the model
lg_accuracy <- 100*sum(diag(lg_cm))/sum(lg_cm)

#--------------------------------------------KNN-------------------------------------------------#
#Building knn model on training set
knn_model <- knn(train = training_set[,c(3,5,8,11,14:20,23,24,28:35)],
                 test = testing_set[,c(3,5,8,11,14:20,23,24,28:35)],
                 cl = training_set[,1],k=5)

#Creating confusion matrix 
knn_cm <- table(knn_model,testing_set[,1])

#Calculating accuracy
knn_accuracy <- 100*sum(diag(knn_cm))/sum(knn_cm)

#--------------------------------------------svm---------------------------------------------------#
#Building svm model on training set
model1 <- svm(Attrition~MonthlyIncome+MonthlyRate+PercentSalaryHike+TotalWorkingYears+
                TrainingTimesLastYear+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                YearsWithCurrManager+JobRole+MaritalStatus+OverTime+Department+BusinessTravel+
                EducationField+EnvironmentSatisfaction+JobInvolvement+JobLevel+
                JobSatisfaction+StockOptionLevel+WorkLifeBalance,data=training_set,
              type = 'C-classification',kernal = 'linear')

#Predicting model on testing set
svm_pred <- predict(model1,newdata = testing_set[, -1])

#Creating confusion matrix
svm_cm <- table(testing_set[,1],svm_pred)

#Calculating accuracy of the model
svm_accuracy <- 100*sum(diag(svm_cm))/sum(svm_cm)

#-------------------------------------------Naive Bayes---------------------------------------------#
#Building naive bayes model on training set
nb_model <- naiveBayes(x=training_set[,c(3,5,8,11,14:20,23,24,28:35)],
                       y=training_set$Attrition)

#Predicting model on testing set
nb_pred <- predict(nb_model,testing_set[,c(3,5,8,11,14:20,23,24,28:35)])

#Creating confusion matrix
nb_cm <- table(nb_pred,testing_set[,1])

#Calculating accuracy of the model
nb_accuracy <- 100*sum(diag(nb_cm))/sum(nb_cm)

#------------------------------------------Decision Tree---------------------------------------------#
#Building model using training set
dt_classifier <- rpart(Attrition~MonthlyIncome+MonthlyRate+PercentSalaryHike+TotalWorkingYears+
                         TrainingTimesLastYear+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+
                         YearsWithCurrManager+JobRole+MaritalStatus+OverTime+Department+BusinessTravel+
                         EducationField+EnvironmentSatisfaction+JobInvolvement+JobLevel+
                         JobSatisfaction+StockOptionLevel+WorkLifeBalance,data=training_set)

#Predicting the model on testing set
dt_pred<- predict(dt_classifier,newdata = testing_set[,-1],type = 'class')

#Creating confusion matrix
dt_cm <- table(testing_set[,1],dt_pred)

#Calculating accuracy of the model
dt_accuracy <- 100*sum(diag(dt_cm))/sum(dt_cm)

#Ploting and texting decision tree diagram 
plot(dt_classifier)
text(dt_classifier)

#------------------------------------------Random Forest----------------------------------------------#
#Building model using training set
rf_classifier <- randomForest(x=training_set[,c(3,5,8,11,14:20,23,24,28:35)],
                              y=training_set$Attrition,ntree =100)

#Predicting the model using testing set
rf_pred <- predict(rf_classifier,newdata = testing_set[,c(3,5,8,11,14:20,23,24,28:35)])

#Creating confusion matrix
rf_cm <- table(rf_pred,testing_set[,1])

#Calculating accuracy of the model
rf_accuracy <- 100*sum(diag(rf_cm))/sum(rf_cm)

#-----------------------------------------K means clustering------------------------------------------#
#Setting the seed of R's random number 
set.seed(6)
wcss <- vector()
for(i in 1:10)wcss[i] <- sum(kmeans(attrition,i)$withinss)
plot(1:10,wcss,type = "b",main =paste('clusters'),xlab ='number of clusters',ylab ='Wcss')

# appyling kmeans to our data set
set.seed(29)
kmeans <- kmeans(attrition,4,iter.max = 200,nstart = 10)

#-----------------------------------------Hierarical clustering---------------------------------------#
#Creating dendogram and plotting the same
dendogram <- hclust(dist(attrition,method = 'euclidean'),method = 'ward.D')
plot(dendogram,main = paste('dendogram'),xlab = "deaths",ylab = 'euclidean distance')

#Creating the model
hc <- hclust(dist(attrition,method = 'euclidean'),method = 'ward.D')
y_hc <- cutree(hc,3)

#-----------------------------------------ANN----------------------------------------------------------#
#connecting system to default available server
h2o.init(nthreads = -1)

#building the ANN model on training set
classifier <- h2o.deeplearning(y = 'Attrition',
                               training_frame = as.h2o(training_set[,-22]),
                               activation = 'tanh',
                               hidden = c(18,18),epochs = 100,
                               train_samples_per_iteration = -2)

#Predicting the ANN model on testing set
prob_pred <- h2o.predict(classifier,newdata = as.h2o(testing_set[-1]))
y_pred <- (prob_pred >0.5)
y_pred <- as.vector(y_pred)

#Creating confusion matrix
cm <- table(testing_set[,1],y_pred)

#Calculating accuracy of the model
ann_accuarcy <- 100*sum(diag(cm))/sum(cm)

#Shutting down the h2o instance running at this address
h2o.shutdown()