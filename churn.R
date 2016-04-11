#libraries
library(corrplot)
library(caret)
library("randomForest")
library(MASS)
library(rpart)
library(leaps)
library(party)


#Read File
file<-read.csv("C:/Users/bhatvm/Desktop/Kaggle 2016/Churn_Data Case Study/Analytical data set/Test Data.csv")
colnames(file)[colnames(file)=="churn_flag..Dependent.variable."] <- "churn_flag"

#Variables
xvar<-file[,!names(file) %in% c("churn_flag","customer_id")]
yvar<-file[,names(file) %in% c("churn_flag")]


# -   DATA EXPLORATION AND VARIABLE EXPLANATION
#Correlations - All Variables
cormat<-cor(file[,2:120])

#Print P Values as well for Correlations
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

corprob<-cor.prob(file[,2:120])

#Plot the correlations
corrplot(cormat, method = "circle")


#flatten the square matrix and export to csv for analysis
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

flatcormat<-flattenSquareMatrix(cormat)
write.table(as.data.frame(flatcormat),"C:/Users/bhatvm/Desktop/Kaggle 2016/Churn_Data Case Study/Analytical data set/cormat.txt")
flatcorprobmat<-flattenSquareMatrix(corprob)

#Name the top correlating variables --- OPTIONAL
findCorrelation(cormat, cutoff = .95, verbose = FALSE,
                names = TRUE)


#-----------------------------------------------------#
#Pick best set of variables in case we run other models like SVM
fit <- glm(churn_flag~.,data=file[,2:120], family="binomial") #----Logistic---#
step<-step(fit,direction="backward",test="F") #---- Runs into too many iterations
step$anova # display results
#------------------------------------------------------#



#Pick best set variables
# I Looked at the correlation matrices and picked the best set of variables 
# since the step wise is time consuming.. We can also use Decision tree to get the best variables


#test and train variables
n<-7000
train<-file[1:n,]
test<-file[(n+1):10000,]

#---------------------------------#
#Methodologies
#---------------------------------#

library(ROCR)
library(pROC)

#METHOD 1---------------
#Decision Tree
dc1<-rpart(churn_flag~., data=train[,2:120], method="class")

plot(dc1, uniform=TRUE, main="Classification Tree for Churn")
text(dc1, use.n=TRUE, all=TRUE, cex=.8)
summary(dc1)

#Validation on test
pred=predict(dc1,test[,2:120], type="class")
dc1.val = data.frame(id = test$churn_flag, cost = pred)

confusionMatrix(test$churn_flag,pred)

#Tuned Model - Package pROC not loading -- DO NOT RUN
'''cvCtrl <- trainControl(method = "repeatedcv", repeats = 10,
                                              classProbs = TRUE)
dc1.tune<-train(churn_flag ~ ., data = train[,2:120], method = "rpart",
      tuneLength = 30,
      trControl = cvCtrl)
'''

#Tuning the Decision Tree
dc2<-rpart(churn_flag~., data=train[,2:120], method="class", control=rpart.control(minsplit = 20, cp = 0.01, 
                                                                                   maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                                                                                   surrogatestyle = 0, maxdepth = 30))
pred2=predict(dc2,test[,2:120], type="class")
dc2.val = data.frame(id = test$churn_flag, cost = pred2)

confusionMatrix(test$churn_flag,pred2)
#NO CHANGE in  ACCURACY





#----------------------Method 2-----------------#
#Random Forest

rf1 <- randomForest(churn_flag~., data=train[,2:120], ntree=500, importance=TRUE, method="response")
importance(rf1)

#Identify features that are important

imp <- importance(rf1, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
ggsave("C:/Users/bhatvm/Desktop/Kaggle 2016/Churn_Data Case Study/Analytical data set/feature_imp_RF.png",p)

plot(rf1)  #-- Cluttered as it shows too many variables


#Validation
pred.rf1=predict(rf1,test[,2:120], type="response")
rf1.val = data.frame(id = test$churn_flag, cost = round(pred.rf1,0))
confusionMatrix(test$churn_flag,round(pred.rf1,0))





#----------METHOD3-------------------#
#Support Vector Machines
library(e1071)

#Pick best variables from Decision Tree values (Easier) based on importance
yvars<-c("recency","sales_q1","visit_months","visits_last_6months")
model <- svm(churn_flag~recency+sales_q1+visit_months+visits_last_6months, data = train[,2:120])

#prediction and Validation
pred.svm=round(predict(model,test[,2:120]),0)
svm.val = data.frame(id = test$churn_flag, cost = pred.svm)
confusionMatrix(test$churn_flag,pred.svm)


#tune svm
tuned <- tune.svm(churn_flag~recency+sales_q1+visit_months+visits_last_6months, data = train[,2:120], gamma = 10^(-2:-1), cost = 1:(10^2))
print(tuned)

#Pick best Parameters
svm_afte_tune<- svm(churn_flag~recency+sales_q1+visit_months+visits_last_6months, data = train[,2:120], kernel="radial", cost=1, gamma=0.5)


#prediction and Validation
pred.svm1=round(predict(svm_afte_tune,test[,2:120]),0)
svm1.val = data.frame(id = test$churn_flag, cost = pred.svm1)
confusionMatrix(test$churn_flag,pred.svm1)


#----------------END OF PROGRAM-------------------#