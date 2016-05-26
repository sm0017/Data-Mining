##### STA 588 
##### Random Forests Presentation Case Study
##### Tatiana Romanchishina, Smita Sukhadeve & John Charles

##### load libraries
library(randomForest)
library(caret)

###### random seed
set.seed(1234)

##### load the data
##### if the file is not in your home directory,
##### modify this path to reflect the real location
wt <- read.csv("MEPS_12_13.csv")

##### relevel the response variable to ensure that 
##### NotAdmitted = 0, Admitted = 1
##### otherwise, might default to Admitted = 0 due to alphabetic order
wt[,12] <- relevel(wt[,12], "NotAdmittedY2")

##### Description of variables:
##### "AGE" - num, age of participant
##### "ERTotExpen" - num, ER total expenditure for year 1
##### "MetStatArea" - cat, Metropolitan Statistical Area or not (binary)
##### "InsCoverage" - cat, insurance coverage: private, public or uninsured
##### "TotPrescr" - num, total number of prescriptions for year 1
##### "TotHealthExpen" - num, total health expenditure for year 1
##### "PhysSummary" - num, physical health summary indicator 
#####                       (imputed from SF-12)
##### "MentSummary" - num, mental health summary indicator 
#####                       (imputed from SF-12)   
##### "USRegion" - cat, NE, MW, S, W
##### "SEX" - cat, sex of participant
##### "WageIncome" - num, wage income
##### "AdmittedY2" - RESPONSE - cat, admitted next year or not (binary)

##### Divide data into train and test sets
##### May be unnecessary when working with one model and not planning 
##### to try different probability cutoff points.
##### However in this case we are comparing different models and 
##### looking at the probability cutoff points
train1 <- sample(1:nrow(wt), 2*nrow(wt)/3)
test1 <- wt[-train1, ]
train1 <- wt[train1, ]

##### Fit a Random Forest model with the default value of mtry.
##### do.trace will print the report for that number of trees
##### importance=T includes 2 different important measures
rf1 <- randomForest(AdmittedY2 ~ ., data = train1, 
                    mtry = floor(sqrt(ncol(train1))), ntree = 501, 
                    do.trace = 100, importance = T)
print(rf1)

##### variable importance
##### print
importance(rf1)

##### plot
varImpPlot(rf1)

##### produce a matrix of class probabilities 
##### (one column for each class and one row for each participant)
probs <- predict(rf1, type = "prob", test1)

##### try different probability cut-off points
preds <- rep(0, nrow(test1))
preds[probs[,2] > 0.5] = "AdmittedY2"
preds[probs[,2] <= 0.5] = "NotAdmittedY2"
preds <- relevel(factor(preds), "NotAdmittedY2")
prop.table(table(preds, test1$AdmittedY2))

preds <- rep(0, nrow(test1))
preds[probs[,2] > 0.3] = "AdmittedY2"
preds[probs[,2] <= 0.3] = "NotAdmittedY2"
preds <- relevel(factor(preds), "NotAdmittedY2")
prop.table(table(preds, test1$AdmittedY2))

preds <- rep(0, nrow(test1))
preds[probs[,2] > 0.1] = "AdmittedY2"
preds[probs[,2] <= 0.1] = "NotAdmittedY2"
preds <- relevel(factor(preds), "NotAdmittedY2")
prop.table(table(preds, test1$AdmittedY2))

##### function from STA 588 by Prof. Cheng Peng
##### Note #7 ROC Analysis on Logistic Classifiers
SenSpec=function(model.name, dataset, cut.off.prob.seq){
  cut.off.prob.seq = as.vector(cut.off.prob.seq)
  np=length(cut.off.prob.seq)
  prob.good=predict(model.name, newdata=dataset, type="prob")
  ###### Here the appropriate response column should be specified
  true.class=dataset$AdmittedY2
  perf.matrix=matrix(rep(0,2*np), byrow=TRUE, ncol=2)
  for(i in 1:np){
    pred.class=true.class
    ##### adding indexing for second column --> prob of Admitted
    pred.class[which(prob.good[,2] >= cut.off.prob.seq[i])] = "AdmittedY2"
    pred.class[which(prob.good[,2] < cut.off.prob.seq[i])] = "NotAdmittedY2"
    confusion=ftable(pred.class, true.class)
    specificity=confusion[1,1]/sum(confusion[,1])
    sensitivity=confusion[2,2]/sum(confusion[,2])
    perf.matrix[i,1] = sensitivity
    perf.matrix[i,2] = specificity
  }
  colnames(perf.matrix)=c("sensitivity", "specificity")
  perf.matrix
}
###################
SenSpec1 <- SenSpec(model.name=rf1, dataset=test1, 
                    cut.off.prob.seq=seq(0,1,length=20))
SenSpec1
TPF.1 = SenSpec1[,1]
FNF.1 = 1 - SenSpec1[,2] 
##### ROC curve
plot(FNF.1, TPF.1, xlim=c(0,1), ylim=c(0,1), type="b", lty=1, col="blue",
     xlab="1 - specificity", ylab="sensitivity", main="ROC Curve")

##### Explore options for tuning parameters
##### {randomForest} tuneRF

set.seed(5678)
##### tuneRF - x and y have to be specified separately
trf <- tuneRF(train1[,-12], train1[,12], ntreeTry=501,
       stepFactor=1.5,improve=1e-8, trace=F, plot=T)
##### store best mtry in a variable
best.m <- trf[trf[, 2] == min(trf[, 2]), 1]

rf2 <- randomForest(AdmittedY2~., data=train1, mtry = best.m, 
                    ntree = 501, do.trace = 100, importance = T)
print(rf2)
importance(rf2)
varImpPlot(rf2)

##### 
probs <- predict(rf2, type = "prob", test1)

preds <- rep(0, nrow(test1))
preds[probs[,2] > 0.5] = "AdmittedY2"
preds[probs[,2] <= 0.5] = "NotAdmittedY2"
preds <- relevel(factor(preds), "NotAdmittedY2")
prop.table(table(preds, test1$AdmittedY2))

preds <- rep(0, nrow(test1))
preds[probs[,2] > 0.3] = "AdmittedY2"
preds[probs[,2] <= 0.3] = "NotAdmittedY2"
preds <- relevel(factor(preds), "NotAdmittedY2")
prop.table(table(preds, test1$AdmittedY2))

preds <- rep(0, nrow(test1))
preds[probs[,2] > 0.1] = "AdmittedY2"
preds[probs[,2] <= 0.1] = "NotAdmittedY2"
preds <- relevel(factor(preds), "NotAdmittedY2")
prop.table(table(preds, test1$AdmittedY2))

SenSpec2 <- SenSpec(model.name=rf2, dataset=test1, 
                    cut.off.prob.seq=seq(0,1,length=20))
SenSpec2
TPF.2 = SenSpec2[,1]
FNF.2 = 1 - SenSpec2[,2] 
plot(FNF.2, TPF.2, xlim=c(0,1), ylim=c(0,1), type="b", lty=1, col="blue",
     xlab="1 - specificity", ylab="sensitivity", main="ROC Curves")
lines(FNF.1, TPF.1, type="b", lty=3, col="purple")


##### {caret} train function for tuning
##### for more info: http://topepo.github.io/caret/training.html

###### the following procedures may take long 
###### (depending on # of combinations of parameters to be tuned)

##### random search 

##### specify the resampling method
control <- trainControl(method="repeatedcv", number=3, 
                        repeats=1, search="random")

set.seed(11111)
##### tuneLength is how many unique random combinations will be considered
rf_random <- train(AdmittedY2 ~ ., data = train1, method="rf",
                   tuneLength=11, trControl=control)
print(rf_random)
plot(rf_random, main="Tuning a RF parameter with random search")

###### grid search

control <- trainControl(method="repeatedcv", number=3, 
                        repeats=1, search="grid")
set.seed(232)

##### grid of parameters to consider
tunegrid <- expand.grid(.mtry=c(1:11))
rf_gridsearch <- train(AdmittedY2 ~ ., data = train1, method ="rf", 
                       tuneGrid = tunegrid, trControl = control)
print(rf_gridsearch)
plot(rf_gridsearch, main="Tuning a RF parameter with grid search")

####### Explore a grid of multiple parameters (covered very briefly in presentation)
####### by extending (!!!) the caret package
####### (covered in more detail below)

##### the following solutions were found here:
##### http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
##### (There is a manual and a custom solution in the link above)
##### Manual solution was not covered in class but is pretty easy. 
##### Custom solution seems harder, but is very neat

##### Here is a manual solution from the above link, modified:
control <- trainControl(method="repeatedcv", number=3, resamples="all",
                        repeats=1, search="grid")

##### grid of mtry values
tunegrid <- expand.grid(.mtry=c(1:3))

modellist <- list()
##### the values for the number of trees we want to try
#### for each of those values we will train a model
#### and store all results in a list for later
for (ntree in c(100, 200, 500)) {
  set.seed(8234)
  fit <- train(AdmittedY2 ~ ., data=train1, method="rf", metric="Accuracy",
               tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results using the list from above

results <- resamples(modellist)
summary(results)
#dotplot(results) ### gives an error 

##### Custom solution:

##### What happens now is we define a custom Random Forests function
##### to pass it to the train function from caret. 
##### Each of the elements defined below, is an element that the caret
##### package expects. 
##### More info here: http://topepo.github.io/caret/custom_models.html
#####Our custom Random forest 

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), 
                                  class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

######## 
control <- trainControl(method="repeatedcv", number=3, repeats=1)
tunegrid <- expand.grid(.mtry=c(1:3), .ntree=c(50, 100, 150))
set.seed(789)
custom <- train(AdmittedY2 ~ ., data=train1, method=customRF, 
                tuneGrid=tunegrid, trControl=control)
custom
plot(custom, main="Tuning RF parameters with custom search")


###### Use the parameters from previous search results
rf3 <- randomForest(AdmittedY2 ~ ., data = train1, mtry = 6, 
                    ntree = 1500, do.trace = 300, importance = T)
importance(rf3)
varImpPlot(rf3)

SenSpec3 <- SenSpec(model.name=rf3, dataset=test1, 
                    cut.off.prob.seq=seq(0,1,length=20))
SenSpec3
TPF.3 = SenSpec3[,1]
FNF.3 = 1 - SenSpec3[,2] 
plot(FNF.3, TPF.3, xlim=c(0,1), ylim=c(0,1), type="b", lty=1, col="blue",
     xlab="1 - specificity", ylab="sensitivity", main="ROC Curves")
abline(0,1, lty=2, col="red")
lines(FNF.1, TPF.1, type="b", lty=3, col="purple")
lines(FNF.2, TPF.2, type="b", lty=3, col="darkgreen")
legend("bottomright", c("RF1","RF2","RF3", "Random Guess"), 
       col=c("purple", "darkgreen", "blue","red"), lty=c(3,3,1,2), bty="n")








