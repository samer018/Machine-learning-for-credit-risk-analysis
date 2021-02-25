install.packages("tidyverse")
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
install.packages("corrplot")
library(corrplot)
install.packages("GGally")
library(GGally)
install.packages("DT")
library(DT)
install.packages("DiagrammeR")
install.packages("https://s3.ca-central-1.amazonaws.com/jeremiedb/share/mxnet/CPU/mxnet.zip", repos = NULL)
library(mxnet)
install.packages("fastDummies")
library(fastDummies)

loan <- read.csv("C:/Users/emili/Downloads/loan.csv/loan.csv",header=T,sep=",")
name_select=c("loan_amnt", "emp_length", "grade", "term", "int_rate",
              "annual_inc", "application_type", "loan_status", "home_ownership" )
loans = loan[name_select]
proportion.missing <- sapply(loans, function(x) return((length(which(is.na(x)))/ length(x))))
loans1 <- loans[, which(proportion.missing <=0.1)]
loans1=na.omit(loans1) 
loans1=as.data.frame(loans1)



#transformation des variables

loans1 = loans1 %>%mutate(loan_outcome = ifelse(loan_status %in% c('Charged Off' , 'Default') , 1, 
                                                ifelse(loan_status == 'Fully Paid' , 0 , 'No info')))
loans1 = loans1 %>%filter(!is.na(annual_inc) , !(home_ownership %in% c('NONE' , 'ANY')) , 
                          emp_length != 'n/a')

data = loans1 %>%select(-loan_status) %>%filter(loan_outcome %in% c(0 , 1))
table(data$loan_outcome)
#17,2% de défaut



####étude de corrélation entre variables quantitatives
data2=data
data2$loan_outcome=as.numeric(data2$loan_outcome)
ind.quant <- sapply(data2, function(x) is.numeric(x) | is.integer(x))
ind.qual <- sapply(data2, function(x) is.factor(x))
# variables quantitative
Data.quant <- data2[ ,ind.quant]
# variables qualitative
Data.qual <- data2[ ,ind.qual]
library(corrplot)
mcor <- cor(Data.quant)
mcor
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


####séparation trainset/testset


set.seed(154) # initialisation du générateur
# Extraction des échantillons
test.ratio=.3   # part de l'échantillon test
npop=nrow(data) # nombre de lignes dans les données
# taille de l'échantillon test
ntest=ceiling(npop*test.ratio) 
# indices de l'échantillon test
testi=sample(1:npop,ntest)
# indices de l'échantillon d'apprentissage
appri=setdiff(1:npop,testi)



# construction de l'échantillon d'apprentissage
datappr=data[appri,] 
# construction de l'échantillon test
datestr=data[testi,] 


####random forest


install.packages("randomForest")
library(randomForest)
y_col=9 #à remplir par l'utilisateur, correspond au numéro de colone de la variable à expliquer
 ###recherche du mtry optimal à ntree fixé à 50


for(i in 1:9)
{
rf=randomForest(x=datappr[,-c(y_col)],y=as.factor(datappr[,"loan_outcome"]),xtest=datestr[,-c(y_col)],ytest=as.factor(datestr[,"loan_outcome"]),ntree=50,mtry=i,do.trace=50,importance=TRUE)

}


#recherche du ntree optimal avec le mtry optimal fixé
rf=randomForest(x=datappr[,-c(y_col)],y=as.factor(datappr[,"loan_outcome"]),xtest=datestr[,-c(y_col)],ytest=as.factor(datestr[,"loan_outcome"]),ntree=300,mtry=2,do.trace=50,importance=TRUE)
plot(rf$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")

#prédiction finale
rf=randomForest(x=datappr[,-c(y_col)],y=as.factor(datappr[,"loan_outcome"]),xtest=datestr[,-c(y_col)],ytest=as.factor(datestr[,"loan_outcome"]),ntree=150,mtry=2,do.trace=50,importance=TRUE)
varImpPlot(rf) #importance des variables
pred.rf.train=rf$predicted
pred.rf.train
pred.rf.test=rf$test$predicted
pred.rf.test
proba=rf$test$votes[,2]
predict_result_rf=cbind(datestr[1:10,],proba[1:10])
predict_result_rf=cbind(predict_result_rf,pred.rf.test[1:10])
install.packages("xtable")
library(xtable)
xtable(predict_result_rf)
confmat_rf_train=table(pred.rf.train,datappr[,"loan_outcome"])
xtable(confmat_rf_train)

confmat_rf_test=table(pred.rf.test,datestr[,"loan_outcome"])
confmat_rf_test
xtable(confmat_rf_test)

accuracy_rf_train= sum(diag(confmat_rf_train)) / sum(confmat_rf_train)
sensitivity_rf_train = confmat_rf_train[2 , 2] / sum(confmat_rf_train[ , 2])
specificity_rf_train = confmat_rf_train[1 , 1] / sum(confmat_rf_train[ , 1])
metrics=c("stability","accuracy-test","sensitivity-test","specificity-test")


accuracy_rf_test= sum(diag(confmat_rf_test)) / sum(confmat_rf_test)
sensitivity_rf_test = confmat_rf_test[2 , 2] / sum(confmat_rf_test[ , 2])
specificity_rf_test = confmat_rf_test[1 , 1] / sum(confmat_rf_test[ , 1])
stability_rf=(accuracy_rf_train-accuracy_rf_test)*100
results_rf=c(stability_rf,accuracy_rf_test,sensitivity_rf_test,specificity_rf_test)
results_rf_final=cbind(metrics,results_rf)
xtable(results_rf_final)


########glm
glm.model = glm(as.factor(loan_outcome) ~ . , datappr , family = binomial(link = 'logit'))
result=summary(glm.model)

xtable(result)
preds_test = predict(glm.model , datestr , type = 'response')
preds_binomial_test = ifelse(preds_test > 0.5 , 1 , 0)
predict_result_glm=cbind(datestr[11:20,],preds_test[11:20])
predict_result_glm=cbind(predict_result_glm,preds_binomial_test [11:20])
xtable(predict_result_glm)

confmat_glm_test = table(preds_binomial_test,datestr$loan_outcome )
xtable(confmat_glm_test)
accuracy_glm_test= sum(diag(confmat_glm_test)) / sum(confmat_glm_test)
sensitivity_glm_test = confmat_glm_test[2 , 2] / sum(confmat_glm_test[ , 2])
specificity_glm_test = confmat_glm_test[1 , 1] / sum(confmat_glm_test[ , 1])


preds_train = predict(glm.model , datappr , type = 'response')
preds_binomial_train = ifelse(preds_train > 0.5 , 1 , 0)
confmat_glm_train = table(preds_binomial_train,datappr$loan_outcome)
confmat_glm_train
xtable(confmat_glm_train)
accuracy_glm_train= sum(diag(confmat_glm_train)) / sum(confmat_glm_train)
sensitivity_glm_train = confmat_glm_train[2 , 2] / sum(confmat_glm_train[ , 2])
specificity_glm_train = confmat_glm_test[1, 1] / sum(confmat_glm_train[ , 1])

stability_glm=(accuracy_glm_train-accuracy_glm_test)*100
results_glm=c(stability_glm,accuracy_glm_test,sensitivity_glm_test,specificity_glm_test)
results_glm_final=cbind(metrics,results_glm)
xtable(results_glm_final)







#######################################################################################################################################
###réseau de neurones

data$grade=ifelse(data$grade=='A',1,ifelse(data$grade=='B',2,ifelse(data$grade=='C',3,
                                                                    ifelse(data$grade=='D',4,ifelse(data$grade=='E',5,ifelse(data$grade=='F',6,7))))))

data$term=ifelse(data$term=="36 months",1,0)
data$application_type=ifelse(data$application_type=="Individual",1,0)
data$home_ownership=ifelse(data$home_ownership=="OWN",1,ifelse(data$home_ownership=="MORTGAGE",2,3))

data$emp_length=ifelse(data$emp_length=='< 1 year',0,ifelse(data$emp_length=="1 year",1,ifelse(data$emp_length=="2 years",
                                                                                               2,ifelse(data$emp_length=="3 years",3,ifelse(data$emp_length=="4 years",4,ifelse(data$emp_length=="5 years",
                                                                                                                                                                                5,ifelse(data$emp_length=="6 years",6,ifelse(data$emp_length=="7 years",7,ifelse(data$emp_length=="8 years",
                                                                                                                                                                                                                                                                 8,ifelse(data$emp_length=="9 years",9,10))))))))))


# normalize data
data_nn=data
data_nn$grade=logb(1+data$grade,base=max(data$grade))
data_nn$loan_amnt=logb(1+data$loan_amnt,base=max(data$loan_amnt))
data_nn$annual_inc=logb(1+data$annual_inc,base=max(data$annual_inc))
data_nn$int_rate=logb(1+data$int_rate,base=max(data$int_rate))



data_nn$emp_length=logb(1+data_nn$emp_length,base=max(data_nn$emp_length))
data_nn$home_ownership=logb(1+data$home_ownership,base=max(data$home_ownership))


# create train data, test data
smp_size <- floor(0.70*nrow(data_nn))
set.seed(1)
train_ind <- sample(seq_len(nrow(data_nn)), size = smp_size)


train <- data_nn[train_ind, ]
test <- data_nn[-train_ind, ]

train.preds <- data.matrix(train[,1:8])

train.target<-as.numeric(train[,9])

test.preds <- data.matrix(test[,1:8])
test.target<-as.numeric(test[,9])




# for 1 hidden layer
# choosing learning.rate, suppose initial value of learning rate=0.01  
# nb of neurons in hidden layer =15, nb of neurons in outputlayer=2 (output is binary)

data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data,name='fc1', num_hidden=15)
act1=mx.symbol.Activation(fc1,name='relu1',act_type='relu')
fc2=mx.symbol.FullyConnected(act1,name='fc2', num_hidden=2)
lro <- mx.symbol.SoftmaxOutput(fc2)
mx.set.seed(0)

rate=0
maxlr=0
for (i in seq(0.01,0.6,by=0.01)){
  logger <- mx.metric.logger$new()
  model <- mx.model.FeedForward.create(
    lro, X=train.preds, y=train.target,eval.data=list(data=test.preds, label=test.target),
    ctx=mx.cpu(), num.round=2, array.batch.size=5000,
    learning.rate=i, momentum=0.9, eval.metric=mx.metric.accuracy,
    epoch.end.callback = mx.callback.log.train.metric(5, logger),array.layout = "rowmajor" )
  
  if(rate<max(logger$eval)){ 
    rate=max(logger$eval)
    maxlr=i
  }
  else next
}

#=> choose learning.rate=0.4
#choosing momentum

rate=0
mn=0
fc1 <- mx.symbol.FullyConnected(data,name='fc1', num_hidden=15)
act1=mx.symbol.Activation(fc1,name='relu1',act_type='relu')
fc2=mx.symbol.FullyConnected(act1,name='fc2', num_hidden=2)
lro <- mx.symbol.SoftmaxOutput(fc2)
for (i in seq(0.1,0.9,by=0.1)){
  
  mx.set.seed(0)
  logger <- mx.metric.logger$new()
  model <- mx.model.FeedForward.create(
    lro, X=train.preds, y=train.target,eval.data=list(data=test.preds, label=test.target),
    ctx=mx.cpu(), num.round=2, array.batch.size=5000,
    learning.rate=0.4, momentum=i, eval.metric=mx.metric.accuracy,
    epoch.end.callback = mx.callback.log.train.metric(5, logger),array.layout = "rowmajor" )
  
  if(rate<max(logger$eval)){ 
    rate=max(logger$eval)
    mn=i
  }
  else  next 
}

#momentum=0.7

#reload the code to see how the validation-accuracy change when nb of neuron in hidden layer varies
data <- mx.symbol.Variable("data")
for (i in 15:25){
  fc1 <- mx.symbol.FullyConnected(data,name='fc1', num_hidden=i)
  act1=mx.symbol.Activation(fc1,name='relu1',act_type='relu')
  fc2=mx.symbol.FullyConnected(act1,name='fc2', num_hidden=2)
  lro <- mx.symbol.SoftmaxOutput(fc2)
  mx.set.seed(0)
  logger <- mx.metric.logger$new()
  model <- mx.model.FeedForward.create(
    lro, X=train.preds, y=train.target,eval.data=list(data=test.preds, label=test.target),
    ctx=mx.cpu(), num.round=5, array.batch.size=5000,
    learning.rate=0.4, momentum=0.7, eval.metric=mx.metric.accuracy,
    epoch.end.callback = mx.callback.log.train.metric(5, logger),array.layout = "rowmajor" )
  
  pred_train=predict(model,train.preds,array.layout ='rowmajor')
  train.lab=max.col(t(pred_train))-1
  
  pred_test=predict(model,test.preds,array.layout ='rowmajor')
  test.lab=max.col(t(pred_test))-1
  
  
  confmat_nn_train=table(train.lab,train.target)
  confmat_nn_test=table(test.lab,test.target)
  
  sensitivity_nn_train = confmat_nn_train[2 , 2] / sum(confmat_nn_train[ , 2])
  specificity_nn_train = confmat_nn_train[1 , 1] / sum(confmat_nn_train[ , 1])
  
  sensitivity_nn_test = confmat_nn_test[2 , 2] / sum(confmat_nn_test[ , 2])
  specificity_nn_test = confmat_nn_test[1 , 1] / sum(confmat_nn_test[ , 1])
  
  print('acc_train, acc_test,sensitivity_nn_train,specificity_nn_train,sensitivity_nn_test,specificity_nn_test ')
  print(c(max(logger$train),max(logger$eval),sensitivity_nn_train,specificity_nn_train,sensitivity_nn_test,specificity_nn_test))
  
  
  next
}







#####################################################################################################################################################
########analyse critique des modèles : étude des sensibilités
data = loans1 %>%select(-loan_status) %>%filter(loan_outcome %in% c(0 , 1))

#########problème de déséquilibre (rééquilibrage en sous échantillonnant la classe majoritaire)

default_ind <- which(data$loan_outcome == 1)
ndefault_ind <- which(data$loan_outcome == 0)
nsamp <- min(length(default_ind), length(ndefault_ind))


pick_default <- sample(default_ind, nsamp)
pick_ndefault <- sample(ndefault_ind, nsamp)

new_data1 <- data[c(pick_default, pick_ndefault), ]
table(new_data1$loan_outcome)

####séparation trainset/testset
set.seed(154) # initialisation du générateur
# Extraction des échantillons
test.ratio=.3   # part de l'échantillon test
npop1=nrow(new_data1) # nombre de lignes dans les données
# taille de l'échantillon test
ntest1=ceiling(npop1*test.ratio) 
# indices de l'échantillon test
testi1=sample(1:npop1,ntest1)
# indices de l'échantillon d'apprentissage
appri1=setdiff(1:npop1,testi1)
# construction de l'échantillon d'apprentissage
datappr1=new_data1[appri1,] 
# construction de l'échantillon test
datestr1=new_data1[testi1,] 

####random forest
library(randomForest)
y_col=9 #à remplir par l'utilisateur, correspond au numéro de colone de la variable à expliquer

rf1=randomForest(x=datappr1[,-c(y_col)],y=as.factor(datappr1[,"loan_outcome"]),xtest=datestr1[,-c(y_col)],ytest=as.factor(datestr1[,"loan_outcome"]),ntree=150,mtry=2,do.trace=50,importance=TRUE)

#prédiction
pred.rf1=rf1$test$predicted
confmat_rf1=table(pred.rf1,datestr1[,"loan_outcome"])
confmat_rf1

accuracy_rf1= sum(diag(confmat_rf1)) / sum(confmat_rf1)
specificity_rf1 = confmat_rf1[1 , 1] / sum(confmat_rf1[ , 1])
sensitivity_rf1 = confmat_rf1[2 , 2] / sum(confmat_rf1[ , 2])

metricsbis=metrics
metricsbis[1]="proportion rééquilibrage"
results_rf1=c("50-50",accuracy_rf1,sensitivity_rf1,specificity_rf1)
results_rf1_final=cbind(metricsbis,results_rf1)
results_rf1_final

##test 2 (40-60)########################
pick_default2 <- sample(default_ind, nsamp)
pick_ndefault2 <- sample(ndefault_ind, nsamp*1.5)
new_data2<- data[c(pick_default2, pick_ndefault2), ]
table(new_data2$loan_outcome)

####séparation trainset/testset
set.seed(154) # initialisation du générateur
# Extraction des échantillons
test.ratio=.3   # part de l'échantillon test
npop2=nrow(new_data2) # nombre de lignes dans les données
# taille de l'échantillon test
ntest2=ceiling(npop2*test.ratio) 
# indices de l'échantillon test
testi2=sample(1:npop2,ntest2)
# indices de l'échantillon d'apprentissage
appri2=setdiff(1:npop2,testi2)


# construction de l'échantillon d'apprentissage
datappr2=new_data2[appri2,] 
# construction de l'échantillon test
datestr2=new_data2[testi2,] 

####random forest
library(randomForest)
y_col=9 #à remplir par l'utilisateur, correspond au numéro de colone de la variable à expliquer
rf2=randomForest(x=datappr2[,-c(y_col)],y=as.factor(datappr2[,"loan_outcome"]),xtest=datestr2[,-c(y_col)],ytest=as.factor(datestr2[,"loan_outcome"]),ntree=150,mtry=2,do.trace=50,importance=TRUE)

#prédiction

pred.rf2=rf2$test$predicted
confmat_rf2=table(pred.rf2,datestr2[,"loan_outcome"])
accuracy_rf2= sum(diag(confmat_rf2)) / sum(confmat_rf2)
specificity_rf2 = confmat_rf2[1 , 1] / sum(confmat_rf2[ , 1])
sensitivity_rf2 = confmat_rf2[2 , 2] / sum(confmat_rf2[ , 2])

results_rf2=c("40-60",accuracy_rf2,sensitivity_rf2,specificity_rf2)
results_rf2_final=cbind(metricsbis,results_rf2)

result_balance=cbind(results_rf1,results_rf2)
result_balance=cbind(metricsbis,result_balance)

xtable(result_balance)


############sensibilité seuil de proba###############
########glm
glm.model.seuil = glm(as.factor(loan_outcome) ~ . , datappr , family = binomial(link = 'logit'))
preds.seuil = predict(glm.model.seuil , datestr , type = 'response')

j= 0
good_prediction = c()
true_positive = c()
true_negative = c()
false_positive=c()
false_negative=c()
for(i in seq(from = 0.1 , to = 0.6 , by = 0.1)){
  j = j + 1
  preds_binomial_seuil = ifelse(preds.seuil >i , 1 , 0)
  confmat_seuil = table(preds_binomial_seuil,datestr$loan_outcome)
  good_prediction[j] = sum(diag(confmat_seuil)) 
  true_positive[j] = confmat_seuil[2 , 2] 
  true_negative[j] = confmat_seuil[1 , 1] 
  false_positive[j]=confmat_seuil[2 , 1] 
  false_negative[j]=confmat_seuil[1 , 2] 
}
threshold = seq(from = 0.1 , to = 0.6 , by = 0.1)
result_seuil = data.frame(threshold , good_prediction, true_positive , true_negative,false_positive,false_negative)
head(result_seuil)
xtable(result_seuil)





