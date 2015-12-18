pml.training <- read.csv("~/Desktop/pml-training.csv")
pml.testing <- read.csv("~/Desktop/pml-testing.csv")

pml.training<-pml.training[-1]
pml.testing<-pml.testing[-1]

library(caret)
library(FSelector)
library(randomForest)

set.seed(24)
training <- createDataPartition(y=pml.training$classe, p=0.6, list=FALSE)
set.training <- pml.training[training, ]
set.testing <- pml.training[-training, ]

set.training[set.training==""]<-NA
na.factor<-c()
for(i in 1:length(set.training)) { 
  if(sum(is.na( set.training[, i])) / nrow(set.training) > 0.5 ) { 
    na.factor<-c(na.factor,i)
  }   
    } 
na.factor
na.variables<-colnames(set.training[na.factor])
na.variables
set.training<-set.training[-na.factor]

summary(set.training)

weights<-information.gain(classe~.,set.training)
print(weights)
factor.train<-cutoff.k(weights,50)
factor.train

formula.train <- as.simple.formula(factor.train, "classe")

print(formula.train)

model.train <- randomForest(formula.train, data=set.training)

prediction.insample <- predict(model.train, set.testing, type = "class")
confusionMatrix(prediction.insample, set.testing$classe)



levels(pml.testing$new_window)<-
  levels(set.training$new_window)

levels(pml.testing$cvtd_timestamp)<-
  levels(set.training$cvtd_timestamp)

answers<-predict(model.train,pml.testing,type="class")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)