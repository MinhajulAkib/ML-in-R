#get the data
df <-read.csv('C:/Users/Akib/Desktop/R/creditcard.csv/student-mat.csv', sep =';')
#split data into train and test set
library(caTools)
#set a seed
set.seed(101)
#split up sample
sample <-sample.split(df$G3,SplitRatio = 0.7)
#70% of data train
train <-subset(df,sample==TRUE)
#30% of data test
test<-subset(df,sample==FALSE)
#run  model
#the general model of building a lineraregression model in R
#look like this
#train and build model
model <-lm(G3 ~ . , data=train)
plot(model)
#predictions
G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3)
colnames(results) <-c('predicted','actual')
results <- as.data.frame(results)
#print(head(results))


#negative values
to_zero <-function(x){
  if(x<0){
    return(0)
  }else{
    return(x)
  }
    
}
#apply zero function
results$predicted <-sapply(results$predicted,to_zero)

#mean square error
mse <-mean((results$actual-results$predicted)^2)
print('MSE')
print(mse)
#RMSE
print("square root of mse")
print(mse^0.5)

###
sse <-sum((results$predicted-results$actual)^2)
sst <-sum((mean(df$G3)-results$actual)^2)

r2<- 1-sse/sst
print('r2')
print(r2)