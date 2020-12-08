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
#interpret the model
print(summary(model))

#residual
res <- residuals(model)
class(res)
res <-as.data.frame(res)
head(res)
ggplot(res,aes(res))+ geom_histogram(fill='blue',alpha=0.5)
