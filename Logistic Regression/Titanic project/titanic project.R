#DATA READ
adult<- read.csv("C:/Users/Akib/Desktop/dataset/CSV files for ML Projects/adult_sal.csv")
print(head(adult))
#
library(dplyr)
#adult <- select(adult,-X)
#print(str(adult))
#print(summary(adult))
#frequency
#table(adult$type_employer)


###Data Cleaning
#combine employer type
unemp <-function(job){
  job<-as.character(job)
  if(job=='Never-Worked' | job=='without-pay'){
    reture('unemployed')
  }else{
    return(job)
  }
}

#######
##APPLY
adult$type_employer <- sapply(adult$type_employer,unemp)
###
print(table(adult$type_employer))

##group self employed and state and local
group_emp <-function(job){
  if(job=='local_gov' | job=='state_gov'){
    reture('sl_gov')
  }else if(job=='self-emp-inc' | job=='self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)
###
print(table(adult$type_employer))


###Marital status
unemp <-function(job){
  job<-as.character(job)
  if(job=='Never-Worked' | job=='without-pay'){
    reture('unemployed')
  }else{
    return(job)
  }
}

####Maritul status
group_merital<-function(mar){
  mar<-as.character(mar)
  #not married
  if(mar=='separeted' | mar=='divorced' | mar='widowed'){
    return('not married')
  }
  #never_married
  else if(mar=='never-married'){
    return(mar)
  }
  #married
  else{
    return('married')
  }
    
}
##apply
adult$marital<-sapply(adult$marital,group_marital)

###country coloumn data clean
table(adult$country)

Asia<- c('China','Hong','India','Iran','Combodia','Japan','Laos','Philippines',
         'Vietnam','Taiwan','Thailand')
North.America<-c('Canada','United-States','Puerto-rico')
Europe<-c('England','France','Germany','Greece','Holand-Netherlands','Hungary',
          'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavis')
Ltin.and.South.America<-c('Columbia','Cuba','Dominican-Republic','Ecuador',
                          'El-Salvador','Guatemala','Haiti','Honduras',
                          'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)',
                          'Peru','Jamaica','Trinadad&Tobago')
other<-c('South')

group_country<-function(ctry){
  if(ctry %in% Asia){
    return('Asia')
  }else if(ctry %in% North.America){
    return('North.America')
  }else if(ctry %in% Europe){
    return('Europe')
  }else if(ctry %in% Latin.and.South.America){
    return(' Latin.and.South.America')
  }else{
    return('other')
  }
    
}
adult$country <-sapply(adult$country,group_country)
# print(table(adult$country))
  
###factor
#adult$type_employer<-sapply(adult$type_employer,factor)
#adult$country<-sapply(adult$country,factor)
#adult$marital<-sapply(adult$marital,factor)
#print(str(adult))


##Missing Data
library(Amelia)
adult[adult=='?'] <-NA
#print(table(adult$type_employer))
adult$type_employer<-sapply(adult$type_employer,factor)
adult$country<-sapply(adult$country,factor)
adult$marital<-sapply(adult$marital,factor)
print(table(adult$type_employer))


missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#Drop missindg data
adult<-na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


##3library
library(ggplot2)
library(dplyr)
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) +theme_bw()
ggplot(adult,aes(hr_per_week)) + geom_histogram() +theme_bw()

head(adult)
#rename country to region
adult <-rename(adult,region = country)
print(head(adult))
pl<-ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black') +theme_bw()
pl


####logistic regression model
help('glm')
library(caTools)
set.seed(101)
sample <- sample.split(adult$income,SplitRatio = 0.7)
#train
train<-subset(adult,sample=T)
#TEST
test <-subset(adult,sample==F)

#model
model <- glm(income ~ . ,family = binomial())
model <- glm(income ~  . ,family=binomial(link='logit'),data=train)
summary(model)
#train the split
new.step.model<-step(model)







