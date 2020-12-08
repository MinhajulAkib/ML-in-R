#get the data
df <-read.csv('C:/Users/Akib/Desktop/R/creditcard.csv/student-mat.csv', sep =';')
head(df)
#summary
summary(df)
#NA value check
any(is.na(df))
#structure
str(df)
#install library
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
#numeric coloumn check only
num.cols <-sapply(df,is.numeric)
# filter
cor.data <-cor(df[,num.cols])
#correlation of numeric variables
print(cor.data)
#correlation packeges
#install.packages('corrgram')
#install.packages("corrplot")
#corrplot
print(corrplot(cor.data,method = 'color'))
#corrgram
#difference corrgram and corrplot
#corrplot is working on filter data,but corrgram working data frame directly
corrgram(df)
corrgram(df, order = TRUE,
         lower.panel = panel.shade, upper.panel = panel.pie)
#ggplot for g3
ggplot(df,aes(x= G3)) +geom_histogram(bins=20,alpha=0.5,fill='blue')


