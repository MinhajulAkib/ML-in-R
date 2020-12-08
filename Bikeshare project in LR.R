df <- read.csv("C:/Users/Akib/Desktop/ML in R/Bike-Sharing-Dataset/hour.csv")
head(df)


####EDA
library(ggplot2)
library(dplyr)
ggplot(df,aes(temp,cnt)) + geom_point()
ggplot(df,aes(temp,cnt)) + geom_point(alpha=0.3,aes(color=temp)) +theme_bw()

#convert to posixct()
df$dteday <-as.POSIXct(df$dteday)
pl <-ggplot(df,aes(dteday,cnt)) +geom_point(aes(color=temp),alpha=0.5)
print(pl)
pl +scale_color_continuous(low='#55D8CE',high='#FF6E2E') + theme_bw()

# correlation
cor(df[,c('temp','cnt')])
geom_boxplot()
ggplot(df,aes(factor(season),cnt)) +geom_boxplot(aes(color=factor(season)))
  +theme_bw()

#feature engineering
df$hr <- sapply(df$dteday,function(x){format(x,"%H")})
print(head(df))

#scatterolot
pl <- ggplot(filter(df,workingday==1),aes(casual,cnt))
pl <- pl +geom_point(position = position_jitter(w=1,h=0),aes(color=temp),alpha=0.3)
pl <- pl +scale_color_gradientn(colors = c('dark blue','blue','light blue','red','yellow','green','white','orange'))
print(pl)
print(pl + theme_bw())


#build model
temp.model <-lm(cnt ~temp,df)
print(summary(temp.model))

#how many bike rental counts at 25 c
-0.0356+381.2949*25
##
temp.test <-data.frame(temp=c(25))
temp.test
predict(temp.model,temp.test)











