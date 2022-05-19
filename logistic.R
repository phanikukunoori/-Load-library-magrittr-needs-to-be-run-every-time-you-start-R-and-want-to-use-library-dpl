#loading Packages
library(modelr)
library(broom)
mydata <- read.csv(file="C:/Users/Phani/Downloads/Chem_data.csv", header=T, stringsAsFactors=T)
# data cleansing
mydata <- mydata[,c(6:14)]
mydata

sum(is.na(mydata))

mydata <- mydata[rowSums(is.na(mydata)) == 0,]


sum(is.na(mydata))

head(mydata)
print(mydata)
summary(mydata)
#scale.data <- scale(mydata[,c(3:8)])

#scale.data

#mydata <- cbind(mydata, scale.data)
#mydata <- mydata[,c(1,2,10,11,12,13,14,15)]

mydata <- as.data.frame(mydata)
str(mydata)

library(caret)




#Creating the Training and Testing data set
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(mydata), replace = T, prob = c(0.6,0.4))
sample
train <- mydata[sample, ]
test <- mydata[!sample, ]

test
logmodel <- glm((PARASITE_STATUS~.), family = binomial(link = "logit"), data = train)
cbind(T4=mydata$., Prob=logmodel$fitted)
plot(PARASITE_STATUS ~ T4, data=mydata)
lines(mydata$T4, logmodel$fitted, type="l", col="red")
curve(predict(logmodel, data.frame(All = x), type = "response"), 
      add = TRUE, lwd = 3, col = "dodgerblue")
summary(logmodel)
confint(logmodel)
#install.packages("nagelkerke")
#library("nagelkerke")
#nagelkerke(logmodel)

summary(logmodel)
head(logmodel)

library(ggplot2)
library(tidyr)
mydata
#xrange(mydata$AGE+GLUC+NAK+T4+CRE)


#mydata$PARASITE_STATUS = predict(logmodel2, mydata, type="response")
#plot(PARASITE_STATUS ~AGE+GLUC+NAK+T4+CRE, data=test, col="steelblue")

#lines(PARASITE_STATUS ~AGE+GLUC+NAK+T4+CRE, mydata, lwd=2)

#Plotting a graph: Probability of PARASITE_STATUS, averAGE+GLUC+NAK+T4+CRE_AGE+GLUC+NAK+T4+CRE~BILI~AGE+GLUC+NAK+T4+CRE~NAK~TP~AGE+GLUC+NAK+T4+CRE~CRE
str(mydata)
mydata %>%
  mutate(prob = ifelse(PARASITE_STATUS == "Positive", 1, 0)) %>%
  ggplot(aes(AGE+ALB+BILI+GLUC+NAK+TP+T4+CRE, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("AGE+GLUC+NAK+T4+CRE") +
  ylab("Probability of PARASITE_STATUS")





logmodel <- glm(PARASITE_STATUS~AGE+GLUC+NAK+T4+CRE, family = "binomial", data =test )
summary(logmodel)

predict(logmodel, data.frame(AGE+GLUC+NAK+T4+CRE, type = "response")
summary(logmodel)$coefficients
 

