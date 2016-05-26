#Reading data for ANOVA
mydata<-read.csv('Dosage.csv')

#Running ANOVA
?aov
aov.mydata<-aov(Alertness~Dosage, mydata)
summary(aov.mydata)

#Calculating Probability manually
pf(8.789, 2, 15, lower.tail = FALSE)

?model.tables
print(model.tables(aov.mydata, "means"), digit=3)

#Example 2 for ANOVA with 2 independant variables
mydata2<-read.csv('Dosage_gender.csv')
aov.mydata2<-aov(Alertness~Dosage+Gender, mydata2)
summary(aov.mydata2)
print(model.tables(aov.mydata2, "means"), digit=3)

##Anova on Regression
data(mtcars)
## Creating Factor Variables
mtcars$cyl <- factor(mtcars$cyl); 
mtcars$vs <- factor(mtcars$vs); 
mtcars$gear <- factor(mtcars$gear); 
mtcars$carb <- factor(mtcars$carb); 
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))
str(mtcars)

#RM
model1 <- lm(mpg ~ ., data = mtcars)
model2 <- lm(mpg ~ cyl+disp+hp+drat+wt+vs+am, data = mtcars)
model3 <- lm(mpg ~ am, data = mtcars)
## Adjusted R-suqre with all variables
summary(model1)$adj.r.squared
## Adjusted R-suqre with am variables
summary(model3)$adj.r.squared
## Summary of my initial model
summary(model2)$adj.r.squared

model_final <- step(model2, direction = "both", trace=0)
summary(model_final)
summary(model2)
summary(model1)

anova(model2, model_final)
anova(model3, model2)
anova(model1, model2)

# model2 is different from model_final and more effective- Null Hypothesis
# adding more variables leads to a better models -Null Hypothesis
#Thumb Rule 1- If p values < 0.05 keep the model with higher number of independent variables
#Thumb Rule 2- If p values > 0.05 keep the model with lower number of independent variables



##Alternative Hypothesis- Using the model with more number of variables does not improve model efficiency



##Predictive Regr
getwd()
##setwd('.//Assignment')
gre<-read.csv('binary.csv')

gre$RN<-1:400

#Subsetting Train Data
sub1<-nrow(gre)*1
Train_obs<-sub1*0.7


#Sampling
sample1<-gre[sample(1:sub1),]
T_Data<-sample1[1:Train_obs,]
V_Data<-sample1[(Train_obs+1):sub1,]

#Sorting
sort_T_data<-T_Data[order(T_Data$RN),]
sort_V_data<-V_Data[order(V_Data$RN),]

str(T_Data)
T_Data$admit<-factor(T_Data$admit)
#GLM Model
model1<-glm(admit~gre+as.factor(rank), 
            data=T_Data, family='binomial')
summary(model1)

V_Data$Preds<-predict(model1, newdata=V_Data, type='response')
V_Data$Adm<-0

V_Data$Adm[(V_Data$Preds > 0.44)]<-1
table(V_Data$Adm, V_Data$admit)

?cdplot
cdplot(as.factor(admit)~rank, data=T_Data)





