library(tidyverse)
data<-read.csv("C:/Users/Efat/Desktop/seneca/semester3/BDM300/project/country_wise_latest.csv")

#descriptive statistics
glimpse(data)
dim(data)
view(data)
names(data)
str(data)
attributes(data)
summary(data)
head(data)
library('Hmisc')
install.packages('psych')
install.packages('Hmisc')
describe(data)  
names(data)
range(data$Confirmed)
unique(coviddataset[c("WHO Region")])
range(coviddataset$Confirmed)
range(coviddataset$Deaths)
range(coviddataset$Recovered)
range(coviddataset$Active)
range(coviddataset$`New cases`)
range(coviddataset$`New deaths`)
range(coviddataset$`New recovered`)
range(coviddataset$`Deaths / 100 Cases`)
range(coviddataset$`Deaths / 100 Recovered`)
range(coviddataset$`Recovered / 100 Cases`)
range(coviddataset$`Confirmed last week`)
range(coviddataset$'1 week change')
range(coviddataset$'1 week % increase') 

#data visualization
#new death vs confirmed
data %>% 
+   ggplot(aes(New.deaths_binned, fill = Confirmed_binned))+
+   geom_bar(position = "dodge",
              +            alpha = 1)+
+   theme_bw()+
+   theme(panel.grid.major = element_blank(),
           +         panel.grid.minor = element_blank())


#new deaths vs new cases
data %>% 
+   ggplot(aes(New.deaths_binned, fill = New.cases_binned))+
+   geom_bar(position = "dodge",
              + alpha = 1)+
+ theme_bw()+
+ theme(panel.grid.major = element_blank(),
 +panel.grid.minor = element_blank())

# new death vs recovered
data %>% 
+   ggplot(aes(New.deaths_binned, fill = Recovered_binned))+
+   geom_bar(position = "dodge",
              +            alpha = 1)+
+   theme_bw()+
+   theme(panel.grid.major = element_blank(),
           +         panel.grid.minor = element_blank())

#new death vs active
data %>% 
+   ggplot(aes(New.deaths_binned, fill = Active_binned))+
+   geom_bar(position = "dodge",
              +            alpha = 1)+
+   theme_bw()+
+   theme(panel.grid.major = element_blank(),
           +         panel.grid.minor = element_blank())




#data binning

labels=c('low','medium_low','medium_high','high')
quantile(data$Confirmed)
breaks_Confirmed=c(10.0,1114.0,5059.0,40460.5,4290259.0)
data$Confirmed_binned<-cut(data$Confirmed,breaks=breaks_Confirmed,
                           lower=TRUE, right=FALSE,labels=labels)
data$Confirmed_binned

quantile(data$Active)
breaks_Active=c(0.0,141.5,1600.0,9149.0,2816444.0)
data$Active_binned<-cut(data$Active,breaks =breaks_Active,include.lowest=TRUE, 
                        right=FALSE,labels=labels )
data$Active_binned


quantile(data$Deaths)
breaks_Death=c(0.0,18.5,108.0,734.0,148011.0 )
data$Deaths_binned<-cut(data$Deaths,breaks =breaks_Death,include.lowest=TRUE, 
                        right=FALSE,labels=labels )
data$Deaths_binned


quantile(data$Recovered)
breaks_Recovered=c(0.0,626.5,2815.0,22606.0,1846641.0 )
data$Recovered_binned<-cut(data$Recovered,breaks =breaks_Recovered,include.lowest=TRUE, 
                        right=FALSE,labels=labels)
data$Recovered_binned


quantile(data$New.cases)
breaks_New.cases=c(0.0,4.0,49.0,419.5,56336.0)
data$New.cases_binned<-cut(data$New.cases,breaks =breaks_New.cases,include.lowest=TRUE, 
                           right=FALSE,labels=labels )
data$New.cases_binned


quantile(data$New.deaths)
breaks_New.deaths=c(0,1,6,1076)
data$New.deaths_binned<-cut(data$New.deaths,breaks =breaks_New.deaths,include.lowest=TRUE, 
                           right=FALSE,labels=c('low','medium','high'))
data$New.deaths_binned

#binning Recovered/100cases
quantile(data$Recovered...100.Cases)
breaks_Recovered...100.Cases=c(0.000,48.770,71.320,86.885,100.000 )
data$Recovered...100.Cases_binned<-cut(data$Recovered...100.Cases,breaks =breaks_Recovered...100.Cases,include.lowest=TRUE, 
                            right=FALSE,labels=labels)
data$Recovered...100.Cases_binned

#binning Deaths/100cases
quantile(data$Deaths...100.Cases)
breaks_Deaths...100.Cases=c(0.000,0.945,2.150,3.875,28.560)
data$Deaths...100.Cases_binned<-cut(data$Deaths...100.Cases,breaks =breaks_Deaths...100.Cases,include.lowest=TRUE, 
                                       right=FALSE,labels=labels)
data$Deaths...100.Cases_binned

#binning 1Week%increase
quantile(data$X1.week...increase)
breaks_X1.week...increase=c(-3.840,2.775,6.890,16.855,226.320)
data$X1.week...increase_binned<-cut(data$X1.week...increase,breaks =breaks_X1.week...increase,include.lowest=TRUE, 
                                    right=FALSE,labels=labels)
data$X1.week...increase_binned

#visualize the missing data
install.packages('naniar')
library(naniar)
vis_miss(data)


#correlation between Deaths & Recovered
cor(data$Deaths, data$Recovered)
table(data$Deaths_binned, data$Recovered_binned)
chisq.test(data$Deaths_binned, data$Recovered_binned,correct=FALSE)

#correlation between Deaths & New.cases
cor(data$Deaths, data$New.cases)
table(data$Deaths_binned, data$New.cases_binned)
chisq.test(data$Deaths_binned, data$New.cases_binned,correct=FALSE)

#correlation between Active & New.cases
cor(data$Active, data$New.cases)
table(data$Active_binned, data$New.cases_binned)
chisq.test(data$Active_binned, data$New.cases_binned,correct=FALSE)

#correlation between Death/100 cases & New.cases
cor(data$Deaths...100.Cases,data$New.cases)
table(data$Deaths...100.Cases_binned, data$New.cases_binned)
chisq.test(data$Deaths...100.Cases_binned, data$New.cases_binned,correct=FALSE)


#correlation between Death/100 cases & Recovered/100 cases
cor(data$Deaths...100.Cases,data$Recovered...100.Cases)
table(data$Deaths...100.Cases_binned, data$Recovered...100.Cases_binned)
chisq.test(data$Deaths...100.Cases_binned, data$Recovered...100.Cases_binned,correct=FALSE)


cor(data$New.cases,data$Confirmed.last.week)

#correlation between New cases & Recovered/100 cases
cor(data$Recovered...100.Cases,data$New.cases)
table(data$Recovered...100.Cases_binned, data$New.cases_binned)
chisq.test(data$Recovered...100.Cases_binned, data$New.cases_binned,correct=FALSE)


#correlation between X1.week...increase& New.cases
cor(data$X1.week...increase,data$New.cases)
table(data$X1.week...increase_binned, data$New.cases_binned)
chisq.test(data$X1.week...increase_binned, data$New.cases_binned,correct=FALSE)


#relationship between WHO.Region & New.deaths
table(data$WHO.Region,data$New.deaths_binned)
chisq.test(data$WHO.Region, data$New.deaths_binned,correct=FALSE)

#relationship between WHO.Region & New.cases
table(data$WHO.Region,data$New.cases_binned)
chisq.test(data$WHO.Region, data$New.cases_binned,correct=FALSE)

#Building a Decision tree with the binned variables 
#split data into training and test data sets
indexes = sample(1:nrow(data), size = 0.3*nrow(data))

#Test dataset 30%
Test_data = data[indexes,]    
dim(Test_data)  #56 24

#Train dataset 70%
Train_data = data[-indexes,] 
dim(Train_data)  #131 24

library(rpart)
des_tree=rpart(New.deaths_binned~New.cases_binned+Deaths...100.Cases_binned+
               Recovered...100.Cases_binned+X1.week...increase_binned+WHO.Region, data=data,method='class')

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart.plot)
library(RColorBrewer)

# plot des_tree
fancyRpartPlot(des_tree,main='decision tree on variables')
rpart.plot(des_tree,clip.right.labs = FALSE,branch = .3)

#extracting the rules of the decision tree
rpart.rules(des_tree,cover=TRUE)

#building the model with the unbinned variables (anova model)
anova_model=rpart(New.deaths~New.cases+Deaths...100.Cases+
                    Recovered...100.Cases+X1.week...increase+WHO.Region, data=data,method='class')
rpart.plot(anova_model)
rpart.rules(anova_model)

#Model Evaluation
Predicted_New.deaths <- predict(des_tree,newdata = Test_data,type='class')
Predicted_New.deaths

#confusion matrix
install.packages('caret')
library('caret')
con_matrix<-confusionMatrix(data=Predicted_New.deaths,reference=Test_data$New.deaths_binned)
con_matrix  #80.36% accuracy
#Mcnemar's Test P-Value : 0.4235 
#(the null hypothesis: the predicted values and the expected values are the same / accepted)
x<-varImp(des_tree,method='class')
plot(x)
?varImp


#random forest
install.packages('randomForest')
library('randomForest')
random_forest<-randomForest(New.deaths_binned~New.cases_binned+Deaths...100.Cases_binned+
               Recovered...100.Cases_binned+X1.week...increase_binned+WHO.Region,data = data,
             method='class', ntree=50, mtry=2, maxnodes = NULL)
random_forest

prediction <-predict(random_forest,Test_data)
prediction
confu_matrix<-confusionMatrix(prediction,Test_data$New.deaths_binned)
confu_matrix  #94.55% accuracy


#Naive Bayes Algorithm
install.packages('naivebayes')
library('naivebayes')
library('dplyr')
library('ggplot2')
library('psych')
str(data)
names(data)
#visualization

data%>%
  ggplot(aes(x=New.deaths_binned,y=New.cases,fill=New.deaths_binned))+
  geom_boxplot()

data%>%
  ggplot(aes(x=New.deaths_binned,y=Deaths...100.Cases,fill=New.deaths_binned))+
  geom_boxplot()

data%>%
  ggplot(aes(x=New.deaths_binned,y=Recovered...100.Cases,fill=New.deaths_binned))+
  geom_boxplot()

data%>%
  ggplot(aes(x=New.deaths_binned,y=X1.week...increase,fill=New.deaths_binned))+
  geom_boxplot()
#setting the model
naive_model<-naive_bayes(New.deaths_binned~New.cases_binned+Deaths...100.Cases_binned+
                           Recovered...100.Cases_binned+X1.week...increase_binned+WHO.Region,data=Train_data,laplace = 1)
#looking at the model
naive_model

#plotting the model
plot(naive_model)

#prediction
prediction1<-predict(naive_model, Train_data,type="prob")
head(cbind(prediction1, Train_data))
#the first country, has 0.5078 chance to have medium new.deaths, but the 
#actual new.deaths level is high.


#model evaluation
prediction2<-predict(naive_model, Test_data)
confu_matrix<-confusionMatrix(prediction2,Test_data$New.deaths_binned)
confu_matrix #96% accuracy


#KNN algorithm
names(data)
#choosing the subset of the dataset to run the algorithm on
data.subset<-data[c('New.deaths_binned','New.cases','Deaths...100.Cases','Recovered...100.Cases','X1.week...increase','WHO.Region')]
head(data.subset)

#normalizing the numerical variables
normalize<-function(x) {
  return((x-mean(x))/(max(x)-min(x)))
}
data.subset.norm<-as.data.frame(lapply(data.subset[2:5],normalize))
head(data.subset.norm)


#spliting the subset into training and testing datasets

indexes = sample(1:nrow(data.subset.norm), size = 0.3*nrow(data))
Train_data<-data.subset.norm[-indexes,] #70% training data
Test_data<-data.subset.norm[indexes,] #30% testing data

#creating a dataframe for 'New.deaths_binned' which is our target variable
Train_data_labels<-data.subset[-indexes,1]
Test_data_labels<-data.subset[indexes,1]

#installing the needed package
install.packages('class')
library(class)

#number of observations
NROW(Train_data_labels)  #131

#building the KNN model
knn.11<-knn(train=Train_data, test=Test_data,cl=Train_data_labels, k=11)
knn.12<-knn(train=Train_data, test=Test_data,cl=Train_data_labels, k=12)
knn.11
knn.12

#model evaluation
Acc.11<-sum(Test_data_labels==knn.11)/NROW(Test_data_labels)*100
Acc.12<-sum(Test_data_labels==knn.12)/NROW(Test_data_labels)*100
Acc.11  #Accuracy of the model with k=11  62.5%
Acc.12  #Accuracy of the model with k==12  67.86%
knn.13<-knn(train=Train_data, test=Test_data,cl=Train_data_labels, k=13)
Acc.13<-sum(Test_data_labels==knn.13)/NROW(Test_data_labels)*100
Acc.13  #Accuracy of the model with k==13  62.5%

#the confusion matrix
table(knn.12,Test_data_labels)

library(caret)
confusionMatrix(table(knn.12,Test_data_labels)) #Accuracy: 67.86%
                                                #p-value of Mcnemar test:0.67

#regression
rsq <- function(x, y) summary(lm(y~x))$r.squared
data<-as.data.frame(data)
typeof(data)

#New.cases,New.deaths
ggplot(data, aes(x=data$New.cases, y=New.deaths)) + geom_point()+geom_smooth(method=lm)
cor(data$New.cases, data$New.deaths)
rsq(data$New.cases,data$New.deaths)

#Deaths...100.Cases,New.deaths
ggplot(data, aes(x=data$Deaths...100.Cases, y=New.deaths)) + geom_point()+geom_smooth(method=lm)
cor(data$Deaths...100.Cases, data$New.deaths)
rsq(data$Deaths...100.Cases,data$New.deaths)

#Recovered...100.Cases,New.deaths
ggplot(data, aes(x=data$Recovered...100.Cases, y=New.deaths)) + geom_point()+geom_smooth(method=lm)
cor(data$Recovered...100.Cases, data$New.deaths)
rsq(data$Recovered...100.Cases,data$New.deaths)

#X1.week...increase,New.deaths
ggplot(data, aes(x=data$X1.week...increase, y=New.deaths)) + geom_point()+geom_smooth(method=lm)
cor(data$X1.week...increase, data$New.deaths)
rsq(data$X1.week...increase,data$New.deaths)
#######################

multi_reg<- lm(New.deaths ~ Deaths...100.Cases + X1.week...increase, data = data)
summary(multi_reg)

multi_reg_2<- lm(New.deaths ~ New.cases + X1.week...increase, data = data)
summary(multi_reg_2)

multi_reg_3<- lm(New.deaths ~ New.cases +  Deaths...100.Cases, data = data)
summary(multi_reg_3)

multi_reg_4<- lm(New.deaths ~ New.cases +  Deaths...100.Cases + Recovered...100.Cases , data = data)
summary(multi_reg_4)

multi_reg_5<- lm(New.deaths ~ New.cases +  Deaths...100.Cases + Recovered...100.Cases + X1.week...increase , data = data)
summary(multi_reg_5)

multi_reg_6<- lm(New.deaths ~  Deaths...100.Cases + Confirmed.last.week ,Active,Recovered...100.Cases, data = data)
summary(multi_reg_6)

multi_reg_7<- lm(New.deaths ~  Deaths  + Confirmed.last.week ,Active,Recovered...100.Cases, data = data)
summary(multi_reg_7)


