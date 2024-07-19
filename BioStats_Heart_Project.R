#Importing data set
ht=read.csv(file.choose(),header=TRUE)
ht
View(ht)
attach(ht)
dim(ht)
ls(ht)
install.packages("dplyr")
library(dplyr)
ht=mutate(ht,age_group=cut(Age,breaks = c(29,45,61,78),labels = c("29-44","45-60","61-77"),right = FALSE))
ht
View(ht)

 #checking missing values
sum(is.na(ht))

#descriptive statistics
summary(ht)

#creating subsets
ht1=subset(ht,Class==1)
View(ht1)
dim(ht1)
ht2=subset(ht,Class=="2")
View(ht2)
dim(ht2)
summary(ht1)
summary(ht2)

########checking for outliers#########

boxplot(ht$Age~ht$Class)
boxplot.stats(ht1$Age)$out       #outlier values of Age
boxplot.stats(ht2$Age)$out
boxplot(ht$Sch~Class)
boxplot.stats(Sch)$out           #outlier values of Sch
boxplot(BP~Class)
boxplot.stats(BP)$out            #outlier values of BP
boxplot(Mhrt~Class)
boxplot.stats(ht1$Mhrt)$out      #outlier values of Mhrt
boxplot(opk~Class)
boxplot.stats(opk)$out           #outlier values of opk

 ########checking for normality###########

qqnorm(ht$Age,col="blue")
qqline(ht$Age,col="red",lwd=2)
library(car)
qqPlot(ht$Age)
qqnorm(ht$BP,col="blue")
qqline(ht$BP,col="red",lwd=2)
qqPlot(ht$BP)
qqnorm(ht$Sch,col="blue")
qqline(ht$Sch,col="red",lwd=2)
qqPlot(ht$Sch)
qqnorm(ht$Mhrt,col="blue")
qqline(ht$Mhrt,col="red",lwd=2)
qqPlot(ht$Mhrt)
qqnorm(ht$opk,col="blue")
qqline(ht$opk,col="red",lwd=2)
qqPlot(ht$opk)

#another way
shapiro.test(ht$Age)
shapiro.test(BP)
shapiro.test(Sch)
shapiro.test(Mhrt)
shapiro.test(opk)
ks.test(ht$opk,"pnorm")

#Data visualizations
library(ggplot2)

#visualisation of univariate category variable of sex using bar chart
ggplot(ht,aes(x=factor(Sex),fill=factor(Sex)))+geom_bar()+geom_text(stat="count",aes(label=stat(count)))+
  labs(x="Sex",y="Count",title="Barchart of Sex")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
  axis.text = element_text(size = 12),  # Adjust axis text size
  axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

t1=table(Sex)
piepercent=round(t1/(sum(t1))*100)
piepercent
pie(t1,piepercent)

#visualisation of univariate category variable of Chp using barchart
table(Chp)
ggplot(ht,aes(x=factor(Chp),fill=factor(Chp)))+geom_bar()+geom_text(stat="count",aes(label=..count..))+
  labs(x="Chp",y="count",title="Barchart of Chp")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

#visualisation of Fbs variable using barchart
table(Fbs)
ggplot(ht,aes(x=factor(Fbs),fill=factor(Fbs)))+geom_bar()+geom_text(stat="count",aes(label=..count..))+
  labs(x="Fbs",y="count",title="Barchart of Fbs")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

#visualisation of Ecg variable using barchart
table(Ecg)
ggplot(ht,aes(x=factor(Ecg),fill=factor(Ecg)))+geom_bar()+geom_text(stat="count",aes(label=..count..))+
  labs(x="Ecg",y="count",title="Barchart of Ecg")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

#visualisation of Exian variable using barchart
table(Exian)
ggplot(ht,aes(x=factor(Exian),fill=factor(Exian)))+geom_bar()+geom_text(stat="count",aes(label=..count..))+
  labs(x="Exian",y="count",title="Barchart of Exian")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

#visualisation of Vessels variable using barchart
table(Vessel)
ggplot(ht,aes(x=factor(Vessel),fill=factor(Vessel)))+geom_bar()+geom_text(stat="count",aes(label=..count..))+
  labs(x="Vessel",y="count",title="Barchart of Vessel")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

# visualisation of Thal variable using barchart 
table(Thal)
ggplot(ht,aes(x=factor(Thal),fill=factor(Thal)))+geom_bar()+geom_text(stat="count",aes(label=..count..))+
  labs(x="Thal",y="count",title="           Barchart of Thal")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") ) # Customize axis title

#visualisation of Age variable using various plots
ggplot(ht,aes(x=Age))+geom_density(kernel="gaussian")
ggplot(ht,aes(x=Age))+geom_area(stat="bin")
ggplot(ht,aes(x=Age))+geom_bar(width=3,fill="#FF6666")

#visualisation of BP variable using various plots
ggplot(ht,aes(x=BP))+geom_density()
ggplot(ht,aes(x=BP))+geom_bar(width=4.5)
ggplot(ht,aes(x=BP))+geom_area(stat="bin")

#visualisation of Sch variable using various plots
ggplot(ht,aes(x=Sch))+geom_density()
ggplot(ht,aes(x=Sch))+geom_bar(width = 10)
ggplot(ht,aes(x=Sch))+geom_area(stat="bin")

#visualisation of Mhrt variable using various plots
ggplot(ht,aes(x=Mhrt))+geom_density()
ggplot(ht,aes(x=Mhrt))+geom_area(stat="bin")
ggplot(ht,aes(x=Mhrt))+geom_bar(width = 7)

#Visualisation of opk variable using various plots
ggplot(ht,aes(x=opk))+geom_freqpoly()
ggplot(ht,aes(x=opk))+geom_histogram(binwidth = 0.5)
ggplot(ht,aes(y=opk))+geom_boxplot()

##sub objective 1:Impact of various factors on the target variable##
#impact of Age on the class variable
table(Class)
ggplot(ht,aes(x=age_group,fill=factor(Class)))+geom_bar(position="dodge")+labs(title = "Distribution of Class Variable within Age Groups",
        x = "Age Group",y = "Count")+geom_text(stat = "count",aes(label=stat(count),hjust=0.5,vjust=0.5))
ggplot(ht,aes(y=Age,x=factor(Class),fill=factor(Class)))+geom_violin()
ggplot(ht,aes(y=Age,x=factor(Class),fill=factor(Class)))+geom_boxplot()
chisq.test(ht$age_group,Class)
table(ht$age_group,Class)

#impact of Sex on class variable
ggplot(ht,aes(x=factor(Sex),y=factor(Class),fill=factor(Sex)))+geom_count()
ggplot(ht,aes(x=Sex,fill=factor(Class)))+geom_bar(position = "dodge")+
  geom_text(stat = "count",aes(label=..count..),size=5)+
  labs(x="Sex",y="Count",title = "Barchart of Sex wrt Class")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold"))
chisq.test(Sex,Class)
table(Sex,Class)

#impact of Chp on class variable
ggplot(ht,aes(x=factor(Chp),fill=factor(Class)))+geom_bar(position = "dodge")+
  geom_text(stat = "count",aes(label=..count..),size=5)+
  labs(x="Chp",y="Count",title = "Barchart of Chp wrt Class")+
  theme(plot.title = element_text(size = 18, face = "bold"),  # Customize plot title
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14, face = "bold") )
ggplot(ht,aes(x=Chp,y=factor(Class),fill=factor(Class)))+geom_violin()
chisq.test(Chp,Class)

#impact of BP on class variable
ggplot(ht,aes(y=BP,x=factor(Class),fill=factor(Class)))+geom_violin()+geom_boxplot(width=0.4,fill="white",color="black")+
       labs(x="Class",y="BP",title="Violin Plot of BP wrt Class")
ggplot(ht,aes(y=BP,x=factor(Class),fill=factor(Class)))+geom_boxplot()
ggplot(ht,aes(x=BP,fill=factor(Class)))+geom_dotplot(dotsize = 0.7)+labs(x="BP",y="Count",title="DotPlot of BP wrt Class")
wilcox.test(BP,Class)

#impact of Sch on class variable
ggplot(ht,aes(y=Sch,x=factor(Class),fill=factor(Class)))+geom_violin()+
  labs(x="Class",y="Sch",title="Violin Plot of Sch wrt Class")
ggplot(ht,aes(y=Sch,x=factor(Class),fill=factor(Class)))+geom_boxplot()
ggplot(ht,aes(x=Sch,fill=factor(Class)))+geom_dotplot(dotsize = 0.7)+labs(x="Sch",y="Count",title="DotPlot of Sch wrt Class")
wilcox.test(Sch,Class)

#impact of Fbs on class variable
ggplot(ht, aes(x = factor(Fbs), fill = factor(Class))) +geom_bar(position = "dodge")+
  labs(title = "Fasting Blood Sugar (Fbs) by Class",x = "Fbs",y = "Count")+
  geom_text(stat="count",aes(label=..count..),size=5)
wilcox.test(Fbs,Class)#test for medians of two groups are equal or not
chisq.test(Fbs,Class)

prop.test(table(Fbs,Class))# checking the proportionality of two variables

#impact of Ecg on class variable
ggplot(ht, aes(x = factor(Ecg), fill = factor(Class))) +geom_bar(position = "dodge")+
  labs(title = "Stacked barchart of Ecg wrt Class",x = "Ecg",y = "Count")+
  geom_text(stat="count",aes(label=..count..),size=5)
chisq.test(Ecg,Class)

#impact of Mhrt on class variable
ggplot(ht,aes(y=Mhrt,x=factor(Class),fill=factor(Class)))+geom_violin()+
  labs(x="Class",y="Mhrt",title="Violin Plot of Mhrt wrt Class")
ggplot(ht,aes(y=Mhrt,x=factor(Class),fill=factor(Class)))+geom_boxplot()
ggplot(ht,aes(x=Mhrt,fill=factor(Class)))+geom_dotplot(dotsize = 0.7)+labs(x="Mhrt",y="Count",title="DotPlot of Mhrt wrt Class")
wilcox.test(Mhrt,Class)

#impact of Exian on class variable
ggplot(ht, aes(x = factor(Exian), fill = factor(Class))) +geom_bar(position = "dodge")+
  labs(title = "Barplot of Exian wrt Class",x = "Exian",y = "Count")+
  geom_text(stat="count",aes(label=..count..),size=5)
wilcox.test(Exian,Class)#test for medians of two groups are equal or not
chisq.test(Exian,Class)

#impact of opk on class variable
ggplot(ht,aes(x=opk,y=factor(Class),fill=factor(Class)))+geom_dotplot(binwidth = 0.1)
ggplot(ht,aes(y=opk,x=factor(Class),fill=factor(Class)))+geom_violin()

#impact of slope on class variable
ggplot(ht,aes(x=slope,fill=factor(Class)))+geom_bar(position = "dodge")+
        geom_text(stat = "count",aes(label=..count..),size=5)
ggplot(ht,aes(x=slope,y=factor(Class),fill=factor(Class)))+geom_violin()
kruskal.test(Class~slope)#test for distributions of groups equal or not
dunn.test(Class,slope)    ##To know which pairwise variables of the distributions
chisq.test(slope,Class)    #are significantly not equal##
   
#impact of vessels on class variable
ggplot(ht,aes(x=Vessel,y=factor(Class),fill=factor(Class)))+geom_violin()
ggplot(ht,aes(x=factor(Vessel),fill=factor(Class)))+geom_bar(position="dodge")+
  geom_text(stat = "count",aes(label=..count..),size=5)
chisq.test(Vessel,Class)

#impact of Thal on class variable
table(Thal)
ggplot(ht,aes(x=factor(Thal),fill=factor(Class)))+geom_bar(position="dodge")+
  geom_text(stat = "count",aes(label=..count..),size=4)
chisq.test(Thal,Class)


# Fit the logistic regression model
install.packages("caTools") 
install.packages("ROCR")     
library(caTools)
library(ROCR)
split=sample.split(ht, SplitRatio = 0.8)
split
train=subset(ht,split=="TRUE")
View(train)
test=subset(ht,split=="FALSE")
View(test)

####checking dimensions of train and test split####
dim(train)
dim(test)
ls(ht)
####Building Logistic Regression model####
logistic_model=glm(Class~Age+BP+Chp+Ecg+Exian+Mhrt+opk+Sch+Sex+slope+Thal+Vessel,
                   data=train)
logistic_model
summary(logistic_model)

pred_model=predict(logistic_model,test,type="response")
pred_model

pred_model=ifelse(pred_model>1.5,2,1)
pred_model

#####Confusion matrix#####
table(test$Class,pred_model)
missing_classer=mean(pred_model != test$Class)
missing_classer
print(paste('Accuracy =', 1 - missing_classer))

#######ROC-AUC  curve########

install.packages("ROCR")
library(ROCR)

ROCPred <- prediction(pred_model, test$Class)
ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

plot(ROCPer)
plot(ROCPer, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1),
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

######AIC OF THE MODEL########
AIC(logistic_model)
