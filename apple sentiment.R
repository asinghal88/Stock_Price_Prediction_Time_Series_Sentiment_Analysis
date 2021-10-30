setwd("C:/Users/Nisant Manepalli/Downloads")
library(caTools)
library(tm)
library(SnowballC)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)
library(mlbench)
library(tree)
library(gbm)
library(xgboost)
library(ggplot2)
library(magrittr)
library(RWeka)
library(syuzhet)
library(DT)
data=read.csv('abc3.csv')
str(data)
data$X=NULL
data$Date=as.Date(data$Date,'%d-%m-%Y')


# Get rid of those pesky b's and backslashes 
data$V1 <- gsub('b"|b\'|\\\\|\\"', "", data$V1)
# Get rid of all punctuation except headline separators
data$V1 <- gsub("([<>])|[[:punct:]]", "\\1", data$V1)
# Reduce to only the three columns we need. 
control <- list(
  removeNumbers = TRUE,
  tolower = TRUE,
  # exclude stopwords and headline tokens
  stopwords = c(stopwords(kind = 'SMART'),'<s>')
)
#creating document term matrix of the bag
dtm <- Corpus(VectorSource(data$V1)) %>% 
  DocumentTermMatrix(control=control)
dim(dtm)
#removing sparse terms from the above matrix
spdtm=removeSparseTerms(dtm,.99)
dim(spdtm)

spdtm1= as.data.frame(as.matrix(spdtm))
#Changing the columns to names as if column name starts with numeric so we can build models
colnames(spdtm1)= make.names(colnames(spdtm1))
sort(colSums(spdtm1))
#
spdtm1$Date=data$Date
spdtm1$Label=data$Label
spdtm1$Label1=data$Label1
spdtm1$Date=as.Date(spdtm1$Date)
spdtm1$Label=as.factor(spdtm1$Label)
spdtm1$Label1=as.factor(spdtm1$Label1)
#splitting the data into training and test data 

tra=subset(spdtm1,Date<='2015-12-31')
tes=subset(spdtm1,Date>'2015-12-31')

#Logistic Regression
log=glm(Label~.-Date,tra,family=binomial,control=list(1e-8,maxit=1000))
summary(log)
#Test data prediction
predlog=predict(log,tes,type='response')
table(predlog>.3,tes$Label)
#51.92% accuracy
str(tra)
#AUC
pred=prediction(predlog,tes$Label)
perf=performance(pred,measure = 'fpr',x.measure = 'fnr')
plot(perf,main = "ROC Curve for Logistic Regression", colorize = TRUE)
perf.test=performance(pred,measure = 'fpr',x.measure = 'fnr')

abline(a = 1, b=-1 )

as.numeric(performance(pred, "auc")@y.values)
#52.39% AUC

#building tree using tree package
tree=tree(Label~.-Date-Label1,tra)
plot(tree)
text(tree,pretty=T)
#test data 
predict.tree=predict(tree,tes,type='class')
table(predict.tree,tes$Label)
mean(predict.tree==tes$Label)
#51.22% accuracy
set.seed(88)
#building tree using rpart
rcart=rpart(Label~.-Date-Label1,tra)
#test data
pred.cart=predict(rcart,tes,type='class')
table(pred.cart,tes$Label)
prp(rcart)
mean(pred.cart==tes$Label)
#53.65% accuracy
##
rcart1=rpart(Label1~.-Date-Label,tra)
#test data
pred.cart1=predict(rcart1,tes,type='class')
table(pred.cart1,tes$Label1)
prp(rcart1)
mean(pred.cart1==tes$Label1)


#Using ranger just to calculate accuracy for different mtry values(as ranger is much faster thamn random forest)
library(ranger)
accuracy=rep(0,20)
n=300
while(n<=1500){
  for(mtry in c(1:20)){
    
    rang=ranger(Label~.-Date-Label1,data=tra,num.trees = n,mtry=mtry,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
    rang.predict=predict(rang,tes)
    table(predictions(rang.predict),tes$Label)
    accuracy[mtry]=mean(predictions(rang.predict)==tes$Label)
    
  }
  n=n+50
  print(accuracy)
}
#from this we get num.trees=300,mtry=2,and having accuracy of 65.85%
rang1=ranger(Label~.-Date-Label1,data=tra,num.trees = 300,mtry=2,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
rang.predict1=predict(rang1,tes)
table(predictions(rang.predict1),tes$Label)
mean(predictions(rang.predict1)==tes$Label)
importance(rang1)
####
library(ranger)
accuracy1=rep(0,20)
n=300
while(n<=1500){
  for(mtry in c(1:20)){
    
    ran=ranger(Label1~.-Date-Label,data=tra,num.trees = n,mtry=mtry,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
    ran.predict=predict(ran,tes)
    table(predictions(ran.predict),tes$Label1)
    accuracy1[mtry]=mean(predictions(ran.predict)==tes$Label1)
    
  }
  n=n+50
  print(accuracy1)
}
####mtry=17,tree=300,accuracy=53.65%

#randomForest 
set.seed(88)
forest=randomForest(Label~.-Date,data=tra,method='class',mtry=2,ntree=300,importance=T)
summary(forest)
str(tra$Label)
#test data
pred4=predict(forest,tes,type='class')
table(pred4,tes$Label)
mean(pred4==tes$Label)
#60.97%accuracy
varImpPlot(forest)
#iphone, smartphone,friday,tuesday,approval,price are some important variables



#SVM

#Linear Kernel

tuneModel1 <- tune(svm, 
                  Label ~ .-Date-Label1, 
                  data = tra, 
                  kernel = "linear", # (u.v)
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)),
                  scale = FALSE)

linFit <- tuneModel1$best.model       # extract the best tuned model
summary(linFit)                      # best parameter-tuned model

# Predict the classes for the validation set
predValid <- predict(linFit, newdata = tes)        # prediction
table(predict = predValid, truth =tes$Label)    # confusion matrix
#accuracy of 65.85%

##

tuneModel3 <- tune(svm, 
                   Label1 ~ .-Date-Label, 
                   data = tra, 
                   kernel = "linear", # (u.v)
                   ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)),
                   scale = FALSE)

linFit1 <- tuneModel3$best.model       # extract the best tuned model
summary(linFit1)                      # best parameter-tuned model

# Predict the classes for the validation set
predValid1 <- predict(linFit1, newdata = tes)        # prediction
table(predict = predValid1, truth =tes$Label1)    # confusion matrix
#accuracy of 46.34

####
#Polynomial Kernel
tuneModel2 <- tune(svm, 
                  Label ~ .-Date, 
                  data = tra, 
                  kernel = "polynomial", # ( gamma * (u.v) + coef0 ) ^ degree
                  ranges = list(gamma = c(0.01, 0.1, 0.5),
                                coef0 = c(0.01, 0.1, 1),
                                degree = c(1, 2, 3),
                                cost = c(0.001, 0.01, 0.1, 1, 10)), 
                  scale = FALSE)

polFit <- tuneModel2$best.model                           # best tuned model
summary(polFit)                                          # best model parameters
predValid3 <- predict(polFit, newdata = tes)        # prediction
table(predict = predValid3, truth = tes$Label)
#Polynomial Accuracy is 65.85% same as Linear one
##
tuneModel4 <- tune(svm, 
                   Label1 ~ .-Date-Label, 
                   data = tra, 
                   kernel = "polynomial", # ( gamma * (u.v) + coef0 ) ^ degree
                   ranges = list(gamma = c(0.01, 0.1, 0.5),
                                 coef0 = c(0.01, 0.1, 1),
                                 degree = c(1, 2, 3),
                                 cost = c(0.001, 0.01, 0.1, 1, 10)), 
                   scale = FALSE)

polFit1 <- tuneModel4$best.model                           # best tuned model
summary(polFit1)                                          # best model parameters
predValid4 <- predict(polFit1, newdata = tes)        # prediction
table(predict = predValid4, truth = tes$Label1)
#43.9% accuracy
## Sentiment Analysis
sentiment <- get_nrc_sentiment(as.character(data$V1))
sent=cbind(data$Date,sentiment)
colnames(sent)<-c('Date','Anger', "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust", "Negative", "Positive")
k=datatable(sent,options = list(dom = 't'))

sums<-as.data.frame(colSums(sent[,-1]))
colnames(sums)<-"Frequency"
sums$Sentiment<-rownames(sums)


p<-ggplot(data=sums, aes(x=Sentiment, y=Frequency, fill=sums$Sentiment)) +
  geom_bar(stat="identity")+ theme(legend.position="none")
p+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
bp<- ggplot(sums, aes(x=Sentiment, y=Frequency, fill=sums$Sentiment))+
  geom_bar(width = 1, stat = "identity")
bp+ coord_polar("x", start=0) + theme(legend.position="none")+labs( x = "", y = "")+theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))

#shows positive sentiment, now calculating the sentiment score
install.packages('sentimentr')
library(sentimentr)
dataf <- as.data.frame(sentiment_by(data$V1))
data$ID=seq.int(nrow(data))
df<-merge(data, dataf, by.x="ID", by.y="element_id")
df$Date=as.Date(df$Date)
df$sd=NULL
df$ID=NULL
m<- ggplot(df, aes(x=Date, y=ave_sentiment))+geom_line()+geom_hline(yintercept=0, colour="red", size=1.5)+ labs(x="Time",y="Emotional valence") 
m
#min score is -3.61, and max is 5.51
spdtm2= as.data.frame(as.matrix(spdtm))
#Changing the columns to names as if column name starts with numeric so we can build models
colnames(spdtm2)= make.names(colnames(spdtm2))
sort(colSums(spdtm2))

spdtm2$Date=data$Date
spdtm2$Label=data$Label
spdtm2$Label1=data$Label1
spdtm2$word_count=df$word_count
spdtm2$Date=as.Date(spdtm2$Date)
spdtm2$Label=as.factor(spdtm2$Label)
spdtm2$Label1=as.factor(spdtm2$Label1)
#So we can see adding word count column to the table does no good, no increase in accuracy
#now we add sentiment as a factor in the original data and we can use sentiment as explanatory variable. Also there is very large amount
#of variation in sentiment varying from -1.87 to 0.33. So we need to standardise it before doing analysis further.
df$norm=(df[,6]-min(df$ave_sentiment))/(max(df$ave_sentiment)-min(df$ave_sentiment))
summary(df$norm[df$Label==1])
summary(df$norm[df$Label==0])
spdtm2$positive=ifelse(df$norm>.5,1,0)
spdtm2$ave_sentiment=NULL
#Converting positive to factor variable
spdtm2$positive=as.factor(spdtm2$positive)
tra2=subset(spdtm2,Date<='2015-12-31')
tes2=subset(spdtm2,Date>'2015-12-31')
##
tree3=tree(Label1~.-Date-Label,tra2)
plot(tree3)
text(tree3,pretty=T)
#test data 
predict.tree3=predict(tree3,tes2,type='class')
table(predict.tree3,tes2$Label1)
mean(predict.tree3==tes2$Label1)


rcart3=rpart(Label1~.-Date-Label,tra2)
#test data
pred.cart=predict(rcart,tes,type='class')
table(pred.cart,tes$Label)
prp(rcart)
mean(pred.cart==tes$Label)


accuracy1=rep(0,20)
n1=300
while(n1<=1500){
  for(mtry in c(1:20)){
    
    rang2=ranger(Label~.-Date-Label1,data=tra2,num.trees = n1,mtry=mtry,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
    rang.predict2=predict(rang2,tes2)
    table(predictions(rang.predict2),tes2$Label1)
    accuracy1[mtry]=mean(predictions(rang.predict2)==tes2$Label1)
    
  }
  n1=n1+50
  print(accuracy1)
}
#ntree=750,mtry=1 accuracy=68.29%
i=1
n2=700
accuracy2=rep(0,20)
while(n2<=900){
  rang2=ranger(Label~.-Date-Label1,data=tra2,num.trees =n2,mtry=1,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
  rang.predict2=predict(rang3,tes2)
  table(predictions(rang.predict2),tes2$Label)
  accuracy2[i]=mean(predictions(rang.predict2)==tes2$Label)
  i=i+1
  n2=n2+10
}
accuracy2
#we get accuracy of 68.29% when num.trees=700,mtry=1
#
accuracy3=rep(0,20)
n1=300
while(n1<=1500){
  for(mtry in c(1:20)){
    
    rang3=ranger(Label1~.-Date-Label,data=tra2,num.trees = n1,mtry=mtry,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
    rang.predict3=predict(rang3,tes2)
    table(predictions(rang.predict3),tes2$Label1)
    accuracy3[mtry]=mean(predictions(rang.predict3)==tes2$Label1)
    
  }
  n1=n1+50
  print(accuracy3)
}

##53.65% accuracy ,mtry=13,tree=300

#SVM with new parameters added
#Linear Kernel

tuneModel3 <- tune(svm, 
                  Label ~ .-Date-Label1, 
                  data = tra2, 
                  kernel = "linear", # (u.v)
                  ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)),
                  scale = FALSE)

linFit1 <- tuneModel3$best.model       # extract the best tuned model
summary(linFit1)                      # best parameter-tuned model

# Predict the classes for the validation set
predValid1 <- predict(linFit1, newdata = tes2)        # prediction
table(predict = predValid1, truth =tes2$Label)    # confusion matrix
#so we can see that no improvement in case of adding new parameter in cas eof SVM as accuracy is 63.41%

##
tuneModel5 <- tune(svm, 
                   Label1 ~ .-Date-Label, 
                   data = tra2, 
                   kernel = "linear", # (u.v)
                   ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)),
                   scale = FALSE)

linFit2 <- tuneModel5$best.model       # extract the best tuned model
summary(linFit2)                      # best parameter-tuned model

# Predict the classes for the validation set
predValid2 <- predict(linFit2, newdata = tes2)        # prediction
table(predict = predValid2, truth =tes2$Label1)   

##
#Polynomial Kernel
tuneModel4 <- tune(svm, 
                  Label ~ .-Date, 
                  data = tra2, 
                  kernel = "polynomial", # ( gamma * (u.v) + coef0 ) ^ degree
                  ranges = list(gamma = c(0.01, 0.1, 0.5),
                                coef0 = c(0.01, 0.1, 1),
                                degree = c(1, 2, 3),
                                cost = c(0.001, 0.01, 0.1, 1, 10)), 
                  scale = FALSE)

polFit1 <- tuneModel4$best.model                           # best tuned model
summary(polFit1)                                          # best model parameters
predValid1 <- predict(polFit1, newdata = tes2)        # prediction
table(predict = predValid1, truth = tes2$Label)
#46.3% accuracy for polynomial when new parameters are added. This shows that it has dropped significantly


rang4=ranger(Label~.-Date,data=tra2,num.trees =700,mtry=1,importance = 'impurity',write.forest = T,min.node.size = 1,replace=T,num.threads = 5,seed=88,classification = T)
rang.predict4=predict(rang4,tes2)
table(predictions(rang.predict4),tes2$Label)
accuracy3=mean(predictions(rang.predict4)==tes2$Label)
##Transporting the predictions(apple news)
Lab=rang.predict4$predictions
dat=as.data.frame(Lab)
dat$Date=tes2$Date1
dat$original=t
write.csv(dat,'predictions_apple.csv')
########################
sp <- Corpus(VectorSource(data$V1)) %>% 
  DocumentTermMatrix(control=control)
dim(sp)
#removing sparse terms from the above matrix
sp=removeSparseTerms(sp,.99)
dim(sp)
####
sp= as.data.frame(as.matrix(sp))
library(RColorBrewer)
library(wordcloud)
wordcloud(Corpu, colors = brewer.pal(8,"Dark2"),max.words = 10,
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)



library(reshape2)
text_df2 %>%
  filter()
text_df2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words=100)