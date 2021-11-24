#Read data into vector
data<-read.csv("/Users/Iwuoha Chimereze/Desktop/New folder/data sets/heart.csv")
data
ncol(data)
str(data)

normalize<-function(x){
  return((x-min(x))/(max(x)- min(x)))}
data.subset.n<-as.data.frame(lapply(data[,1:13],normalize))
head(data.subset.n)

set.seed(123)#to get random samples
data.d<-sample(1:nrow(data.subset.n),size = nrow(data.subset.n)*0.7,replace=FALSE)

train.data<-data[data.d,]
test.data<-data[-data.d,]

#creating separate data frame for "outcome" which is the target
train.data_labels<-data[data.d,14]
test.data_labels<-data[-data.d,14]

library(class)# knn package

NROW(train.data_labels)
#for k=14
knn.13<-knn(train=train.data,test=test.data,cl=train.data_labels,k=13)
#for k=15
knn.15<-knn(train=train.data,test=test.data,cl=train.data_labels,k=15)
#accuracy for 14
acc.13<-100* sum(test.data_labels==knn.13)/NROW(test.data_labels)
#accuracy for 15
acc.15<-100* sum(test.data_labels==knn.15)/NROW(test.data_labels)

acc.13
acc.15

confusionMatrix(table(knn.14,test.data_labels))
table(knn.14,test.data_labels)

#loop to generate multiple accuracy
i=1
k.optm=1
for (i in 1:15)
{
  knn.mod<- knn(train = train.data,test = test.data,cl=train.data_labels,k=i)
  k.optm[i]<-100 * sum(test.data_labels==knn.mod)/NROW(test.data_labels)
  k=i
  cat(k,'=',k.optm[i],'\n')
  
}
#graphical representation of accuracy
plot(k.optm,type = "b",xlab = "k - Value",ylab="Accuracy level") 

