#简单查看缺失数据
require(Amelia)
missmap(train.raw,main='Titanic缺失数据图',
        col=c('yellow','black'),legend=FALSE)
#生还与死亡对比
barplot(table(train.raw$Survived),names.arg=c('死亡'，'生还'),
        main="生还 vs 死亡"
#不同年龄生还率，连续值离散化
age.breaker=c(0,18,50,100)
age.cut= cut(train.raw$Age,breaks=age.breaker,labels=c("小孩","成年人","老人"))
survive.rate.age=table(train.raw$Survived,train.raw$age.cut)
barplot(survive.rate.age,
        main="不同年龄生还 vs 死亡",
        legend.text=c("死亡","生还"),
        args.legend=list(x="topleft"))
round((survive.rate.age[2,]/colSums(survive.rate.age))*100,2)
#马斯克图
mosaicplot(train.raw$Pclass ~ train.raw$Survived,
           main='不同舱位等级生还 VS 死亡'，shade=FALSE,
           color=TRUE,xlab='舱位'，ylab='生还')
#相关性分析
require(corrgram)
#字符变量先不考虑
train.corrgram=train.raw
corrgram.vars = c("Survived", "Pclass", "Sex", "Age",
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(train.corrgram[,corrgram.vars], lower.panel=panel.ellipse, 
         upper.panel=panel.pie,text.panel=panel.txt, main="泰坦尼克生还率相关性分析")

###初次建模
#构建训练集和测试集
install.packages('caret')
require(caret)

set.seed(201802)
inTrain = creatDataPartirion(train.raw$Survived,
                                         p = 0.8,list=FALSE)
training=train.raw[inTrain,]
test=train.raw[-inTrain,]
#第一个模型
#填补缺失数据较多的年龄
first.class.age=median(training[training$Pclass=='1',]$Age,na.rm=T)
second.class.age=median(training[training$Pclass=="2",]$Age,na.rm=T)
third.class.age=median(training[training$Pclass=="3",]$Age,na.rm=T)
training[is.na(training$Age)&training$Pclass=="1",]$Age=first.class.age
model1.logit.1<-train(Survived~ Sex + Pclass + Age + Embarked + Fare,
                      data=training,method='glm')
model1.logit.1
#用测试集测试一下
first.class.age=median(test[test$Pclass=='1',]$Age,na.rm=T)
second.class.agae=median(test[test$Pclass=='2',]$Age,na.rm=T)
test[test$Pclass='1' & is.na(test$Age),]$Age)=first.class.age
predict.model1.1=predict(model1.logit.1,test)
table(test$Survived,predict.model.1)
sensitivity(test$Survived,predict.model.1)#敏感度 真阳性/(真阳性+假阴性)
specificity(test$Survived,predict.model.1)#特异度 
#ROC曲线
require(ROCR)
predictions.model.1=prediction(c(predict.model.1),labels=test$Survived)
perf = performance(predictions.model.1, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve",col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
