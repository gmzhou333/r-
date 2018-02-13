#处理缺失
install.packages('mice')
library(mice)
#数据处理专用包,类似SQL 
 +filter() select() arrange() 
 +mutate()添加新变量
 +group_by 对数据分组
install.packages('dplyr')
require(dplyr)
install.packages('tidyr')
require(tidyr)
 +gather()
#重命名
install.packages('reshape')
require('reshape')
#链式操作（管道）
+ 将多行语句嵌套写入一行，并提高可读性
+ %>%(读成 then)
#####开始正文#####
setwd("C:/Users/pc/Desktop/东证期货论文/原始数据/数据")
variables<-read.csv("基础表训练集.csv")
data<-read.csv("基础表训练集2.csv")
new_variables<-tbl_df(variables)
head(variables)
#链式操作
new_variables%>%
 select(...)%>%
 arrange(desc(...))

new_variables%>%
 select(贷款发放日期，贷款最近一次还款日期)%>%
 mutate(所差时间 = 贷款最近一次还款日期-贷款发放日期）

new_variables%>%
 group_by(收入)%>%
 summarize(fun,na.rm=T)

#探索数据集基本结构
names(variables)
summary(variables)
str(variables)
dim(variables)
table(variables)
#异常数值探索
s0=subset(variables,子集满足某一条件)
s1=subset(variables,教育=='专科'|教育=='专科及以下'|教育=='初中'|教育=='高中')$教育
names(s1)<-'初等教育'
head(s1)
s2=s1=subset(variables,教育=='本科'|教育==5)$教育
names(s2)<-'本科教育'
s3=subset(variables,教育=='博士研究生'|教育=='硕士及以上'|教育=='硕士研究生')$教育
names(s3)<-'高等教育'
edu.level<-rbind(s1,s2,s3)
#连续数据离散化
cut(variables$收入,breaks=unique(quantile(variables$收入)))
table(cut(variables$收入,breaks=unique(quantile(variables$收入))))
#转换成类别型数据
variables$是否本地户籍<-as.factor( variables$是否本地户籍)
variables$是否有公积金<-as.factor( variables$是否有公积金)
variables$目标变量值<-as.factor(variables$目标变量值)
variables$工作城市<-as.factor(variables$工作城市)
#转换成数值型数据
variables$收入<-as.numeric(variables$收入)
variables$贷记卡逾期总额<-levels(variables$贷记卡逾期总额)[variables$贷记卡逾期总额]
variables$贷记卡逾期总额<-as.numeric(variables$贷记卡逾期总额)
variables$贷款逾期总额<-levels(variables$贷款逾期总额)[variables$贷款逾期总额]
variables$贷款逾期总额<-as.numeric(variables$贷款逾期总额)
variables$准贷记卡逾期总额<-as.numeric(variables$准贷记卡逾期总额)
variables$个人贷款笔数<-as.numeric(variables$个人贷款笔数)
variables$贷记卡账户数<-as.numeric(variables$贷记卡账户数)
variables$贷款账户数<-as.numeric(variables$贷款账户数)
variables$准贷记卡账户数<-as.numeric(variables$准贷记卡账户数)
variables$查询原因总次数<-as.numeric(variables$查询原因总次数)
variables$未销户贷记卡最近6个月平均使用额度<-as.numeric(variables$未销户贷记卡最近6个月平均使用额度)
variables$未结清贷款6个月平均使用额度<-as.numeric(variables$未结清贷款6个月平均使用额度)
variables$未结清准贷记卡6个月平均使用额度<-as.numeric(variables$未结清准贷记卡6个月平均使用额度)
#重新标记变量（不能含有缺失值）
getedu = function(data){
    edu.start = regexpr("",)

}
edu.filter=c('专科以下','本科学历','硕士以上')
recodeTitle

#因变量和自变量赋值
y<-variables[26]
X<-variables[8:25]
X_mean<-apply(X,2,mean,na.rm=T)
X<-replace_na(data=X,replace=list(X_mean))
summary(X)
head(X)
#探索缺失值
md.pattern(variables)
new.variables<-variables[complete.cases(variables),]
nrow(new.variables)=nrow(variables[!complete.cases(variables),])
apply(variables,1,function(x) sum(is.na(x))) #apply函数计算行缺失值
#删除缺失值
new.variables2<-na.omit(variables) 
#使用最常出现的值
variables[is.na(variables$),]=median(variables$ ,na.rm= T)
hist(variables$贷记卡逾期总额) #需要删除
summary(variables$贷记卡逾期总额)
hist(variables$贷款逾期总额)#需要删除
hist(variables$个人贷款笔数)
variables[is.na(variables$个人贷款笔数),'个人贷款笔数']=median(variables$个人贷款笔数,na.rm=T)
summary(variables)
hist(variables$贷记卡账户数)#无缺失值
hist(variables$贷款账户数)
variables[is.na(variables$贷款账户数),'贷款账户数']=mean(variables$贷款账户数,na.rm=T)
variables[is.na(variables$是否有公积金),'是否有公积金']='1'
variables[is.na(variables$收入),'收入']=mean(variables$收入,na.rm=T)
#根据变量间关系
variables<-variables[-manyNAs(variables),]
lm(x1~x2,data=variables) #若x1,x2自变量之间存在线性关系
variables[is.na(x1),'x1']=f(variables[is.na(x2),'x2'])
cor(variables[,c(11,13:22)],use='complete.obs')
symnum(cor(variables[,c(11,13:22)],use='complete.obs'))
cor(variables$贷记卡逾期总额,variables$贷款逾期总额,use='complete.obs')
#多重插补
variimp<-mice(variables,seed=1234567)
fit<-with(imp,lm(y~X))
fit<-with(imp,analysis)
pooled<-pool(fit)
summary(pooled)
#数据框的缺失值操作
new.variables3<-variables[is.na(variables)]
variables[is.na(variables)]<-0
#第一个模型
model1.logit.1<-lm(目标变量值~是否本地籍 + 教育 +  婚姻 + 收入 + 是否有公积金 +
个人贷款笔数 + 贷记卡账户数 + 贷款账户数 + 查询原因总次数 +未销户贷记卡最近6个月平均使用额度 + 
未结清贷款6个月平均使用额度,data=variables)
model1.logit.1
