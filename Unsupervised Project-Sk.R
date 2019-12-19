library(readxl)
colesdata<-read_excel("Simulated Coles Data.xlsx",sheet = "Coles Transactions")
View(colesdata)
dim(colesdata)
str(colesdata)
summary(colesdata)
##########are there any NA values#####


table(is.na(colesdata))

library(mice)
md.pattern(colesdata)

library(VIM)
aggr(colesdata[,-8],col=c("blue","red"),prop=FALSE,
     numbers=T,sortvars=T,labels=names(colesdata),cex.axis=.7,gap=3,
     ylab=c("Histogram of missing data","pattern"))
aggr(colesdata,prop=TRUE,numbers=TRUE)
marginplot(colesdata[,c("homeown","pmethod")])
marginplot(colesdata[,c("homeown","PostCode")])


########## impute missing values#########

table(is.na(colesdata$income))
which(is.na(colesdata$income))
colesdata$income[which(is.na(colesdata$income))]<-
  mean(colesdata$income,na.rm = T)

table(is.na(colesdata$age))
which(is.na(colesdata$age))
colesdata$age[which(is.na(colesdata$age))]<-mean(colesdata$age,na.rm = T)
colesdata$age=ceiling(colesdata$age)
summary(colesdata$age)

table(is.na(colesdata$nchildren))
which(is.na(colesdata$nchildren))
table(colesdata$nchildren>10)
colesdata$nchildren[colesdata$nchildren>10]<-NA
colesdata$nchildren[which(is.na(colesdata$nchildren))]<-mean(colesdata$nchildren,na.rm = T)
colesdata$nchildren=floor(colesdata$nchildren)

summary(colesdata$nchildren)


table(is.na(colesdata$pmethod))
summary(colesdata$pmethod)
colesdata$pmethod[colesdata$pmethod>3]<-NA
colesdata$pmethod[which(is.na(colesdata$pmethod))]<-mean(colesdata$pmethod,na.rm = T)
colesdata$pmethod=floor(colesdata$pmethod)

table(is.na(colesdata$homeown))
summary(colesdata$homeown)
colesdata$homeown[colesdata$homeown>2]<-NA
colesdata$homeown[which(is.na(colesdata$homeown))]<-mean(colesdata$homeown,na.rm = T)
colesdata$homeown=floor(colesdata$homeown)


table(is.na(colesdata$fruit))
colesdata$fruit
summary(colesdata$fruit)

colesdata$fruit[colesdata$fruit=="11"]<-1
colesdata$fruit[colesdata$fruit=="o"]<-0
colesdata$fruit[colesdata$fruit=="3"]<-1
colesdata$fruit[colesdata$fruit=="4"]<-1
colesdata$fruit[colesdata$fruit=="6"]<-1
colesdata$fruit[colesdata$fruit=="7"]<-1
colesdata$fruit[which(is.na(colesdata$fruit))]<-mean(colesdata$fruit,na.rm = T)
colesdata$fruit=floor(colesdata$fruit)
summary(colesdata$fruit)


summary(colesdata$fruitjuice)
table(colesdata$fruitjuice==2)
colesdata$fruitjuice[colesdata$fruitjuice=="2"]<-mean(colesdata$fruitjuice,na.rm = T)
colesdata$fruitjuice=floor(colesdata$fruitjuice)


table(is.na(colesdata$cannedveg))
which(is.na(colesdata$cannedveg))

table(is.na(colesdata$PizzaBase))
which(is.na(colesdata$PizzaBase))

table(is.na(colesdata$milk))
which(is.na(colesdata$milk))

table(is.na(colesdata$confectionery))
which(is.na(colesdata$confectionery))

table(is.na(colesdata$cereal))
which(is.na(colesdata$cereal))
colesdata$cereal[which(is.na(colesdata$cereal))]<-mean(colesdata$cereal,na.rm = T)
colesdata$cereal=floor(colesdata$cereal)
summary(colesdata$cereal)

####### Histograms of continuous variables ###########

par(mfrow=c(2,2))
hist(colesdata$Value,xlab="value",col = "2",
     main="value spent by customers ",border = "5",breaks = 70)
hist(colesdata$income,xlab="income",col = "3",border = "4",
     main = "income of customers",breaks = 50)
hist(colesdata$age,xlab="age",col="4",border="2",main = "age of customers")
hist(colesdata$nchildren,col = "5",xlab="number of children",
     main="number of children",border = "6",breaks = 30)




######### check outliers #########
par(mfrow=c(1,2))

boxplot(colesdata$Value,xlab="values of purchases",
        main="Amount spent by customers",horizontal = T,col = "8")
boxplot.stats(colesdata$Value)
summary(colesdata$Value)
newvalue=colesdata$Value[colesdata$Value<400]
boxplot(newvalue,horizontal = T,main="Amount spent by customers",xlab="values excluding extremes")
summary(newvalue)




boxplot(colesdata$pmethod,horizontal = T,col = "3")
boxplot.stats(colesdata$pmethod)
summary(colesdata$pmethod)


boxplot(colesdata$sex,col = "2")
boxplot.stats(colesdata$sex)

boxplot(colesdata$homeown,horizontal = T,col = "5")
boxplot.stats(colesdata$homeown)
summary(colesdata$homeown)


boxplot(colesdata$income,horizontal = T,xlab="Income of customers in dollars")
boxplot.stats(colesdata$income)
summary(colesdata$income)
newincome<-colesdata$income[colesdata$income<150000]
boxplot(newincome,horizontal = T,xlab="Income excluding extreme",col="4")
summary(newincome)




boxplot(colesdata$age,horizontal = T)
summary(colesdata$age)


boxplot(colesdata$nchildren,horizontal = T)
summary(colesdata$nchildren)

######## histogram of revised income and value variables ########


table(is.na(colesdata))
coles1<-colesdata[,-8]
table(is.na(coles1))
md.pattern(coles1)
final_cleandata<-na.omit(coles1)

par(mfrow=c(2,2))

pmethod.class<-as.factor(coles1$pmethod)
Class1bar<-summary(pmethod.class) ;
barplot(Class1bar, 
        xlab="payment method 1=cash,2=creditcard,3=eftpos", 
        ylab="Frequency")

SEX<-as.factor(coles1$sex)
Class2bar<-summary(SEX) ; 
barplot(Class2bar, xlab="1=males, 2=females", ylab="Frequency")

home_class<-as.factor(coles1$homeown)
Class3bar<-summary(home_class) ; 
barplot(Class3bar, xlab="1=homeown,2=rent", ylab="Frequency")

children<-as.factor(coles1$nchildren)
Class4bar<-summary(children) ; 
barplot(Class4bar, xlab="number of children", ylab="Frequency")


write.csv(final_cleandata,file = "C:\\Users\\amans\\Desktop\\sukhjeet\\
          macquarie uni\\Data mining\\first project\\cleaned_data_nopostcode.csv")

coles<-read.csv("cleaned_data_nopostcode.csv",header = T,sep = ",")
attach(coles)

product_variables<-coles[,c(10:53)]
summary(product_variables)
md.pattern(coles)

write.csv(product_variables,file = "C:\\Users\\amans\\Desktop\\sukhjeet\\macquarie uni
          \\Data mining\\first project\\association_data.csv")

############## Applying Association Rules ############

library(arules)
library(Matrix)

mba_analysis<-read.csv("association_data.csv",header = T,colClasses = "factor")
mba_analysis1<-mba_analysis[,-1]

trans <- read.transactions("association_data.csv", format = "basket", sep=",",cols=NULL)


class(trans)
summary(trans)

write(trans[1:5])#First 5 records from trans data set can be displayed on the screen by using inspect##

trans_i <- read.csv("association_data.csv",
                    header=T, sep=",")

summary(trans_i)
trans_i <- trans_i[-1] ## exclude PurchaseID
trans_i <- as(trans_i, "matrix") ## change format from data.frame to matrix 
trans_i <-as(trans_i, "itemMatrix") ## change format from matrix to itemMatrix
dimnames(trans_i) <- list(NULL, paste("item", c(1:5), sep="")) ## change from P1..P5 to item1 .. item5



inspect(trans_i[1:5])

ct<-crossTable(trans_i)
ct[1:5,1:5]

## Item frequency plot
itemFrequencyPlot(trans_i, topN=43, ylim=c(0,0.8))

TransRules<-apriori(trans_i, parameter = list(minlen=2,maxlen=5, supp = 0.1, conf = 0.8))

inspect(TransRules[1:10])

inspect(sort(TransRules, decreasing = TRUE, na.last = NA, by = "support")[1:10])


######## cluster analysis done by following video ###########

final_cleandata=read.csv("cleaned_data_nopostcode.csv",header=T)
colesmba=final_cleandata[-1]
dim(colesmba)

CA=data.matrix(colesmba)

table(is.na(final_cleandata))
attach(final_cleandata)

###### selecting sample #######
smple<-colesmba[sample(nrow(colesmba),10000),]
View(sample)

smple_short<-smple[,c(2,6:8)]
View(smple_short)
smple_matrix<-data.matrix(smple_short)
wss<-nrow(smple_matrix)*sum(apply(smple_matrix,2,var))
for(i in 2:10) wss[i]<-sum(kmeans(smple_matrix,centers = i)$withinss)
plot(1:10,wss,type = "b",xlab = "Number of kmeans clusters",ylab = "Within sum of squares")

summary(smple_matrix)
fit=kmeans(smple_matrix,4)
aggregate(smple_matrix,by=list(fit$cluster),FUN = mean)
smple_matrix<-data.frame(smple_matrix,fit$cluster)

library(cluster)
clusplot(smple_matrix,fit$cluster,main="Cluster plot of continuous variables")


#######cluster analysis####


library(plyr)
final_cleandata=read.csv("cleaned_data_nopostcode.csv",header=T)
final_cleandata=final_cleandata[,-1]
attach(final_cleandata)


final_cleandata$agecat[final_cleandata$age>65]="Elder"
final_cleandata$agecat[final_cleandata$age>50&final_cleandata$age<=65]="Old Adults"
final_cleandata$agecat[final_cleandata$age>30&final_cleandata$age<=50]="Adults"
final_cleandata$agecat[final_cleandata$age>20&final_cleandata$age<=30]="Young Adults"
final_cleandata$agecat[final_cleandata$age<=20]="Children"

final_cleandata$agegp[final_cleandata$age>38]="Above 38 years"
final_cleandata$agegp[final_cleandata$age<=38]="Below or equal to 38 years"

catage<-as.factor(final_cleandata$agecat)
Class4bar<-summary(catage) ; 
barplot(Class4bar, xlab="age groups", ylab="Frequency",xlim = c(0,6))

groupage<-as.factor(final_cleandata$agegp)
classage<-summary(groupage);
barplot(classage,xlab = "age groups",ylab = "frequency",xlim = c(0,2))

final_cleandata$sincome[final_cleandata$income<=10000]="Low"
final_cleandata$sincome[final_cleandata$income>10000&final_cleandata$income<=30000]="Medium"
final_cleandata$sincome[final_cleandata$income>30000&final_cleandata$income<=50000]="High"
final_cleandata$sincome[final_cleandata$income>50000&final_cleandata$income<=100000]="Very high"
final_cleandata$sincome[final_cleandata$income>100000]="Extremely high"

final_cleandata$incgroup[final_cleandata$income<70000]="Low income"
final_cleandata$incgroup[final_cleandata$income>=70000]="High income"

groupinc<-as.factor(final_cleandata$incgroup)
classinc<-summary(groupinc);
barplot(classinc,xlab = "income groups",ylab = "Frequency",xlim = c(0,2))

table(is.na(final_cleandata$sincomeincome))
income_cat<-as.factor(final_cleandata$sincome)
Class4bar<-summary(income_cat) ; 
barplot(Class4bar, xlab="Groups based on income", ylab="Frequency",xlim = c(0,6))


only_demographics<-final_cleandata[,c("Value","age","income","nchildren")]

###usually we do not know how many clusters are in the data set
## one way of finding out how many clusters would be looking at the within cluster sum of squares

wss=(nrow(only_demographics))*sum(apply(only_demographics,2, var))#calculating within-group sum of squares

for (i in 2:10) wss[i]=sum(kmeans(only_demographics,centers=i)$withinss)
par(bg="light yellow")
plot(1:10,wss,type="b",xlab="Number of kmeans clusters with adjusted values",
     ylab="Within-group sum of squares",col="red")

####### kmeans cluster analysis ########
fit=kmeans(only_demographics,4)
aggregate(only_demographics,by=list(fit$cluster),FUN = mean)
only_demographics<-data.frame(only_demographics,fit$cluster)

library(cluster)
clusplot(only_demographics,fit$cluster,main="Cluster plot of continuous variables")

