library(gamlss) 
data(plasma)  #### reading the dataset ####
View(plasma)
table(which(plasma$betaplasma==0)) ## checking which record has value 0 for betaplasma ##

sub_plasma<-plasma[-c(257),c(1,2,3,4,5,8,9,11,13)]

#### graphical examination of the variables ####
library(GGally)
geov=ggpairs(sub_plasma,columns = c(1,4,6,7,8,9))
geov

### as the distribution of the outcome variable is non-normal we will use log-transformation to get a normal distribution.Though the distribution of numerical covariates is also non-normal but at this stage we do not apply any transformation as there is no assumption of normality of the covariates under multiple linear regression model.###

####transformaing outcome variable as it is right skewed ####
sub_plasma$log_betaplasma<-log(sub_plasma$betaplasma)
hist(sub_plasma$log_betaplasma,col="grey",xlab = "log(betaplasma)",ylab = "Frequency",
     main = "Histogram of outcome variable after transformation")

### The distribution of alcohol is extremely right skewed, there seems an extreme value or outlier under this variable so we can remove that value ###
summary(sub_plasma$alcohol)
boxplot(sub_plasma$alcohol,horizontal = T,col = "seagreen",xlab="Number of alcohol drinks consumed per week",main="Boxplot displaying the alcohol drinks consumed in a week")

## as it is clear from the boxplot that there is an extreme value possibly greater than 40 which can be considered as implausible so excluding it from the data is a good choice ##

table(which(sub_plasma$alcohol>40)) ## to see which record has extreme value under alcohol variable

sub_plasma<-sub_plasma[-62,] ## removing a record with value for alcohol=203 ##
boxplot(sub_plasma$alcohol,horizontal = T,col = "seagreen",xlab="Number of alcohol drinks consumed per week",main="Boxplot displaying the alcohol drinks consumed in a week")

library(car)
sub_plasma$sex<-recode(sub_plasma$sex,'1 ="Male";2 ="Female"')
sub_plasma$sex<-as.factor(sub_plasma$sex)
sub_plasma$smokstat<-recode(sub_plasma$smokstat,'1 ="Never";2 ="Former";3="Current"')
sub_plasma$smokstat<-as.factor(sub_plasma$smokstat)
sub_plasma$vituse<-recode(sub_plasma$vituse,'1 ="Daily";2 ="Occasionally";3="None"')
sub_plasma$vituse<-as.factor(sub_plasma$vituse)

### looking at the boxplots of categorical covariates with log_betaplasma ###
ggplot(sub_plasma,aes(x=sex,y=log_betaplasma))+geom_boxplot(fill="seagreen")
ggplot(sub_plasma,aes(x=smokstat,y=log_betaplasma))+geom_boxplot(fill="midnightblue")
ggplot(sub_plasma,aes(x=vituse,y=log_betaplasma))+geom_boxplot(fill="chocolate")


##setting reference level never for smoking status and none for vituse

sub_plasma$vituse<-relevel(factor(sub_plasma$vituse),ref="None")
sub_plasma$smokstat<-relevel(factor(sub_plasma$smokstat),ref="Never")

##Building a Multiple regression model using all predictors

model_all<-lm(log_betaplasma~age+sex+smokstat+bmi+vituse+fiber+alcohol+betadiet,data=sub_plasma)
summary(model_all)
anova(model_all)

par(mfrow=c(1,2))
plot(model_all,which=(1:2))

Model1<-lm(log_betaplasma~age+sex+smokstat+bmi+vituse+fiber+betadiet+
             alcohol,data=sub_plasma)
summary(Model1)
anova(Model1)
par(mfrow=c(1,2))
plot(Model1,which = (1:2))

Model2<-lm(log_betaplasma~age+sex+smokstat+bmi+vituse+fiber+betadiet,data=sub_plasma)
summary(Model2)
anova(Model2)
par(mfrow=c(1,2))
plot(Model2,which=(1:2))

Model_2<-lm(log_betaplasma~age+sex+bmi+vituse+fiber+betadiet+smokstat,data=sub_plasma)
summary(Model_2)
anova(Model_2)
par(mfrow=c(1,2))
plot(Model_2,which=(1:2))

Model3<-lm(log_betaplasma~age+sex+smokstat+bmi+vituse+fiber,data=sub_plasma)
summary(Model3)
anova(Model3)
par(mfrow=c(1,2))
plot(Model3,which=(1:2))

Model4<-lm(log_betaplasma~age+sex+bmi+vituse+fiber+smokstat,data=sub_plasma)
summary(Model4)
anova(Model4)
par(mfrow=c(1,2))
plot(Model4,which = (1:2))

Finalmodel<-lm(log_betaplasma~age+sex+bmi+fiber+vituse,data=sub_plasma)
summary(Finalmodel)
anova(Finalmodel)
plot(Finalmodel,which=c(1:2))