
# STA6443 Midterm
#Ashley Cortez

setwd("~/OneDrive/School/Fall 2021/STA 6443/Midterm/RStudio/CSV Files")
bweight=read.csv("birthweight.csv", header=TRUE)

bweight$Black=as.factor(bweight$Black)
bweight$Married=as.factor(bweight$Married)
bweight$Boy=as.factor(bweight$Boy)
bweight$MomSmoke=as.factor(bweight$MomSmoke)
bweight$Ed=as.factor(bweight$Ed)

str(bweight)

############################
#Exercise 1
############################

#(a)
boxplot(bweight$Weight, main = "Infant Birth Weight", ylab="Grams")
qqnorm(bweight$Weight); qqline(bweight$Weight, col=2)
shapiro.test(bweight$Weight) 
#P-value 0.1153>0.05 - we do not have enough evidence to reject the null hypothesis - 
#conclusion: data follows normal distribution

#(b)
boxplot(Weight~MomSmoke, data = bweight) #0 is non-smoking mom, 1 is smoking mom


#(c)
shapiro.test(bweight$Weight[bweight$MomSmoke == "0"]); shapiro.test(bweight$Weight[bweight$MomSmoke == "1"])
#non-smoking mom p-value 0.3549
#smoking mom p-value 0.2

#Both p-values are larger than significance level, so we do not have enough evidence to reject the null hypothesis - 
#our conclusion is that the data follows a normal distribution


############################
#Exercise 2
############################

plot(Weight~MomSmoke, data = bweight)

var.test(Weight~MomSmoke, bweight,
         alternative = "two.sided") #p-value 0.8009 - Weight and MomSmoke have same variance

t.test(Weight ~ MomSmoke, bweight, 
       alternative = "two.sided", var.equql=TRUE) #p-value 0.002995 - reject the null 



############################
#Exercise 3
############################

#(a)

table(bweight$Weight); table(bweight$MomSmoke) #Unbalanced

aov.weight= aov(Weight~MomSmoke, data = bweight)
LeveneTest(aov.weight) #Large P-vale of 0.4114 - we do not reject the null; keep ANOVA model
summary(aov.weight) #Very small P-value of 0.00233 - significant effect: there is at least one mean that differs 
plot(aov.weight)
#The plots match the normality assumption, specifically the Normal QQ plot.
boxplot(Weight~MomSmoke, data = bweight)
lm.weight= lm(Weight~MomSmoke, data = bweight)
anova(lm.weight)
summary(lm.weight)$r.squared #31% of the variation of weight can be explained by the MomSmoke

#(b)
aov.weight2  <- Anova(aov.weight, type=3)
aov.weight2
#Small p-value - there exists a significant effect of MomSmoke on infant weight. 


############################
#Exercise 4
############################

#(a)
aov.weight <- aov(Weight ~ Black + Married + Boy + MomSmoke + Ed  , data = bweight)
Anova(aov.weight, type=3)

#(b)
aov.weight1 <-   aov(Weight ~ Black + MomSmoke, data = bweight)
Anova(aov.weight1, type = 3)
plot(aov.weight1)

lm.weight1= lm(Weight ~ Black + MomSmoke, data = bweight)
anova(lm.weight1)
summary(lm.weight1)$r.squared

#(c)
ScheffeTest(aov.weight1)
#Black: 0 is white, 1 is black 
#Momsmoke: 0 is non-smoking, 1 is smoking 

#Black: Mean(white)>Mean(black)
#MomSmoke: Mean(non-smoking mom)>Mean(smoking mom)





