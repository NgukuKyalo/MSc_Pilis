# MSc_Pilis
MSc_Thesis work
# Pilis Data 
# James Msc. three years

install.packages("tinytex")
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)
library(lattice)

#importing the dataset
setwd("C:/Users/user/Desktop/centre of ecological research/Thesis 1/Progress3")
pilis3years <- read.table("Pilis_3years.txt", header = TRUE)
View(pilis3years)
colnames(pilis3years)
str(pilis3years)

#Convert the categorical data to factors
pilis3years$Treatm<-factor(pilis3years$Treatm)
pilis3years$Year<-factor(pilis3years$Year)
pilis3years$Blokk<-factor(pilis3years$Blokk)

#Summary statistics
summary(pilis3years$Cover)
summary(pilis3years$SpNo)
boxplot(pilis3years$SpNo~pilis3years$Treatm)
boxplot(pilis3years$SpNo~pilis3years$Year)

#Cover
summary(pilis3years$Cover)
boxplot(pilis3years$Cover~pilis3years$Treatm)
boxplot(pilis3years$Cover~pilis3years$Year)
boxplot(pilis3years$Cover~pilis3years$Blokk)

#Species number
summary(pilis3years$SpNo)
boxplot(pilis3years$SpNo~pilis3years$Year)
boxplot(pilis3years$SpNo~pilis3years$Treatm)

# Classical ANOVA
# Lets start with the Specie number
bwplot(pilis3years$SpNo~pilis3years$Year|pilis3years$Treatm,
       ylab="Species Number",
       xlab = "Year")
bwplot(pilis3years$SpNo~pilis3years$Treatm|pilis3years$Year,
       ylab = "Species Number",
       xlab = "Total Cover %")

# For the cover
bwplot(pilis3years$Cover~pilis3years$Year|pilis3years$Treatm,
       ylab = "Total Cover %",
       xlab = "Year")


#Lets try to run linear model and check which factors were significantly lets start with SpNo
mod1.1 <- lm(SpNo~Treatm+Year+Blokk, data = pilis3years)
anova(mod1.1)
summary(mod1.1)
par(mfrow=c(2,2))
plot(mod1.1)

#Let us check the linear mixed model for the total cover
mod2.2<- lm(Cover~Treatm+Year+Blokk, data = pilis3years)
anova(mod2.2)
summary(mod2.2)

# Advanced statistics
# Installing the necessary packages
install.packages("nlme")
install.packages("lme4")
install.packages("MuMIn")
install.packages("lsmeans")
install.packages("multcompView")
install.packages("multcomp")
install.packages("plotrix")

#Loading the packages
library(Matrix)
library(nlme)
library(lme4)
library(MuMIn)
library(lsmeans)
library(emmeans)
library(multcompView)
library(multcomp)
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(plotrix)

# Lets start with the cover
# Preparing an histogram
#Kolmogorov smirnov - compares distributions with normals, then examines normal distributions
#pnorm is a generated normal distribution

for( i in 8 : 8)
{
  Pilis<-paste(colnames(pilis3years[i]), "_hist.jpg", sep="")
  jpeg (file=Pilis)
  KS<-ks.test(scale(pilis3years[ ,i]),pnorm)
  KS2<-c("KS=",round(KS$statistic, digits=3)
         ,"p=", round(KS$p.value,digits=3))
  hist(pilis3years[,i],breaks=15, main=KS2, xlab=colnames(pilis3years[i]))
  dev.off()
}

# Visualization
x <-interaction(pilis3years$Fence, pilis3years$Treatm, pilis3years$Year)
y <- pilis3years$Cover
levels(x)
SE<-function(y){sd(y,na.rm=TRUE)/sqrt(length(y))}
S.mean<-tapply(y, x, mean,na.rm=TRUE)
S.mean.se<-tapply(y, x, SE)
S.mean.se.upper<-S.mean+S.mean.se
S.mean.se.lower<-S.mean-S.mean.se
S.mean.sd<-tapply(y, x, sd, na.rm=TRUE)
S.mean.sd.upper<-S.mean+S.mean.sd
S.mean.sd.lower<-S.mean-S.mean.sd

#z=1:length(levels(x))
z=c(1,2,3,4,5,6,7,8,9,10, 12,13,14,15,16,17,18,19,20,21, 23,24,25,26,27,28,29,30,31,32)
length(z)
colvec=rep(c("darkgreen","darkgreen","#aa0000","#aa0000","#bc5fd3","#bc5fd3","#ffa500","#ffa500","#0066ff","#0066ff"),6)
colvec2=rep(c("darkgreen","#aa0000","#bc5fd3","#ffa500","#0066ff"),6)
pchvec=rep(c(19,1),30)
Treatment_cod <- rep(c("C","CC","G","P", "R"),6)
Kezeles_poz<- c(1.5,3.5,5.5,7.5,9.5, 12.5,14.5,16.5,18.5,20.5, 23.5,25.5,27.5,29.5,31.5, 
                34.5,36.5,38.5,40.5,42.5, 45.5,47.5,49.5,51.5,53.5, 56.5,58.5,60.5,62.5,64.5)
multcomp_kod <- c("ab","a","ab","b","ab","b","ab","ab","ab","ab", 
                  "a","a","c","c","c","c","bc","bc","ab","ab",
                  "a","a","bc","c","bc","c","bc","c","ab","ab")
multcomp_kod2 <- c("a","b","b","b","ab", "a","b","b","b","ab",
                   "a","b","b","b","ab")
multcomp_fence<-rep(c("*","*","*","",""),6)

plot(S.mean,type="n",pch=19, cex.lab=1.2, cex.axis=1,bty="n", xaxt="n", ylab="Cover (%)", xlab="", ylim=c(0,200),xlim=c(0,32))
segments(z, S.mean.sd.upper,z, S.mean.sd.lower, lwd=3, col=colvec)
segments(z, S.mean.se.upper,z, S.mean.se.lower, col="white", lwd=4)
points(z, S.mean,pch=pchvec, cex=1.2, col=colvec)

axis(1,at=Kezeles_poz, labels=FALSE,tcl=-0.3)
axis(side=1,at=c(5.5,16.5,27.5),labels=c("2015","2018","2023"),
     line=0.3,col="white")
axis.break(1,11,style="zigzag",breakcol="white",brw=0.04)
axis.break(1,22,style="zigzag",breakcol="white",brw=0.04)
axis.break(1,33,style="zigzag",breakcol="white",brw=0.04)


text(x=Kezeles_poz,y=0,labels=Treatment_cod,col=colvec2)

text(x=Kezeles_poz,y=10,labels=multcomp_kod2)
text(x=Kezeles_poz,y=200,labels=multcomp_fence,col=colvec2)

#GLMM for cover

mod_full_cover<-lmer(Cover~Treatm*Year*Fence+ (1|Blokk),data=pilis3years)
mod1_cover<-lmer(Cover~Treatm+Year+ Treatm:Year + (1|Blokk),data=pilis3years)
mod2_cover<-lmer(Cover~Treatm+Year+Fence+ Treatm:Year + Treatm:Fence + Year:Fence + (1|Blokk),data=pilis3years)
mod3_cover<-lmer(Cover~Treatm+Year+ Treatm:Year + Treatm:Fence +  (1|Blokk),data=pilis3years)

anova(mod_full_cover)
anova(mod1_cover)
anova(mod3_cover)
anova(mod2_cover,mod3_cover)
summary(mod3_cover)
r.squaredGLMM(mod3)

# Diagnostics for the models
qqnorm(residuals(mod1_cover))
qqline(residuals(mod1_cover),lty=2)
plot(fitted(mod3_cover),residuals(mod3_cover),xlab="Fitted values",ylab="Residuals")
plot(mod3_cover)

# The multicomp
cld(lsmeans(mod1_cover,  ~ Treatm | Year),alpha=0.05,Letters=letters,adjust="tukey")

# lets try the species number
x <- interaction(pilis3years$Treatm, pilis3years$Year)
y <- pilis3years$SpNo
levels(x)
SE<-function(y){sd(y,na.rm=TRUE)/sqrt(length(y))}
S.mean<-tapply(y, x, mean,na.rm=TRUE)
S.mean.se<-tapply(y, x, SE)
S.mean.se.upper<-S.mean+S.mean.se
S.mean.se.lower<-S.mean-S.mean.se
S.mean.sd<-tapply(y, x, sd, na.rm=TRUE)
S.mean.sd.upper<-S.mean+S.mean.sd
S.mean.sd.lower<-S.mean-S.mean.sd
z<-c(1,2,3,4,5, 7,8,9,10,11, 13,14,15,16,17)
length(z)

colvec2=rep(c("darkgreen","#aa0000","#bc5fd3","#ffa500","#0066ff"),6)
#pchvec=rep(c(19,1),30)
Kezeles_kod <- rep(c("C","CC","G","P", "R"),6)
multcomp_kod<-rep(c("a","cd","b","bc","d"),6)
