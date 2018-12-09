#Zadatak 1
install.packages("foreign")
library("foreign")
Grupa_3 <- read.spss("C:/Users/Katarina/Desktop/Baze za projektni rad/Grupa 3.sav", to.data.frame = TRUE)
str(idi)
#srednja vrednost
mean(Grupa_3$Fixed2017)
mean(Grupa_3$Literacy2017)
mean(Grupa_3$Mobile2017)
#funkcija za izracunavanje modusa
Mode <- function (x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#modus
Mode(Grupa_3$Fixed2017)
Mode(Grupa_3$Literacy2017)
Mode(Grupa_3$Mobile2017)

#standardna devijacija
sd(Grupa_3$Fixed2017)
sd(Grupa_3$Literacy2017)
sd(Grupa_3$Mobile2017)

#medijana
median(Grupa_3$Fixed2017)
median(Grupa_3$Literacy2017)
median(Grupa_3$Mobile2017)

#min
min(Grupa_3$Fixed2017)
min(Grupa_3$Literacy2017)
min(Grupa_3$Mobile2017)

#max
max(Grupa_3$Fixed2017)
max(Grupa_3$Literacy2017)
max(Grupa_3$Mobile2017)

# razmak varijacije
max(IDI$Mobile2017)-min(IDI$Mobile2017)

#IQR
IQR(Grupa_3$Literacy2017)

#Skewness & Kurtoisis
install.packages("moments")
library("moments")
skewness(Grupa_3$Fixed2017)
kurtosis(Grupa_3$Mobile2017)

#Zadatak 2
#absolutne frekvencije
summary(Grupa_3$HDI_level)

#relativne frekvencije
HDIlevel.table <- table(idi$HDI_level)
relfreq=prop.table(HDIlevel.table)
relfreq[1]*100 + relfreq[2]*100

#Zadatak 3
boxplot(Grupa_3$Computer2017)
quantile(Grupa_3$Computer2017)
median(Grupa_3$Computer2017)

#Zadatak 4
shapiro.test(idi$Mobile2017)
install.packages("car")
library(car)
fit<-aov(Grupa_3$Mobile2017~Grupa_3$HDI_level)
summary(fit)

#Zadatak 5
hmHDI  <- subset(Grupa_3, (HDI_level=="Medium" | HDI_level=="Very High"))
install.packages("coin")
library(coin)
shapiro.test(hmHDI$Computer2017)
wilcox_test(hmHDI$Computer2017~hmHDI$HDI_level)

#Zadatak 6
shapiro.test(Grupa_3$Secondary2017)
shapiro.test(Grupa_3$Tertiary2017)
cor(Grupa_3$Secondary2017, Grupa_3$Tertiary2017, method="pearson")
cor.test(Grupa_3$Secondary2017, Grupa_3$Tertiary2017, method="pearson")

#Zadatak 7
install.packages("gmodels")
library("gmodels")
CrossTable(Grupa_3$HDI_level, Grupa_3$Health_level, prop.r=F, prop.c=F,
           prop.t=F, prop.chisq=T, chisq=T)

#Zadatak 8
hmHDI  <- subset(Grupa_3, (HDI_level=="Low" | HDI_level=="Medium"))
shapiro.test(hmHDI$Mobile2017)
install.packages("car")
library(car)
leveneTest(hmHDI$Mobile2017, hmHDI$HDI_level, center="mean")
t.test(hmHDI$Mobile2017~hmHDI$HDI_level, var.equal = TRUE)

#Zadatak 9
boxplot(Grupa3$Mobile2017)

#Zadatak 10
shapiro.test(Grupa_3$Literacy2017)
t.test(Grupa_3$Literacy2017,mu=73.5)
























