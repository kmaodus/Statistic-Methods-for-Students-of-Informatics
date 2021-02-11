data = read.csv(file.choose(), header = T, sep = ',')

attach(data)
summary(data)
head(data)
#------------------------------A ZADATAK------------------------------
# a)	Opišite varijable statističkog skupa i grafički ih prikažite. 
# Izračunajte korelacije između kvantitativnih varijabli i grafički ih prikažite. 

#Clear packages
p_unload(all)
detach("package:datasets", unload = TRUE) #for base

install.packages("ggpubr")
install.packages("pacman")
require(pacman)
pacman::p_load(pacmam, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, 
               plotly, rio, rmarkdown, shiny, stringr, tidyr)
library(datasets)

head(data)
install.packages("psych")
#install.packages("mnormt")
p_load(psych)


#Graficki prikazi - kvalitativne 

distinct(.data = data, Country, .keep_all = FALSE) #popis država
count<-distinct(.data = data, Status, .keep_all = FALSE)

plot(data$Year, data$Life.expectancy, xlab = "Godina", ylab = "Očekivani životni vijek")
plot(data$BMI, data$Life.expectancy, xlab = "BMI", ylab = "Očekivani životni vijek")
plot(data$Status, data$Life.expectancy, xlab = "Status", ylab = "Očekivani životni vijek")
plot(data$percentage.expenditure, data$infant.deaths, xlab = "Postotni izdaci za zdravstvo", ylab = "Broj umrlih novorođenčadi")
plot(data$Year, data$infant.deaths, xlab = "Postotni izdaci za zdravstvo", ylab = "Broj umrlih novorođenčadi")
plot(data$Alcohol, data$Adult.Mortality, xlab = "alk", ylab = "ODrasli")
plot(data$Schooling, data$Alcohol, xlab = "alk", ylab = "ODrasli")
hist(data$BMI)
hist(data$Hepatitis.B)
hist(data$Schooling, main = "Histogram godina obrazovanja", xlab = "Godine obrazovanja", ylab = "Frekvencija")
hist(data$thinness.5.9.years, main = "Histogram mršavosti djece od 5-9 godina", xlab = "Postotak mršavosti", ylab = "Frekvencija")
hist(data$thinness..1.19.years, main = "Histogram mršavosti djece od 1-19 godina", xlab = "Postotak mršavosti", ylab = "Frekvencija")
hist(data$Polio, main = "Histogram imunizacije dječje paralize  kod jednogodišnjaka", xlab = "Postotak imunizacije dječje paralize", ylab = "Frekvencija")
hist(data$Measles, main = "Histogram prijavljenih slučajeva ospica", xlab = "Broj prijavljenih slučajeva na 1000 stanovnika", ylab = "Frekvencija")
hist(data$Adult.Mortality, main = "Histogram smrtnosti odraslih osoba", xlab = "Broj smrtnih slučajeva na 1000 stanovnika", ylab = "Frekvencija")
hist(data$BMI, main = "Histogram indeksa tjelesne mase", xlab = "Prosječni BMI cjelokupne populacije", ylab = "Frekvencija")
hist(data$under.five.deaths, main = "Histogram broja umrlih ispod pet godina", xlab = "broj umrlih ispod pet godina na 1000 stanovnika", ylab = "Frekvencija")
hist(data$Total.expenditure, main = "Histogram općih državnih izdataka za zdravstvo ", xlab = "Opći državni izdaci za zdravstvo u postotku od ukupnih državnih izdataka (%)", ylab = "Frekvencija")
hist(data$Diphtheria, main = "Histogram pokrivenosti imunizacijom difterijskog tetanusa (DTP3) kod jednogodišnjaka", xlab = "Postotak imunizacije", ylab = "Frekvencija")
hist(data$HIV.AIDS, main = "HIV/AIDS", xlab = "Broj smrti od HIV / AIDS na 1000 osoba (0-4 godine)", ylab = "Frekvencija")
hist(data$GDP, main = "Histogram BDP-a", xlab = "Bruto domaći proizvod po glavi stanovnika, izražen u USD", ylab = "Frekvencija")
hist(data$Income.composition.of.resources, main = "Histogram sastava dohotka", xlab = "Indeks ljudskog razvoja s obzirom na sastav dohotka resursa (indeks se kreće od 0 do 1)", ylab = "Frekvencija")
hist(data$Hepatitis.B, main = "Histogram imunizacije hepatitisa B", xlab = "Pokrivenost imunizacijom protiv hepatitisa B (HepB) kod jednogodišnjaka (%)", ylab = "Frekvencija")
hist(data$percentage.expenditure, main = "Histogram postotnih izdataka za zdravstvo", xlab = "Izdaci za zdravstvo u postotku bruto domaćeg proizvoda po glavi stanovnika (%)", ylab = "Frekvencija")
hist(data$Life.expectancy, main = "Histogram očekivanog životnog vijeka", xlab = "Godina", ylab = "Frekvencija")
hist(data$infant.deaths, main = "Histogram broja umrle novorođenčadi", xlab = "Broj umrle novorođenčadi na 1000 djece", ylab = "Frekvencija")
hist(data$Alcohol, main = "Histogram alkohola", xlab = "Zabilježen po glavi stanovnika (15+), izražen u litrama čistog alkohola", ylab = "Frekvencija")
hist(data$Year, main = "Histogram alkohola", xlab = "Zabilježen po glavi stanovnika (15+), izražen u litrama čistog alkohola", ylab = "Frekvencija")


#-----------------------------KORELACIJE-----------------------------
head(data)
library("ggpubr")
ggscatter(data, x = "BMI", y = "Life.expectancy", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Indeks tjelesne mase", ylab = "Očekivani životni vijek u godinama")

# Shapiro-Wilk normality test
shapiro.test(data$BMI) #p-value < 2.2e-16
# Shapiro-Wilk normality test
shapiro.test(data$Life.expectancy) #p-value < 2.2e-16
#podaci nisu normalno distribuirani

plot(data$BMI, data$Life.expectancy)


library("ggpubr")
ggqqplot(data$BMI, xlab = "Teoretska", ylab = "Indeks tjelesne mase")
ggqqplot(data$Life.expectancy, xlab = "Teoretska",ylab = "Očekivani životni vijek")
qqnorm(data$BMI, xlab = "Teoretska", ylab = "Indeks tjelesne mase")
qqline(data$BMI)
qqnorm(data$Life.expectancy)
qqline(data$Life.expectancy)


res <- cor.test(data$BMI, data$Life.expectancy, 
                method = "pearson")
res

res2 <-cor.test(data$BMI, data$Life.expectancy,  method = "spearman")
res2



# select variables
myvars <- c("Year", "Life.expectancy", "Adult.Mortality", "infant.deaths", "Alcohol", "percentage.expenditure",
            "Hepatitis.B", "Measles", "BMI", "under.five.deaths", "Polio", "Total.expenditure", "Diphtheria", 
            "HIV.AIDS", "GDP", "Population", "thinness..1.19.years", "thinness.5.9.years", 
            "Income.composition.of.resources", "Schooling")
dataKorelacija <- data[myvars]
res3 <- cor(dataKorelacija, use = "complete.obs")
round(res3, 2)

install.packages("Hmisc")
library("Hmisc")
#res2 <- rcorr(as.matrix(dataKorelacija), type = c("pearson","spearman"))
res2 <- rcorr(as.matrix(dataKorelacija), type = c("pearson"))
res2$P
res2$r
res2

install.packages("corrplot")
library(corrplot)
corrplot(res3, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 65)

cor(dataKorelacija, method = "pearson", use = "complete.obs")




par(mfrow=c(2,1))
hist(data$BMI [data$Year == "2015"],
     breaks = 9,
     xlim = c(0, 3),
     main = "histogram",
     xlab = "",
     col = "red")


count<-table(data$Country)
count
postotci<-round(100*count/dim(Podaci)[1],2)
barplot(count)
pie(count)
barplot(postotci)
barplot(postotci, main="Struktura po državama", xlab = "Country", ylab = "%", las=1)
pie(count, main = "Struktura po državama")
box()
plot(data$Country)
plot(data$Status) #
data %>% distinct(Country)


#Graficki prikazi - kvantitativne varijable

psych::describe(Podaci$Life.expectancy)
psych::describe(Podaci$BMI)



#Korelacije
cor.test(data$Life.expectancy, data$Alcohol)

#------------------------------B ZADATAK------------------------------
# b)	Temeljem kvantitativnih varijabli Number of years schooling i Average Body mass index of entire population (BMI) definirajte nove varijable na sljedeći način

NumberOfYearsSchooling <- cut(data$Schooling, breaks=c(0, 8, 12, 22), 
                              labels=c(0, 1, 2), as.factor.result=TRUE)

#Schooling[1:20]
#NumberOfYearsSchooling[1:20]

data1 = cbind(data, NumberOfYearsSchooling)
detach(data)
attach(data1)

BMIqualitative <- cut(data1$BMI, breaks=c(0,15,16,18.5,25,30,40,100), 
                      labels = c("vrlo jaka pothranjenost", "jaka pothranjenost", 
                                 "umjerena pothranjenost", "normalna težina", 
                                 "umjerena pretilost", "jaka pretilost", 
                                 "vrlo jaka pretilost"))

#BMI[0:10]
#BMIqualitative[0:10]

data2 = cbind(data1, BMIqualitative)
detach(data1)
attach(data2)

dataFinal <- data2[complete.cases(data2),]
detach(data2)
attach(dataFinal)


plot(NumberOfYearsSchooling, main = "Kvalitativna varijabla Number Of Years Schooling", xlab = "")
plot(BMIqualitative, main = "Kvalitativna varijabla BMI")


#------------------------------C ZADATAK------------------------------
# c)	Opišite novodobivene varijable statističkog skupa i grafički ih prikažite. 

plot(NumberOfYearsSchooling, main = "Kvalitativna varijabla Number Of Years Schooling", xlab ="")
pie(NumberOfYearsSchooling, main = "Kvalitativna varijabla Number Of Years Schooling", xlab = "")
plot(BMIqualitative, main = "Kvalitativna varijabla BMI")
pie(BMIqualitative, main = "Kvalitativna varijabla BMI")



#------------------------------D ZADATAK------------------------------
# d)	Korištenjem hi-kvadrat testa ispitajte postoji li povezanost kvalitativnih statističkih varijabli 
#       Number of years schooling kvalitativna i BMI kvalitativna. 
head(dataFinal)
NumberOfYearsSchooling = as.factor(NumberOfYearsSchooling)
BMIqualitative = as.factor(BMIqualitative)

tab <- table(NumberOfYearsSchooling, BMIqualitative)
tab
barplot(tab, beside = T, legend = T)

CTest <- chisq.test(tab, correct = TRUE)
CTest

detach(dataFinal)

#------------------------------E ZADATAK------------------------------
# e)	Korištenjem hi-kvadrat testa ispitajte postoji li povezanost kvalitativnih statističkih varijabli 
#       Status (Developed/Developing) i BMI kvalitativna. 
remove(CTest)
remove(tab)
dataFinal$Status=as.factor(Status)

tab <- table(Status, dataFinal$BMIqualitative)
tab

length(dataFinal$Status)
length(dataFinal$BMIqualitative)

barplot(tab, beside = T, legend = T)

CTest <- chisq.test(tab, correct = TRUE)
CTest

#------------------------------F ZADATAK------------------------------

#f)	Korištenjem odgovarajućeg parametarskog i neparametarskog testa 
#ispitajte postoje li razlike varijable Life expectancy in years  po 
#razinama (modalitetima)  varijable Number of years schooling kvalitativna.
#Za provedbu parametarskog testa ispitajte pretpostavke za provedbu. 
#Ukoliko se korištenjem parametarskog testa utvrdi da postoje 
#signifikantne razlike provedite i post hoc test. 

#Parametarski test - ANOVA jednofaktorska

#Provjera pretpostavki za provedbu ANOVE

NumberOfYearsSchooling0 <- subset(dataFinal, subset = NumberOfYearsSchooling==0)
NumberOfYearsSchooling1 <- subset(dataFinal, subset = NumberOfYearsSchooling==1)
NumberOfYearsSchooling2 <- subset(dataFinal, subset = NumberOfYearsSchooling==2)

shapiro.test(NumberOfYearsSchooling0$Life.expectancy)
shapiro.test(NumberOfYearsSchooling1$Life.expectancy)
shapiro.test(NumberOfYearsSchooling2$Life.expectancy)

tapply(dataFinal$Life.expectancy, dataFinal$NumberOfYearsSchooling, var)
bartlett.test(Life.expectancy ~ NumberOfYearsSchooling, data=dataFinal)
library(car)
leveneTest(dataFinal$Life.expectancy, dataFinal$NumberOfYearsSchooling, center = mean)

#ANOVA
boxplot(Life.expectancy ~ NumberOfYearsSchooling, data=dataFinal, 
        xlab = "Number of years schooling", ylab="Life expectancy", las=1)
tapply(dataFinal$Life.expectancy, dataFinal$NumberOfYearsSchooling, mean)
tapply(dataFinal$Life.expectancy, dataFinal$NumberOfYearsSchooling, sd)


AnovaModel1 <- aov(Life.expectancy ~ NumberOfYearsSchooling, data=dataFinal)
summary(AnovaModel1)

#Među kojim grupama postoji signifikantna razlika? 

TukeyHSD(AnovaModel1)
library(multcomp)
.Pairs <- glht(AnovaModel1, linfct = mcp(NumberOfYearsSchooling = "Tukey"))
summary(.Pairs)
confint(.Pairs)
cld(.Pairs)
plot(confint(.Pairs))


#Neparametarski test: Kruskal-Wallisov test
kruskal.test(Life.expectancy ~ NumberOfYearsSchooling, data=dataFinal)

#ZAKLJUČAK: POSTOJE SIGNIFIKANTNE RAZLIKE IZMEĐU GRUPA - provedeni su 
#post hoc testovi (Tukey)


#------------------------------G ZADATAK------------------------------

#g)	Korištenjem odgovarajućeg parametarskog i neparametarskog testa 
#ispitajte postoje li razlike varijable Life expectancy in years  po 
#razinama (modalitetima)  varijable BMI kvalitativna. Za provedbu 
#parametarskog testa ispitajte pretpostavke za provedbu. Ukoliko se 
#korištenjem parametarskog testa utvrdi da postoje signifikantne razlike 
#provedite i post hoc test. 

#Provjera pretpostavki za provedbu ANOVE

BMI0 <- subset(dataFinal, subset = BMIqualitative=="vrlo jaka pothranjenost")
BMI1 <- subset(dataFinal, subset = BMIqualitative=="jaka pothranjenost")
BMI2 <- subset(dataFinal, subset = BMIqualitative=="umjerena pothranjenost")
BMI3 <- subset(dataFinal, subset = BMIqualitative=="normalna težina")
BMI4 <- subset(dataFinal, subset = BMIqualitative=="umjerena pretilost")
BMI5 <- subset(dataFinal, subset = BMIqualitative=="jaka pretilost")
BMI6 <- subset(dataFinal, subset = BMIqualitative=="vrlo jaka pretilost")

shapiro.test(BMI0$Life.expectancy)
shapiro.test(BMI1$Life.expectancy)
shapiro.test(BMI2$Life.expectancy)
shapiro.test(BMI3$Life.expectancy)
shapiro.test(BMI4$Life.expectancy)
shapiro.test(BMI5$Life.expectancy)
shapiro.test(BMI6$Life.expectancy)

tapply(dataFinal$Life.expectancy, dataFinal$BMIqualitative, var)
bartlett.test(Life.expectancy ~ BMIqualitative, data=dataFinal)
library(car)
leveneTest(dataFinal$Life.expectancy, dataFinal$BMIqualitative, center = mean)

#ANOVA
boxplot(Life.expectancy ~ BMIqualitative, data=dataFinal, 
        xlab = "BMI value", ylab="Life expectancy", las=1)
tapply(dataFinal$Life.expectancy, dataFinal$BMIqualitative, mean)
tapply(dataFinal$Life.expectancy, dataFinal$BMIqualitative, sd)


AnovaModel2 <- aov(Life.expectancy ~ BMIqualitative, data=dataFinal)
summary(AnovaModel2)

#Među kojim grupama postoji signifikantna razlika? 

TukeyHSD(AnovaModel2)
library(multcomp)
.Pairs <- glht(AnovaModel2, linfct = mcp(BMIqualitative = "Tukey"))
summary(.Pairs)
confint(.Pairs)
cld(.Pairs)
plot(confint(.Pairs))


#Neparametarski test: Kruskal-Wallisov test
kruskal.test(Life.expectancy ~ BMIqualitative, data=dataFinal)

#ZAKLJUČAK: POSTOJE SIGNIFIKANTNE RAZLIKE IZMEĐU GRUPA - provedeni su 
#post hoc testovi (Tukey)

#------------------------------G ZADATAK------------------------------

#g)	Korištenjem odgovarajućeg parametarskog i neparametarskog testa 
#ispitajte postoje li razlike varijable Life expectancy in years  po 
#razinama (modalitetima)  varijable BMI kvalitativna. Za provedbu 
#parametarskog testa ispitajte pretpostavke za provedbu. Ukoliko se 
#korištenjem parametarskog testa utvrdi da postoje signifikantne razlike 
#provedite i post hoc test. 

#Provjera pretpostavki za provedbu ANOVE

BMI0 <- subset(dataFinal, subset = BMIqualitative=="vrlo jaka pothranjenost")
BMI1 <- subset(dataFinal, subset = BMIqualitative=="jaka pothranjenost")
BMI2 <- subset(dataFinal, subset = BMIqualitative=="umjerena pothranjenost")
BMI3 <- subset(dataFinal, subset = BMIqualitative=="normalna težina")
BMI4 <- subset(dataFinal, subset = BMIqualitative=="umjerena pretilost")
BMI5 <- subset(dataFinal, subset = BMIqualitative=="jaka pretilost")
BMI6 <- subset(dataFinal, subset = BMIqualitative=="vrlo jaka pretilost")

shapiro.test(BMI0$Life.expectancy)
shapiro.test(BMI1$Life.expectancy)
shapiro.test(BMI2$Life.expectancy)
shapiro.test(BMI3$Life.expectancy)
shapiro.test(BMI4$Life.expectancy)
shapiro.test(BMI5$Life.expectancy)
shapiro.test(BMI6$Life.expectancy)

tapply(dataFinal$Life.expectancy, dataFinal$BMIqualitative, var)
bartlett.test(Life.expectancy ~ BMIqualitative, data=dataFinal)
library(car)
leveneTest(dataFinal$Life.expectancy, dataFinal$BMIqualitative, center = mean)

#ANOVA
boxplot(Life.expectancy ~ BMIqualitative, data=dataFinal, 
        xlab = "BMI value", ylab="Life expectancy", las=1)
tapply(dataFinal$Life.expectancy, dataFinal$BMIqualitative, mean)
tapply(dataFinal$Life.expectancy, dataFinal$BMIqualitative, sd)


AnovaModel2 <- aov(Life.expectancy ~ BMIqualitative, data=dataFinal)
summary(AnovaModel2)

#Među kojim grupama postoji signifikantna razlika? 

TukeyHSD(AnovaModel2)
library(multcomp)
.Pairs <- glht(AnovaModel2, linfct = mcp(BMIqualitative = "Tukey"))
summary(.Pairs)
confint(.Pairs)
cld(.Pairs)
plot(confint(.Pairs))


#Neparametarski test: Kruskal-Wallisov test
kruskal.test(Life.expectancy ~ BMIqualitative, data=dataFinal)

#ZAKLJUČAK: POSTOJE SIGNIFIKANTNE RAZLIKE IZMEĐU GRUPA - provedeni su 
#post hoc testovi (Tukey)

#------------------------------H ZADATAK------------------------------

#Korištenjem dvofaktorske analize varijace ispitajte postoje li 
#signifikantne razlike  varijable Life expectancy in years  
#po tretmanima varijabli Status i Number of years schooling kvalitativna. 


library(multcomp)

dataFinal$Status = as.factor(dataFinal$Status)

## Provedba dvofaktorske ANOVE

AnovaModel <- (lm(Life.expectancy ~ Status * dataFinal$NumberOfYearsSchooling))
Anova(AnovaModel)

library(RcmdrMisc)

plotMeans(Life.expectancy, dataFinal$NumberOfYearsSchooling, error.bars = 'conf.int')


#------------------------------I ZADATAK------------------------------

#Korištenjem odgovarajućeg parametarskog i neparametarskog testa 
#ispitajte postoje li razlike varijable Life expectancy in years  
#po razinama (modalitetima)  varijable Status. 
#Za provedbu parametarskog testa ispitajte pretpostavke za provedbu.


## Neparametarski test

tapply(Life.expectancy, Status, median)
wilcox.test(Life.expectancy ~ Status, alternative = 'greater')

## Ispitivanje normalnosti po grupama

status_developed = dataFinal[Status == 'Developed',]
status_developing = dataFinal[Status == 'Developing',]

shapiro.test(status_developed$Life.expectancy)
ks.test(status_developed$Life.expectancy, 'pnorm')

shapiro.test(status_developing$Life.expectancy)
ks.test(status_developing$Life.expectancy, 'pnorm')

## Ispitivanje jednakosti varijanci

tapply(Life.expectancy, Status, var)

var.test(Life.expectancy ~ Status, alternative = 'two.sided', conf.level = .95)
bartlett.test(Life.expectancy ~ Status)

library(car)
leveneTest(Life.expectancy, Status, center = mean)


#------------------------------J ZADATAK------------------------------

#Provedite regresijsku analizu kod koje će zavisna varijabla biti Life expectancy in years, 
#a nezavisne sve ostale kvantitativne varijable. 
#Komentirajte parametre regresije: koeficijent determinacije i korigirani koeficijent determinacije. 
#Interpretirajte skupni i pojedinačne testove signifikantnosti regresije za svaku
#od promatranih nezavisnih varijabli. Komentirajte rezultate regresije. 
#Provedite izbor varijabli koristeći neku od metoda 
#za izbor varijabli  (Forward Selection Procedure, Backward Selection Procedure, Backward/Forward). 
#Nacrtajte normalni prikaz rezidualnih vrijednosti.


## Računanje regresijskog modela: sve grane

RegModelLifeExpectancy <- lm(Life.expectancy ~ Adult.Mortality + infant.deaths + Alcohol + percentage.expenditure + Hepatitis.B + Measles + BMI + under.five.deaths + Polio + Total.expenditure + Diphtheria + HIV.AIDS + GDP + Population + thinness..1.19.years + thinness.5.9.years + Income.composition.of.resources + Schooling, data = dataFinal)
summary(RegModelLifeExpectancy)

## fitted vs residuals
#plot(RegModelLifeExpectancy, which = 1, pch = 20, lwd = 2, font = 2, font.lab = 2, font.sub = 2, las = 1)

## qqplot
plot(RegModelLifeExpectancy, which = 2, pch = 20, lwd = 2, font = 2, font.lab = 2, font.sub = 2, las = 1)
shapiro.test(residuals(RegModelLifeExpectancy))

## Izbor varijabli: sve varijable

library(MASS)
step <- stepAIC(RegModelLifeExpectancy, direction = 'both')