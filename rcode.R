
hf <- read.csv("heart_failure.csv")

head(hf)

mod1<-lm(serum_creatinine~serum_sodium+anaemia,data=hf) 
mod2<-lm(log(serum_creatinine)~log(serum_sodium)+anaemia,data=hf)

summary(mod1) 
summary(mod2)

yes_a <- hf[hf$anaemia == 1,]
no_a <- hf[hf$anaemia == 0,]
no_a

dim(yes_a)
dim(no_a)
<-cor(yes_a$serum_creatinine, yes_a$serum_sodium)
cor(no_a$serum_creatinine, no_a$serum_sodium)
cor

library(psych)

r.test(129, -0.23288, r=23-0.15476, n2= 170)
max(hf$age)

hf_50 <- hf[hf$age < 50,]
hf_60 <- hf[hf$age < 60 & hf$age >= 50,]
hf_70 <- hf[hf$age < 70 & hf$age >= 60,]
hf_80 <- hf[hf$age < 80 & hf$age >= 70,]
hf_90 <- hf[hf$age < 90 & hf$age >= 80,]
hf_o <- hf[hf$age >= 90,]

yes_a5 <- hf_50[hf_50$anaemia == 1,]
no_a5 <- hf_50[hf_50$anaemia == 0,]

yes_a6 <- hf_60[hf_60$anaemia == 1,]
no_a6 <- hf_60[hf_60$anaemia == 0,]

yes_a7 <- hf_70[hf_70$anaemia == 1,]
no_a7 <- hf_70[hf_70$anaemia == 0,]

yes_a8 <- hf_80[hf_80$anaemia == 1,]
no_a8 <- hf_80[hf_80$anaemia == 0,]

yes_a9 <- hf_90[hf_90$anaemia == 1,]
no_a9 <- hf_90[hf_90$anaemia == 0,]

yes_ao <- hf_o[hf_o$anaemia == 1,]
no_ao <- hf_o[hf_o$anaemia == 0,]

install.packages("psychometric") # install package with function
library(psychometric)

CIr(cor(yes_a5$serum_creatinine,yes_a5$serum_sodium),dim(yes_a5)[1],.95)
CIr(cor(no_a5$serum_creatinine,no_a5$serum_sodium),dim(no_a5)[1],.95)
##^
CIr(cor(yes_a6$serum_creatinine,yes_a6$serum_sodium),dim(yes_a6)[1],.95)
CIr(cor(no_a6$serum_creatinine,no_a6$serum_sodium),dim(no_a6)[1],.95)

CIr(cor(yes_a7$serum_creatinine,yes_a7$serum_sodium),dim(yes_a7)[1],.95)
CIr(cor(no_a7$serum_creatinine,no_a7$serum_sodium),dim(no_a7)[1],.95)

CIr(cor(yes_a8$serum_creatinine,yes_a8$serum_sodium),dim(yes_a8)[1],.95)
CIr(cor(no_a8$serum_creatinine,no_a8$serum_sodium),dim(no_a8)[1],.95)

CIr(cor(yes_a9$serum_creatinine,yes_a9$serum_sodium),dim(yes_a9)[1],.95)
CIr(cor(no_a9$serum_creatinine,no_a9$serum_sodium),dim(no_a9)[1],.95)

CIr(cor(yes_ao$serum_creatinine,yes_ao$serum_sodium),dim(yes_ao)[1],.95)
CIr(cor(no_ao$serum_creatinine,no_ao$serum_sodium),dim(no_ao)[1],.95)

mod1<-lm(serum_creatinine~serum_sodium+anaemia,data=healthcare)
summary(mod1)

SCATTER PLOT:
  
  ggplot(healthcare, aes(x = serum_sodium, y = serum_creatinine, color = factor(anaemia))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line without confidence interval
  labs(title = "Scatter Plot of Ejection Rate vs. Serum Sodium",
       x = "Serum Sodium",
       y = "Serum Creatinine",
       color = "Anemia") +
  theme_minimal()

RESIDUAL:
  
  predicted <- mod1$fitted.values
residuals <- mod1$residuals
healthcare1 <- cbind(healthcare, predicted, residuals)


library(ggplot2)
ggplot(healthcare1,aes(x = predicted, y = residuals))+
  geom_point()+
  labs(title =
         "Residual Plot")+
  ylab("Residuals")+
  xlab("Predicted Satisfaction")+
  geom_abline(slope = 0, intercept = 0)

NORMAL QQ:
  ggplot(data =healthcare1, aes(sample = residuals))+
  stat_qq()+
  stat_qq_line()+
  labs(title = "Normal Quantile Plot of Residuals")+
  xlab("Theoretical Normal Quantiles")+
  ylab("Residuals")


TRANSFORMATION LOG MODEL:
  
  mod2 <- lm(log(serum_creatinine) ~ log(serum_sodium) + anaemia, data = healthcare)
summary(mod2)

plot(healthcare,pch = 19,cex = .5)
round(cor(healthcare),3)

##Full model
modelFullLego <- lm(Amazon.Price~Number.of.Pieces+Theme+Number.of.Unique.Pieces+Number.of.Minifigures,
                    data=lego2)

summary(modelFullLego)

vif(modelFullLego)