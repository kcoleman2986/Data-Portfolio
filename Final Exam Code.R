library(tidyverse)
library(vtable)
library(lubridate)
library(DescTools)
library(lmtest)
library(sandwich)
library(stargazer)
library(readxl)
library(car)
library(VIM)
library(mice)
library(lattice)
library(AER)
library(plm)

# Part 1

7311.17 + 3985.20 *(4.5) -.2 * (1500) + 8406.79 * (1) - 416.38 * (1) -2376.51 * (0)

1500 + 10000

7311.17 + 3985.2 * (4) -.2 * (11500) + 8406.79 * (0) - 416.38 * (0) - 2376.51 * (0)

32934.98 - 20951.97

3985.2 - 3538.84

517.70 + 346.98

346.98 / 864.68

323.7 + 5.15 * (23) - 169.78 * (1)

323.7 + 5.15 * (23) - 169.78 * (0)

442.15 - 272.37



5.44 + .015 * (23) -.421 * (1)

5.44 + .015 * (23) -.421 * (0)

5.785 - 5.364

.015^2

######################################

# Part 2

setwd("C:/Users/kjcol/Desktop/Graduate School/Fall 23/Quantitative Methods/Final Exam")

College_data <- read_csv("CollegeDistance.csv")

glimpse(College_data)


sumtable(College_data)

reg_1 <- lm(ed~dist, data= College_data)

coeftest(reg_1)

summary(reg_1)

reg_2 <- lm(ed~dist + bytest + female + black + hispanic + incomehi + ownhome + dadcoll + cue80 + stwmfg80, data = College_data)

coeftest(reg_2)

summary(reg_2)

8.82 + -.031 * (2)

8.82 + -.031 * (3)

8.758 - 8.727

8.82 + -.031 * (6)

8.82 + -.031 * (7)

8.634 - 8.603


reg_3 <- lm(log(ed)~dist + female + bytest + tuition + black + hispanic + incomehi + ownhome + dadcoll + momcoll +cue80 +stwmfg80,
            data= College_data)



2.26 + -.002 *(2)

2.26 + -.002 *(3)

2.256 -2.254


2.26 + -.002 *(6)

2.26 + -.002 * (7)

2.248 -2.246

reg_4 <- lm(ed~ dist + I(dist^2) + female + bytest + tuition + black + hispanic
            + incomehi + ownhome + dadcoll +momcoll + cue80 + stwmfg80, data= College_data)


9.012 + -.08 * (2) + .004 * (2^2)

9.012 + -.08 * (3) + .004 * (3^2)


8.808 - 8.868


9.012 + -.08 * (6) + .004 * (6^2)

9.012 + -.08 * (7) + .004 * (7^2)

8.648 - 8.676




stargazer(
  reg_1,reg_2,reg_3,reg_4,
  digits =3,
  se = list(reg_1$rse, reg_2$rse, reg_3$rse, reg_4$rse),
  header = FALSE,
  type = "html",
  title = "Distance affects Years Education",
  model.numbers = FALSE,
  out= "college.html",
  column.labels = c("(a)", "(b)", "(c)", "(d)"
))


###################################################################################

# Part 3


guns_data <- read_csv("Guns.csv")

glimpse(guns_data)


sumtable(guns_data)

gun_reg_1 <- lm(log(vio)~shall, data = guns_data)

summary(gun_reg_1)


gun_reg_2 <- lm(log(vio)~shall + incarc_rate + density + avginc + pop + pb1064
                + pw1064 + pm1029, data= guns_data)


summary(gun_reg_2)

.36 - .44


gun_reg_state <- plm(log(vio) ~ shall + incarc_rate + density + avginc + pop + pb1064 + pw1064 + pm1029, 
                     data = guns_data, index = "stateid", model = "within")


summary(gun_reg_state)

gun_reg_time <- plm(log(vio) ~ shall + incarc_rate + density + avginc + pop + pb1064 + pw1064 + pm1029, 
                    data = guns_data, index = c("stateid","year"), effect = "time",  model = "within")

summary(gun_reg_time)


stargazer(
  gun_reg_1,gun_reg_2,gun_reg_state ,gun_reg_time,
  digits =3,
  se = list(gun_reg_1$rse, gun_reg_2$rse,gun_reg_state$rse, gun_reg_time$rse),
  header = FALSE,
  type = "html",
  title = "Shall law affects Violent Crime Rates",
  model.numbers = FALSE,
  out= "guns.html",
  column.labels = c("(a)", "(b)", "(c)", "(d)"
  ))



###########################################################################################

# Part 4



smoking_data <- read_csv("Smoking.csv")

glimpse(smoking_data)

sumtable(smoking_data)


smoke_logit <- glm(smoker ~ smkban + age + hsdrop + hsgrad + colsome + colgrad, 
                   data = smoking_data, family = binomial(link="logit"))

summary(smoke_logit)

smoke_logit$rse <- sqrt(diag(vcovHC(smoke_logit, type="HC1")))

psuedoR2_logit <- 1 - (smoke_logit$deviance) / (smoke_logit$null.deviance)





#all workers
smoke_logit <- glm(smoker ~ smkban + age + hsdrop + hsgrad + colsome + colgrad +black + hispanic +female, 
                      data = smoking_data, family = binomial(link="logit"))


predict(smoke_logit, type = "response")

mean(predict(smoke_logit, type = "response"))

#smoking ban
smoking_ban <- subset(smoking_data, smkban ==1)

smokeban_logit <- glm(smoker ~ smkban + age + hsdrop + hsgrad + colsome + colgrad +black + hispanic +female, 
                      data = smoking_ban, family = binomial(link="logit"))

predict(smokeban_logit, type = "response")

mean(predict(smokeban_logit, type = "response"))

# no smoking ban

no_ban <- subset(smoking_data, smkban ==0)

no_ban_logit <- glm(smoker ~ smkban + age + hsdrop + hsgrad + colsome + colgrad +black + hispanic + female, 
                    data = no_ban, family = binomial(link="logit"))

predict(no_ban_logit, type = "response")

mean(predict(no_ban_logit, type = "response"))


.29 -.21

smoke_diff_model <- lm(smoker~smkban, data= smoking_data)

t.test(smoker~smkban, data= smoking_data)


smoke_lpm <- lm(smoker~smkban + female + age +I(age^2) + hsdrop +hsgrad + colsome +colgrad + black + hispanic, data = smoking_data)


summary(smoke_lpm)

smoke_2_logit <- glm(smoker~smkban + female + age +I(age^2) + hsdrop +hsgrad + colsome +colgrad + black + hispanic, data = smoking_data
                     , family=binomial(link="logit"))
summary(smoke_2_logit)



smoke_probit <- glm(smoker~smkban + female + age +I(age^2) + hsdrop +hsgrad + colsome +colgrad + black + hispanic, data = smoking_data
                     , family=binomial(link="probit"))

summary(smoke_probit)


stargazer(
  smoke_diff_model,smoke_lpm,smoke_2_logit ,smoke_probit ,
  digits =3,
  se = list(smoke_diff_model$rse, smoke_lpm$rse, smoke_2_logit$rse, smoke_probit$rse),
  header = FALSE,
  type = "html",
  title = "Smoking Ban Affects Smokers",
  model.numbers = FALSE,
  out= "smoking.html",
  column.labels = c("(a)", "(b)", "(c)", "(d)"
  ))

smoke_2_logit$rse <- sqrt(diag(vcovHC(smoke_2_logit, type="HC1")))

psuedoR2_logit <- 1 - (smoke_2_logit$deviance)/ (smoke_2_logit$null.deviance)

smoke_probit$rse <- sqrt(diag(vcovHC(smoke_probit, type="HC1")))

psuedoR2_probit <- 1 - (smoke_probit$deviance)/ (smoke_probit$null.deviance)

########################################################

# Part 5


women_data <- read_csv("Mroz.csv")

glimpse(women_data)

sumtable(women_data)



women_reg_1 <- lm(lwage~educ, data = women_data)

summary(women_reg_1)

women_reg_1$rse <- sqrt(diag(vcovHC(women_reg_1, type="HC1"))) 



women_iv <- ivreg(lwage~educ | fatheduc, data= women_data)

women_iv$rse <- sqrt(diag(vcovHC(women_iv, type="HC1"))) 



women_reg_2 <- lm(lwage~educ + age + exper + expersq, data= women_data)

summary(women_reg_2)


women_iv_2 <- ivreg(lwage~educ + age + exper +expersq | fatheduc + motheduc, data = women_data)

# No F statistic given
summary(women_iv_2)

relevance_test <- lm(lwage~educ + age +exper +expersq +fatheduc +motheduc, data= women_data)


summary(relevance_test)

exo_test <- linearHypothesis(relevance_test, c("fatheduc = 0", "motheduc = 0"), test = "Chisq")

pchisq(exo_test[2,5], df=2, lower.tail = FALSE)



stargazer(
  women_reg_1, women_iv, women_reg_2 , women_iv_2 ,
  digits =3,
  se = list(women_reg_1$rse, women_iv$rse, women_reg_2$rse, women_iv_2$rse),
  header = FALSE,
  type = "html",
  title = "Education Affects on Wages",
  model.numbers = FALSE,
  out= "women.html",
  column.labels = c("(a)", "(b)", "(c)", "(d)"
  ))


  
