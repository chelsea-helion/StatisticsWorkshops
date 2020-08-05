library(effects)
library(olsrr)
library(mediation)
library(interactions)
library(jtools)

## read the data in
envData <- read.csv("/Users/tua37526/Desktop/glbwarm.csv")

##plot the IV/DV
plot(envData$negemot, envData$govact)

##look at the correlation between the two variables
cor.test(envData$negemot, envData$govact)

## run a simple linear regression of the same relationship
govAct.lm <- lm(govact ~ negemot, data = envData)
summary(govAct.lm)
plot(effect("negemot", govAct.lm), grid = TRUE)

govActIdeo.lm <- lm(govact ~ ideology, data = envData)
summary(govActIdeo.lm)
plot(effect("ideology", govActIdeo.lm), grid = TRUE)

govAct.res <- resid(govAct.lm)
plot(envData$govact, govAct.res)

## let's move on to multiple regression
govActMultiple.lm <- lm(govact ~ negemot + posemot + ideology + sex + age, data = envData)
summary(govActMultiple.lm)
k <- ols_step_all_possible(govActMultiple.lm)
plot(k)
k2 <- ols_step_best_subset(govActMultiple.lm)
plot(k2)
k2

# oh hey model 2 -- you look great!

## ok, now let's move on to mediation

## let's test the hypothesis that negative emotion (negemot) 
## mediates the relationship between party identification (1 = democrat, 
## 2 = republican) and government action

#first, let's examine the c path (party ID predicting government action)
# we need to remove independents (coded as 3) from the dataset

envData_DemRep <- subset(envData, partyid == 1 | partyid == 2)
envData_DemRep
envData_DemRep$partyid_recode[envData_DemRep$partyid == 1] <- 0
envData_DemRep$partyid_recode[envData_DemRep$partyid == 2] <- 1

## great! now let's run a simple OLS regression on party id predicting government action ## C PATH

govAct_DemRep.lm <- lm(govact ~ partyid_recode, data = envData_DemRep)
summary(govAct_DemRep.lm)
plot(effect("partyid_recode", govAct_DemRep.lm), grid = TRUE)

## ok, now let's test the a path (party id predicting negative emotion)

negemot_DemRep.lm <- lm(negemot ~ partyid_recode, data = envData_DemRep)
summary(negemot_DemRep.lm)
plot(effect("partyid_recode", negemot_DemRep.lm), grid = TRUE)

## ok now finally, the b path (negemot predicting government action)

govAct_negemot.lm <- lm(govact ~ negemot, data = envData_DemRep)
summary(govAct_negemot.lm)
plot(effect("negemot", govAct_negemot.lm), grid = TRUE)

## ok, we've got all the pieces, now let's test a mediation model
med.fit <- lm(negemot ~ partyid_recode, data = envData_DemRep)
summary(med.fit)
out.fit <- lm(govact ~ negemot + partyid_recode, data = envData_DemRep)
summary(out.fit)
med.out <- mediation::mediate(med.fit, out.fit, treat = "partyid_recode", mediator = "negemot", boot = TRUE, sims = 5000)
summary(med.out)
plot(med.out)

## moving on to moderation
## ok, so we know that negative emotion mediates the relationship between 
## party id and goverment action, such that democrats have stronger negative attitudes 
## about climate change, and negative emotion is associated with a stronger desire for government action

## let's test the hypothesis that age moderates the relationship between party id and gov action
## when you are doing moderation, it's helpful to mean-center your variables, as it makes the intercept more interpretable!

envData_DemRep$age_c <- scale(envData_DemRep$age, center = TRUE, scale = FALSE)
mean(envData_DemRep$age)
envData_DemRep$age_22c <- (envData_DemRep$age - 22)

#let's also set party id as a factor
envData_DemRep$partyid_recode <- as.factor(envData_DemRep$partyid_recode)

## great, now let's run a moderation model!

govact_ageParty.lm <- lm(govact ~ age_22c * partyid_recode, data = envData_DemRep)
summary(govact_ageParty.lm)
summ(govact_ageParty.lm)
plot(effect("age_22c:partyid_recode", govact_ageParty.lm), grid = TRUE)

govact_ageParty.lm <- lm(govact ~ age * partyid_recode, data = envData_DemRep)
summary(govact_ageParty.lm)
interact_plot(govact_ageParty.lm, pred = age, modx = partyid_recode)
interact_plot(govact_ageParty.lm, pred = age, modx = partyid_recode, interval = TRUE, int.width = .95)
interact_plot(govact_ageParty.lm, pred = age, modx = partyid_recode, plot.points = TRUE, interval = TRUE, int.width = .95)

## simple slopes analysis
sim_slopes(govact_ageParty.lm, pred = age, modx = partyid_recode, johnson_neyman = FALSE)






