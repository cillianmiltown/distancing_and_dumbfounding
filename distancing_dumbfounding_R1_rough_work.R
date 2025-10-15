
rm(list=ls())



# https://onlinelibrary.wiley.com/doi/full/10.1111/lang.12667

# https://www.rdocumentation.org/packages/WebPower/versions/0.9.4/topics/wp.logistic

# https://ccsarapas.github.io/lighthouse/reference/p_to_OR.html


# install.packages("mclogit")
# remotes::install_github("ccsarapas/lighthouse")
# install.packages('WebPower')

library(mclogit)
library(pwr)
library(effectsize)
library(lighthouse)
library(WebPower)


# create and save a value for the sample size

N <- 240

save(N,file = "N.RData")

source("simulate_data_scenario_within.R")

x <- simulated_data_scenarios_within_sc


table(x$temp,x$scenario)

m1 <- mblogit(formula=cs~temp*scenario
              , random = list(~1|ResponseId)
              , data = x, method=c("PQL"), estimator=c("ML"))
summary(m1)


m1

coefs <- summary(m1)$coefficients
LLs <- coefs[,1] + qnorm(.025)*coefs[,2]
ULs <- coefs[,1] + qnorm(.975)*coefs[,2]
OR <- exp(coefs[,1])
ORLL <- exp(LLs)
ORUL <- exp(ULs)
HHES <- coefs[,1]/1.81 # Hasselblad and Hedges Effect Size

round(cbind(coefs, LLs, ULs), 4)
round(cbind(OR, ORLL, ORUL, HHES), 4)


round(cbind(coefs, LLs, ULs, OR, ORLL, ORUL, HHES), 4)


# or = ((p1/(1-p1))/(p2/(1-p2)))
# or*(p2/(1-p2)) = p1/(1-p1)
# p2/(1-p2) = (p1/(1-p1))/or
# p2 = (1-p2) * ((p1/(1-p1))/or)


OR_to_p2(.6, .3)

or <- 1.437
p0 <- .6
p1 <- OR_to_p2(p0, or)
p1

p0

wp.logistic(n = NULL, p0 = p0, p1 = p1, alpha = 0.05,
            power = 0.9, family = "normal", parameter = c(0,1))



##### The below runs a sample analysis on the Pilot data #####




load("onetoeight.RData")

x <- onetoeight
x$InCS

levels(x$InCS[1])
x$cs <- factor(x$InCS, levels =
                 c(
                   "It's wrong and I can provide a valid reason."
                   ,"It's wrong but I can't think of a reason."
                   ,"There is nothing wrong."
                 ))


x$ResponseId <- 1:length(x$InCS)

m1 <- mblogit(formula=cs~condition, 
              random = list(~1|ResponseId),  
              data = x, method=c("PQL"), estimator=c("ML"))
summary(m1)


m1

coefs <- summary(m1)$coefficients
LLs <- coefs[,1] + qnorm(.025)*coefs[,2]
ULs <- coefs[,1] + qnorm(.975)*coefs[,2]
OR <- exp(coefs[,1])
ORLL <- exp(LLs)
ORUL <- exp(ULs)
HHES <- coefs[,1]/1.81 # Hasselblad and Hedges Effect Size

round(cbind(coefs, LLs, ULs), 4)
round(cbind(OR, ORLL, ORUL, HHES), 4)


round(cbind(coefs, LLs, ULs, OR, ORLL, ORUL, HHES), 4)

OR


OR_to_p2(.6373802, .4201)

or <- .4201
p0 <- .6373802
p1 <- OR_to_p2(p0, or)
p1

p0

wp.logistic(n = NULL, p0 = p0, p1 = p1, alpha = 0.05,
            power = 0.9, family = "normal"
            , parameter = c(0,1)
)



Edmond Awad
Daryl Cameron (or someone from his lab?)
Lucius Caviola
Simone Schnall (she gave an impromptu talk at SPSP anyway…)
Onurcan Yilmaz
Emilie Caspar (she is at Ghent, is very neuro focussed but basically does Milgram-light stuff with very hard to reach samples such as genocide survivors, haven’t seen her talk yet though)
Matti Wilks
Jared Piazza (though maybe a bit of a similar focus as Matti)
Valerio Capraro?
Naomi Ellemers
Bastian Jaeger
Diego Reinero
Robbie Sutton (though maybe too Kent heavy if we already have Roger)
Alexa Weiss
Michal Parzuchowski
- If we really want a butts in seats person, we could try one of the major American names? Controversial but Haidt? Afaik, he has been skipping SPSP because of their DEI policies, so might be up to some Euro travel? 
