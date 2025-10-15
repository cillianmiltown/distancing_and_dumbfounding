
#devtools::install_github("jacobbien/simulator")
suppressMessages(library(simulator))
library(tidyverse)
library(desnum)
#install.packages("truncnorm")
library(truncnorm)
#install.packages("psych")
library(psych)
#install.packages("faux")
suppressMessages(library(faux))
library(conflicted)
conflicts_prefer(dplyr::mutate, .quiet = TRUE)

sample(c(1:26),26, replace = F)

rm(list = ls())

source("set_up_sample_data_scenario_within.R")

N <- 685
# session_seed <- 4
save(N,file = "N.RData")

load("N.RData")

# N <- 300

df <- sample_data


empty_df <- df[0,]
x <- empty_df

conditions <- levels(empty_df$condition6)


conditions <- c("control","distant","near")


x <- data.frame(
  matrix(NA,nrow = N*4,ncol = length(df))
) %>% 
  `colnames<-`(colnames(empty_df)) %>% 
  # set the scenarios and conditions
  mutate(
    condition6 = 
      rep(conditions, each=4, length=N*4)
  ) %>% 
  # use the condition5 variable to code the IVs
  # create a "psych" variable (psychological distance)
  mutate(
    # create a "condition5" variable (controls merged)
    condition5 = condition6
    # dplyr::recode(condition6
    #               , "ctrl_psych" = "ctrl"
    #               , "ctrl_temp" = "ctrl"
    #               , "oth_fut" = "oth_fut"
    #               , "oth_now" = "oth_now"
    #               , "slf_fut" = "slf_fut"
    #               , "slf_now" = "slf_now"
    # )
    # create a "psych" variable (psychological distance)
    # , psych = 
    #   dplyr::recode(condition6
    #                 , "ctrl" = "self"
    #                 , "oth_ctrl" = "other"
    #                 , "oth_fut" = "other"
    #                 , "oth_now" = "other"
    #                 , "slf_fut" = "self"
    #                 , "slf_now" = "self"
    #   )
    # # create a "temp variable for temporal distance
    # , temp = 
    #   dplyr::recode(condition6
    #                 , "ctrl" = "control"
    #                 , "oth_ctrl" = "control"
    #                 , "oth_fut" = "future"
    #                 , "oth_now" = "now"
    #                 , "slf_fut" = "future"
    #                 , "slf_now" = "now"
    #   )
  ) %>% 
  dplyr::select(-ends_with("_DO")) %>% 
  dplyr::select(-c(attn_1,attn_2,before,before_dtls)) #%>% 
  #dplyr::select(-c(rsn_st1, ca1, sw1, rsn1, ca2, sw2, rsn2, ca3, sw3, rsn3, rsn_st2))



x$ResponseId <- rep(1:(length(x$ResponseId)/4), each=4) #c(1:length(x$ResponseId))

x$scenario <- rep(levels(empty_df$scenario))
# 
# x$ResponseId
# 
# rep(c(1:length(x$ResponseId)),4)
# 
# head(x)

x$condition <- x$condition5
x$temp <- x$condition


control <- x[which(x$condition5=="control"),]
distant <- x[which(x$condition5=="distant"),]
near <- x[which(x$condition5=="near"),]


#### Create Scenario variable and set up overall simulation ####

y <- control
# y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.67, 0.20, 0.13))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
y$sw1 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw2 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw3 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
control <- y

y <- distant
# y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.70, 0.17, 0.13))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
y$sw1 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw2 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw3 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
distant <- y

y <- near
# y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.09, 0.15))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
y$sw1 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw2 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw3 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
near <- y


x <- rbind(control
           , distant
           , near
)
           # , oth_now
           # , slf_fut
           # , slf_now)

head(x)

x$temp <- x$condition6

make_factors <- function(x){
  df <- x
  df$scenario <- as.factor(df$scenario)
  df$condition5 <- as.factor(df$condition5)
  df$condition6 <- as.factor(df$condition6)
  df$psych <- as.factor(df$psych)
  df$temp <- as.factor(df$temp)
  df$cs <- as.factor(df$cs)
  df
}

x <- make_factors(x)

df3 <- x

df3$Ju1_bin <- car::recode(df3$ju1_2,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")
df3$Ju2_bin <- car::recode(df3$ju2_2,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")

df3$j1 <- as.vector(car::recode(df3$Ju1_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'"))
df3$j2 <- as.vector(car::recode(df3$sw1, "'Yes'='wrong';'No'='not wrong'"))
df3$j3 <- as.vector(car::recode(df3$sw2, "'Yes'='wrong';'No'='not wrong'"))
df3$j4 <- as.vector(car::recode(df3$sw3, "'Yes'='wrong';'No'='not wrong'"))
df3$j5 <- as.vector(plyr::revalue(df3$cs, c("There is nothing wrong"="not wrong", "It's wrong but I cannot explain why"="wrong","It's wrong and I can provide a valid reason"="wrong")))
#df3$j5 <- as.vector(car::recode(df3$cs_temp, "'FALSE'='wrong';'TRUE'='not wrong'"))
df3$j6 <- as.vector(car::recode(df3$Ju2_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'"))

df3$ch1 <- df3$j1!=df3$j2
df3$ch2 <- df3$j2!=df3$j3
df3$ch3 <- df3$j3!=df3$j4
df3$ch4 <- df3$j4!=df3$j5
df3$ch5 <- df3$j5!=df3$j6

simulated_data_overall_scenario_within <- x




#### Set up simulation with scenario differences ####


x <- simulated_data_overall_scenario_within
x$scenario

# Heinz Jennifer Julie and Mark Trolley

#### Heinz ####

x <- simulated_data_overall_scenario_within

x <- x[which(x$scenario=="Heinz"),]

reasons <- .66
dumb <- .16
nothing <- .18

control <- x[which(x$condition5=="control"),]
distant <- x[which(x$condition5=="distant"),]
near <- x[which(x$condition5=="near"),]


y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- distant
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing=.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
distant <- y

y <- near
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb+.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
near <- y

x <- rbind(control
           , distant
           , near)

head(x)


make_factors <- function(x){
  df <- x
  df$scenario <- as.factor(df$scenario)
  df$condition5 <- as.factor(df$condition5)
  df$condition6 <- as.factor(df$condition6)
  df$psych <- as.factor(df$psych)
  df$temp <- as.factor(df$temp)
  df$cs <- as.factor(df$cs)
  df
}

x <- make_factors(x)
Heinz <- x

#### Julie and Mark ####

x <- simulated_data_overall_scenario_within

x <- x[which(x$scenario=="Julie and Mark"),]

reasons <- .63
dumb <- .19
nothing <- .18


control <- x[which(x$condition5=="control"),]
distant <- x[which(x$condition5=="distant"),]
near <- x[which(x$condition5=="near"),]


y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- distant
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing=.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
distant <- y

y <- near
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb+.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
near <- y

x <- rbind(control
           , distant
           , near)

head(x)


make_factors <- function(x){
  df <- x
  df$scenario <- as.factor(df$scenario)
  df$condition5 <- as.factor(df$condition5)
  df$condition6 <- as.factor(df$condition6)
  df$psych <- as.factor(df$psych)
  df$temp <- as.factor(df$temp)
  df$cs <- as.factor(df$cs)
  df
}

x <- make_factors(x)

J_and_M <- x

#### Jennifer ####

x <- simulated_data_overall_scenario_within

x <- x[which(x$scenario=="Jennifer"),]

reasons <- .72
dumb <- .16
nothing <- .12


control <- x[which(x$condition5=="control"),]
distant <- x[which(x$condition5=="distant"),]
near <- x[which(x$condition5=="near"),]



y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- distant
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing=.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
distant <- y

y <- near
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb+.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
near <- y

x <- rbind(control
           , distant
           , near)


head(x)


make_factors <- function(x){
  df <- x
  df$scenario <- as.factor(df$scenario)
  df$condition5 <- as.factor(df$condition5)
  df$condition6 <- as.factor(df$condition6)
  df$psych <- as.factor(df$psych)
  df$temp <- as.factor(df$temp)
  df$cs <- as.factor(df$cs)
  df
}

x <- make_factors(x)

Jennifer <- x

#### Trolley ####

x <- simulated_data_overall_scenario_within

x <- x[which(x$scenario=="Trolley"),]

reasons <- .68
dumb <- .12
nothing <- .20


control <- x[which(x$condition5=="control"),]
distant <- x[which(x$condition5=="distant"),]
near <- x[which(x$condition5=="near"),]




y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- distant
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing=.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
distant <- y

y <- near
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb+.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
near <- y

x <- rbind(control
           , distant
           , near)


head(x)


make_factors <- function(x){
  df <- x
  df$scenario <- as.factor(df$scenario)
  df$condition5 <- as.factor(df$condition5)
  df$condition6 <- as.factor(df$condition6)
  df$psych <- as.factor(df$psych)
  df$temp <- as.factor(df$temp)
  df$cs <- as.factor(df$cs)
  df
}

x <- make_factors(x)

Trolley <- x

#### combine the scenarios ####

simulated_data_scenarios_within_sc <- rbind(Heinz,J_and_M,Jennifer,Trolley)

y <- simulated_data_scenarios_within_sc

y$sw1 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw2 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))
y$sw3 <- sample(c("Yes","No"),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.24))

df3 <- y



df3 <- x

df3$Ju1_bin <- car::recode(df3$ju1_2,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")
df3$Ju2_bin <- car::recode(df3$ju2_2,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")

df3$j1 <- as.vector(car::recode(df3$Ju1_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'"))
df3$j2 <- as.vector(car::recode(df3$sw1, "'Yes'='wrong';'No'='not wrong'"))
df3$j3 <- as.vector(car::recode(df3$sw2, "'Yes'='wrong';'No'='not wrong'"))
df3$j4 <- as.vector(car::recode(df3$sw3, "'Yes'='wrong';'No'='not wrong'"))
df3$j5 <- as.vector(plyr::revalue(df3$cs, c("There is nothing wrong"="not wrong", "It's wrong but I cannot explain why"="wrong","It's wrong and I can provide a valid reason"="wrong")))
#df3$j5 <- as.vector(car::recode(df3$cs_temp, "'FALSE'='wrong';'TRUE'='not wrong'"))
df3$j6 <- as.vector(car::recode(df3$Ju2_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'"))

df3$ch1 <- df3$j1!=df3$j2
df3$ch2 <- df3$j2!=df3$j3
df3$ch3 <- df3$j3!=df3$j4
df3$ch4 <- df3$j4!=df3$j5
df3$ch5 <- df3$j5!=df3$j6


#simulated_data <- simulated_data_scenarios
#rm(list = ls())
#setdiff(ls(), c("simulated_data","simulated_data_scenarios","simulated_data_overall"))
#rm(list=setdiff(ls(), c("simulated_data","simulated_data_scenarios","simulated_data_overall")))


rm("conditions", "control", "distant", "near", "df", "dumb", "empty_df", "Heinz", "J_and_M",
   "Jennifer", "make_factors", "N", "nothing", "oth_ctrl", "oth_fut", "oth_now",
   "reasons", "sample_data", "slf_fut", "slf_now", "Trolley", "x", "y")

