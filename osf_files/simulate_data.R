
#devtools::install_github("jacobbien/simulator")
suppressMessages(library(simulator))
library(tidyverse)
library(desnum)
library(truncnorm)
library(psych)
suppressMessages(library(faux))
library(conflicted)
conflicts_prefer(dplyr::mutate, .quiet = TRUE)

source("set_up_sample_data.R")

# N <- 4800
# session_seed <- 4
# save(N,file = "N.RData")

load("N.RData")

df <- sample_data


empty_df <- df[0,]
x <- empty_df

conditions <- levels(empty_df$condition6)



x <- data.frame(
  matrix(NA,nrow = N,ncol = length(df))
) %>% 
  `colnames<-`(colnames(empty_df)) %>% 
  # set the scenarios and conditions
  mutate(
    condition6 = 
      rep(conditions, N/6)
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
    , psych = 
      dplyr::recode(condition6
                    , "ctrl" = "self"
                    , "oth_ctrl" = "other"
                    , "oth_fut" = "other"
                    , "oth_now" = "other"
                    , "slf_fut" = "self"
                    , "slf_now" = "self"
      )
    # create a "temp variable for temporal distance
    , temp = 
      dplyr::recode(condition6
                    , "ctrl" = "control"
                    , "oth_ctrl" = "control"
                    , "oth_fut" = "future"
                    , "oth_now" = "now"
                    , "slf_fut" = "future"
                    , "slf_now" = "now"
      )
  ) %>% 
  select(-ends_with("_DO")) %>% 
  select(-c(attn_1,attn_2,before,before_dtls)) %>% 
  select(-c(rsn_st1, ca1, sw1, rsn1, ca2, sw2, rsn2, ca3, sw3, rsn3, rsn_st2))

x$ResponseId <- c(1:length(x$ResponseId))
head(x)

control <- x[which(x$condition5=="ctrl"),]
oth_ctrl <- x[which(x$condition5=="oth_ctrl"),]
oth_fut <- x[which(x$condition5=="oth_fut"),]
oth_now <- x[which(x$condition5=="oth_now"),]
slf_fut <- x[which(x$condition5=="slf_fut"),]
slf_now <- x[which(x$condition5=="slf_now"),]


#### Create Scenario variable and set up overall simulation ####

y <- control
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.67, 0.20, 0.13))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- oth_ctrl
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.70, 0.17, 0.13))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_ctrl <- y

y <- oth_fut
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.76, 0.9, 0.15))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_fut <- y

y <- oth_now
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.65, 0.12, 0.23))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_now <- y

y <- slf_fut
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.64, 0.12, 0.24))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_fut <- y

y <- slf_now
y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T, prob=c(0.62, 0.14, 0.24))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_now <- y


x <- rbind(control
           , oth_ctrl
           , oth_fut
           , oth_now
           , slf_fut
           , slf_now)

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

simulated_data_overall <- x




#### Set up simulation with scenario differences ####


x <- simulated_data_overall
x$scenario

# Heinz Jennifer Julie and Mark Trolley

#### Heinz ####

x <- simulated_data_overall

x <- x[which(x$scenario=="Heinz"),]

reasons <- .66
dumb <- .16
nothing <- .18

control <- x[which(x$condition5=="ctrl"),]
oth_ctrl <- x[which(x$condition5=="oth_ctrl"),]
oth_fut <- x[which(x$condition5=="oth_fut"),]
oth_now <- x[which(x$condition5=="oth_now"),]
slf_fut <- x[which(x$condition5=="slf_fut"),]
slf_now <- x[which(x$condition5=="slf_now"),]


y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- oth_ctrl
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.03, dumb-.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_ctrl <- y

y <- oth_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing+.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_fut <- y

y <- oth_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.02, dumb-.08, nothing+.10))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_now <- y

y <- slf_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb-.08, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_fut <- y

y <- slf_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.05, dumb-.06, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_now <- y


x <- rbind(control
           , oth_ctrl
           , oth_fut
           , oth_now
           , slf_fut
           , slf_now)

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

x <- simulated_data_overall

x <- x[which(x$scenario=="Julie and Mark"),]

reasons <- .63
dumb <- .19
nothing <- .18

control <- x[which(x$condition5=="ctrl"),]
oth_ctrl <- x[which(x$condition5=="oth_ctrl"),]
oth_fut <- x[which(x$condition5=="oth_fut"),]
oth_now <- x[which(x$condition5=="oth_now"),]
slf_fut <- x[which(x$condition5=="slf_fut"),]
slf_now <- x[which(x$condition5=="slf_now"),]



y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- oth_ctrl
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.03, dumb-.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_ctrl <- y

y <- oth_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing+.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_fut <- y

y <- oth_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.02, dumb-.08, nothing+.10))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_now <- y

y <- slf_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb-.08, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_fut <- y

y <- slf_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.05, dumb-.06, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_now <- y



x <- rbind(control
           , oth_ctrl
           , oth_fut
           , oth_now
           , slf_fut
           , slf_now)

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

x <- simulated_data_overall

x <- x[which(x$scenario=="Jennifer"),]

reasons <- .72
dumb <- .16
nothing <- .12


control <- x[which(x$condition5=="ctrl"),]
oth_ctrl <- x[which(x$condition5=="oth_ctrl"),]
oth_fut <- x[which(x$condition5=="oth_fut"),]
oth_now <- x[which(x$condition5=="oth_now"),]
slf_fut <- x[which(x$condition5=="slf_fut"),]
slf_now <- x[which(x$condition5=="slf_now"),]



y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- oth_ctrl
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.03, dumb-.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_ctrl <- y

y <- oth_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing+.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_fut <- y

y <- oth_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.02, dumb-.08, nothing+.10))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_now <- y

y <- slf_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb-.08, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_fut <- y

y <- slf_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.05, dumb-.06, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_now <- y



x <- rbind(control
           , oth_ctrl
           , oth_fut
           , oth_now
           , slf_fut
           , slf_now)

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

x <- simulated_data_overall

x <- x[which(x$scenario=="Trolley"),]

reasons <- .68
dumb <- .12
nothing <- .20


control <- x[which(x$condition5=="ctrl"),]
oth_ctrl <- x[which(x$condition5=="oth_ctrl"),]
oth_fut <- x[which(x$condition5=="oth_fut"),]
oth_now <- x[which(x$condition5=="oth_now"),]
slf_fut <- x[which(x$condition5=="slf_fut"),]
slf_now <- x[which(x$condition5=="slf_now"),]



y <- control
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons, dumb, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
control <- y

y <- oth_ctrl
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.03, dumb-.03, nothing))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_ctrl <- y

y <- oth_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons+.09, dumb-.11, nothing+.02))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_fut <- y

y <- oth_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.02, dumb-.08, nothing+.10))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
oth_now <- y

y <- slf_fut
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.03, dumb-.08, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_fut <- y

y <- slf_now
#y$scenario <- sample(levels(empty_df$scenario),(length(y$ResponseId)),rep = T, prob=c(0.25, 0.25, 0.25, 0.25))
y$ju1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=2.9, sd=1.7)
y$cf1_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.5, sd=1.3)
y$cs       <- sample(levels(empty_df$cs),(length(y$ResponseId)),rep = T
                     , prob=c(reasons-.05, dumb-.06, nothing+.11))
y$ju2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=3.0, sd=1.8)
y$cf2_2    <- rtruncnorm(n=(length(y$ResponseId)), a=1, b=7, mean=5.7, sd=1.4)
slf_now <- y



x <- rbind(control
           , oth_ctrl
           , oth_fut
           , oth_now
           , slf_fut
           , slf_now)

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

simulated_data_scenarios <- rbind(Heinz,J_and_M,Jennifer,Trolley)

simulated_data <- simulated_data_scenarios
#rm(list = ls())
#setdiff(ls(), c("simulated_data","simulated_data_scenarios","simulated_data_overall"))
#rm(list=setdiff(ls(), c("simulated_data","simulated_data_scenarios","simulated_data_overall")))


rm("conditions", "control", "df", "dumb", "empty_df", "Heinz", "J_and_M",
   "Jennifer", "make_factors", "N", "nothing", "oth_ctrl", "oth_fut", "oth_now",
   "reasons", "sample_data", "slf_fut", "slf_now", "Trolley", "x", "y")
