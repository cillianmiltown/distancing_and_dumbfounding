
# rm(list=ls())

library(tidyverse)


df <- read_csv("sample_data.csv")

head(df)
variable.names(df)

df <- df[-c(1:2),]

df$ctrl
df$slf_fut
df$slf_now
df$oth_ctrl
df$oth_fut
df$oth_now

df$condition5 <- NA
df$scenario <- NA
df$conditino6 <- NA

ctrl <- df[which(is.na(df$ctrl)==F),]
ctrl$condition5 <- rep("ctrl")
ctrl$condition6 <- rep("ctrl")
# 
# n <- length(ctrl$conditino6)
# ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.5, 0.5))
# ctrl1 <- ctrl[ind, ]
# ctrl2 <- ctrl[!ind, ]
# 
# ctrl1$condition6 <- rep("ctrl_psych")
# ctrl2$condition6 <- rep("ctrl_temp")
# ctrl <- rbind(ctrl1,ctrl2)
# 
# rm(n,ind,ctrl1,ctrl2)

oth_ctrl <- df[which(is.na(df$oth_ctrl)==F),]
oth_ctrl$condition5 <- rep("oth_ctrl")
oth_ctrl$condition6 <- rep("oth_ctrl")

slf_fut <- df[which(is.na(df$slf_fut)==F),]
slf_fut$condition5 <- rep("slf_fut")
slf_fut$condition6 <- rep("slf_fut")

slf_now <- df[which(is.na(df$slf_now)==F),]
slf_now$condition5 <- rep("slf_now")
slf_now$condition6 <- rep("slf_now")

oth_fut <- df[which(is.na(df$oth_fut)==F),]
oth_fut$condition5 <- rep("oth_fut")
oth_fut$condition6 <- rep("oth_fut")

oth_now <- df[which(is.na(df$oth_now)==F),]
oth_now$condition5 <- rep("oth_now")
oth_now$condition6 <- rep("oth_now")


df <- rbind(ctrl, slf_fut, slf_now, oth_ctrl, oth_fut, oth_now)


Jen <- df %>% dplyr::select(
  "ResponseId"
  , "condition5"
  , "condition6"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Jen"))
Jen$scenario <- rep("Jennifer")
Jen <- `colnames<-`(Jen, c("ResponseId"
                           ,"condition5"
                           ,"condition6"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))

Jmk <- df %>% dplyr::select(
  "ResponseId"
  , "condition5"
  , "condition6"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Jmk"))
Jmk$scenario <- rep("Julie and Mark")
Jmk <- `colnames<-`(Jmk, c("ResponseId"
                           ,"condition5"
                           ,"condition6"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))

Try <- df %>% dplyr::select(
  "ResponseId"
  , "condition5"
  , "condition6"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Try"))
Try$scenario <- rep("Trolley")
Try <- `colnames<-`(Try, c("ResponseId"
                           ,"condition5"
                           ,"condition6"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))

Hnz <- df %>% dplyr::select(
  "ResponseId"
  , "condition5"
  , "condition6"
  , "scenario"
  , "attn_1","attn_2"
  , "before", "before_dtls"
  , starts_with("Hnz"))
Hnz$scenario <- rep("Heinz")
Hnz <- `colnames<-`(Hnz, c("ResponseId"
                           ,"condition5"
                           ,"condition6"
                           ,"scenario"
                           ,"attn_1"
                           ,"attn_2"
                           ,"before"
                           ,"before_dtls"
                           ,"ju1_2"
                           ,"cf1_2"  
                           ,"rsn_st1"
                           ,"ca1"
                           ,"ca1_DO"
                           ,"sw1"
                           ,"sw1_DO"
                           ,"rsn1"
                           ,"rsn1_DO"
                           ,"ca2"    
                           ,"ca2_DO"
                           ,"sw2"
                           ,"sw2_DO"
                           ,"rsn2"
                           ,"ca3"
                           ,"sw3"
                           ,"sw3_DO"
                           ,"rsn3"   
                           ,"rsn3_DO"
                           ,"cs"
                           ,"cs_DO"
                           ,"rsn_st2"
                           ,"ju2_2"
                           ,"cf2_2"))


df <- rbind(Jen, Jmk, Try, Hnz)

table(df$ResponseId)
df <- df[which(is.na(df$ju1_2)==F),]
table(df$ResponseId)


df$psych <- rep(NA)
df$temp <- rep(NA)

ctrl <- df[which(df$condition5=="ctrl"),]
# ctrl_psych <- df[which(df$condition6=="ctrl_psych"),]
# ctrl_temp <- df[which(df$condition6=="ctrl_temp"),]
oth_ctrl  <- df[which(df$condition6=="oth_ctrl"),]
oth_fut <- df[which(df$condition5=="oth_fut"),]
oth_now <- df[which(df$condition5=="oth_now"),]
slf_fut <- df[which(df$condition5=="slf_fut"),]
slf_now <- df[which(df$condition5=="slf_now"),]

# ctrl_psych$psych <- rep("self")
# ctrl_psych$temp <- rep("control")
# 
# ctrl_temp$psych <- rep("self")
# ctrl_temp$temp <- rep("control")

ctrl$psych <- rep("self")
ctrl$temp <- rep("control")
oth_ctrl$psych <- rep("other")
oth_ctrl$temp <- rep("control")
oth_fut$psych <- rep("other")
oth_now$psych <- rep("other")
slf_fut$psych <- rep("self")
slf_now$psych <- rep("self")
oth_fut$temp <- rep("future")
oth_now$temp <- rep("now")
slf_fut$temp <- rep("future")
slf_now$temp <- rep("now")

df <- rbind(
  # ctrl_psych
  # , ctrl_temp
  ctrl
  , oth_ctrl
  , oth_fut
  , oth_now
  , slf_fut
  , slf_now
)



df$ju1_2
as.numeric(df$ju1_2)
df$ju1_2 <- as.numeric(df$ju1_2)

df$ju2_2
as.numeric(df$ju2_2)
df$ju2_2 <- as.numeric(df$ju2_2)


df$cf1_2
as.numeric(df$cf1_2)
df$cf1_2 <- as.numeric(df$cf1_2)

df$cf2_2
as.numeric(df$cf2_2)
df$cf2_2 <- as.numeric(df$cf2_2)

as.factor(df$condition5)
as.factor(df$condition6)

df$scenario <- as.factor(df$scenario)
df$condition5 <- as.factor(df$condition5)
df$condition6 <- as.factor(df$condition6)
df$psych <- as.factor(df$psych)
df$temp <- as.factor(df$temp)
df$cs <- as.factor(df$cs)

table(df$condition5)
table(df$scenario)
table(df$scenario,df$condition5)

sample_data <- df

# list=setdiff(ls(), c("sample_data"))
# list
# rm(list=setdiff(ls(), c("sample_data")))

rm("ctrl", "df", "Hnz", "Jen", "Jmk", "oth_ctrl", "oth_fut", "oth_now", "slf_fut", "slf_now","Try")
