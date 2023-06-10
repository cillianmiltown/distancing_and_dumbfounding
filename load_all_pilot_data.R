rm(list = ls())
library(citr)
#install.packages("sjstats")
library(plyr)
library(foreign)
library(car)
library(desnum)
library(ggplot2)
library(extrafont)
#devtools::install_github("crsh/papaja")
library(papaja)
#library("dplyr")
library("afex")
library("tibble")
library(scales)
#install.packages("metap")
library(metap)
library(pwr)
library(lsr)
#install.packages("sjstats")
library(sjstats)
library(DescTools)
#inatall.packages("ggstatsplot")
#library(ggstatsplot)
library(VGAM)
library(nnet)
library(mlogit)
library(reshape2)

#### Study 1 ####

prep_data_distancing <- function(a,b){
  
  ds1 <- a
  ds2 <- b
  
  ds1 <- ds1[which(ds1$InCS!="NA"),]
  ds2 <- ds2[which(ds2$InCS!="NA"),]
  
  
  ds1$condition <- rep("1manip", length(ds1$gender))
  ds2$condition <- rep("2control", length(ds2$gender))
  
  #create variable labelling participants
  participant <- c(1:(length(ds1$gender)+length(ds2$gender)))
  
  
  
  #merge the data frames
  ds6 <- cbind(participant,rbind.data.frame(ds1[1:82],ds2[1:82]))
  
  
  #ds6 <- rbind(ds1[1:82], ds2[1:82])
  ds6$condition <- c(ds1$condition, ds2$condition)
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  ds1$InJu1 <- to_num(ds1$InJu1)
  ds1$InJu2 <- to_num(ds1$InJu2)
  ds1$InCf1 <- to_num(ds1$InCf1)
  ds1$InCf2 <- to_num(ds1$InCf2)
  
  
  ds2$InJu1 <- to_num(ds2$InJu1)
  ds2$InJu2 <- to_num(ds2$InJu2)
  ds2$InCf1 <- to_num(ds2$InCf1)
  ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  #rm(ds6)
  
  
  
  df3$InJu1 <- to_num(df3$InJu1)
  df3$InJu2 <- to_num(df3$InJu2)
  df3$InCf1 <- to_num(df3$InCf1)
  df3$InCf2 <- to_num(df3$InCf2)
  df3$InRsn2 <- to_num(df3$InRsn2)
  
  df3$Ju1_bin <- recode(df3$InJu1,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")
  df3$Ju2_bin <- recode(df3$InJu2,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")
  df3$condition
  
  df3$wrong1 <- recode(df3$Ju1_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'")
  
  df3$wrong2<- recode(df3$Ju2_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'")
  
  
  df3$j1 <- as.vector(recode(df3$Ju1_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'"))
  df3$j2 <- as.vector(recode(df3$InSWr1, "'Yes'='wrong';'No'='not wrong'"))
  df3$j3 <- as.vector(recode(df3$InSWr2, "'Yes'='wrong';'No'='not wrong'"))
  df3$j4 <- as.vector(recode(df3$InSWr3, "'Yes'='wrong';'No'='not wrong'"))
  df3$j5 <- as.vector(revalue(df3$InCS, c("There is nothing wrong."="not wrong", "It's wrong but I can't think of a reason."="wrong","It's wrong and I can provide a valid reason."="wrong")))
  #df3$j5 <- as.vector(recode(df3$cs_temp, "'FALSE'='wrong';'TRUE'='not wrong'"))
  df3$j6 <- as.vector(recode(df3$Ju2_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'"))
  
  df3$ch1 <- df3$j1!=df3$j2
  df3$ch2 <- df3$j2!=df3$j3
  df3$ch3 <- df3$j3!=df3$j4
  df3$ch4 <- df3$j4!=df3$j5
  df3$ch5 <- df3$j5!=df3$j6
  
  df3$Rch1 <- df3$InAR1!=df3$InAR2
  df3$Rch2 <- df3$InAR2!=df3$InAR3
  
  df3$Rs_changed <- (df3$Rch1+df3$Rch2)!=FALSE
  df3$Rs_changed_tot <- df3$Rch1+df3$Rch2
  
  df3$changed <- (df3$ch1+df3$ch2+df3$ch3+df3$ch4+df3$ch5)!=FALSE
  df3$changed_tot <- df3$ch1+df3$ch2+df3$ch3+df3$ch4+df3$ch5
  
  df3$rs1 <- as.vector(rep("unstated", length=length(df3$gender)))
  df3$rs2 <- as.vector(recode(df3$InAR1,"'Yes, I have a reason'='reason';'No, I have no reason'='no reason';'Unsure'='unstated'"))
  df3$rs3 <- as.vector(recode(df3$InAR2,"'Yes, I have a reason'='reason';'No, I have no reason'='no reason';'Unsure'='unstated'"))
  df3$rs4 <- as.vector(recode(df3$InAR3,"'Yes, I have a reason'='reason';'No, I have no reason'='no reason';'Unsure'='unstated'"))
  df3$rs5 <- as.vector(revalue(df3$InCS, c("There is nothing wrong."="unstated", "It's wrong but I can't think of a reason."="no reason","It's wrong and I can provide a valid reason."="reason")))
  df3$rs6 <- as.vector(rep("unstated", length=length(df3$gender)))
  
  
  df3$rs1_bin <- as.vector(recode(df3$InAR1,'"Yes, I have a reason"="reason";"No, I have no reason"="no reason"; "Unsure"="no reason"'))
  df3$rs2_bin <- as.vector(recode(df3$InAR2,'"Yes, I have a reason"="reason";"No, I have no reason"="no reason"; "Unsure"="no reason"'))
  df3$rs3_bin <- as.vector(recode(df3$InAR3,'"Yes, I have a reason"="reason";"No, I have no reason"="no reason"; "Unsure"="no reason"'))
  
  df3$reason_always <- df3$rs1_bin=="reason"&df3$rs2_bin=="reason"&df3$rs3_bin=="reason"
  
  
  names(df3)[names(df3) == 'v_164'] <- 'pot_hrm'
  names(df3)[names(df3) == 'v_165'] <- 'box'
  names(df3)[names(df3) == 'v_166'] <- 'rugb'
  
  df3$pot_hrm <- to_num(df3$pot_hrm)
  df3$pot_hrm_bin <- recode(df3$pot_hrm,"1='wrong';2='wrong';3='wrong';4='neutral';5='right';6='right';7='right'")
  df3$all_harm <- (df3$pot_hrm_bin!="wrong"|df3$box=="No"|df3$rugb=="No"|df3$hrm_Qs=="no harm")
  df3$all_harm <- recode(df3$all_harm, "TRUE='true';FALSE='false';NA='missing'")
  
  
  df3$all_harm_norm <- (df3$pot_hrm_bin!="wrong"|df3$box=="No"|df3$rugb=="No"|df3$hrm_Qs=="no harm")&(df3$normQ=="Violating an established moral norm, just for fun or personal enjoyment, is wrong only in situations where someone is harmed as a result, but is acceptable otherwise."|df3$nrm_st=="norms not mentioned")
  df3$all_harm_norm <- recode(df3$all_harm_norm, "TRUE='true';FALSE='false';NA='missing'")
  
  
  df3$nrm_all <- (df3$normQ=="Violating an established moral norm, just for fun or personal enjoyment, is wrong only in situations where someone is harmed as a result, but is acceptable otherwise."|df3$nrm_st=="norms not mentioned")
  df3$nrm_all <- recode(df3$nrm_all, "TRUE='true';FALSE='false';NA='missing'")
  
  
  
  df3$Roz_fully_C <- (df3$normQ== 'Violating an established moral norm, just for fun or personal enjoyment, is wrong only in situations where someone is harmed as a result, but is acceptable otherwise.' & df3$Hrm_oth=="Yes, I am able to believe this"& df3$Hrm_rel=="Yes, I am able to believe this")
  df3$Roz_fully_C <- recode(df3$Roz_fully_C, "TRUE='true';FALSE='false';NA='missing'")
  
  
  
  df3$Dumb_response <- revalue(df3$InCS, c("It's wrong but I can't think of a reason."="admission"))
  df3[which(df3$critical_slide_wrong=="changed"),]$Dumb_response <- revalue(df3$Dumb_response[which(df3$critical_slide_wrong=="changed")], c("It's wrong and I can provide a valid reason."="unsupported declarations"))
  
  require(dplyr)
  df3 <- df3 %>%
    mutate(Dumb_response = ifelse(is.na(Dumb_response),0,Dumb_response))
  
  detach("package:dplyr", unload=TRUE)
  
  cbind.data.frame(df3$InCS,df3$Dumb_response)
  df3$Dumb_response <- recode(df3$Dumb_response, "'0'='unsupported declaration';'2'='admission';'1'='nothing wrong';'3'='reasons'")
  
  df3$Dumb_response
  
  df3$Dumb_incl_string <- droplevels(df3$Dumb_incl_string)
  
  
  df3$right_change <- df3$InCS# revalue(df3$InCS, c("It's wrong but I can't think of a reason."="admission"))
  df3[which(df3$Ju1_bin=="right"),]$right_change <- revalue(df3[which(df3$Ju1_bin=="right")]$right_change, c("There is nothing wrong."="right initially"))
  
  require(dplyr)
  df3 <- df3 %>%
    mutate(right_change = ifelse(is.na(right_change),0,right_change))
  
  detach("package:dplyr", unload=TRUE)
  
  cbind.data.frame(df3$InCS,df3$right_change)
  df3$right_change <- recode(df3$right_change, "'0'='right initially';'2'='dumbfounded';'1'='nothing wrong';'3'='reasons'")
  
  
  
  
  df3$SocDes <- c(df3$v_182==F)+
    c(df3$v_214==F)+
    c(df3$v_213==T)+
    c(df3$v_212==T)+
    c(df3$v_211==F)+
    c(df3$v_210==F)+
    c(df3$v_209==F)+
    c(df3$v_208==T)+
    c(df3$v_207==T)+
    c(df3$v_206==T)
  
  df3
  
}

#setwd("distancing")
list.files()
ds1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_1/distancing_manip.sav", to.data.frame = TRUE)
ds2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_1/distancing_control.sav", to.data.frame = TRUE)
#setwd("..")
ds6 <- prep_data_distancing(ds1,ds2)
df3 <- ds6


table(ds1$InCS)
table(ds2$InCS)


table(ds1$InCS)/length(ds1$gender)
table(ds2$InCS)/length(ds2$gender)

rm(ds1,ds2,prep_data_distancing)


df4 <- df3[which(df3$condition=="1manip"),]
df5 <- df3[which(df3$condition=="2control"),]

df3$NFC <- df3$SocDes

# distance 1 #

df <- cbind.data.frame(df3$InCS,df3$SocDes)
colnames(df) <- c("InCS","SocDes")
write.foreign(as.data.frame(df), "ds1.txt", "ds1.sps", package="SPSS")

one <- df3




#### Study 2 ####

study2fun <- function(){
  
  #df1 <- read.spss("manip_coded.sav", to.data.frame=T)
  #df2 <- read.spss("control_coded.sav", to.data.frame=T)
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_2/pilot_control_M1.sav", to.data.frame=T )
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_2/pilot_manip_M1.sav", to.data.frame=T )
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_2/pilot_manip_M1.sav", to.data.frame=T )
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_2/pilot_control_M1.sav", to.data.frame=T )
  
  x <- df1
  y <- df2
  
  
  df1$condition <- rep("1manip", length(df1$gender))
  df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  df3 <- rbind.data.frame(df1[1:146],df2[1:146])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  df3$SocDes <- c(df3$v_182==F)+
    c(df3$v_214==F)+
    c(df3$v_213==T)+
    c(df3$v_212==T)+
    c(df3$v_211==F)+
    c(df3$v_210==F)+
    c(df3$v_209==F)+
    c(df3$v_208==T)+
    c(df3$v_207==T)+
    c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  
  df3 <- x
  
  df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  
  df3
}

df3 <- study2fun()

pilot <- df3

pilot_honest <- df3[which(df3$NFC_lie<16|is.na(df3$NFC_lie)),]


df4 <- df3[which(df3$condition=="1manip"),]
df5 <- df3[which(df3$condition=="2control"),]

#setwd("..")

two_tot<- df3
dftot <- df3
df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
two <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]



#### Study 3 ####


distance_three <- function(){
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_3/anne_short_b.sav", to.data.frame = TRUE)
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_3/anne_long_b.sav", to.data.frame = TRUE)
  
  df1$condition <- rep("short", length(df1$gender))
  df2$condition <- rep("long", length(df2$gender))
  
  
  x <- df1
  y <- df2
  
  
  #df1$condition <- rep("1manip", length(df1$gender))
  #df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  screen <- function(x){
    x <- x[which(x$v_289!= "Basketball"),]
    x <- x[which(x$v_289!= "Baseball"),]
    x <- x[which(x$v_289!= "Bowling"),]
    
    x$v_289
    
    
    #x <- x[which(x$v_287==                 "9                                                                                                                                                                                                                                                              "),]
    
    #strsplit(as.character(as.vector(x$v_287)), "")[[x]]
    
    x$v_287
    
    x$v_286
    
    #y <- x[which(x$v_286=="not quoted"),]
    
    #y
    
    x <- x[which(x$v_288=="Yes"),]
    x$v_288
    
    x
  }
  
  
  df2 <- screen(df2)
  df1 <- screen(df1)
  
  df3 <- rbind.data.frame(df1[1:149],df2[1:149])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  # df3$SocDes <- c(df3$v_182==F)+
  # c(df3$v_214==F)+
  # c(df3$v_213==T)+
  # c(df3$v_212==T)+
  # c(df3$v_211==F)+
  # c(df3$v_210==F)+
  # c(df3$v_209==F)+
  # c(df3$v_208==T)+
  # c(df3$v_207==T)+
  # c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  #x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  
  x$crt_tot <- x$crt1+x$crt2+x$crt3
  
  df3 <- x
  
  #df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  
  df3
  
}



df1x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_3/anne_short_b.sav", to.data.frame = TRUE)
df2x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_3/anne_long_b.sav", to.data.frame = TRUE)

N <- length(df1x$gender)+length(df2x$gender)
rm(df1x,df2x)


df3 <- distance_three()

three_tot <- df3
dftot <- df3
df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
three <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]



#### Study 4 ####

distance_four <- function(){
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_4/R_1_short_b.sav", to.data.frame = TRUE)
  df2 <- read.spss('pilot_data/raw_SPSS_files/pilot_study_4/R_1_long.sav', to.data.frame = TRUE)
  
  df1$condition <- rep("short", length(df1$gender))
  df2$condition <- rep("long", length(df2$gender))
  
  
  x <- df1
  y <- df2
  
  
  #df1$condition <- rep("1manip", length(df1$gender))
  #df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  screen <- function(x){
    x <- x[which(x$v_289!= "Basketball"),]
    x <- x[which(x$v_289!= "Baseball"),]
    x <- x[which(x$v_289!= "Bowling"),]
    
    x$v_289
    
    
    #x <- x[which(x$v_287==                 "9                                                                                                                                                                                                                                                              "),]
    
    #strsplit(as.character(as.vector(x$v_287)), "")[[x]]
    
    x$v_287
    
    x$v_286
    
    #y <- x[which(x$v_286=="not quoted"),]
    
    #y
    
    x <- x[which(x$v_288=="Yes"),]
    x$v_288
    
    x
  }
  
  
  df2 <- screen(df2)
  df1 <- screen(df1)
  
  df3 <- rbind.data.frame(df1[1:149],df2[1:149])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  # df3$SocDes <- c(df3$v_182==F)+
  # c(df3$v_214==F)+
  # c(df3$v_213==T)+
  # c(df3$v_212==T)+
  # c(df3$v_211==F)+
  # c(df3$v_210==F)+
  # c(df3$v_209==F)+
  # c(df3$v_208==T)+
  # c(df3$v_207==T)+
  # c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  #x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  x$crt_tot <- x$crt1+x$crt2+x$crt3
  df3 <- x
  
  #df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  
  df3
  
}


#x <- df1
df1x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_4/R_1_short_b.sav", to.data.frame = TRUE)
df2x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_4/R_1_long.sav", to.data.frame = TRUE)


N <- length(df1x$gender)+length(df2x$gender)
rm(df1x,df2x)


df3 <- distance_four()

four_tot <- df3
dftot <- df3
#df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
four <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
#four <- df3




#### Study 5 ####


distance_five <- function(){
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_5/shorttime_shortpsy.sav", to.data.frame = TRUE)
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_5/longtime_shortpsy.sav", to.data.frame = TRUE)
  
  df1$condition <- rep("short", length(df1$gender))
  df2$condition <- rep("long", length(df2$gender))
  
  
  x <- df1
  y <- df2
  
  
  #df1$condition <- rep("1manip", length(df1$gender))
  #df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  screen <- function(x){
    x <- x[which(x$v_289!= "Basketball"),]
    x <- x[which(x$v_289!= "Baseball"),]
    x <- x[which(x$v_289!= "Bowling"),]
    
    x$v_289
    
    
    #x <- x[which(x$v_287==                 "9                                                                                                                                                                                                                                                              "),]
    
    #strsplit(as.character(as.vector(x$v_287)), "")[[x]]
    
    x$v_287
    
    x$v_286
    
    #y <- x[which(x$v_286=="not quoted"),]
    
    #y
    
    x <- x[which(x$v_288=="Yes"),]
    x$v_288
    
    x
  }
  
  
  df2 <- screen(df2)
  df1 <- screen(df1)
  
  df3 <- rbind.data.frame(df1[1:149],df2[1:149])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  # df3$SocDes <- c(df3$v_182==F)+
  # c(df3$v_214==F)+
  # c(df3$v_213==T)+
  # c(df3$v_212==T)+
  # c(df3$v_211==F)+
  # c(df3$v_210==F)+
  # c(df3$v_209==F)+
  # c(df3$v_208==T)+
  # c(df3$v_207==T)+
  # c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  #x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  x$crt_tot <- x$crt1+x$crt2+x$crt3
  
  df3 <- x
  
  #df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  
  df3
  
}



df1x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_5/shorttime_shortpsy.sav", to.data.frame = TRUE)
df2x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_5/longtime_shortpsy.sav", to.data.frame = TRUE)

N <- length(df1x$gender)+length(df2x$gender)
rm(df1x,df2x)

df3 <- distance_five()

five_tot <- df3
dftot <- df3
df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
five <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]






#### Study 6 ####


distance_ca_scale <- function(){
  
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_6/scale_short_time_long_psy_ mar19.sav", to.data.frame = TRUE)
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_6/scale_long_time_long_psy_mar19.sav", to.data.frame = TRUE)
  
  df1$condition <- rep("short", length(df1$gender))
  df2$condition <- rep("long", length(df2$gender))
  
  
  x <- df1
  y <- df2
  
  
  #df1$condition <- rep("1manip", length(df1$gender))
  #df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  screen <- function(x){
    x <- x[which(x$v_289!= "Basketball"),]
    x <- x[which(x$v_289!= "Baseball"),]
    x <- x[which(x$v_289!= "Bowling"),]
    
    x$v_289
    
    
    #x <- x[which(x$v_287==                 "9                                                                                                                                                                                                                                                              "),]
    
    #strsplit(as.character(as.vector(x$v_287)), "")[[x]]
    
    x$v_287
    
    x$v_286
    
    #y <- x[which(x$v_286=="not quoted"),]
    
    #y
    
    x <- x[which(x$v_288=="Yes"),]
    x$v_288
    
    x
  }
  
  
  df2 <- screen(df2)
  df1 <- screen(df1)
  
  df3 <- rbind.data.frame(df1[1:149],df2[1:149])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  # df3$SocDes <- c(df3$v_182==F)+
  # c(df3$v_214==F)+
  # c(df3$v_213==T)+
  # c(df3$v_212==T)+
  # c(df3$v_211==F)+
  # c(df3$v_210==F)+
  # c(df3$v_209==F)+
  # c(df3$v_208==T)+
  # c(df3$v_207==T)+
  # c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  #x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  x$crt_tot <- x$crt1+x$crt2+x$crt3
  
  df3 <- x
  
  #df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  df3$InCS_ju <- as.numeric(df3$v_291)
  df3$InCS_rsn <- as.numeric(df3$v_296)
  
  df3
  
}


df1x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_6/scale_short_time_long_psy_ mar19.sav", to.data.frame = TRUE)
df2x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_6/scale_long_time_long_psy_mar19.sav", to.data.frame = TRUE)

N <- length(df1x$gender)+length(df2x$gender)
rm(df1x,df2x)
# df3$v_290
# df3$v_291
# df3$v_296
df3 <- distance_ca_scale()

six_tot <- df3
dftot <- df3
df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
six <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]

df3 <- six




#### Study 7 ####

distance_ca <- function(){
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_7/ca_shorttime_longpsy_b.sav", to.data.frame = TRUE)
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_7/ca_longtime_longpsy_b.sav", to.data.frame = TRUE)
  
  df1$condition <- rep("short", length(df1$gender))
  df2$condition <- rep("long", length(df2$gender))
  
  
  x <- df1
  y <- df2
  
  
  #df1$condition <- rep("1manip", length(df1$gender))
  #df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  screen <- function(x){
    x <- x[which(x$v_289!= "Basketball"),]
    x <- x[which(x$v_289!= "Baseball"),]
    x <- x[which(x$v_289!= "Bowling"),]
    
    x$v_289
    
    
    #x <- x[which(x$v_287==                 "9                                                                                                                                                                                                                                                              "),]
    
    #strsplit(as.character(as.vector(x$v_287)), "")[[x]]
    
    x$v_287
    
    x$v_286
    
    #y <- x[which(x$v_286=="not quoted"),]
    
    #y
    
    x <- x[which(x$v_288=="Yes"),]
    x$v_288
    
    x
  }
  
  
  df2 <- screen(df2)
  df1 <- screen(df1)
  
  df3 <- rbind.data.frame(df1[1:149],df2[1:149])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  # df3$SocDes <- c(df3$v_182==F)+
  # c(df3$v_214==F)+
  # c(df3$v_213==T)+
  # c(df3$v_212==T)+
  # c(df3$v_211==F)+
  # c(df3$v_210==F)+
  # c(df3$v_209==F)+
  # c(df3$v_208==T)+
  # c(df3$v_207==T)+
  # c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  #x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  
  x$crt_tot <- x$crt1+x$crt2+x$crt3
  
  df3 <- x
  
  #df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  
  df3
  
}



df1x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_7/ca_shorttime_longpsy_b.sav", to.data.frame = TRUE)
df2x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_7/ca_longtime_longpsy_b.sav", to.data.frame = TRUE)

N <- length(df1x$gender)+length(df2x$gender)
rm(df1x,df2x)

df3 <- distance_ca()
seven <- df3
seven_tot <- df3
dftot <- df3
df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
seven <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]

df3 <- seven




#### Study 8 ####

distance_vig <- function(){
  
  df1 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_8/in_distanced_vignette_manip.sav", to.data.frame = TRUE)
  df2 <- read.spss("pilot_data/raw_SPSS_files/pilot_study_8/in_distanced_vignette_control.sav", to.data.frame = TRUE)
  
  df1$condition <- rep("short", length(df1$gender))
  df2$condition <- rep("long", length(df2$gender))
  
  
  x <- df1
  y <- df2
  
  
  #df1$condition <- rep("1manip", length(df1$gender))
  #df2$condition <- rep("2control", length(df2$gender))
  
  cbind(df1[1:2],df1$condition)
  
  
  screen <- function(x){
    x <- x[which(x$v_289!= "Basketball"),]
    x <- x[which(x$v_289!= "Baseball"),]
    x <- x[which(x$v_289!= "Bowling"),]
    
    x$v_289
    
    
    #x <- x[which(x$v_287==                 "9                                                                                                                                                                                                                                                              "),]
    
    #strsplit(as.character(as.vector(x$v_287)), "")[[x]]
    
    x$v_287
    
    x$v_286
    
    #y <- x[which(x$v_286=="not quoted"),]
    
    #y
    
    x <- x[which(x$v_288=="Yes"),]
    x$v_288
    
    x
  }
  
  
  df2 <- screen(df2)
  df1 <- screen(df1)
  
  df3 <- rbind.data.frame(df1[1:149],df2[1:149])
  df3$condition <- c(df1$condition,df2$condition)
  #df3 <- df3[which(df3$attn1!=3 ),]
  
  # df3$SocDes <- c(df3$v_182==F)+
  # c(df3$v_214==F)+
  # c(df3$v_213==T)+
  # c(df3$v_212==T)+
  # c(df3$v_211==F)+
  # c(df3$v_210==F)+
  # c(df3$v_209==F)+
  # c(df3$v_208==T)+
  # c(df3$v_207==T)+
  # c(df3$v_206==T)
  
  to_num <- function(x){
    #levels(x) <- c(0:6)
    x <- recode(x, '0=NA;"strongly disagree"=1; "moderately disagree"=2; "slightly disagree"=3; "slightly agree"=4; "moderately agree"=5;"strongly agree"=6')
    #levels(x) <- c(0:6)
    x <- suppressWarnings(as.numeric(x))
    x
  }
  
  NFC_rev <- function(x){
    
    b <- recode(x, "1=6;2=5;3=4;4=3;5=2;6=1")
    
    b <- suppressWarnings(as.numeric(b))
    b
  }
  
  
  x <- df3
  
  x$v_219 <- to_num(x$v_219)
  x$v_220 <- NFC_rev(to_num(x$v_220))
  x$v_221 <- to_num(x$v_221)
  x$v_222 <- to_num(x$v_222)
  x$v_223 <- NFC_rev(to_num(x$v_223))
  x$v_224 <- to_num(x$v_224)
  x$v_225 <- NFC_rev(to_num(x$v_225))
  x$v_226 <- to_num(x$v_226)
  x$v_227 <- to_num(x$v_227)
  x$v_228 <- to_num(x$v_228)
  x$v_229 <- to_num(x$v_229)
  x$v_230 <- NFC_rev(to_num(x$v_230))
  x$v_231 <- NFC_rev(to_num(x$v_231))
  x$v_232 <- to_num(x$v_232)
  x$v_233 <- to_num(x$v_233)
  x$v_234 <- NFC_rev(to_num(x$v_234))
  x$v_235 <- to_num(x$v_235)
  x$v_236 <- to_num(x$v_236)
  x$v_237 <- NFC_rev(to_num(x$v_237))
  x$v_238 <- NFC_rev(to_num(x$v_238))
  x$v_239 <- to_num(x$v_239)
  x$v_240 <- to_num(x$v_240)
  x$v_241 <- NFC_rev(to_num(x$v_241))
  x$v_242 <- to_num(x$v_242)
  x$v_243 <- NFC_rev(to_num(x$v_243))
  x$v_244 <- to_num(x$v_244)
  x$v_245 <- to_num(x$v_245)
  x$v_246 <- NFC_rev(to_num(x$v_246))
  x$v_247 <- NFC_rev(to_num(x$v_247))
  x$v_248 <- to_num(x$v_248)
  x$v_249 <- to_num(x$v_249)
  x$v_250 <- to_num(x$v_250)
  x$v_251 <- to_num(x$v_251)
  x$v_252 <- to_num(x$v_252)
  x$v_253 <- to_num(x$v_253)
  x$v_254 <- NFC_rev(to_num(x$v_254))
  x$v_255 <- to_num(x$v_255)
  x$v_256 <- to_num(x$v_256)
  x$v_257 <- to_num(x$v_257)
  x$v_258 <- NFC_rev(to_num(x$v_258))
  x$v_259 <- NFC_rev(to_num(x$v_259))
  x$v_260 <- to_num(x$v_260)
  x$v_261 <- to_num(x$v_261)
  x$v_262 <- to_num(x$v_262)
  x$v_263 <- to_num(x$v_263)
  x$v_264 <- to_num(x$v_264)
  x$v_265 <- NFC_rev(to_num(x$v_265))
  
  
  x$NFC_lie <- x$v_236 + x$v_240 + x$v_257 + x$v_261 + x$v_264
  
  x$NFC_lie
  x$NFC <- x$v_219 + x$v_220 + x$v_221 + x$v_222 + x$v_223 + x$v_224 + x$v_225 + x$v_226 + x$v_227 + x$v_228 + x$v_229 + x$v_230 +
    x$v_231 + x$v_232 + x$v_233 + x$v_234 + x$v_235 + x$v_237 + x$v_238 + x$v_239 + x$v_241 + x$v_242 + x$v_243 + 
    x$v_244 + x$v_245 + x$v_246 + x$v_247 + x$v_248 + x$v_249 + x$v_250 + x$v_251 + x$v_252 + x$v_253 + x$v_254 + x$v_255 + x$v_256 +
    x$v_258 + x$v_259 + x$v_260 + x$v_262 + x$v_263 + x$v_265
  
  x$nfc_order <- x$v_219 + x$v_224 + x$v_229 + x$v_238 + x$v_242 + x$v_246 + x$v_252 + x$v_253 + x$v_255 + x$v_265
  x$nfc_pred <- x$v_223 + x$v_225 + x$v_226 + x$v_237 + x$v_244 + x$v_245 + x$v_248 + x$v_263
  x$nfc_dec <- x$v_230 + x$v_231 + x$v_232 + x$v_234 + x$v_235 + x$v_241 + x$v_258
  x$nfc_amb <- x$v_221 + x$v_227 + x$v_233 + x$v_239 + x$v_249 + x$v_250 + x$v_251 + x$v_256 + x$v_260
  x$nfc_clos <- x$v_220 + x$v_222 + x$v_228 + x$v_243 + x$v_247 + x$v_254 + x$v_259 + x$v_262
  
  #x$attnJM <- c(x$attn1=="right") + c(x$attn2=="right") + c(x$attn3=="right")
  x$crt_tot <- x$crt1+x$crt2+x$crt3
  
  df3 <- x
  
  #df3$crt <- c(df3$baseball=="right")+ c(df3$machine=="right")+ c(df3$lillypad=="right")
  
  to_num <- function(x){
    levels(x) <- c(1:7)
    x <- as.numeric(x)
    
  }
  #use 'to_num' to rename target variable/create a new variable
  ds6 <- df3
  ds6$InJu1 <- to_num(ds6$InJu1)
  ds6$InJu2 <- to_num(ds6$InJu2)
  ds6$InCf1 <- to_num(ds6$InCf1)
  ds6$InCf2 <- to_num(ds6$InCf2)
  
  
  # ds1$InJu1 <- to_num(ds1$InJu1)
  # ds1$InJu2 <- to_num(ds1$InJu2)
  # ds1$InCf1 <- to_num(ds1$InCf1)
  # ds1$InCf2 <- to_num(ds1$InCf2)
  # 
  # 
  # ds2$InJu1 <- to_num(ds2$InJu1)
  # ds2$InJu2 <- to_num(ds2$InJu2)
  # ds2$InCf1 <- to_num(ds2$InCf1)
  # ds2$InCf2 <- to_num(ds2$InCf2)
  
  df3 <- ds6
  df3 <- df3[which(is.na(df3$gender)==FALSE),]
  
  
  df3
  
}



df1x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_8/in_distanced_vignette_manip.sav", to.data.frame = TRUE)
df2x <- read.spss("pilot_data/raw_SPSS_files/pilot_study_8/in_distanced_vignette_control.sav", to.data.frame = TRUE)

N <- length(df1x$gender)+length(df2x$gender)
rm(df1x,df2x)

df3 <- distance_vig()
eight <- df3
eight_tot <- df3
dftot <- df3
df3 <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]
eight <- df3[which(df3$NFC_lie<15|is.na(df3$NFC_lie)),]

df3 <- eight

#### Combined (no exclusion) ####

# one: psych
# two: psych
# three: temp
# four: temp
# five: temp (psych short)
# six: temp (scale)
# seven: temp
# eight: temp (confounded)


x <- one
# combine variables of interest
one_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(one_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")
# distinguish between psychological distance 'psych' and temporal distance 'temp'
x <- one_cb
x$condition <- recode(x$condition, "'1manip'='long'; '2control'='control'")
x$temp_psych <- recode(x$temp, "'1manip'='short_long'; '2control'='control'")
x$psych <- recode(x$psych, "'1manip'='long'; '2control'='control'")
x$temp <- rep("short", length(x$gender))
one_cb <- x


x <- two

two_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(two_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- two_cb
x$condition <- recode(x$condition, "'1manip'='long'; '2control'='control'")
x$temp_psych <- recode(x$temp, "'1manip'='short_long'; '2control'='control'")
x$psych <- recode(x$psych, "'1manip'='long'; '2control'='control'")
x$temp <- rep("short", length(x$gender))
two_cb <- x



x <- three_tot

three_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(three_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- three_cb
x$psych <- rep("long", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_long'; 'long'='long_long'")
three_cb <- x

x <- four_tot

four_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(four_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- four_cb
x$psych <- rep("long", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_long'; 'long'='long_long'")
four_cb <- x

x <- five_tot

five_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(five_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- five_cb
x$psych <- rep("short", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_short'; 'long'='long_short'")
five_cb <- x

x <- seven_tot

seven_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(seven_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- seven_cb
x$psych <- rep("long", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_long'; 'long'='long_long'")
seven_cb <- x


# onetofive <- rbind(one_cb, two_cb, three_cb, four_cb, five_cb)
# twotofive <- rbind(two_cb, three_cb, four_cb, five_cb)
# threetofive <- rbind(three_cb, four_cb, five_cb)

onetoseven_tot <- rbind(one_cb, two_cb, three_cb, four_cb, five_cb, seven_cb)

#### Combined ####


x <- one
# combine variables of interest
one_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(one_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")
# distinguish between psychological distance 'psych' and temporal distance 'temp'
x <- one_cb
x$condition <- recode(x$condition, "'1manip'='long'; '2control'='control'")
x$temp_psych <- recode(x$temp, "'1manip'='short_long'; '2control'='control'")
x$psych <- recode(x$psych, "'1manip'='long'; '2control'='control'")
x$temp <- rep("short", length(x$gender))
one_cb <- x


x <- two

two_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(two_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- two_cb
x$condition <- recode(x$condition, "'1manip'='long'; '2control'='control'")
x$temp_psych <- recode(x$temp, "'1manip'='short_long'; '2control'='control'")
x$psych <- recode(x$psych, "'1manip'='long'; '2control'='control'")
x$temp <- rep("short", length(x$gender))
two_cb <- x



x <- three

three_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(three_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- three_cb
x$psych <- rep("long", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_long'; 'long'='long_long'")
three_cb <- x

x <- four

four_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(four_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- four_cb
x$psych <- rep("long", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_long'; 'long'='long_long'")
four_cb <- x

x <- five

five_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(five_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- five_cb
x$psych <- rep("short", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_short'; 'long'='long_short'")
five_cb <- x

x <- seven

seven_cb <- cbind.data.frame(x$condition,x$condition,x$condition,x$InCS,x$NFC,x$InJu1,x$InJu2,x$gender,x$age)
colnames(seven_cb) <- c("condition","temp","psych","InCS","NFC","InJu1","InJu2","gender","age")

x <- seven_cb
x$psych <- rep("long", length(x$gender))
x$temp_psych <- recode(x$temp, "'short'='short_long'; 'long'='long_long'")
seven_cb <- x


# onetofive <- rbind(one_cb, two_cb, three_cb, four_cb, five_cb)
# twotofive <- rbind(two_cb, three_cb, four_cb, five_cb)
# threetofive <- rbind(three_cb, four_cb, five_cb)

onetoseven <- rbind(one_cb, two_cb, three_cb, four_cb, five_cb, seven_cb)
# 
# df3 <- onetoseven
# df3$condition <- recode(df3$condition, "'1manip'='long'")
# 
# onetofive <- df3
# df3 <- twotofive
# df3$condition <- recode(df3$condition, "'1manip'='long'")
# twotofive <- df3
# df3 <- threetofive
# df3$condition <- recode(df3$condition, "'1manip'='long'")
# threetofive <- df3

df3 <- onetoseven
#car::recode(df3$Ju1_bin, "'wrong'='wrong'; 'neutral'='not wrong'; 'right'='not wrong'")

#### rw ####

# Saving on object in RData format
save(one, file = "pilot_data/loaded_data/one.RData")
save(two,two_tot, file = "pilot_data/loaded_data/two.RData")
save(three,three_tot, file = "pilot_data/loaded_data/three.RData")
save(four,four_tot, file = "pilot_data/loaded_data/four.RData")
save(five,five_tot, file = "pilot_data/loaded_data/five.RData")
save(six,six_tot, file = "pilot_data/loaded_data/six.RData")
save(seven,seven_tot, file = "pilot_data/loaded_data/seven.RData")
save(eight,eight_tot, file = "pilot_data/loaded_data/eight.RData")
save(onetoseven,onetoseven_tot, file = "pilot_data/loaded_data/onetoseven.RData")


## save Study 1
write.csv(one, file="pilot_data/csv_files/pilot_study_1.csv",row.names = FALSE)


## save Study 2
write.csv(two_tot, file="pilot_data/csv_files/pilot_study_2_full.csv",row.names = FALSE)
write.csv(two, file="pilot_data/csv_files/pilot_study_2_clean.csv",row.names = FALSE)

## save Study 3
write.csv(three_tot, file="pilot_data/csv_files/pilot_study_3_full.csv",row.names = FALSE)
write.csv(three, file="pilot_data/csv_files/pilot_study_3_clean.csv",row.names = FALSE)

## save Study 4
write.csv(four_tot, file="pilot_data/csv_files/pilot_study_4_full.csv",row.names = FALSE)
write.csv(four, file="pilot_data/csv_files/pilot_study_4_clean.csv",row.names = FALSE)

## save Study 5
write.csv(five_tot, file="pilot_data/csv_files/pilot_study_5_full.csv",row.names = FALSE)
write.csv(five, file="pilot_data/csv_files/pilot_study_5_clean.csv",row.names = FALSE)

## save Study 6
write.csv(six_tot, file="pilot_data/csv_files/pilot_study_6_full.csv",row.names = FALSE)
write.csv(six, file="pilot_data/csv_files/pilot_study_6_clean.csv",row.names = FALSE)

## save Study 7
write.csv(seven_tot, file="pilot_data/csv_files/pilot_study_7_full.csv",row.names = FALSE)
write.csv(seven, file="pilot_data/csv_files/pilot_study_7_clean.csv",row.names = FALSE)

## save Study 8
write.csv(eight_tot, file="pilot_data/csv_files/pilot_study_8_full.csv",row.names = FALSE)
write.csv(eight, file="pilot_data/csv_files/pilot_study_8_clean.csv",row.names = FALSE)

## save combined (Studies 1,2,3,4,5,7)
write.csv(onetoseven_tot, file="pilot_data/csv_files/pilot_combined_123457_full.csv",row.names = FALSE)
write.csv(onetoseven, file="pilot_data/csv_files/pilot_combined123457_clean.csv",row.names = FALSE)
































rm(list = ls())


