library(readr)
library(dplyr)
library(tidyverse)
library(corrplot)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
df<- read_csv('df.csv')
#Correlaciones
x1 <- df
x1 <- x1[,4:length(x1)]
y <- df[,c('v_vol_foo_normal_format1')]
corr <- cor(x1,y)
corr

y <- df[,c('lnventas')]
corr <- cor(x1,y)
corr

tvdata <- df %>%select("v_vol_foo_normal_format1",contains("tv") & - contains("dp") & contains("trans"))
corrtv <- round(cor(tvdata),2)
corrplot(corrtv, method="number", mar=c(3,3,5,3), type="upper")

radiodata <- df %>%select(contains("radio") & - contains("dp") & contains("trans"))
corrradio <- round(cor(radiodata),2)
corrplot(corrradio, method="number", mar=c(3,3,5,3), type="upper")

extdata <- df %>%select(contains("ext") & - contains("dp") & contains("trans"))
corrext <- round(cor(extdata),2)
corrplot(corrext, method="number", mar=c(3,3,5,3), type="upper")

instadata <- df %>%select(contains("instagram") & - contains("new") & - contains("dp") & contains("trans"))
corrinsta <- round(cor(instadata),2)
corrplot(corrinsta, method="number", mar=c(3,3,5,3), type="upper")

facedata <- df %>%select(contains("facebook")& - contains("new") & - contains("dp") & contains("trans"))
corrface <- round(cor(facedata),2)
corrplot(corrface, method="number", mar=c(3,3,5,3), type="upper")

corrfaceinsta <- round(cor(instadata,facedata),2)
corrplot(corrfaceinsta, method="number", mar=c(3,3,5,3), type="upper")


twitterdata <- df %>%select(contains("twitter") & - contains("dp") & contains("trans"))
corrtwitter <- round(cor(twitterdata),2)
corrplot(corrtwitter, method="number", mar=c(3,3,5,3), type="upper")

youtubedata <- df %>%select("v_vol_foo_normal_format1",contains("youtube") & - contains("dp")  & - contains("new") & contains("trans"))
corryoutube <- round(cor(youtubedata),2)
corrplot(corryoutube, method="number", mar=c(3,3,5,3), type="upper")

mediosdata<- df %>%select((contains("medios") | contains("tv_new") | contains("radio_new") | contains("ext_new") )& - contains("dp") & contains("trans"))
corrmedios <- round(cor(mediosdata),2)
corrplot(corrmedios, method="number", mar=c(3,3,5,3), type="upper")

corrdata<- df %>%select("v_vol_foo_normal_format1","youtube_new_trans","radio_new_trans","tv_new_trans", "ext_new_trans", "twitter_new_trans", "instagram_new_trans", "facebook_new_trans")
corrdata <- round(cor(corrdata),2)
corrplot(corrdata, method="number", mar=c(3,3,5,3), type="upper")

dpdata <- df %>%select(contains("dp") & contains("log"))
corrdp <- round(cor(dpdata),2)
#corrplot(corrdp, method="number", mar=c(3,3,5,3), type="upper")


#Variables del modelo sin log
Contribvar=df %>% select("v_vol_foo_normal_format1","precip","lnprecios","log_pbase_compet1_format5","log_pbase_compet4_format2",
             "sqrt_dp_compet1_format5","sqrt_dp_compet4_format3", "sqrt_dp_promo_display_brochure_foo_normal_format1",
             "sqrt_dp_compet7","tv_foo_camp1_a_trans","tv_foo_camp8_2020_trans", "youtube_foo_2021_trans",
             "facebook_color_plan_2020_trans","navidad20v2","navidad21v2", "navidad22v2", "primagost19",
             "oct21")
corrdata <- round(cor(Contribvar),2)
corrplot(corrdata, mar=c(3,3,5,3), type="upper")
