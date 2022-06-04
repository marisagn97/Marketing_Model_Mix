library(readr)
library(dplyr)
library(grid)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(gridExtra)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
sales <- read_csv('data/mmm_sales.csv')
macro <- read_csv('data/mmm_macro.csv')
spend <- read_csv('data/mmm_spend.csv')
compets <- read_csv('data/mmm_compets.csv')
canibalizacion <- read_csv('data/mmm_canib.csv')

#Me quedo en un primer momento con el pbase de los compets y de la canibalización
compets2<-compets%>%filter(fecha>'2019-01-01')%>%
  select(where(~ any(. != 0))) %>%
  select(starts_with("pbase"),fecha,starts_with("dp"))

canib2<-canibalizacion%>%filter(fecha>'2019-01-01')%>%
  select(where(~ any(. != 0))) %>%
  select(starts_with("pbase"),fecha)

# #Vamos a ver los compets más presentes en tienda(dp más altos)
# competsdp<-compets%>%filter(fecha>'2019-01-01')%>%
#   select(where(~ any(. != 0))) %>%
#   select(starts_with("dp_compet"))
# Qcompetsdp <- as_tibble(sapply(colnames(competsdp[2:length(competsdp)]), function( y ) {
#   quantile( x = unlist( competsdp[,  y ] ), 
#             c(0, .25, .5, .75, 1),
#             na.rm = TRUE )
# }))
competspb_names<- names(compets2%>%select(.,starts_with("pbase")))
competsdp_names<- names(compets2%>%select(.,starts_with("dp")))
compets2[paste("log",competspb_names,sep="_")] <- log(compets2[competspb_names])
compets2[paste("sqrt",competsdp_names,sep="_")] <- compets2[competsdp_names]^0.5
competsp<- compets2

canibpb_names<- names(canib2%>%select(.,starts_with("pbase")))
canib2[paste("log",canibpb_names,sep="_")] <- log(canib2[canibpb_names])
canibp_base <- canib2

##DATOS (competsp y canibp_base lo meto luego)
a=list(sales,macro,spend)
data= Reduce(function(...) merge(..., all=TRUE), a)
data <- data%>%filter(fecha>'2019-01-01')


reemplaza<- function(texto) {
  texto_output <- str_to_lower(texto)
  texto_output <- str_replace_all(texto_output, "yt", "youtube")
  texto_output <- str_replace(texto_output, "fb", "facebook")
  texto_output <- str_replace(texto_output, "ig", "instagram")
  return(texto_output)
}

data <- data %>%
  rename_with(
    .fn = reemplaza,
    .cols = matches("fb|yt|ig")
  )

#Creo columnas
data <- data%>% mutate(year=as.numeric(format(data$fecha,'%Y')),
                    month=month(data$fecha,label=TRUE),
                    week=week(data$fecha),
                    tv_new = rowSums(select(., starts_with("tv"))),
                    ext_new = rowSums(select(., starts_with("ext"))),
                    facebook_new = rowSums(select(., starts_with("facebook"))),
                    instagram_new= rowSums(select(., starts_with("instagram"))),
                    twitter_new = rowSums(select(., starts_with("twitter"))),
                    radio_new = rowSums(select(., starts_with("radio"))),
                    youtube_new = rowSums(select(., starts_with("youtube"))))
data<- data %>%select(where(~ any(. != 0)))
df <- data%>%select(.,fecha,week,year,v_vol_foo_normal_format1,pbase_foo_normal_format1,starts_with("youtube"),
                    starts_with("facebook"),starts_with("instagram"),
                    starts_with("ext"),starts_with("twitter"),
                    starts_with("tv"),starts_with("radio"),dif_temp,
                    precip,laboralidad,starts_with("dp"),starts_with("pbase"))

df <- df %>% mutate(medios=facebook_new+instagram_new+youtube_new+twitter_new,
                    lnventas=log(v_vol_foo_normal_format1),
                   lnprecios=log(pbase_foo_normal_format1))

#Transformacion a log de la dp
dp_var<- df%>%select(.,starts_with("dp"))
dp_names<- names(df%>%select(.,starts_with("dp")))
for (i in (1:length(dp_var))){
  v=dp_names[i]
  var=dp_var[,i]
  df[paste("sqrt",v,sep="_")] <- var^(0.5)
}

df <- merge(df, competsp, by="fecha")
df <- merge(df, canibp_base, by="fecha")

Quantiles=as_tibble(sapply(colnames(df[4:length(df)]), function( y ) {
  quantile( x = unlist( df[,  y ] ), 
            c(0, .25, .5, .75, 1),
            na.rm = TRUE )
}))




##############TRANSFORMACIONES###############

funcion_propia <- function(x, ads, hill) {
  x_trans <- x / (x + hill)
  x_trans <- stats::filter(x_trans * (1 - ads), filter = ads, method = "recursive")
  return(x_trans)
}

##TV
m=max(df$tv_new)
adstocks_posibles <- seq(0.4, 0.9, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

T <- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("tv")) %>% 
    mutate(across(contains("tv"), ~funcion_propia(., 
                                                  df_combinaciones$Var1[i], 
                                                  df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("tv")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )

})
TV <- T %>% filter(rowname=='tv_new')
parametrosTV <- TV[which.max(TV$v_vol_foo_normal_format1),]
QTV <- t(as_tibble(quantile(df$tv_new)))[]
colnames(QTV) <- c('min','Q1','Q2','Q3','max')
parametrosTV <- cbind(parametrosTV,QTV)



##Radio
m=max(df$radio_new)
adstocks_posibles <- seq(0.2, 0.6, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

R<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("radio")) %>% 
    mutate(across(contains("radio"), ~funcion_propia(., 
                                                  df_combinaciones$Var1[i], 
                                                  df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("radio")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
RA<- R %>% filter(rowname=='radio_new')
parametrosR <- RA[which.max(RA$v_vol_foo_normal_format1),]
QR<- t(as_tibble(quantile(df$radio_new)))[]
colnames(QR) <- c('min','Q1','Q2','Q3','max')
parametrosR<- cbind(parametrosR,QR)

##Twitter
m=max(df$twitter_new)
adstocks_posibles <- seq(0.1, 0.4, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

Tw<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("twitter")) %>% 
    mutate(across(contains("twitter"), ~funcion_propia(., 
                                                  df_combinaciones$Var1[i], 
                                                  df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("twitter")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
TW<- Tw %>% filter(rowname=='twitter_new')
parametrosTw <- TW[which.max(TW$v_vol_foo_normal_format1),]
QT <- t(as_tibble(quantile(df$twitter_new)))[]
colnames(QT) <- c('min','Q1','Q2','Q3','max')
parametrosTw <- cbind(parametrosTw,QT)


##EXT
m=max(df$ext_new)
adstocks_posibles <- seq(0.2, 0.6, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

E<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("ext")) %>% 
    mutate(across(contains("ext"), ~funcion_propia(., 
                                                       df_combinaciones$Var1[i], 
                                                       df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("ext")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
Ex<- E%>% filter(rowname=='ext_new')
parametrosEx <- Ex[which.max(Ex$v_vol_foo_normal_format1),]
QEx <- t(as_tibble(quantile(df$ext_new)))[]
colnames(QEx) <- c('min','Q1','Q2','Q3','max')
parametrosEx <- cbind(parametrosEx,QEx)

##Facebook
m=max(df$facebook_new)
adstocks_posibles <- seq(0.1, 0.4, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

F<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("facebook")) %>% 
    mutate(across(contains("facebook"), ~funcion_propia(., 
                                                   df_combinaciones$Var1[i], 
                                                   df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("facebook")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
Fb<- F%>% filter(rowname=='facebook_new')
parametrosFb<- Fb[which.max(Fb$v_vol_foo_normal_format1),]
QF<- t(as_tibble(quantile(df$facebook_new)))[]
colnames(QF) <- c('min','Q1','Q2','Q3','max')
parametrosFb <- cbind(parametrosFb,QF)

##youtube
m=max(df$youtube_new)
adstocks_posibles <- seq(0.1, 0.4, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

Y<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("youtube")) %>% 
    mutate(across(contains("youtube"), ~funcion_propia(., 
                                                        df_combinaciones$Var1[i], 
                                                        df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("youtube")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
You<- Y%>% filter(rowname=='youtube_new')
parametrosY<- You[which.max(You$v_vol_foo_normal_format1),]
QY <- t(as_tibble(quantile(df$youtube_new)))[]
colnames(QY) <- c('min','Q1','Q2','Q3','max')
parametrosY <- cbind(parametrosY,QY)


##instagram
m=max(df$instagram_new)
adstocks_posibles <- seq(0.1, 0.4, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

I<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("instagram")) %>% 
    mutate(across(contains("instagram"), ~funcion_propia(., 
                                                       df_combinaciones$Var1[i], 
                                                       df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("instagram")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
Ins<- I%>% filter(rowname=='instagram_new')
parametrosIns<- Ins[which.max(Ins$v_vol_foo_normal_format1),]
QIn <- t(as_tibble(quantile(df$instagram_new)))[]
colnames(QIn) <- c('min','Q1','Q2','Q3','max')
parametrosIns<- cbind(parametrosIns,QIn)


##medios
m=max(df$medios)
adstocks_posibles <- seq(0.1, 0.4, 0.1)
hills_posibles <- seq(0.5,1,0.1)*m
df_combinaciones <- as_tibble(expand.grid(adstocks_posibles, hills_posibles))

M<- map_dfr(seq_len(nrow(df_combinaciones)), function(i) {
  df_redux <- df%>% 
    select(v_vol_foo_normal_format1, contains("medios")) %>% 
    mutate(across(contains("medios"), ~funcion_propia(., 
                                                         df_combinaciones$Var1[i], 
                                                         df_combinaciones$Var2[i])))
  
  df_corr <- cor(x = df_redux %>% select(contains("medios")), 
                 y = df_redux %>% select(v_vol_foo_normal_format1)) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  
  df_corr %>% 
    mutate(
      adstock = df_combinaciones$Var1[i],
      hill_parameter = df_combinaciones$Var2[i],
    )
  
})
parametrosM<- M[which.max(M$v_vol_foo_normal_format1),]
QM <- t(as_tibble(quantile(df$medios)))[]
colnames(QM) <- c('min','Q1','Q2','Q3','max')
parametrosM <- cbind(parametrosM,QM)

######
aplica_hill <- function(x, param) return(x / (x + param))
transforma_variable <- function(x, hill, adstock) {
  x_ads <- stats::filter((1 - adstock) * aplica_hill(x, hill), filter = adstock, method = "recursive")
  x_ads <- as.numeric(x_ads)
  return (x_ads)
}

df <- df %>% 
  mutate(
    across(
    contains("tv") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosTV$hill_parameter, parametrosTV$adstock))
  ),
    across(
    contains("medios") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosM$hill_parameter, parametrosM$adstock))
  ),
    across(
    contains("radio") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosR$hill_parameter, parametrosR$adstock))
  ),
    across(
    contains("twitter") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosTw$hill_parameter, parametrosTw$adstock))
  ),
    across(
    contains("ext") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosEx$hill_parameter, parametrosEx$adstock))
  ),
    across(
    contains("youtube") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosY$hill_parameter, parametrosY$adstock))
  ),
  across(
    contains("instagram") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosIns$hill_parameter, parametrosIns$adstock))
  ),
  across(
    contains("facebook") & - contains("dp"), 
    list(trans = ~transforma_variable(., parametrosFb$hill_parameter, parametrosFb$adstock))
  ),
  )
                    


parametros <- rbind(parametrosEx,parametrosFb,parametrosIns,parametrosR,parametrosM,parametrosTV,parametrosTw,parametrosY)
adstocksplot<-ggplot(data=parametros, aes(x=rowname, y=adstock)) +
  geom_bar(stat="identity", fill='lightblue')
adstocksplot
p1 <- ggplot(df,aes(fecha,facebook_new_trans))+geom_line()
p2 <- ggplot(df,aes(fecha,twitter_new_trans))+geom_line()
p3 <- ggplot(df,aes(fecha,instagram_new_trans))+geom_line()
p4 <- ggplot(df,aes(fecha,youtube_new_trans))+geom_line()
p5 <- ggplot(df,aes(fecha,radio_new_trans))+geom_line()
p6 <- ggplot(df,aes(fecha,tv_new_trans))+geom_line()
p7 <- ggplot(df,aes(fecha,ext_new_trans))+geom_line()
p8 <- ggplot(df,aes(fecha,medios_trans))+geom_line()
grid.arrange(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  p7,
  p8,
  nrow = 2
)

#Variables necesarias modelos

#Por ejemplo veo que necesito la variable navidad
df <- df %>% 
  mutate(navidad = if_else(fecha <= as.Date("2019-01-09") | (as.Date("2019-12-21") <= fecha & fecha<= as.Date("2020-01-09")) | (as.Date("2020-12-21") <= fecha & fecha<= as.Date("2021-01-09")) | (as.Date("2021-12-21") <= fecha & fecha<= as.Date("2022-01-09")) ,1, 0),
         verano = if_else((as.Date("2019-07-01") <= fecha & fecha<= as.Date("2019-08-31")) | (as.Date("2020-07-01") <= fecha & fecha<= as.Date("2020-08-31")) | (as.Date("2021-07-01") <= fecha & fecha<= as.Date("2021-08-31")) ,1, 0),
         #octubre19=if_else((as.Date("2019-10-01") <= fecha & fecha<= as.Date("2019-10-30")),1, 0),
         twitter_subidas = if_else(as.Date("2020-06-14") == fecha, 1, 0),
         
  )


# navidad19=if_else(fecha <= as.Date("2019-01-09") ,1, 0),
# navidad20=if_else((as.Date("2019-12-21") <= fecha & fecha<= as.Date("2020-01-09")),1, 0),
# navidad21=if_else((as.Date("2020-12-21") <= fecha & fecha<= as.Date("2021-01-09")),1, 0),
# navidad22=if_else((as.Date("2021-12-21") <= fecha & fecha<= as.Date("2022-01-09")) ,1, 0),

#Variable 1semana del mes
df <- df %>% 
  mutate(primerasem = if_else(week %in% seq(1, 52, 4)  ,1, 0))

#Creo nueva variable radio
df <- df %>%  
  mutate(radio1 = if_else(radio_new_trans!=0, 1, 0))

#Creo nueva variable ext
df <- df %>%  
  mutate(ext1 = if_else(ext_new_trans!=0, 1, 0))

#Creo variable Covid
df <- df %>% 
  mutate(Covid = if_else((as.Date("2020-03-15") <= fecha & fecha<= as.Date("2020-06-21"))  ,1, 0))


#Vamos a crear una variable en la semanas que se alcanza el pico mas alto de navidad
#1ero ordeno df por max en la columna vol
d2 <- df[order(df$v_vol_foo_normal_format1),]
View(d2)
df <- df %>% 
  mutate(navidad20v2=if_else(fecha== as.Date("2019-12-29")|fecha== as.Date("2019-12-22"),1, 0),
         navidad21v2=if_else(fecha== as.Date("2020-12-27"),1, 0),
         navidad22v2=if_else(fecha== as.Date("2021-12-26")|fecha== as.Date("2022-01-02") ,1, 0)
         )
write.csv(df, "df.csv", row.names = FALSE)


cat(names(df), sep = "\n + ")
#gráficos
#medios=face,insta youtube y twitter
df %>%
  select(fecha, tv_new_trans,tv_new) %>%
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

df %>%
  select(fecha, medios_trans,medios) %>%
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")



df %>%
  select(fecha, v_vol_foo_normal_format1,medios_trans) %>%
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

df %>%
  select(fecha, v_vol_foo_normal_format1,contains("ext") & contains("trans") & -contains("new")) %>%
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
# facebook_color_plan_2020, facebook_foo,facebook_new,
# facebook_foo_producto_2020,facebook_producto_2021
#filter(as.Date("2019-06-01") <= fecha & fecha<= as.Date("2019-12-01")) %>% 