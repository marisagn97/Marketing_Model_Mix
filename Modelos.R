library(readr)
library(dplyr)
library(grid)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(gridExtra)
library(lmtest)
library(tseries)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
df<- read_csv('df.csv')

Modeloregresion<- function(fit_lm) {
  df_diagnosis <- tibble(
    fecha = df$fecha,
    original = df$lnventas, 
    ajuste = fit_lm$fitted.values, 
    residuos = fit_lm$residuals
  )
  p <- df_diagnosis %>% 
    ggplot() + 
    geom_line(aes(x = fecha, y = residuos))+
    ggtitle("Residuos")
  print(p)
  #Veo donde no ajusta bien
  q <- df_diagnosis %>%
    select(-residuos) %>%  
    pivot_longer(-fecha) %>% 
    ggplot() + 
    geom_line(aes(x = fecha, y = value, col = name))+
    ggtitle("Ajuste")
  print(q)
  return(summary(fit_lm))
}


Modeloregresionnolog<- function(fit_lm) {
  df_diagnosis <- tibble(
    fecha = df$fecha,
    original = df$v_vol_foo_normal_format1, 
    ajuste = fit_lm$fitted.values, 
    residuos = fit_lm$residuals
  )
  p <- df_diagnosis %>% 
    ggplot() + 
    geom_line(aes(x = fecha, y = residuos))+
    ggtitle("Residuos")
  print(p)
  #Veo donde no ajusta bien
  q <- df_diagnosis %>%
    select(-residuos) %>%  
    pivot_longer(-fecha) %>% 
    ggplot() + 
    geom_line(aes(x = fecha, y = value, col = name))+
    ggtitle("Ajuste")
  print(q)
  return(summary(fit_lm))
}


#Gráficos

df %>% 
  as_tibble() %>% 
  select(fecha, lnventas, lnprecios, sqrt_dp_foo_normal_format1) %>% 
  pivot_longer(-c(fecha)) %>% 
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_wrap(~ name, scales = "free_y") 



df %>% 
  as_tibble() %>% 
  select(week, lnventas, lnprecios, sqrt_dp_foo_normal_format1) %>% 
  pivot_longer(-c(week)) %>% 
  ggplot() + 
  geom_line(aes(x = week, y = value)) + 
  facet_wrap(~ name, scales = "free_y") 



# ver variables 
cat(names(df), sep = "\n + ")


df <- df %>% 
  mutate(v06_01_19=if_else(as.Date("2019-01-06") == fecha ,1,0),
    sem_pregenerales_21_04_19=if_else(as.Date("2019-04-21") == fecha ,1,0),
    twitter_foo_new=if_else(as.Date("2019-04-14") <= fecha & fecha<= as.Date("2019-04-28") ,twitter_foo,0),
    radio21_4_19=if_else(as.Date("2019-04-21") == fecha ,radio_foo_2019_trans,0),
         v_04_11_ag19=if_else(fecha== as.Date("2019-08-04")|fecha== as.Date("2019-08-11") ,1, 0),
    v_18_08_19=if_else(as.Date("2019-08-18") == fecha ,1,0),
    faceabril2019=if_else(as.Date("2019-03-31") <= fecha & fecha<= as.Date("2019-05-01") ,facebook_foo_producto_2019_trans, 0),
    faceoct2019=if_else(as.Date("2019-09-16") <= fecha & fecha<= as.Date("2019-10-15"),facebook_foo_producto_2019_trans, 0),
    instagram_foo_producto_2019_trans_new=if_else(as.Date("2019-04-07") <= fecha & fecha<= as.Date("2019-04-21"),instagram_foo_producto_2019_trans, 0),
    ext_foo_pm_2019_trans_new= if_else(ext_foo_pm_2019_trans!=0,1, 0), 
    v_05_12_jan20=if_else(as.Date("2020-01-05") <= fecha & fecha<= as.Date("2020-01-12") ,1,0),
    v_05_19_jan20=if_else(as.Date("2020-01-05") <= fecha & fecha<= as.Date("2020-01-19") ,1,0),
    v_12_01_20=if_else(as.Date("2020-01-12") == fecha ,1,0),  
    v_05_01_20=if_else(as.Date("2020-01-05") == fecha ,1,0),
    v_19_23_enfeb20=if_else(as.Date("2020-01-19") <= fecha & fecha<= as.Date("2020-02-23") ,1,0),
    v_09_23_feb20=if_else(as.Date("2020-02-09") <= fecha & fecha<= as.Date("2020-02-23") ,1,0),
      primsemconfinamiento_15_03_20=if_else(as.Date("2020-03-15") == fecha ,1,0),#primsemconfinamiento
         v_31_05_20=if_else(as.Date("2020-05-31") == fecha ,1,0),
         v_07_06_20=if_else(as.Date("2020-06-07") == fecha ,1,0),
    v_21_06_20=if_else(as.Date("2020-06-21") == fecha ,1,0),
    v_21_05junjul20=if_else(as.Date("2020-06-21") <= fecha & fecha<= as.Date("2020-07-05") ,1,0),
    
         v_12_07_20=if_else(as.Date("2020-07-12") == fecha ,1,0), #jul20
         v_19_07_20=if_else(as.Date("2020-07-19") == fecha ,1,0),
         v_12_19_jul20=if_else(as.Date("2020-07-12") <= fecha & fecha<= as.Date("2020-07-19") ,1,0),
        
    v_16_08_20=if_else(fecha== as.Date("2020-08-16") ,1,0),
    v_06_09_20=if_else(fecha== as.Date("2020-09-06") ,1,0),
         v_6_13_dec20=if_else(as.Date("2020-12-06") <= fecha & fecha<= as.Date("2020-12-13") ,1,0),
    v_27_12_20=if_else(as.Date("2020-12-27") == fecha ,1,0), #navidad21v2
    medios1era20=if_else(as.Date("2020-03-01") <= fecha & fecha< as.Date("2020-07-01") ,medios_trans,0),
    medios2da20=if_else(as.Date("2020-08-09") <= fecha & fecha<= as.Date("2020-09-06") ,medios_trans,0),
    ext_foo_producto_2020_trans_new=if_else(as.Date("2020-12-20")<= fecha & fecha< as.Date("2020-12-27"),ext_foo_producto_2020_trans,0),
    
    v_03_01_21=if_else(as.Date("2021-01-03") == fecha ,1,0),
    v_28_04_marabr21=if_else(as.Date("2021-03-28") <= fecha & fecha<= as.Date("2021-04-04") ,1,0),
    v_18_25_abr21=if_else(as.Date("2021-04-18") <= fecha & fecha<= as.Date("2021-04-25") ,1,0),
         v_04_07_21=if_else(as.Date("2021-07-04") == fecha ,1,0),
    v_18_07_21=if_else(as.Date("2021-07-18") == fecha ,1,0),
    v_18_01_julag21=if_else(as.Date("2021-07-18") <= fecha & fecha<= as.Date("2021-08-01") ,1,0),
         
         
         v_22_08_21=if_else(as.Date("2021-08-22") == fecha ,1,0),
         v_29_08_21=if_else(as.Date("2021-08-29") == fecha ,1,0),
         v_22_29_ag21=if_else(as.Date("2021-08-22") <= fecha & fecha<= as.Date("2021-08-29") ,1,0),
    v_05_12_sep21=if_else(as.Date("2021-09-05") <= fecha & fecha<= as.Date("2021-09-12") ,1,0),
        v_19_09_21=if_else(as.Date("2021-09-19") == fecha ,1,0),
         v_26_09_21=if_else(as.Date("2021-09-26") == fecha ,1,0),
         
         v_10_17_oct21=if_else(as.Date("2021-10-10") <= fecha & fecha<= as.Date("2021-10-17") ,1,0),
         v_24_10_21=if_else(as.Date("2021-10-24") == fecha ,1,0),
         v_10_17_24_oct21=if_else(as.Date("2021-10-10") <= fecha & fecha<= as.Date("2021-10-24") ,1,0),
         
         v_05_12_21=if_else(as.Date("2021-12-05") == fecha ,1,0),
         v_19_12_21=if_else(as.Date("2021-12-19") == fecha ,1,0),
    medios2da21_1=if_else(as.Date("2021-06-06") <= fecha & fecha< as.Date("2021-10-31") ,medios_trans,0),
    youtube_foo_2021_trans_new=if_else(as.Date("2021-10-31")<= fecha,0,youtube_foo_2021_trans),
    medios2da21_2=if_else(as.Date("2021-10-31") <= fecha & fecha<= as.Date("2021-12-31") ,medios_trans,0),
    ext_foo_producto_2021_trans_new=if_else(as.Date("2021-12-05")<= fecha & fecha< as.Date("2022-01-02"),ext_foo_producto_2021_trans,0),
         v_09_01_22=if_else(as.Date("2022-01-09") == fecha, 1,0),
    sqrt_dp_foo_normal_format1_modified=if_else(as.Date("2021-10-24") < fecha ,sqrt_dp_foo_normal_format1, 0)
  )


#Variable objetivo con log. Una sola variable navidad.
#V1
fit_lm <- lm(
  lnventas ~
    +1
  +precip
  + lnprecios
  #+ sqrt_dp_foo_normal_format1
  #+ sqrt_dp_promo_foo_normal_format1
  #+sqrt_dp_foo_normal_format1_modified
  + sqrt_dp_promo_display_brochure_foo_normal_format1
  #+ log_pbase_compet1_format5
  #+ sqrt_dp_compet1_format5
  #+ sqrt_dp_compet7
  #+ sqrt_dp_compet10_format5
   + tv_foo_camp1_a_trans
   + tv_foo_camp8_2020_trans
  #+tv_new_trans
  + youtube_foo_2021_trans
  #+ instagram_color_plan_2020_trans
  # + twitter_foo_trans
 +radio21_4_19
 +medios1era20
 #+medios2da20
 #+instagram_foo_producto_2019_trans_new
  #+medios2da20
 #+medios2da21_2
  #+ radio_foo_2019_trans
  # + radio_foo_2019_trans
  # + radio_new_trans
  # + twitter_foo_trans
  # + twitter_color_plan_2020_trans
  # + twitter_subidas
  #+ ext_foo_pm_2019_trans
 +ext_foo_pm_2019_trans_new
   + ext_foo_producto_2020_trans_new
  # + ext_foo_producto_2021_trans
 #+ ext_camp11_producto_2021_trans
  # + ext_new_trans
  # + youtube_foo_producto_2020_trans
  #+ youtube_foo_2021_trans
  # + youtube_new_trans
  # + instagram_foo_producto_2019_trans
  # + instagram_foo_producto_2020_trans
  # + instagram_color_plan_2020_trans
  # + instagram_producto_2021_trans
  #+ instagram_new_trans
  # + facebook_foo_trans
  # + facebook_foo_producto_2019_trans
  # + facebook_foo_producto_2020_trans
  # + facebook_color_plan_2020_trans
  # + facebook_producto_2021_trans
  # + facebook_new_trans
  # + ext1
  #+radio1
 #+ Covid
  + navidad
 +v06_01_19
 +v_04_11_ag19
 +v_05_19_jan20
 +v_09_23_feb20
 +primsemconfinamiento_15_03_20
 +v_28_04_marabr21
 +v_18_25_abr21
 #+v_12_07_20
 #+v_16_08_20
 +v_21_05junjul20
 +v_6_13_dec20
 #+v_03_01_21
 +v_04_07_21
 +v_22_08_21
 +v_26_09_21
 +v_10_17_oct21
 +v_24_10_21
 +v_05_12_21
 +v_19_12_21
 +v_09_01_22
  , data = df
)
Modeloregresion(fit_lm)
matrix_coef <- summary(fit_lm)$coefficients  # Extract coefficients in matrix
matrix_coef


plot(fit_lm)
#Test normalidad residuos
shapiro.test(fit_lm$residuals)
#pvalor>0.05 entonces no se rechaza la hipotesis de normalidad

#Test de linealidad
resettest(fit_lm)
#H0: los terminos cúadrático, cúbico o ambos son iguales a cero
#pvalor>0.05 no se rechaza la hipotesis nula, entonces se puede asumir que el modelo está correctamente específicado

#Test de independencia
#H0: No existe correlación entre los residuos.
# realizar la prueba de Durbin-Watson
dwtest(fit_lm)
#pvalor<0.05 hay evidencias para rechazar H0


#perform Breusch-Pagan Test
bptest(fit_lm) #Ho: Los residuos son homocedasticos
#pvalor>0.05 implica que no hay evidencias para rechazar la hipotesis de homocedasticidad

#Calcular proporción de la var objetivo que explica cada una de ellas

Contribvar=names(fit_lm$coefficients[c(-1)])
contrib <- df[Contribvar]
for (i in seq(1:length(Contribvar))){
  var=Contribvar[i]
  contrib[var] <- contrib[var]*fit_lm$coefficients[var]
}

contrib["intercept"]=fit_lm$coefficients["(Intercept)"]
# comprobacion
rowSums (contrib)-fit_lm$fitted.values

contrib2 <- (contrib/ fit_lm$fitted.values)

exp_y_1<- exp(fit_lm$fitted.values)
pbse <- df$pbase_foo_normal_format1
Volexpl <- pbse*(contrib2*exp_y_1) #Volumen explicado por cada variable
Volexpl ["dato_ajustado"] <- exp_y_1
Volexpl ["vol"] <- df$v_vol_foo_normal_format1
Volexpl ["fecha"] <- df$fecha
Volexpl ["pbase"] <- pbse
Volexpl <- Volexpl[, ncol(Volexpl):1]
write.csv2(Volexpl, "Contriblog1nav.csv", row.names = FALSE)

Volexpl2 <- contrib #Volumen explicado por cada variable
Volexpl2 ["fecha"] <- df$fecha
Volexpl2 <- Volexpl2[, ncol(Volexpl2):1]

df_ventas<- Volexpl2 %>%
  pivot_longer(-fecha) %>% 
  group_by(name) %>% 
  summarise(ventas = sum(value), .groups = "drop")

ggplot(df_ventas, aes(x = reorder(name,ventas), y = ventas)) + 
  geom_col(fill="lightblue")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_text(aes(label = round(ventas)))+
  coord_flip()

df_ventas2<- Volexpl2%>%
  pivot_longer(-fecha) %>% 
  group_by(name)
ggplot(df_ventas2, aes(fill=reorder(name,value), y=value, x=fecha)) + 
  geom_bar(position="stack", stat="identity")


df_ventas2_1<- Volexpl2 %>%
  select(fecha, matches("tv_|youtube|twitter|facebook|ext|medios|intercept|radio"))%>%
  pivot_longer(-fecha) %>% 
  group_by(name)
ggplot(df_ventas2_1, aes(fill=reorder(name,value), y=value, x=fecha)) + 
  geom_bar(position="stack", stat="identity")

df_ventas2_1mod<- Volexpl2 %>%
  select(fecha, matches("tv_|youtube|twitter|facebook|ext|medios|radio"))%>%
  pivot_longer(-fecha) %>% 
  group_by(name)
ggplot(df_ventas2_1mod, aes(fill=reorder(name,value), y=value, x=fecha)) + 
  geom_bar(position="stack", stat="identity")


#Variable objetivo sin log. Varias variables navidad.

fit_lmv <- lm(
  v_vol_foo_normal_format1 ~
    +1
   + precip
  #+laboralidad
  #+dif_temp
   + pbase_foo_normal_format1 
  #+ pbase_compet4_format2
  #+ pbase_compet1_format5
  #+ sqrt_dp_foo_normal_format1
  #+ sqrt_dp_promo_display_brochure_foo_normal_format1
  #+ sqrt_dp_compet7
   + tv_foo_camp1_a_trans
   + tv_foo_camp8_2020_trans
  # + medios_trans
  # + radio_foo_2019_trans
  # + radio_new_trans
  # + twitter_foo_trans
  # + twitter_color_plan_2020_trans
  # + twitter_new_trans
  # + ext_foo_pm_2019_trans
  +ext_foo_pm_2019_trans_new
  #+ ext_foo_producto_2020_trans
  +ext_foo_producto_2020_trans_new
  # + ext_foo_producto_2021_trans
  # + ext_camp11_producto_2021_trans
  # + ext_new_trans
  # + youtube_foo_producto_2020_trans
  # + youtube_new_trans
  #+instagram_foo_producto_2019_trans_new
  #+youtube_foo_2021_trans_new
  #+twitter_foo_new
  # + instagram_foo_producto_2019_trans
  # + instagram_foo_producto_2020_trans
  # + instagram_color_plan_2020_trans
  # + instagram_producto_2021_trans
  # + instagram_new_trans
  # + facebook_foo_trans
  # + facebook_foo_producto_2019_trans
  # + facebook_foo_producto_2020_trans
  # + facebook_color_plan_2020_trans
  # + facebook_producto_2021_trans
  # + facebook_new_trans
  #+ youtube_foo_2021_trans
  +radio21_4_19
  #+ ext_foo_producto_2020_trans
  #+ext_foo_producto_2021_trans_new
  # + facebook_color_plan_2020_trans
  # + facebook_foo_trans
  #+faceabril2019
  #+ facebook_foo_producto_2019_trans
   #+ facebook_foo_producto_2020_trans
  #+faceoct2019
  # + facebook_producto_2021_trans
  +medios1era20
  +medios2da20
   +medios2da21_1
   +medios2da21_2
  + navidad20v2
  +navidad21v2
  +navidad22v2
  #+twitter_subidas
  +v06_01_19 #primjan19
  #+sem_pregenerales_21_04_19
  +v_04_11_ag19 #primagost19
  +v_18_08_19
  +v_05_01_20 #primjan20
  #+v_12_01_20 #secjan20
  +v_05_12_jan20 #primsjan20
  +v_19_23_enfeb20
  # +primsemconfinamiento_15_03_20
  # +v_31_05_20
  #+v_07_06_20
  +v_21_05junjul20#jun20
  #+v_12_07_20 #jul20
  #+v_19_07_20
  #+v_12_19_jul20
  +v_06_09_20
  +v_6_13_dec20 #dec20
  +v_03_01_21 #primjan21
  +v_28_04_marabr21
  +v_18_25_abr21
  #+v_04_07_21
   #+v_22_08_21
  #+v_18_07_21
  +v_18_01_julag21
  #+v_01_08_21
    +v_29_08_21
  +v_05_12_sep21
    +v_19_09_21
    +v_26_09_21
   +v_10_17_oct21 #oct21
   +v_24_10_21
  #+v_05_12_21
  +v_19_12_21
  #+v_09_01_22
  , data = df
)
Modeloregresionnolog(fit_lmv)
matrix_coef <- summary(fit_lmv)$coefficients  # Extract coefficients in matrix
matrix_coef

plot(fit_lmv)


#Test de linealidad
resettest(fit_lmv)
#H0: los terminos cúadrático, cúbico o ambos son iguales a cero
#pvalor<0.05 no se rechaza la hipotesis nula, entonces se puede asumir que el modelo está correctamente específicado

#Test de independencia
#H0: No existe correlación entre los residuos.
# realizar la prueba de Durbin-Watson
dwtest(fit_lmv)
#pvalor<0.05 hay evidencias para rechazar H0

#Test normalidad residuos
shapiro.test(fit_lmv$residuals)
#pvalor>0.05 entonces no hay evidencias para rechazar la hipotesis de normalidad

#perform Breusch-Pagan Test
bptest(fit_lmv) #Ho: Los residuos son homocedasticos
#pvalor> 0.05 implica que no hay evidencias para rechazar la hipotesis de homocedasticidad



#Calcular proporción de la var objetivo que explica cada una de ellas

Contribvar=names(fit_lmv$coefficients[c(-1)])
contrib <- df[Contribvar]
for (i in seq(1:length(Contribvar))){
  var=Contribvar[i]
  contrib[var] <- contrib[var]*fit_lmv$coefficients[var]
}
contrib["intercept"]=fit_lmv$coefficients["(Intercept)"]
# comprobacion
rowSums (contrib)-fit_lmv$fitted.values

y_1<- fit_lmv$fitted.values
pbse <- df$pbase_foo_normal_format1

  
Volexplv <-pbse*contrib


Volexplv ["dato_ajustado"] <- fit_lmv$fitted.values
Volexplv ["vol"] <- df$v_vol_foo_normal_format1
Volexplv ["pbase"] <- pbse
Volexplv ["fecha"] <- df$fecha
Volexplv <- Volexplv[, ncol(Volexplv):1]
write.csv2(Volexplv, "Contribvariasnav.csv", row.names = FALSE)

contrib["fecha"] <- df$fecha

df_ventas<- contrib %>% 
  pivot_longer(-fecha) %>% 
  group_by(name) %>% 
  summarise(ventas = sum(value), .groups = "drop")

ggplot(df_ventas, aes(x = reorder(name,ventas), y = ventas)) + 
  geom_col(fill="lightblue")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_text(aes(label = round(ventas)))+
  coord_flip()

df_ventas2<- contrib %>%
  pivot_longer(-fecha) %>% 
  group_by(name)
ggplot(df_ventas2, aes(fill=reorder(name,value), y=value, x=fecha)) + 
  geom_bar(position="stack", stat="identity")


df_ventas2_1<- contrib %>%
  select(fecha, matches("tv_|youtube|twitter|facebook|ext|medios|intercept|radio"))%>%
  pivot_longer(-fecha) %>% 
  group_by(name)
ggplot(df_ventas2_1, aes(fill=reorder(name,value), y=value, x=fecha)) + 
  geom_bar(position="stack", stat="identity")














