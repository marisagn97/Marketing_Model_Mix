library(readr)
library(dplyr)
library(grid)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(gridExtra)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
df<- read_csv('df.csv')


#Gráficas lineales (Series de tiempo)
require(scales)
Gvol <- ggplot(data=df, aes(x=fecha, y=v_vol_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)
Gvolyear <- ggplot(data=df, aes(x=week,y=v_vol_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)

Gvollog <- ggplot(data=df, aes(x=fecha, y=log(v_vol_foo_normal_format1))) +
  geom_line()+ scale_y_continuous(labels = comma)
Gvolyearlog <- ggplot(data=df, aes(x=week,y=log(v_vol_foo_normal_format1))) +
  geom_line()+ scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)

#Tiendas a las que esta expuesto
Gexpuesto<- ggplot(data=df, aes(x=fecha, y=dp_foo_normal_format1)) +
  geom_line()
Gexpuestoyear<-ggplot(data=df, aes(x=week,y=dp_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)
min(df$dp_foo_normal_format1)#para saber el min al que esta expuesto
#la exposicion da un bajon a finales de 2021

#Grafica TV
GTV<- ggplot(data=df, aes(x=fecha, y=tv_new_trans))+geom_line()
GTVyear<-ggplot(data=df, aes(x=week, y=tv_new_trans))+
geom_line()+
scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
facet_grid(year~.)

#Grafico TV con vol ventas
#grid.newpage()
#grid.draw(cbind(ggplotGrob(GTVyear), ggplotGrob(Gvolyear),size = "last"))

#Estacionalidad
v_vol_foo_normal_format1.ts = ts(df$v_vol_foo_normal_format1, start = c(2019,1), frequency = 52)
v_vol_foo_normal_format1.ts.desc = decompose(v_vol_foo_normal_format1.ts)
plot(v_vol_foo_normal_format1.ts.desc)

#Estacionalidad log
v_vol_foo_normal_format1.ts = ts(log(df$v_vol_foo_normal_format1), start = c(2019,1), frequency = 52)
v_vol_foo_normal_format1.ts.desc = decompose(v_vol_foo_normal_format1.ts)
plot(v_vol_foo_normal_format1.ts.desc)




#GRAFICAS DESCRIPTIVO
df %>% 
  select(fecha, v_vol_foo_normal_format1,tv_new_trans) %>% 
  pivot_longer(-fecha) %>% 
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~  . , scales = "free_y")


#Graficamos la radio 
df %>% 
  select(fecha, v_vol_foo_normal_format1, radio_new_trans) %>% 
  pivot_longer(-fecha) %>% 
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~  . , scales = "free_y")
