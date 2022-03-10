library(readr)
library(dplyr)
library(grid)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
sales <- read_csv('data/mmm_sales.csv')
names(sales)
macro <- read_csv('data/mmm_macro.csv')
names(macro)
spend <- read_csv('data/mmm_spend.csv')
names(spend)
compets <- read_csv('data/mmm_compets.csv')
names(compets)
canibalizacion <- read_csv('data/mmm_canibalización.csv')
names(canibalizacion)
a=list(sales,macro,spend,compets,canibalizacion)
df= Reduce(function(...) merge(..., all=T), a)
df <- df%>% mutate(tv = rowSums(select(., starts_with("tv"))))
write.csv(df, "datos.csv")

#Veo si hay alguna fecha duplicada
sum(duplicated(df$fecha))
#Veo si hay na
sum(is.na(df))

#Gráficas lineales (Series de tiempo)
require(scales)
G1 <- ggplot(data=df, aes(x=fecha, y=v_vol_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)
G2 <- ggplot(data=df, aes(x=fecha, y=v_euros_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)
G3 <- ggplot(data=df, aes(x=fecha, y=pbase_foo_normal_format1)) +
  geom_line()
G4 <- ggplot(data=df, aes(x=fecha, y=dp_promo_foo_normal_format1)) +
  geom_line()
G5 <- ggplot(data=df, aes(x=fecha, y=dp_foo_normal_format1)) +
  geom_line()
grid.newpage()
grid.draw(rbind(ggplotGrob(G1), ggplotGrob(G2), ggplotGrob(G3),size = "last"))
G6 <- ggplot(data=df, aes(x=fecha)) +
  geom_line(aes(y=facebook_foo),color='blue')+
  geom_line(aes(y=twitter_foo),color='orange')+
  geom_line(aes(y=web_foo),color='green')+
  geom_line(aes(y=you_tube_foo),color='red')+
  scale_y_continuous(labels = comma)
G6#En facebook aparecen muchos más anuncios
#Se observa gran bajada de los precios en 2014
#Esta bajada viene siendo progresiva desde un inicio
#Parece que esta recuperando desde 2015
#la serie es estacional, posteriormente estudiaré esto

G7 <- ggplot(data=df, aes(x=fecha)) +
  geom_line(aes(y=tv_foo_camp1_a),color='blue')+
  geom_line(aes(y=twitter_foo),color='orange')+
  geom_line(aes(y=web_foo),color='green')+
  geom_line(aes(y=you_tube_foo),color='red')+
  scale_y_continuous(labels = comma)
G7#En ypoutube aparecen muchos más anuncios

G8 <- ggplot(data=df, aes(x=fecha, y=tv))+
  geom_line()
G8

grid.newpage()
grid.draw(rbind(ggplotGrob(G7), ggplotGrob(G8),size = "last"))

#Vamos a ver que dias coinciden campañas y cuantas
Campaign <- df%>%select(starts_with("tv"))
Campaign[Campaign !=0.0] <-1
fecha=df$fecha
Campaign <- cbind(fecha,Campaign)
Campaign<- Campaign%>% mutate(Coinciden= rowSums(Campaign[,c (2:12)], na.rm = TRUE))
Coincidencias <- Campaign %>% select(fecha,Coinciden) %>%  filter(Coinciden>1)
#Coinciden campañas en 84 ocasiones

#Voy a calcular la estacionalidad
