library(readr)
library(dplyr)
library(grid)
library(ggplot2)
library(lubridate)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
sales <- read_csv('data/mmm_sales.csv')
macro <- read_csv('data/mmm_macro.csv')
spend <- read_csv('data/mmm_spend.csv')
compets <- read_csv('data/mmm_compets.csv')
canibalizacion <- read_csv('data/mmm_canibalización.csv')
a=list(sales,macro,spend,compets,canibalizacion)
df= Reduce(function(...) merge(..., all=T), a)
df <- df%>% mutate(tv = rowSums(select(., starts_with("tv"))))
write.csv(df, "datos.csv")

#Veo si hay alguna fecha duplicada
sum(duplicated(df$fecha))
#Veo si hay na
sum(is.na(df))
#Veo fecha inicial y final
min(df$fecha)
max(df$fecha)
#compruebo dim df para ver que no falten semanas
dim(df)
#Creo una columna con los años y otra con la semana
df['year']=as.numeric(format(df$fecha,'%Y'))
df['month']=month(df$fecha,label=TRUE)
df['week']=week(df$fecha)
#Gráficas lineales (Series de tiempo)
require(scales)
G1 <- ggplot(data=df, aes(x=fecha, y=v_vol_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)
G1_1 <- ggplot(data=df, aes(x=week,y=v_vol_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)
G2 <- ggplot(data=df, aes(x=fecha, y=v_euros_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)
G3 <- ggplot(data=df, aes(x=fecha, y=pbase_foo_normal_format1)) +
  geom_line()
grid.newpage()
grid.draw(rbind(ggplotGrob(G1), ggplotGrob(G2), ggplotGrob(G3),size = "last"))

G4 <- ggplot(data=df, aes(x=fecha, y=dp_promo_foo_normal_format1)) +
  geom_line()
G4_1 <- ggplot(data=df, aes(x=week,y=dp_promo_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)
G5 <- ggplot(data=df, aes(x=fecha, y=dp_foo_normal_format1)) +
  geom_line()
G5_1 <-ggplot(data=df, aes(x=week,y=dp_foo_normal_format1)) +
  geom_line()+ scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)
min(df$dp_foo_normal_format1)#para saber el min al que esta expuesto

G6 <- ggplot(data=df, aes(x=fecha)) +
  geom_line(aes(y=facebook_foo),color='blue')+
  geom_line(aes(y=twitter_foo),color='orange')+
  geom_line(aes(y=web_foo),color='green')+
  geom_line(aes(y=you_tube_foo),color='red')+
  scale_y_continuous(labels = comma)

G6_1 <- ggplot(data=df, aes(x=week)) +
  geom_line(aes(y=facebook_foo),color='blue')+
  geom_line(aes(y=twitter_foo),color='orange')+
  geom_line(aes(y=web_foo),color='green')+
  geom_line(aes(y=you_tube_foo),color='red')+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)

G7 <- ggplot(data=df, aes(x=fecha, y=tv))+
  geom_line()
G7_1 <-ggplot(data=df, aes(x=week, y=tv))+
  geom_line()+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)


#Vamos a ver que dias coinciden campañas y cuantas
Campaign <- df%>%select(starts_with("tv"))
ValTV <- as.data.frame(apply(Campaign[,2:13], 2, table))
MeanTV <- apply(Campaign[,2:13], 2, mean)
Campaign[Campaign !=0.0] <-1
fecha=df$fecha
Campaign <- cbind(fecha,Campaign)
Campaign<- Campaign%>% mutate(Coinciden= rowSums(Campaign[,c (2:12)], na.rm = TRUE))
Coincidencias <- Campaign %>% select(fecha,Coinciden) %>%  filter(Coinciden>1)
#Coinciden campañas en 84 ocasiones

#Voy a calcular la estacionalidad del vol
v_vol_foo_normal_format1.ts = ts(df$v_vol_foo_normal_format1, start = c(2011,8), frequency = 52)
boxplot(v_vol_foo_normal_format1.ts ~ cycle(v_vol_foo_normal_format1.ts))
v_vol_foo_normal_format1.ts.desc = decompose(v_vol_foo_normal_format1.ts)
plot(v_vol_foo_normal_format1.ts.desc)