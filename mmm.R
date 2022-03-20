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
canibalizacion <- read_csv('data/mmm_canib.csv')
a=list(sales,macro,spend,compets,canibalizacion)
df= Reduce(function(...) merge(..., all=T), a)
df <- df%>%filter(fecha>'2019-01-01')
write.csv(df, "datos.csv")
#Creo una columna con los años y otra con la semana
df <- df %>% mutate(year=as.numeric(format(df$fecha,'%Y')),
                    month=month(df$fecha,label=TRUE),
                    week=week(df$fecha),
                    tv_foo = rowSums(select(., starts_with("tv"))))
df<- df %>%select(where(~ any(. != 0)))

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
GTV<- ggplot(data=df, aes(x=fecha, y=tv_foo))+
  geom_line()
GTVyear<-ggplot(data=df, aes(x=week, y=tv_foo))+
  geom_line()+
  scale_x_continuous(breaks = seq(from = 1, to = 53, by =5 ))+
  facet_grid(year~.)

#Grafico TV con vol ventas
grid.newpage()
grid.draw(cbind(ggplotGrob(GTVyear), ggplotGrob(Gvolyear),size = "last"))

#Correlaciones
newspend <- spend %>%filter(fecha>'2019-01-01')%>%select(where(~ any(. != 0))) %>% colnames()
x1 <- df[,newspend]
x1 <- x1[,2:length(x1)]
x2 <- df[,c('dp_foo_normal_format1','v_euros_foo_normal_format1','pbase_foo_normal_format1', 'dif_temp', 'precip', 'laboralidad', 'tv_foo')]
x <- cbind(x1,x2)
y <- df[,c('v_vol_foo_normal_format1')]
corr <- cor(x,y)
corr[order(abs(corr))]

#Estacionalidad
v_vol_foo_normal_format1.ts = ts(df$v_vol_foo_normal_format1, start = c(2019,1), frequency = 52)
v_vol_foo_normal_format1.ts.desc = decompose(v_vol_foo_normal_format1.ts)
plot(v_vol_foo_normal_format1.ts.desc)

#Estacionalidad log
v_vol_foo_normal_format1.ts = ts(log(df$v_vol_foo_normal_format1), start = c(2019,1), frequency = 52)
v_vol_foo_normal_format1.ts.desc = decompose(v_vol_foo_normal_format1.ts)
plot(v_vol_foo_normal_format1.ts.desc)

#Sin log TV
v <- df$tv_foo
m <- max(v)
a <- seq(0.5,1,0.1)*m
Tvmod <- matrix(data=NA, nrow=length(v), ncol=length(a))
for (j in (1:length(a))){
  Tvmod[ , j] <- v/(v+a[j])
}
#Tvmod <- as.data.frame(Tvmod)
p <- seq(0.4,0.9,0.1)
len <- length(p)
list <- vector(mode = "list", length = len)
for (j in (1:len)){
  list[[j]]=Tvmod*p[j]
}
Mtv=cbind(list[[1]],list[[2]],list[[3]],list[[4]],list[[5]],list[[6]])
corrTV <- cor(Mtv,y)
which.max(corrTV) #esto implica 2a,3p, es decir
a[2]
p[3]

#logTV
corrTV2 <- cor(Mtv,log(y))
which.max(corrTV2) #esto implica 1a,1p, es decir
a[1]
p[1]