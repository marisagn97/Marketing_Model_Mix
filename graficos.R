library(dplyr)
library(ggplot2)
setwd("C:/Users/malon/Desktop/Master/2022-mmm_retail/")
df<- read_csv('df.csv')
#gráficos
#medios=face,insta youtube y twitter


#tv, ext, radio, face,insta youtube y twitter

#tv
df %>%
  select(fecha, v_vol_foo_normal_format1,tv_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

df %>%
  select(fecha, v_vol_foo_normal_format1,contains("tv") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

df %>%
  select(fecha, v_vol_foo_normal_format1,tv_foo_camp1_a_trans,tv_foo_camp8_2020_trans) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#ext
df %>%
  select(fecha, v_vol_foo_normal_format1,ext_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("ext") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,ext_foo_producto_2020_trans) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#radio
df %>%
  select(fecha, v_vol_foo_normal_format1,radio_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("radio") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,radio21_4_19 ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#medios
df %>%
  select(fecha, v_vol_foo_normal_format1,medios) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("medios") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,medios1era20,medios2da21_2) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#facebook
df %>%
  select(fecha, v_vol_foo_normal_format1,facebook_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("facebook") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#instagram
df %>%
  select(fecha, v_vol_foo_normal_format1,instagram_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("instagram") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,instagram_foo_producto_2019_trans_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#youtube
df %>%
  select(fecha, v_vol_foo_normal_format1,youtube_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("youtube") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,youtube_foo_2021_trans) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

#twitter
df %>%
  select(fecha, v_vol_foo_normal_format1,twitter_new) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")
df %>%
  select(fecha, v_vol_foo_normal_format1,contains("twitter") & - contains("dp") & - contains("trans") ) %>% 
  pivot_longer(-fecha) %>%
  ggplot() + 
  geom_line(aes(x = fecha, y = value)) + 
  facet_grid(name ~ ., scales = "free")

View(df[c("fecha","twitter_foo")])
View(df[c("fecha","instagram_foo_producto_2019")])
View(df[c("fecha","ext_foo_producto_2020_trans")])
#  filter(as.Date("2019-01-01") <= fecha & fecha<= as.Date("2019-05-31") ) %>% 