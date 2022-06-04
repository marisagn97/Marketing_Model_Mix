library(tidyverse)


# Inversión ---------------------------------------------------------------
df_inversion <- read_csv("inversion_tfm.csv")

mediosFY20=sum(df_inversion  %>%  filter(media=="instagram"|media=="twitter"|media=="facebook"|media=="youtube")%>%
  select(FY20))
mediosFY21=sum(df_inversion  %>%  filter(media=="instagram"|media=="twitter"|media=="facebook"|media=="youtube")%>%
                 select(FY21))
# Add the new row
df_inversion<- rbind(df_inversion,c("medios",mediosFY20,mediosFY21))
df_inversion$FY20_21 <- rowSums(cbind(as.numeric(df_inversion$FY20),as.numeric(df_inversion$FY21)))
# df$name <- rbind(df$FY20,)
# df$country <- rbind(df$FY21 )

# Contribución ------------------------------------------------------------
df_contrib <- read_csv2("Contriblog1nav.csv")
#Contriblog1nav
#Contribvariasnav
df_datos <- df_contrib %>% 
  pivot_longer(-fecha) %>% 
  mutate(fy = case_when(
    fecha >= "2018-09-01" & fecha < "2019-09-01" ~ "FY19", 
    fecha >= "2019-09-01" & fecha < "2021-09-01" ~ "FY20_21", 
    fecha >= "2021-09-01" & fecha < "2022-09-01" ~ "FY22" 
  )) %>% 
  group_by(fy, name) %>% 
  filter(fy %in% c("FY20_21")) %>%
  summarise(ventas = sum(value), .groups = "drop")

df_ventas_vars <- df_contrib %>%
  select(fecha, matches("tv_|youtube|twitter|facebook|ext|medios")) %>% 
  pivot_longer(-fecha) %>% 
  mutate(fy = case_when(
    fecha >= "2018-09-01" & fecha < "2019-09-01" ~ "FY19", 
    fecha >= "2019-09-01" & fecha < "2021-09-01" ~ "FY20_21", 
    fecha >= "2021-09-01" & fecha < "2022-09-01" ~ "FY22" 
  )) %>% 
  group_by(fy, name) %>% 
  filter(fy %in% c("FY20_21")) %>%
  summarise(ventas = sum(value), .groups = "drop")

df_ventas_medios <- df_ventas_vars %>%
  mutate(media = case_when(
    str_detect(name, "tv") ~ "tv", 
    str_detect(name, "youtube") ~ "youtube", 
    str_detect(name, "facebook") ~ "facebook", 
    str_detect(name, "twitter") ~ "twitter", 
    str_detect(name, "ext") ~ "ooh",
    str_detect(name, "medios") ~ "medios", 
    TRUE ~ "rest"
  )) %>% 
  group_by(media, fy) %>% 
  summarise(ventas = sum(ventas), .groups = "drop")

v06_01=df_datos$ventas[df_datos $name=="v06_01_19"]
v411=df_datos$ventas[df_datos $name=="v_04_11_ag19"]
v188=df_datos$ventas[df_datos $name=="v_18_08_19"]

v05_01=df_datos$ventas[df_datos $name=="v_05_01_20"]

v31_05=df_datos$ventas[df_datos $name=="v_31_05_20"]
junjul=df_datos$ventas[df_datos $name=="v_21_05junjul20"]
v613=df_datos$ventas[df_datos $name=="v_6_13_dec20"]

v3_01=df_datos$ventas[df_datos $name=="v_03_01_21"]
v28_04=df_datos$ventas[df_datos $name=="v_28_04_marabr21"]
v1825=df_datos$ventas[df_datos $name=="v_18_25_abr21"]
v4_07=df_datos$ventas[df_datos $name=="v_04_07_21"]
v22_08=df_datos$ventas[df_datos $name=="v_22_08_21"]
#v26_09=df_datos$ventas[df_datos $name=="v_26_09_21"]
v1017=df_datos$ventas[df_datos $name=="v_10_17_oct21"]
#v24_10=df_datos$ventas[df_datos $name=="v_24_10_21"]
v5_12=df_datos$ventas[df_datos $name=="v_05_12_21"]
v19_12=df_datos$ventas[df_datos $name=="v_19_12_21"]
#v9_01=df_datos$ventas[df_datos $name=="v_09_01_22"]

# #log
# df_ventas_medios$ventas[df_ventas_medios $media=="ooh"]=df_ventas_medios$ventas[df_ventas_medios$media=="ooh"]+
#   0.11*(v613)+0.1*(v4_07+v22_08+v1017+v5_12+v19_12)
# df_ventas_medios$ventas[df_ventas_medios $media=="tv"]=df_ventas_medios$ventas[df_ventas_medios$media=="tv"]+
#   0.89*(v613)+0.79*(v4_07+v22_08+v1017+v5_12+v19_12)+v411
# df_ventas_medios$ventas[df_ventas_medios $media=="youtube"]=df_ventas_medios$ventas[df_ventas_medios$media=="youtube"]+
#   +0.11*(v4_07+v22_08+v1017+v5_12+v19_12)

#nolog

df_ventas_medios$ventas[df_ventas_medios $media=="medios"]=df_ventas_medios$ventas[df_ventas_medios$media=="medios"]+
  +0.28*(v1017+v19_12)
df_ventas_medios$ventas[df_ventas_medios $media=="tv"]=df_ventas_medios$ventas[df_ventas_medios$media=="tv"]+
  0.64*(v1017+v19_12)+0.89*(v05_01+junjul+v613+v3_01+v28_04+v1825)+
  v06_01+v411+v188
df_ventas_medios$ventas[df_ventas_medios $media=="ext"]=df_ventas_medios$ventas[df_ventas_medios$media=="youtube"]+
  +0.11*(v05_01+junjul+v613+v3_01+v28_04+v1825)+ 0.08*(v1017+v19_12)
# ROI ---------------------------------------------------------------------
roi <- df_ventas_medios %>%
  inner_join(
    df_inversion%>%
      select("media","FY20_21")%>%
      pivot_longer(-media, names_to = "fy", values_to = "spend")
  ) %>%
  mutate(roi = ventas / as.numeric(spend))


write.csv(roi, "ROIlog.csv", row.names = FALSE)
#ROIvariasnav
#ROIlog