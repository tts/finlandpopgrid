library(tidyverse)
library(sf)
library(patchwork)

url <- "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0"
req <- "&request=GetFeature&typeName=vaestoruutu:vaki2018_1km"
f <- "&outputFormat=json"
q <- paste0(url, req, f)

raw <- st_read(q, quiet = TRUE)

data_t_r <- raw %>% 
  mutate(Top = map_int(bbox, 2),
         Right = map_int(bbox, 3)) %>% 
  select(Top, Right, vaesto)

data <- st_drop_geometry(data_t_r)

# https://twitter.com/Thoughtfulnz/status/1256465193741062144
SN <- data %>% 
  mutate(SN = Top - min(Top)) %>% 
  group_by(SN) %>% 
  summarise(Pop = sum(vaesto)) %>% 
  ggplot(aes(x=Pop, y=SN, yend=SN)) + geom_segment(xend=0) +
  xlab("Population") + ylab("kilometres from S to N")
WE <- data %>% 
  mutate(WE = Right - min(Right)) %>% 
  group_by(WE) %>% 
  summarise(Pop = sum(vaesto)) %>% 
  ggplot(aes(y=Pop, x=WE, xend=WE)) + geom_segment(yend=0) +
  xlab("kilometres from W to E") + ylab("Population")
SN+WE



