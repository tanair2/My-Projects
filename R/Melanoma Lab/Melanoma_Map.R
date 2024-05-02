library(tidyverse)
library(maps)
MainStates = map_data("state")
names(MainStates)[names(MainStates) == "region"] = "Name"
Melanoma_Map = inner_join(MainStates, Melanoma, by = "Name")
view(Melanoma_Map)
ggplot(data = Melanoma_Map, aes(x = long, y = lat, group = group, fill = Mel))+
  geom_polygon(color = "black")+
  labs(title = "Melanoma Rates per State", x = "longitude", y = "latitude")+
  scale_fill_distiller(palette = "YlGn")+
  theme_classic()

ggplot(data = Melanoma_Map, aes(x = Sun, y = Mel))+
  geom_text(aes(label=Abb),hjust=0, vjust=0, size = 3, fontface = 2)+
  labs(title = "State Sun Exposure and Melanoma Rates", x = "Sun Exposure Level", y = "Melanoma Rate")+
  theme_classic()+
  geom_smooth(method = "lm", se = TRUE, color = "orange")

ggplot(data = Melanoma_Map, aes(x = Sun, y = Mel, color = group)) +
  geom_point() +
  labs(x = "Sun Exposure", y = "Melanoma Rates") +
  geom_smooth(method = "lm", se = FALSE, color = "black")
mod_simple_ST = lm(data = Melanoma_Map, Sun ~ Mel)
summary(mod_simple_ST)

ggplot(data = Melanoma_Map, aes(x = Temp, y = Mel))+
  geom_text(aes(label=Abb),hjust=0, vjust=0, size = 3, fontface = 2)+
  labs(title = "State Temperature and Melanoma Rates", x = "Sun Exposure Level", y = "Melanoma Rate")+
  theme_classic()+
  geom_smooth(method = "lm", se = TRUE, color = "orange")

ggplot(data = Melanoma_Map, aes(x = Age, y = Mel))+
  geom_text(aes(label=Abb),hjust=0, vjust=0, size = 3, fontface = 2)+
  labs(title = "Average Age per state and Melanoma Rates", x = "Sun Exposure Level", y = "Melanoma Rate")+
  theme_classic()+
  geom_smooth(method = "lm", se = TRUE, color = "orange")

ggplot(data = Melanoma_Map, aes(x = White, y = Mel))+
  geom_text(aes(label=Abb),hjust=0, vjust=0, size = 3, fontface = 2)+
  labs(title = "Proportion of Non-Hispanic White Residents and Melanoma Rates", x = "Sun Exposure Level", y = "Melanoma Rate")+
  theme_classic()+
  geom_smooth(method = "lm", se = TRUE, color = "orange")


ggplot(data = Melanoma_Map, aes(x = Sun, y = Mel, color = White_Bin)) +
  geom_text(aes(label=Abb),hjust=0, vjust=0, size = 3, fontface = 1) +
  labs(title = "sun Exposure and Melanoma Rates by Non-Hispanic White population Proportion per State", 
       x = "Sun Exposure", y = "Melanoma Rates", color = "Proportion White")+
  geom_smooth(method = "lm", se = FALSE)

add_mod = lm(data = Melanoma_Map, Mel ~ Sun + White_Bin)
summary(add_mod)

int_mod = lm(data = Melanoma_Map, Mel ~ Sun * White_Bin)
summary(int_mod)

  