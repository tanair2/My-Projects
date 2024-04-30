library(tidyverse)
view(Plants_1)

ggplot(data = Plants_1, aes(x = Fert, y = BiomassT2, fill = Fert))+
  geom_jitter(width = 0.05, alpha = 0.5)+
  scale_fill_brewer(palette = "Set2")+
  geom_boxplot(width = 0.05)+
  scale_y_continuous(breaks = seq(144,380,25))+
  labs(title = "Hydrangea Biomass After experiment", x = "Species", y = "Biomass(Grams)")

ggplot(data = Plants_1, aes(x = Fert, y = BiomassT2, fill = Fert))+
  geom_jitter(width = 0.05, alpha = 0.5)+
  scale_fill_brewer(palette = "Set2")+
  geom_boxplot(width = 0.05)+
  scale_y_continuous(breaks = seq(144,380,25))+
  labs(title = "Hydrangea Biomass based on Fertilizer Used", x = "Fertilizers", y = "Biomass(Grams)")

model = aov(data = Plants_1, BiomassT2 ~ Species)
summary(model)

model1 = aov(data = Plants_1, BiomassT2 ~ Fert)
summary(model1)

Plants_1$Row = factor(Plants_1$Row, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
Plants_1$Column = factor(Plants_1$Column, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
ggplot(data = Plants_1, aes(x = Column, y = Row, fill = BiomassT2))+
  geom_tile(color = "black")+
  scale_fill_distiller(palette = "Spectral", name = "Sepal Length")+
  labs(title = "Hydrangea Biomass based on Row and Column", x = "Column", y = "Row")

ggplot(data = Plants_1, aes(x = Row, y = BiomassT2, fill = Row))+
  geom_boxplot(width = 0.1)+
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Biomass by Row", x = "Row", y = "Bomass(Grams)")+
  theme(legend.position = "none")

Plants_1 %>%
  filter(Row != "1") %>%
  ggplot(aes(x = Fert, y = BiomassT2, fill = Species)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_manual(values = c("lightpink", "steelblue1", "coral"))+
  labs(title = "Biomass per Species based on Fertilizer Used", x = "Fertilizer Used", "Biomass(Grams)")
