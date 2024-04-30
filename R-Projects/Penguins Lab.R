library(tidyverse)
library(ggplot2)
view(penguins)

group_mean <- aggregate(bill_length_mm ~ species, data = penguins, mean)
group_mean

# Barplot
ggplot(group_mean, aes(x= species, y= bill_length_mm)) + 
  geom_bar(stat = "identity", fill = "green")+
  labs(title = "R/ R studio: Average Bill Length per Species", x = "Species", y = "Average Bill Length")
