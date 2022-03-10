library(tidyverse)

background <- "#EFE5D7"
crimson <- "#dc143c"

df <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge10/data.csv")
colnames(df) <- c("year", "pct")

df <- df %>%
  mutate(neg = 100 - pct) %>%
  pivot_longer(!year, names_to = "enrolled", values_to = "val") %>%
  mutate(scaled = case_when(year == "1876" ~ val*1.2,
                         year == "1886" ~ val*1.5,
                         year == "1896" ~ val*2)) %>%
  as.data.frame() 

ggplot(df) +
  geom_bar(position = "stack", stat = "identity", width = 3.5,
           aes(fill = enrolled, x = year, y = scaled)) +
  scale_y_reverse() +
  scale_fill_manual(values = c("black", crimson)) +
  theme_void() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = background, color = background),
        plot.title = element_text(family = "mono", size = 9.5, face = "bold",
                                  color = "black", hjust = 0.5),
        plot.subtitle = element_text(family = "mono", size = 7.5, face = "bold",
                                     color = "black", hjust = 0.5, vjust = -7)) +
  geom_text(data = df %>% filter(enrolled == "pct"), 
            mapping = aes(x = year, y = as.numeric(scaled) / 2, 
                          label = paste0(val, "%")), 
            color = "black", family = "mono",
            fontface = "bold", size = 5) +
  geom_text(aes(x = year, y = 0, label = year), color = "black", family = "mono",
            fontface = "bold", nudge_y = 3, size = 4.5) +
  geom_point(size = 10, pch = 15, aes(x = 1876, y = 145), color = crimson) +
  geom_point(size = 10, pch = 15, aes(x = 1876, y = 165), color = "black") +
  geom_text(aes(x = 1872.9, y = 144.75, label = "PROPORTION              OF CHILDREN ENROLLED\nPROPORTION              D'ENFANTS ENREGISTRÉS."),
            size = 2.5, family = "Times", color = "#7A7875", hjust = 0) +
  geom_text(aes(x = 1872.9, y = 164.75, label = "PROPORTION              OF CHILDREN NOT ENROLLED\nPROPORTION              D'ENFANTS NON ENREGISTRÉS."),
            size = 2.5, family = "Times", color = "#7A7875", hjust = 0) +
  labs(title = "\nPROPORTION OF TOTAL NEGRO CHILDREN OF SCHOOL AGE WHO ARE ENROLLED IN THE PUBLIC SCHOOLS.\n\nPROPORTION DES ENFANTS NÈGRES EN ÂGE D'ÉCOLE ENREGISTRÉS DANS LES ÉCOLES PUBLIQUES.",
       subtitle = "DONE BY ATLANTA UNIVERSITY .\n\n\n")

ggsave("challenge10.png", height = 7.6, width = 7)
