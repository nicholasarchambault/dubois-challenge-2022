library(tidyverse)
library(showtext)
library(cowplot)


font_add_google(name = "Public Sans")
font_add_google(name = "Cormorant Garamond")
showtext_auto()

background <- "#EFE5D7"
crimson <- "#ca183b"
green <- "#2e8059"
blue <- "#064da1"

df <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv")

df <- df %>%
  pivot_longer(cols = 3:5, names_to = "status", values_to = "percent") %>%
  mutate(percent = case_when(Gender == "Male" ~ -1 * percent,
                             Gender == "Female" ~ percent)) %>%
  unite(status, c(Gender, status), sep = "-") %>%
  mutate(status = factor(status, levels = c("Male-Widowed", "Male-Married", 
                                            "Male-Single", "Female-Widowed",
                                            "Female-Married", "Female-Single")))

y_labs <- c(unique(df$Group)[-length(unique(df$Group))], "AGES.\nOVER 65")

text_data <- data.frame(label = c("WIDOWED", "WIDOWED", 
                                  "MARRIED", "MARRIED", "SINGLE", "SINGLE"),
                        x = c(-90, 88, -55, 55, -35, 35),
                        y = c(8.63, 7.5, 6, 6, 1.5, 1.5),
                        a = c(60, -60, 45, -45, 45, -45))

g <- ggplot() +
  geom_col(data = df, mapping = aes(x = percent, y = Group, fill = status),
           width = 1, alpha = 0.94) +
  scale_fill_manual(values = c("Male-Widowed" = green, "Male-Married" = crimson, 
                               "Male-Single" = blue, "Female-Widowed" = green,
                               "Female-Married" = crimson, 
                               "Female-Single" = blue)) + 
  scale_x_continuous(breaks = seq(-100, 100, 10), 
                     minor_breaks = seq(-100, 100, 2),
                     labels = c(seq(100, 10, -10), "", seq(10, 100, 10))) +
  scale_y_discrete(labels = y_labs) +
  geom_text(data = text_data, mapping = aes(x = x, y = y, label = label,
                                            angle = a),
            family = "Public Sans", fontface = "bold", size = 5) +
  coord_cartesian(expand = F) +
  theme(legend.position = "None",
        panel.grid = element_line(color = "black", size = 0.5),
        plot.background = element_rect(fill = background, color = background),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 2.2, 1, 0.1), "cm"),
        axis.title.x = element_text(family = "Public Sans", face = "bold", 
                                    size = 10),
        axis.text.x = element_text(family = "Public Sans", face = "bold", 
                                   color = "black", size = 10),
        axis.text.y = element_text(family = "Public Sans", face = "bold", 
                                   color = "black", size = 11),
        plot.title = element_text(family = "Cormorant Garamond",
                                  color = "black", hjust = 0.5, size = 20),
        plot.subtitle = element_text(family = "Cormorant Garamond", size = 14,
                                     color = "black", hjust = 0.5)) +
  labs(title = "Conjugal condition of American Negroes according to age periods.",
       subtitle = "\nCondition conjugale des Nègres Americains au point de vue de l'age.\n\nDone by Atlanta University.\n\n\n",
       x = "PER CENTS .")

final <- ggdraw() +
  draw_plot(g) +
  draw_label(label = "MALES.", x = 0.35, y = 0.8, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "FEMALES.", x = 0.67, y = 0.8, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "0-15  ", x = 0.93, y = 0.115, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "15-20", x = 0.93, y = 0.1925, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "20-25", x = 0.93, y = 0.2725, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "25-30", x = 0.93, y = 0.3525, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "30-35", x = 0.93, y = 0.43125, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "35-45", x = 0.93, y = 0.51, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "45-55", x = 0.93, y = 0.59, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "55-65", x = 0.93, y = 0.67, color = "black", size = 11, 
             fontfamily = "Public Sans", fontface = "bold") +
  draw_label(label = "AGES.\nOVER 65", x = 0.894, y = 0.749, color = "black", 
             size = 11, fontfamily = "Public Sans", fontface = "bold", 
             hjust = 0)

ggsave(final, filename = "challenge07.png", height = 10, width = 8, unit = "in")

