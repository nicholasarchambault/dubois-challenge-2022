library(tidyverse)
library(ggforce)
library(showtext)

showtext_auto()
font_add_google("Public Sans")

background <- "#EFE5D7"
df <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge02/data.csv")
df <- unique(df)
colnames(df) <- c("year", "val")

wedge_data <- data.frame(x0 = c(-3, 3, 5, 3, -3),
                         y0 = c(-3, -3, 0, 3, 3),
                         r2 = c(5.5, 8, 14, 16.75, 17.5),
                         r = c(0.3, 0.3, 0.7, 1, 1),
                         start = c(3.8, 2.1, 1.4, 0.55, 5.5),
                         end = c(4.4, 2.7, 1.7, 0.8, 5.75),
                         fl = c("#d2b48c", "#4682b4", "#ffd700", "#d9c7af",
                                  "#dc143c"),
                         cl = c("#d2b48c", "#4682b4", "#ffd700", "#d9c7af",
                                "#dc143c"))

text_data <- data.frame(label = c(df$year, scales::dollar(df$val)),
                        x = c(0, 0, 0, 0, 0, 0, 0, -5.8, 6, 14.5, 11.2, -11.5),
                        y = c(-8, -9.42, -13.1, -19.7, -20.9, -21.7, 0, -5, -6.2, 0.1,
                              13.5, 14.1),
                        angle = c(0, 0, 0, 0, 0, 0, 0, 35, -45, 0, 51, -51),
                        cl = c("#eae0d9", "black", "#eae0d9", "black", "black",
                               "black", "#eae0d9", "black", "#eae0d9", "black", "black", "black"),
                        size = c(4.9, 4, 4.9, 4.9, 4.1, 4.1, 5, 4.1, 4.6, 5, 5, 5),
                        n = c(1, 0.35, 0.9, 0.8, 0.4, 0.4, 0, 0, 0, 0, 0, 0))
  

ggplot(df) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 22), fill = "#dc143c", color = "#dc143c") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 21.25), fill = "#d9c7af", color = "#d9c7af") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 20.5), fill = "#ffd700", color = "#ffd700") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 14), fill = "#4682b4", color = "#4682b4") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 9.75), fill = "#d2b48c", color = "#d2b48c") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 9), fill = "black", color = "black") +
  coord_fixed() + 
  theme_void() +
  theme(plot.title = element_text(family = "Public Sans", size = 20, face = "bold",
                                  color = "black", hjust = 0.5),
        plot.background = element_rect(fill = background, color = background)) +
  labs(title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY GEORGIA NEGROES .") +
  geom_arc_bar(data = wedge_data, 
           mapping = aes(x0 = x0, y0 = y0, r0 = r2, r = r, start = start,
                         end = end, fill = I(fl), color = I(cl))) +
  geom_text(data = text_data,
            mapping = aes(x = x, y = y, label = label, angle = angle,
                          size = I(size), nudge_y = n, color = I(cl)))
ggsave("challenge02.png", height = 10, width = 8, unit = "in")
