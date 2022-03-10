library(tidyverse)
library(showtext)
library(cowplot)
library(grid)
library(gridExtra)

background <- "#EFE5D7"
font_add_google(name = "Public Sans")
font_add_google(name = "Bai Jamjuree")
showtext_auto()

df <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge04/data.csv")

colnames(df) <- c("year", "value")
summary(df)

text_data <- data.frame(label = c("KU-KLUXISM", "FINANCIAL PANIC.", "LYNCHING.",
                                  "POLITICAL\n     UNREST.",
                                  "DISENFRANCHISEMENT\nAND\nPROSCRIPTIVE\nLAWS.",
                                  "RISE OF\n                 THE NEW\n                                             INDUSTRIALISM."),
                        x = c(1872, 1894, 1892, 1877, 1895.5, 1882),
                        y = c(650000, 700000, 1550000, 2300000, 2350000, 4050000),
                        a = c(90, 90, 0, 0, 0, 0))

g3_text_data <- data.frame(label = c("DOLLARS", "$   ", "$   ", "4,000,000",
                                     "$   ", "$   ", "3,000,000", "$   ", "$   ",
                                     "2,000,000", "$   ", "$   ", "1,000,000",
                                     "$   ", "$   "),
                           x = rep(1870.5, 15),
                           y = c(4750000, 4500000, 4300000, 4000000, 3600000,
                                 3400000, 3000000, 2600000, 2400000, 2000000,
                                 1600000, 1400000, 1000000, 600000, 400000))

segment_data <- data.frame(x = rep(0.115, 4),
                           xend = rep(0.161, 4),
                           y = c(0.2286, 0.42875, 0.629, 0.8294),
                           yend = c(0.2286, 0.42875, 0.629, 0.8294))

g <- ggplot() +
  scale_x_continuous(breaks = seq(1870, 1900, 5),
                     minor_breaks = seq(1870, 1900, 1),
                     labels = seq(1870, 1900, 5)) +
  scale_y_continuous(minor_breaks = seq(0, 5000000, 100000)) +
  coord_cartesian(expand = F, ylim = c(0, 4800000), xlim = c(1870, 1900)) +
  geom_text(data = text_data, mapping = aes(x = x, y = y, label = label,
                                            angle = a), 
            size = 3, family = "Bai Jamjuree", color = alpha("black", 0.5)) +
  theme(panel.grid.major = element_line(size = 0.25),
        panel.grid = element_line(color = alpha("red", 0.1)),
        panel.background = element_rect(fill = background, 
                                        color = alpha("black", 0.3)),
        plot.background = element_rect(fill = background, 
                                       color = background),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "Bai Jamjuree", 
                                   color = alpha("black", 0.7), 
                                   size = 7),
        plot.title = element_blank(),
        plot.margin = margin(0, 0.5, 0, 0.5, unit = "cm"))


g2 <- ggplot() +
  geom_line(data = df, mapping = aes(x = year, y = value), 
            color = alpha("black", 0.7), size = 0.3) +
  geom_line(data = df, mapping = aes(x = year, y = value + 100000),
            color = alpha("black", 0.7), size = 0.3) +
  geom_line(data = df %>% filter(year >= 1874 & year < 1900), 
            mapping = aes(x = year, y = value + 51000), size = 4.9) +
  theme_nothing()

g3 <- ggplot() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(minor_breaks = seq(0, 5000000, 100000)) +
  coord_cartesian(expand = F, ylim = c(0, 4800000), xlim = c(1870, 1871)) +
  theme(panel.grid.major = element_line(size = 0.25),
        panel.grid = element_line(color = alpha("red", 0.1)),
        panel.background = element_rect(fill = background, 
                                        color = alpha("black", 0.3)),
        plot.background = element_rect(fill = background, 
                                       color = background),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_blank()) +
  geom_text(data = g3_text_data, mapping = aes(x = x, y = y, label = label),
            size = 2, family = "Bai Jamjuree", color = alpha("black", 0.7))

f <- plot_grid(g3, g, align = "h", nrow = 1, rel_widths = c(1/12, 6/10))

ggdraw() +
  draw_plot(f) +
  draw_plot(g2, y = 0.06, x = 0.125, height = 1, width = 0.88) +
  theme(plot.background = element_rect(fill = background, color = background),
        plot.title = element_text(hjust = 0.5, size = 12, family = "Public Sans",
                                  face = "bold", vjust = -2)) +
  geom_segment(data = segment_data,
               mapping = aes(x = x, xend = xend, y = y, yend = yend),
               color = alpha("red", 0.1), size = 0.25) +
  labs(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY GEORGIA NEGROES.\n")

ggsave("challenge04.png", height = 8, width = 6, unit = "in")
