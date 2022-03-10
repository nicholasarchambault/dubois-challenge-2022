library(tidyverse)
library(gridExtra)
library(ggforce)
library(showtext)

background <- "#EFE5D7"

showtext_auto()
font_add_google("Bai Jamjuree")
font1 <- "Bai Jamjuree"

df <- data.frame(year = c(1860, 1870, 1880, 1890, "(1900?)"),
                 rate = c(99, 92.1, 81.6, 67.2, 50),
                 neg_rate = -1*df$rate)

ggplot(data = df) +
  geom_bar(aes(x = neg_rate, y = rate), stat = "identity", width = 2.7,
           fill = "black") +
  geom_segment(data = df,
               mapping = aes(x = -101, 
                             xend = neg_rate + 1, 
                             y = rate, 
                             yend = rate), 
               color = "black", size = 6.5, lineend = "round") +
  geom_segment(data = df,
               mapping = aes(x = -101, 
                             xend = neg_rate + 1, 
                             y = rate, 
                             yend = rate), 
               color = background, size = 6, lineend = "round") +
  ggtitle("ILLITERACY.") +
  theme(plot.background = element_rect(fill = background, color = background),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = font1, size = 8, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = "mono", color = "black",
                                  face = "bold", size = 16),
        plot.margin = margin(t = 0.5, r = 0, l = -0.5, b = 0.5, "cm")) +
  scale_y_continuous(limits = c(0, 105)) +
  scale_x_continuous(limits = c(-120, -45), labels = NULL) +
  geom_text(data = df, 
            mapping = aes(x = neg_rate, y = 0, 
                          label = c(paste0("\n", rate[1:4], "%"), "\n(50%?)")),
            color = "black", family = font1, size = 4) +
  geom_text(data = df, mapping = aes(x = -107, y = rate, label = year),
            color = "black", family = font1, size = 4) +
  annotate(geom = "text", x = -110, y = 0.05, label = "PERCENT OF\nILLITERACY.", 
           hjust = 0.5, family = font1, 
           color = "black", size = 4)

ggsave("challenge06.png", heigh = 8, width = 6)  


  
