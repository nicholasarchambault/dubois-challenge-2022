library(tidyverse)
library(gridExtra)
library(ggforce)
library(showtext)
library(cowplot)


font_add_google(name = "Public Sans")
showtext_auto()

background <- "#EFE5D7"
brown <- "#654321"

df <- data.frame(field = c("BUSINESS", "CLASSICAL", 
                           "PROFESSIONAL", "SCIENTIFIC",
                           "NORMAL", "INDUSTRIAL"),
                 students = c(12, 98, 152, 161, 383, 2252))



gg <- df %>%
  mutate(field = paste(field, 
                       stringr::str_pad(as.character(students), width = 18 - nchar(field)),
                       sep = ""),
         field = forcats::fct_reorder(field, desc(students))) %>%
  ggplot(df, mapping = aes(x = field, y = students)) +
  geom_bar(stat = "identity", color = brown, fill = brown, width = 0.2) + 
  coord_flip() +
  theme(plot.background = element_rect(fill = background, color = background),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(hjust = 0, family = "Public Sans", 
                                   color = brown,
                                   size = 9),
        plot.margin = margin(t = -1, r = 1, l = 1, b = 1, "cm")
        ) +
  scale_x_discrete(expand = c(0.2, 0))

title <- ggpubr::text_grob("NUMBER OF NEGRO STUDENTS TAKING\nTHE VARIOUS COURSES OF STUDY\nOFFERED IN GEORGIA SCHOOLS .",
                           size = 11, face = "bold", family = "Public Sans")

segment_data <- data.frame(x = c(0.2, 0.2),
                           xend = c(0.916, 0.55),
                           y = c(0.15335, 0.09755),
                           yend = c(0.15335, 0.09755))

arr <- grid.arrange(gg, nrow = 1, 
                    top = title)
ggdraw(arr) +
  theme(plot.background = element_rect(fill = background, color = background)) +
  geom_segment(data = segment_data, 
               mapping = aes(x = x, xend = xend, y = y, yend = yend),
               color = brown, size = 4.7) +
  geom_arc(aes(x0 = 0.913, y0 = 0.18075, r = 0.0272, start = pi, end = 0),
           color = brown, size = 4.7) +
  geom_arc(aes(x0 = 0.201, y0 = 0.12545, r = 0.0279, start = 2*pi, end = pi),
           color = brown, size = 4.7)

ggsave("challenge09.png")

