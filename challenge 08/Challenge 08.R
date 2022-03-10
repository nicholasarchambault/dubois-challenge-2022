library(tidyverse)
background <- "#EFE5D7"

df <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge08/data.csv")

colnames(df) <- c("year", "value")
df <- df %>%
  mutate(color = c("#ffc0cb", "#4682b4", "#654321", "#ffd700", background, 
                   "#dc143c"),
         label = c('1875_______ $ 21,186',
                   '1880_____ $  498,532',
                   '1885____ "   736,170',
                   '1890____ " 1,173,624',
                   '1895____ " 1,322,694',
                   '1899____ " 1,434,975'),
         ystart = c(25, 23, 21, 19, 17, 15))

slope <- 2 * (min(df$ystart) - nrow(df)) / max(df$value)

df <- df %>% 
  mutate(yfinal = ystart - slope*value)

polygons <- df %>%
  mutate(x_1 = 0,
         x_2 = value,
         x_3 = value,
         x_4 = 0,
         y_1 = ystart,
         y_2 = yfinal,
         y_3 = yfinal + 2,
         y_4 = ystart + 2) %>%
  select(year, x_1:y_4) %>%
  pivot_longer(cols = -year,
               names_sep = "_",
               names_to = c("coordinate", "idx")) %>%
  pivot_wider(names_from = "coordinate",
              values_from = value) %>%
  left_join(df, by = "year")


g <- ggplot(polygons, aes(x = x, y = y, group = year)) +
  geom_polygon(aes(group = year, fill = I(color), color = I("black"))) +
  scale_y_continuous(expand = expansion(add = c(3, -5))) +
  scale_x_continuous(expand = expansion(add = c(0, -600000))) +
  coord_polar() +
  geom_text(data = polygons %>%
              filter(idx == 1) %>%
              select(year, x, y, value, label),
            aes(x = x, y = y, label = label),
            color = "black", size = 3, 
            family = "mono", adj = 1.05, nudge_y = 1) +
  theme_void() +
  ggtitle("ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGROES.\n\n") +
  theme(plot.title = element_text(family = "mono", size = 14, color = "black",
                                  face = "bold", hjust = 0.5),
        plot.margin = margin(0.5, 0, 0, 0, "cm"))

cowplot::ggdraw(g) + 
  theme(plot.background = element_rect(fill = background, color = background))

ggsave("challenge08.png", height = 7, width = 6)






