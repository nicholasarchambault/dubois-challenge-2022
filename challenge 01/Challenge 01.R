library(maps)
library(ggplot2)
library(tidyverse)
library(ggforce)
library(gridExtra)
library(geomtextpath)
library(showtext)

font_add_google(name = "Public Sans")
font_add_google(name = "Bai Jamjuree")
showtext_auto()

background <- "#EFE5D7"
world <- map_data("world")
us_states <- map_data("state")

south_black <- c("north carolina", "south carolina", "tennessee", 
                 "georgia", "alabama", "mississippi", "florida")

south_brown <- c("maryland", "virginia", "west virginia", "kentucky",
                 "arkansas", "louisiana")

us_states <- us_states[us_states$region %in% south_black | 
                         us_states$region %in% south_brown, ]

shade_black <- c("Mauritania", "Mali", "Niger", "Chad", "Sudan", "Eritrea", 
                 "Ethiopia", "Djibouti", "Somalia", "Central African Republic", 
                 "Cameroon", "Nigeria", "Benin", "Togo", "Ghana", "Ivory Coast", 
                 "Guinea", "Liberia", "Sierra Leone", "Senegal", "Gabon", 
                 "Guinea-Bissau", "Burkina Faso", "Republic of Congo",
                 "Democratic Republic of the Congo", "Uganda", "Kenya", 
                 "Burundi", "Tanzania", "Angola", "Zambia", "Malawi", 
                 "Mozambique", "Zimbabwe", "Namibia", "Botswana", "Brazil", 
                 "Cuba", "Jamaica", "Dominican Republic", "Haiti", "Bahamas", 
                 "Rwanda", "South Sudan", "Western Sahara", "Gambia", 
                 "Equatorial Guinea")

shade_brown <- c("South Africa", "Lesotho", "Swaziland", "Morocco", "Algeria",
                 "Tunisia", "Libya", "Egypt", "Suriname", "Guyana", 
                 "French Guiana", "Venezuela", "Panama", "Costa Rica", "Belize",
                 "Nicaragua", "Honduras")

americas <- read.csv("Americas.csv", stringsAsFactors = F, as.is = T)
americas$country[americas$country == "United States"] <- "USA"

americas <- americas %>% 
  select(country) %>% 
  inner_join(world, by = c(country = "region"))

m1 <- ggplot(americas, aes(long, 
                           lat, 
                           group = group)) +
  geom_polygon(fill = case_when(americas$country %in% shade_black ~ "black",
                                americas$country %in% shade_brown ~ "#654321",
                                TRUE ~ "#d2b48c"), 
               color = NA) +
  geom_map(aes(map_id = region), 
           map = us_states, 
           data = us_states, 
           color = NA, 
           fill = case_when(us_states$region %in% south_black ~ "black",
                            us_states$region %in% south_brown ~ "#654321")) +
  coord_map("orthographic", 
            orientation = c(5, -97, 0)) +
  theme_void() +
  theme(panel.background = element_rect(fill = background,
                                        color = background),
        plot.margin = margin(0, -3.33, 0, 0, "cm"))

world2 <- world[!world$region %in% c("Antarctica", "Greenland", "Brazil", 
                                     "Australia"), ]


m2 <- ggplot(world2, aes(long, lat, group = group)) +
  geom_polygon(fill = case_when(world2$region %in% shade_black ~ "black",
                                world2$region %in% shade_brown ~ "#654321",
                                TRUE ~ "#d2b48c"), 
               color = NA) +
  coord_map("orthographic", 
            orientation = c(5, 42, 0)) +
  theme_void() + 
  theme(panel.background = element_rect(fill = background,
                                        color = background),
        plot.margin = margin(0, 0, 0, -3.3, "cm"))

points_data <- data.frame(x = c(0.665, 0.335), y = c(0.5, 0.5))

textcurve_data <- data.frame(label = c("DISTRIBUTION OF", "THE NEGRO RACE"),
                             x = c(0.39, 0.53),
                             xend = c(0.47, 0.61),
                             y = c(1.02, 0.83),
                             yend = c(0.83, 1.02))

segment_data <- data.frame(x = c(0.575, 0.575, 0.575, 0.585),
                           xend = c(0.41, 0.375, 0.385, 0.4775),
                           y = c(0.43, 0.43, 0.43, 0.3),
                           yend = c(0.61, 0.715, 0.745, 0.36))

g <- grid.arrange(m1, m2, nrow = 1)
cowplot::ggdraw(g) +
  theme(plot.background = element_rect(fill = background, color = background),
        plot.margin = margin(2, 0.075, 4, 0.075, "cm")) +
  geom_point(data = points_data, mapping = aes(x = x, y = y),
             size = 81, pch = 1, color = "gray43", alpha = 0.5) +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, 
                                  vjust = 22, 
                                  size = 12, 
                                  face = "bold",
                                  family = "Public Sans"),
        plot.subtitle = element_text(hjust = 0.5, 
                                     vjust = 28, 
                                     size = 8, 
                                     face = "bold",
                                     family = "Public Sans")) +
  geom_textcurve(data = textcurve_data, mapping = aes(label = label,
                                                      x = x,
                                                      xend = xend,
                                                      y = y,
                                                      yend = yend),
                 curvature = -0.15, color = "gray43", size = 2.5, 
                 family = "Bai Jamjuree") +
  annotate(geom = "text", x = 0.349, y = -0.0825,
           label = "=",
           family = "Bai Jamjuree",
           color = "gray43",
           size = 5.5,
           hjust = 0) +
  annotate(geom = "text", x = 0.37, y = -0.12,
           label = "ROUTES OF THE AFRICAN SLAVE TRADE.\nTHE STATE OF GEORGIA.",
           family = "Bai Jamjuree",
           color = "gray43",
           size = 2.5,
           hjust = 0) +
  annotate(geom = "text", x = 0.225, y = -0.35,
           label = "            THIS CASE IS DEVOTED TO A SERIES OF CHARTS,MAPS AND OTHER DEVI-\nCES DESIGNED TO ILLUSTRATE THE DEVELOPMENT OF THE AMERICAN NEGRO IN A\nSINGLE TYPICAL STATE OF THE UNITED STATES.",
           family = "Bai Jamjuree",
           color = "gray43",
           size = 2.5,
           hjust = 0) +
  annotate(geom = "text", x = 0.5, y = -0.55,
           label = '" THE PROBLEM OF THE 20th CENTURY IS THE PROBLEM OF THE\nCOLOR-LINE."',
           family = "Bai Jamjuree",
           color = "gray43",
           size = 2.5,
           hjust = 0.5) +
  geom_point(size = 3, pch = 19, aes(x = 0.3566, y = -0.15), color = "black") +
  geom_point(size = 2, pch = 18, aes(x = 0.3566, y = -0.15), color = "#d2b48c") +
  geom_point(size = 1.5, pch = 18, aes(x = 0.37, y = 0.725), color = "#d2b48c") +
  geom_segment(data = segment_data, mapping = aes(x = x, 
                                                  xend = xend, 
                                                  y = y,
                                                  yend = yend),
               color = "gray43", size = 0.2) +
  geom_curve(aes(x = 0.575, y = 0.43, xend = 0.565, yend = 0.79),
             size = 0.2, color = "gray43", curvature = -1.1, angle = 270) +
  labs(title = "THE GEORGIA NEGRO .",
       subtitle = "A SOCIAL STUDY\nBY\nW.E.BURGHART DU BOIS.")


ggsave("challenge01.png")


