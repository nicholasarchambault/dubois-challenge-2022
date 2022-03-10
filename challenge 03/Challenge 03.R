library(maps)
library(ggplot2)
library(tidyverse)
library(showtext)
library(cowplot)


font_add_google(name = "Public Sans")
showtext_auto()

# colors
dubois_palette <- c("black",
                    "#d2b48c", # tan
                    "#7A7875", # dark gray
                    "#dc143c", # red
                    "#C6C2BE", # light gray
                    "#ffc0cb", # pink
                    "#654321", # brown
                    "#ffd700", # yellow
                    "navy", 
                    "#E9DCCB") # UNDER

background <- "#EFE5D7"

# Import data
df <- read.csv("https://raw.githubusercontent.com/nicholasarchambault/dubois-data-portraits/master/challenge/2022/challenge03/data.csv")
colnames(df) <- tolower(colnames(df))

# Fix data -- much of it is incompatible with Du Bois' visual
df <- df %>% 
  mutate(
  population = case_when(state %in% c("GA") ~ "750,000 NEGROES AND OVER",
                         state %in% c("MS", "AL", "SC", "VA") ~ 
                           "600,000 - 750,000",
                         state %in% c("LA", "NC") ~ "500,000 - 600,000",
                         state %in% c("TX", "TN", "AR") ~ "300,000 - 500,000",
                         state %in% c("KY", "MD") ~ "200,000 - 300,000",
                         state %in% c("FL", "PA", "MO") ~ "100,000 - 200,000",
                         state %in% c("NY", "OH", "IL") ~ "50,000 - 100,000",
                         state %in% c("KS", "IN", "WV", "NJ", "DE") ~ 
                           "25,000 - 50,000",
                         state %in% c("CA", "MI", "IA", "CT", "MA") ~ 
                           "10,000 - 25,000",
                         state %in% c("WA", "OR", "ID", "NV", "MT", "WY", "CO", 
                                      "AZ", "NM", "ND", "SD", "NE", "OK", "WI", 
                                      "ME", "VT", "NH", "RI", "MN", "UT", "AK", 
                                      "HI") ~ "UNDER - 10,000"))

# Format dashes
df$population <- gsub(" - ", "—", df$population)

# Create factor
df$f_pop <- factor(df$population, 
                   levels = c("750,000 NEGROES AND OVER", 
                              "100,000—200,000",
                              "600,000—750,000",
                              "50,000—100,000",
                              "500,000—600,000",
                              "25,000—50,000",
                              "300,000—500,000",
                              "10,000—25,000",
                              "200,000—300,000", 
                              "UNDER—10,000"))

# Import states
us_states <- map_data("state")

# Join data
df <- 
  left_join(df, data.frame(state.abb, state.name), 
            by = c("state" = "state.abb")) %>%
  mutate(state.name = tolower(state.name)) %>%
  left_join(us_states, by = c("state.name" = "region"))

# Plot
m <- ggplot(data = df, aes(x = long, y = lat, group = group, fill = f_pop)) +
  scale_fill_manual(values = dubois_palette) +
  geom_polygon(color = "black", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(scale = "none") + 
  theme_void() +
  theme(plot.background = element_rect(fill = background,
                                       color = background)) +
  ggtitle("RELATIVE NEGRO POPULATION OF THE STATES OF THE\nUNITED STATES.") + 
  theme(text = element_text(family = "Public Sans"),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, vjust = -5, size = 9, 
                                  face = "bold", margin = margin(0, 0, 85, 0))) +
  theme(legend.position = "bottom", 
        legend.key.size = unit(0.3, 'cm'),
        legend.background = element_rect(fill = background,
                                         color = background),
        legend.spacing.x = unit(0.15, 'cm'), 
        legend.spacing.y = unit(0.3, 'cm'),
        legend.title = element_blank(), 
        legend.box.margin = margin(-5, 1, 15, 1),
        legend.text = element_text(size = 7)) +
  guides(fill = guide_legend(ncol = 2, byrow = T))

ggsave("challenge03.png", m, path = "/Users/nicholasarchambault/Desktop")

