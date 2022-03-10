library(tidyverse)
library(showtext)
library(lubridate)

background <- "#EFE5D7"
crimson <- "#dc143c"

showtext_auto()
font_add_google("Bai Jamjuree")
font1 <- "Bai Jamjuree"

df <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge05/data.csv")
colnames(df) <- colnames(df) %>% tolower()

df <- df %>%
  pivot_longer(cols = 2:3, names_to = "group", values_to = "val") %>%
  mutate(dates = ymd(paste0(year, "-01-01"))) %>%
  select(-year) %>%
  mutate(label = case_when(val == 1.3 ~ "1.3%",
                           val == 100 ~ "100%",
                           TRUE ~ as.character(val)),
         val = case_when(dates == ymd("1870-01-01") & group == "free" ~ 6,
                         dates == ymd("1870-01-01") & group == "slave" ~ 94,
                         TRUE ~ val))

timeline <- seq.Date(min(df$dates), max(df$dates), by = 125)
cuts <- rnorm(length(timeline), mean = 97.1, sd = .05)
cuts_df <- tibble(dates = timeline,
                  cuts = cuts)

dates <- seq.Date(min(df$dates), max(df$dates), by = 100)

top <- 97.1
bottom <- 96.9
middle <- 97.0


for (i in 1:(length(dates) - 1)) {
  middle[i + 1] <- middle[i] + rnorm(1, mean = 0, sd = 0.01)
  if (middle[i + 1] > top) {
    middle[i + 1] <- top - 0.01
  }
  else if (middle[i + 1] < bottom) {
    middle[i + 1] <- bottom + 0.01
  }
}

walk <- tibble(dates = dates, val = middle) %>%
  filter(dates < ymd("1870-01-01")) %>%
  bind_rows(., 
            tibble(dates = ymd("1870-01-01"),
                   val = middle[length(middle)])) %>%
  mutate(group = "group")

d <- bind_rows(df, walk)

d %>%
  filter(group != "group") %>%
  ggplot(aes(dates, val, fill = group)) +
  geom_area(color = background, size = 0.2, outline.type = "full") +
  scale_fill_manual(values = c(crimson, "black")) +
  geom_area(data = d %>% filter(group == "group" & dates < ymd("1870-01-02")),
            fill = background, color = "black") +
  geom_vline(xintercept = ymd(paste0((seq(1790, 1870, by = 10)), "-01-01"))[2:8],
             color = background) +
  geom_text(data = d %>% filter(group == "free"), aes(y = 100.4, label = label),
            hjust = 0, size = 4, family = font1) +
  coord_flip(ylim = c(97, 101),
             xlim = c(ymd("1870-01-01"), ymd("1790-01-01")), clip = "off") +
  annotate(geom = "text", x = ymd("1786-01-01"), y = 100.6,
           family = font1, label = "PERCENT\nOF\nFREE NEGROES", 
           lineheight = .75) +
  scale_x_date(breaks = ymd(paste0((seq(1790, 1870, by = 10)), "-01-01")), 
               date_labels = "%Y", expand = c(0, 0)) +
  scale_y_continuous(position = "right", breaks = c(97:100),
                     labels = c("3%", "2%", "1%", "")) +
  theme(aspect.ratio = 2/1,
        legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.x = element_line(color = alpha("black", 0.5)),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family = font1, face = "bold",
                                  size = 22, margin = margin(b = 40, t = 15)),
        plot.title.position = "plot",
        axis.line.y = element_line(color = background),
        axis.text = element_text(family = font1),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16, margin = margin(r = 3))) +
  labs(x = "", y = "", title = " SLAVES AND FREE NEGROES.\n", caption = "\n")

ggsave("challenge05.png", bg = background, 
       width = unit(8.5, "in"), height = unit(11, "in"))



