library(tidyverse)
library(showtext)
library(ggpubr)

global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

nh_temps %>%
  mutate(Hemisphere = "Northern Hemisphere") %>%
  rbind(sh_temps %>% mutate(Hemisphere = "Southern Hemisphere")) %>%
  select(Year, Hemisphere, DJF, MAM, JJA, SON) %>%
  pivot_longer(cols = DJF:SON, names_to = "season", values_to = "temp") %>%
  mutate(Year = case_when(season == "DJF" ~ Year,
                          season == "MAM" ~ Year + 0.25,
                          season == "JJA" ~ Year + 0.5,
                          TRUE ~ Year + 0.75),
         season = case_when(season == "DJF" ~ "Dec-Feb",
                            season == "MAM" ~ "Mar-May",
                            season == "JJA" ~ "Jun-Aug",
                            TRUE ~ "Sep-Nov"),
         season = factor(season, levels = c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov"))) %>%
  ggplot(aes(x=Year, y=temp)) +
  geom_point(aes(color = season)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x=Year, y=temp, xend = Year, yend= 0), color = "gray70", alpha = 0.2) +
  geom_bracket(
    aes(xmin = xmin, xmax = xmax, label = label),
    data = data.frame(xmin = c(1951), xmax = c(1980), label = c("Average")), y.position = c(1.5), label.size = 3, tip.length = 1, color = "#999797") +
  facet_wrap(~Hemisphere, nrow=1) +
  labs(
    title = "Global Surface Temperature Change Overtime", 
    subtitle = "This visualization presents the long-term variations in global surface temperatures, broken down by meteorological seasons.\nThe data highlights temperature anomalies relative to the 1951-1980 baseline, offering insight into the shifting climate patterns over time.", 
    caption = "Lia-me52 | Data from GISTEMP", 
    y = "Temperature deviation (°C)"
  ) +
  scale_color_manual(values = c("slategray2", "darkolivegreen3", "khaki1", "tan2")) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"), 
    plot.subtitle = element_text(size=9),
    plot.title = element_text(face = "bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

