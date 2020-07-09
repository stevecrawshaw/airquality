#animate predicted de - weathered data from the covid modelled data

wants <- c("tidyverse", "here", "data.table", "lubridate", "gganimate", "gifski", "ggthemes")
has   <- wants %in% rownames(installed.packages())
if (any(!has))
install.packages(wants[!has])
lapply(wants, library, character.only = T)

dwp_site_data <- readRDS(here::here("data", "dwp_site_data.rds"))
source("../airquality_GIT/gg_themes.R")

#keep base graph and animated graph separate for testing lool
dwp_plot <- dwp_site_data %>% 
  mutate(yday = yday(date)) %>%
  filter(pollutant == "NOX") %>% 
  ggplot(aes(x = date, y = pred, colour = site)) +
  geom_point(size = 5) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Date",
       y = "Predicted NOx",
       colour = "Site",
       title = "    De - weathered NOx at continuous analyser sites") +
    theme_ppt_single() +
  theme(plot.title = element_text(hjust = 0.5))

dwp_plot

dwp_plot_animate <- dwp_plot +
  transition_time(yday) +
  shadow_wake(wake_length = 0.4)

dwp_plot_animate  

animate(dwp_plot_animate, height = 500, width = 800, fps = 30, duration = 20, end_pause = 60, res = 100)

anim_save(here::here("plots", "animated_dwp_nox_july2020.gif"))
