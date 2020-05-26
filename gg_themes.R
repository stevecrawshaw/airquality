wants <- c("tidyverse", "here", "openair")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

Sys.setenv(TZ = "Etc/GMT-0")
# dateFrom = "2020-02-01"
# dateTo = "2020-02-29"
# startdate <- as.Date(dateFrom)
# enddate <- as.Date(dateTo)

# source("S:\\SUSTAIN\\EnvQual\\Air_Quality\\Projects\\R Projects\\airquality_GIT\\importODS.r")
# #download data
# aq_data_DT_ggtheme <- importODSAQ(
#     siteid = "all",
#     pollutant = c("no2", "nox", "no"),
#     dateFrom = dateFrom,
#     dateTo = dateTo
# )
# # 
# saveRDS(aq_data_DT_ggtheme, file = here::here("data", "aq_data_DT_ggtheme.rds"))
# 
# 
# aq_data_DT_ggtheme <- readRDS(here::here("data", "aq_data_DT_ggtheme.rds"))
#generate plot data for testing
# plot_DT <- aq_data_DT_ggtheme %>%
#     select(-siteid) %>%
#     timeAverage(avg.time = "day", type = "location") %>%
#     rename(Date = date, Location = location) %>%
#     pivot_longer(cols = no2:no, names_to = "Pollutant", values_to = "Concentration") %>%
#     mutate(Pollutant = as.factor(toupper(Pollutant)))
# 
# glimpse(plot_DT)

#-----------------------FACET THEME------------------------------

#this theme for inclusion in a report where there are multiple facets
#eg for each site

theme_report_facet <- function () { 
    theme_bw(base_size=12) %+replace% 
        theme(
            plot.title = element_text(size = 18, face = "bold", margin = margin(0,0,15,0), hjust = 0.5),
            panel.background  = element_blank(),
            plot.background = element_rect(fill="white", colour = NA), 
            legend.background = element_rect(fill="transparent", colour=NA),
            legend.key = element_rect(fill="transparent", colour=NA),
            strip.text.y = element_text(color = "black", face = "bold")
            )
}

#test plot facet
# p_line_facet <- plot_DT %>% 
#     ggplot(aes(x = Date, y = Concentration, color = Pollutant)) +
#     geom_line(lwd = 1) +
#     facet_wrap(~ Location)
# 
# p_line_facet +
#     theme_report_facet()

#-----------------------------POWERPOINT THEME SINGLE CHART-------------------------

#this theme is for a single chart where the intention is to include in a powerpoint
#so the axes labels, titles etc are in bold and large

theme_ppt_single <- function(){
    theme_bw(base_size = 14) %+replace%
        theme(
            plot.title = element_text(size = 18, face = "bold", margin = margin(0,0,15,0), hjust = 0.5),
            plot.subtitle = element_text(size = 12, face = "bold", margin = margin(10,0,10,0)),
            panel.grid.major = element_line(color = "darkgray"),
            axis.text.x = element_text(size = 18, face = "bold"),
            axis.text.y = element_text(size = 18, face = "bold"),
            axis.title.y = element_text(size = 18, face = "bold", angle = 90),
            axis.title.x = element_text(size = 18, face = "bold"),
            legend.text = element_text(size = 18),
            strip.text.y = element_text(color = "black", face = "bold")
        )
    
}

#test plot single ppt
# single_ppt <- plot_DT %>%
#     filter(Location == "Wells Road") %>% 
#     ggplot(aes(x = Date, y = Concentration, color = Pollutant)) +
#     geom_line(lwd = 1) +
#     labs(title = "Daily mean concentrations: Fishponds Road",
#          y = quickText(" Concentration ugm-3"))
# 
# single_ppt +
#     theme_ppt_single()
