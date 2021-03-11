# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "lubridate", "forcats",
            "patchwork")

# Theme
### ------------------------------------------------------------------------###
# Additional settings (i.e. for figures)
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 16),
    axis.ticks.x = element_line(size = .5),
    axis.text = element_text(colour = "black", size = 16)
    #    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Load data (all notifications)
### ------------------------------------------------------------------------###
bcontrol.df <- import("./data/TemporaryBorderControls - 2015-2021.rds")

# Total length of controls throughout observation period
bcontrol.df <- bcontrol.df %>%
  mutate(check_length = interval(ymd(begin), ymd(end)) / days(1),
         check_length = if_else(check_length == 0, 1, check_length)) %>% 
  group_by(member_state) %>%
  mutate(total_length = sum(check_length, na.rm = TRUE)) %>%
  ungroup()

# Segment plot
tbc_segment.fig <- ggplot(data = bcontrol.df,
                          aes(x = as.Date(begin), xend = as.Date(end), 
                              y = fct_rev(member_state), yend = member_state)) +
  geom_segment(size = 5) +
  annotate("rect", xmin = as.Date("2015-09-13") - days(7),
           xmax = as.Date("2015-09-13"), 
           ymin = -Inf, ymax = Inf, 
           colour = "#bdbdbd", alpha = .5) +
  annotate("text", x = as.Date("2016-01-01"), y = "Iceland",
           hjust = 0,
           label = "Inital border controls in\nrelation to the migration\nand refugee crisis.") +
  annotate("segment", x = as.Date("2015-09-13") + days(5), 
           xend =  as.Date("2016-01-01") - days(5), 
           y = "Italy", yend = "Iceland") +
  annotate("rect", xmin = as.Date("2020-03-11") - days(7), 
           xmax = as.Date("2020-03-11"), 
           ymin = -Inf, ymax = Inf, 
           colour = "#bdbdbd", alpha = .5) +
  annotate("text", x = as.Date("2019-01-01") - months(6), y = "Lithuania",
           hjust = 0,
           label = "Initial border controls\nin relation to COVID-19.") +
  annotate("segment", x = as.Date("2019-01-01") + months(6), 
           xend =  as.Date("2020-03-11") - days(14), 
           y = "Lithuania", yend = "Liechtenstein") +
  scale_x_date(breaks = seq(as.Date("2015-01-01"), 
                            as.Date("2022-10-01"), "years"), 
               date_labels = "%Y") +
    scale_y_discrete(drop = FALSE) +
    labs(x = "", y = "") +
    theme.basic

# Save plots
ggsave(
  plot = tbc_segment.fig, "./figures/Fig 1 - TBC_SegmentPlot.tiff", 
  width = 11, height = 8, unit = "in",  dpi = 300
)

