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
bcontrol.df <- import("./data/TemporaryBorderControls - 2006-2021.rds")

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
  annotate("text", x = as.Date("2011-01-01") + months(6), y = "Italy",
           hjust = 0,
           label = "Initial border controls in\nrelation to the migration\nand refugee crisis.") +
  annotate("segment", x = as.Date("2015-01-01") - months(4), 
           xend =  as.Date("2015-09-13") - months(1), 
           y = "Italy", yend = "Iceland") +
  annotate("rect", xmin = as.Date("2020-03-11") - days(7), 
           xmax = as.Date("2020-03-11"), 
           ymin = -Inf, ymax = Inf, 
           colour = "#bdbdbd", alpha = .5) +
  annotate("text", x = as.Date("2016-01-01") + months(3), y = "Liechtenstein",
           hjust = 0,
           label = "Initial border controls\nin relation to COVID-19.") +
  annotate("segment", x = as.Date("2019-01-01") + months(3), 
           xend =  as.Date("2020-03-11") - months(1), 
           y = "Liechtenstein", yend = "Latvia") +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), 
                            as.Date("2022-10-01"), "years"), 
               date_labels = "%Y",
               limits = as.Date(c("2006-01-01", "2022-01-01")))  +
    scale_y_discrete(drop = FALSE) +
    labs(x = "", y = "") +
    theme.basic +
    theme(axis.text.x = element_text(colour = "black", size = 14))

# Save plots
ggsave(
  plot = tbc_segment.fig, "./figures/Fig 1 - TBC_SegmentPlot.tiff", 
  width = 11, height = 8, unit = "in",  dpi = 300
)

