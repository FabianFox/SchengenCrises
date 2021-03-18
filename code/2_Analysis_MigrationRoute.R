# Irregular migration by route
# Data from: https://frontex.europa.eu/we-know/migratory-map/

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "lubridate", "forcats",
            "patchwork", "janitor")

# English locale (for this session)
Sys.setlocale("LC_TIME", "English")

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

# Load data from https://frontex.europa.eu/assets/Migratory_routes/2021/Detections_of_IBC_2021_02_05.xlsx
### ------------------------------------------------------------------------###
rfg.df <- import("https://frontex.europa.eu/assets/Migratory_routes/2021/Detections_of_IBC_2021_02_05.xlsx") %>%
  clean_names() %>%
  pivot_longer(cols = -c(1:3), names_to = "date", values_to = "number") %>%
  mutate(
    date = parse_date_time(date, orders = c("m/Y"), truncated = 1),
    main_routes = case_when(
      route %in% c("Black Sea Route", 
                   "Circular Route from Albania to Greece",
                   "Eastern Borders Route", 
                   "Other", 
                   "Western African Route",
                   "Western Mediterranean Route") ~ "Other",
      route == "Central Mediterranean Route" ~ "Central Mediterranean",
      route == "Eastern Mediterranean Route" ~ "Eastern Mediterranean",
      route == "Western Balkan Route" ~ "Western Balkan",
      TRUE ~ NA_character_)) 

# Aggregate arrivals by route and month
rfg_agg.df <- rfg.df %>%
  filter(year(date) > 2014) %>%
  group_by(date = ymd(date), main_routes) %>%
  summarise(number = sum(number)) %>%
  ungroup()

# Plot
arrivals.fig <- ggplot(rfg_agg.df, aes(x = date, y = number, linetype = 
                                         factor(main_routes, 
                                                levels = c(
                                                  "Central Mediterranean",
                                                  "Eastern Mediterranean",
                                                  "Western Balkan",
                                                  "Other")))) +
  geom_line(stat = "identity") +
  scale_x_date(breaks = seq(as.Date("2015-01-01"), 
                            as.Date("2022-06-01"), "year"),
               date_labels = "%Y") +
  scale_linetype_manual(values = c("dotted", "solid", "dashed", "dotdash")) +
  labs(x = "", y = "", caption = "Data: Frontex (2021)", linetype = "Routes") +
  theme.basic

# Export plots
### ------------------------------------------------------------------------###
ggsave(
  plot = arrivals.fig, "./figures/Fig 2 - Arrivals by route.tiff", 
  width = 12, height = 6, unit = "in",  dpi = 300
)
