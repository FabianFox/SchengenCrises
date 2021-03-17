# Irregular migration by route
# Data from: https://frontex.europa.eu/we-know/migratory-map/

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "countrycode", "lubridate", "forcats",
            "patchwork", "janitor")

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
border_crossings.df <- import("https://frontex.europa.eu/assets/Migratory_routes/2021/Detections_of_IBC_2021_02_05.xlsx") %>%
  clean_names() %>%
  pivot_longer(cols = -c(1:3), names_to = "date", values_to = "number") %>%
  mutate(date = format(parse_date_time(date, orders = c("m/Y")), "%m-%Y"))
