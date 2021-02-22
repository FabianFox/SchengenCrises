# Data on temporary land border controls in the Schengen Area
# Directed data on border level

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "qdap", "rio", "countrycode", "janitor", "lubridate", 
            "rvest", "patchwork", "eurostat")

# Load data created in 1_CreateTemporaryBorderControls - 2015-2021.R
### ------------------------------------------------------------------------###
bcontrol.df <- import("./data/TemporaryBorderControls - 2015-2021.rds")

# Make the dataset dyadic
## -------------------------------------------------------------------------- ##
# Load data:
# - Direct Contiguity
# Directed dyads retrieved from http://www.correlatesofwar.org/data-sets/direct-contiguity
# Latest observation: 2016
contdird <- import(file = "./data/contdird.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

# Select only the latest observation (2016), land borders 
# (conttype: 1 (land, river) & 2 (12-mile sea)) and remove unnecessary variables
contdird <- contdird %>%
  select(state1no, state2no, year, conttype) %>%
  filter(year == 2016 & conttype %in% c(1:2))

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "XKX")

# (2) Transform
contdird <- contdird %>%
  mutate(state1 = countrycode(sourcevar = state1no, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = state2no, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         eu_name1 = countrycode(sourcevar = state1, origin = "iso3c", 
                                destination = "eurostat"),
         eu_name2 = countrycode(sourcevar = state2, origin = "iso3c", 
                                destination = "eurostat")) %>%
  select(state1, state2, eu_name1, eu_name2, conttype, year) %>%
  filter(eu_name1 %in% c(eu_countries$code, efta_countries$code) &
           eu_name2 %in% c(eu_countries$code, efta_countries$code))

# EU states and neighbors
eu_neighbours <- contdird %>%
  group_by(state1) %>%
  summarise(neighbours_iso3 = paste0(state2, collapse = ",")) %>%
  select(state1, neighbours_iso3)

# Save data on EU neighbours
# export(eu_neighbours, "./data/EU_neighbours.rds")

# Join to original data frame
bcontrol_dyad.df <- bcontrol.df %>%
  left_join(y = eu_neighbours, by = c("iso3_state" = "state1"))

# Make state and neighbor data longer 
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  separate(neighbours_iso3, into = paste0("n", 1:9), extra = "drop") %>%
  pivot_longer(cols = n1:n9, values_to = "neighbour") %>%
  filter(!is.na(neighbour)) %>%
  select(-name) %>%
  mutate(neighbour_eu = countrycode(neighbour, "iso3c", "eurostat"),
         neighbour_name = countrycode(neighbour, "iso3c", "country.name.en"))

# Arrange by nb
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  arrange(desc(nb)) %>%
  mutate(reasons_scope = str_squish(reasons_scope)) %>%
  select(nb, member_state, iso3_state, begin,	end, implemented, signed, 
         reasons_scope, neighbour_name, neighbour_iso3 = neighbour)

# Add Schengen membership for neighboring countries
schengen.df <- import("./data/SchengenMembership.rds")

# Join
bcontrol_dyad.df <- bcontrol_dyad.df %>%
  left_join(y = schengen.df, by = c("neighbour_iso3" = "iso3_state")) %>%
  rename("signed_neighbour" = "signed.y",
         "implemented_neighbour" = "implemented.y",
         "signed" = "signed.x",
         "implemented" = "implemented.x")

# Export for manual editing
### ------------------------------------------------------------------------###
export(bcontrol_dyad.df, "./data/TemporaryBorderControls_2021-02-22.xlsx")

# Import and finalize as directed dyadic data
### ------------------------------------------------------------------------###
# Edited to include only land border closures (edited on 2021/02/22)
bcontrol.df <- import("./data/TemporaryBorderControls_2021-02-22.xlsx")

# Remove controls unrelated to land borders
bcontrol_land.df <- bcontrol.df %>%
  mutate(land_border_control = ifelse(land_border_control == "T", TRUE, FALSE)) %>%
  filter(land_border_control == TRUE) %>%
  select(-land_border_control)

# Export
### ------------------------------------------------------------------------###
export(bcontrol_land.df, "./data/TBC_Land.rds")
