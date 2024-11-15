library(readxl)
library(mapdata)
library(ggplot2)
library(dplyr)
library(writexl)
library(sf)
library(rnaturalearth)
library(plotly)
library(jsonlite)

# HISTORICAL HURRICANE 2 TASK 1 --------
hurricane1 <- read_excel("Hurricane-Dataset.xlsx", 
                         sheet = "Historical Hurricane 1")
hurricane2 <- read_excel("Hurricane-Dataset.xlsx", 
                         sheet = "Historical Hurricane 2")
exposures <- read_excel("Hurricane-Dataset.xlsx", 
                        sheet = "Exposures")

# Reformats nameless hurricanes to align with hurricane1 naming convention
index_name <- hurricane2$storm_name == "NOT NAMED"
hurricane2$storm_name[index_name] <- "NOT_NAMED"

# Assigns WIND_WMO from hurricane1 to the entries on hurricane2 with
# the corresponding name and datetime
# Assumption: no two hurricanes exist at the same time with the same name
hurricane2$wmo <- hurricane1$WMO_WIND[match(paste(hurricane2$storm_name, hurricane2$date),
                                            paste(hurricane1$NAME, hurricane1$ISO_TIME))]

# write_xlsx(hurricane2, "modified_hurricane2.xlsx")

# DATA PREP AND EXAMPLE FOR APP --------
portfolio <- exposures %>% 
  group_by(PolicyYear) %>% 
  summarize('Total Insured Value' = sum(`Total Insured Value`),
            'Premium' = sum(Premium),
            'Losses - Non Catastrophe' = sum(`Losses - Non Catastrophe`))

subset_location <- function(df, i){
  location_exposures <<- df %>% filter(Location == i)
}

#subset_location(exposures, 2)  


ggplot(exposures, aes(x = PolicyYear, y = Premium, color = as.character(Location))) +
   geom_line() +
  labs(x = "Year", y = "Premium", color = "Location",  title = "Value Over Time")

#mod exposures vector to fit data in app
exposures_mod <- select(exposures, -"Latitude", -"Longitude")
exposures_mod$Location <- as.character(exposures_mod$Location)

#Define the aggregate portfolio data
portfolio <- exposures_mod %>% 
  group_by(PolicyYear) %>% 
  summarize('Total Insured Value' = sum(`Total Insured Value`),
            'Premium' = sum(Premium),
            'Losses - Non Catastrophe' = sum(`Losses - Non Catastrophe`))
portfolio$Location <- "Aggregate"

#Define custom function to subset exposures 
subset_location <- function(df, i){
  location_exposures <<- df %>% filter(Location == i)
}

##Create the data list for each location's data
data_list <- list("Portfolio" = portfolio)

for (i in 1:35){
  subset_location(exposures_mod,i)
  data_list[[as.character(i)]] <- location_exposures
}

#sample proof of concept:
location_selection <- bind_rows(data_list[c("Portfolio", "1", "4")])


# MANAGEMENT REQUEST 2 PART 1 ----------

#NOTE: The result of this script is also visually displayed in the results of Mgmt Request 2 Part 4's output map

#Define spatial map data for world
world_sf <- st_as_sf(x = map_data("world"),
                      coords = c("long", "lat"))

#Sort location data by location. Total Insured Value is assumed to be maximized at the most recent year.
location_data <- exposures %>% 
  group_by(Location) %>%
  summarize('Total Insured Value'      = max(`Total Insured Value`),
            'Premium'                  = max(Premium),
            'Losses - Non Catastrophe' = sum(`Losses - Non Catastrophe`),
            'longitude'                = first(Longitude),
            'latitude'                 = first(Latitude)
            )
  

#Base map of the world
base_map <- plot_geo() %>%
  add_trace(
    mode = "lines",
    geojson = jsonlite::toJSON(world_sf),
    fillcolor = "gray80",
    line = list(color = toRGB("black"))
  )

#Add locations to the map
insuredValueMap <- add_trace(
  base_map,
  mode = "markers",
  lon = ~location_data$longitude,
  lat = ~location_data$latitude,
  marker = list(color = "darkred", size = 10*log(location_data$`Total Insured Value`/100000)),
  hovertext = ~paste0("Location: ", location_data$Location, "<br>", 
                      "Total Insured Value: ", location_data$`Total Insured Value`),
  name = "Insured Location"
)

# Display the interactive map
insuredValueMap

# MANAGEMENT REQUEST 2 PART 2 -------------

hurricane_sf <- st_as_sf(x = hurricane1,
                     coords = c("LON", "LAT"))
location_sf <- st_as_sf(x = location_data,
                     coords = c("longitude", "latitude"))


# Calculate distances between hurricanes and locations
distances <- st_distance(hurricane_sf, location_sf)

# Find pairs within the threshold
close_pairs <- which(distances <= 1, arr.ind = TRUE)

# Flag hurricanes and locations
hurricane1$risk <- FALSE
location_data$risk <- FALSE
location_data$interactions <- 0

hurricane1[unique(close_pairs[, 1]), "risk"] <- TRUE
location_data[unique(close_pairs[, 2]), "risk"] <- TRUE
location_data$interactions <- sapply(unique(location_data$Location), 
                                     function(idx) 
                                       {sum(close_pairs[, 2] == idx)}
                                     )

writexl::write_xlsx(x = location_data, "exposures_mod.xlsx")

# Same code as above but for hurricane2 data. Generates location_data2
# NOTE: Only locations 1 and 18 change. 
# They are flagged with hurricane1 but not flagged with hurricane2

# location_data2 <- location_data
# hurricane_sf2 <- st_as_sf(x = hurricane2,
#                      coords = c("longitude", "latitude"))
# location_sf <- st_as_sf(x = location_data,
#                      coords = c("longitude", "latitude"))
# distances <- st_distance(hurricane_sf2, location_sf)
# close_pairs <- which(distances <= 1, arr.ind = TRUE)
# hurricane2$risk <- FALSE
# location_data2$risk <- FALSE
# hurricane2[unique(close_pairs[, 1]), "risk"] <- TRUE
# location_data2[unique(close_pairs[, 2]), "risk"] <- TRUE

# MANAGEMENT REQUEST 2 PART 3 -------
#Note: Requires the output from MR2P2

hurricane1_mod <- hurricane1
hurricane1_mod$LOCATION <- NA
hurricane1_mod$LOCATION[close_pairs[, 1]] <- close_pairs[, 2]


max_wind_all <- hurricane1_mod %>% 
  group_by(NAME, SEASON) %>%
  summarize('Max Wind' = max(WMO_WIND))

max_wind_flagged <- hurricane1_mod %>%
  filter(risk) %>%
  group_by(NAME, SEASON, LOCATION) %>%
  summarize('Passing Wind' = max(WMO_WIND))

hurricaneRiskWind <- left_join(max_wind_flagged, max_wind_all)

writexl::write_xlsx(x = hurricaneRiskWind, "hurricaneRiskWind.xlsx")

# MANAGEMENT REQUEST 2 PART 4 --------

#NOTE: requires output from MANAGEMENT REQUEST 2 PART 1

hurricane_sf <- st_as_sf(x = hurricane1,
                         coords = c("LON", "LAT"))
location_sf <- st_as_sf(x = location_data,
                        coords = c("longitude", "latitude"))


#Define a threshold for what makes a location high catastrophe risk
highThreshold <- mean(location_data$interactions)

#Assign a risk class to each location
location_data <- location_data %>%
  mutate("maxLossRisk" = case_when(
    interactions == 0 ~ "LOW",
    interactions < highThreshold ~ "MEDIUM",
    TRUE ~ "HIGH"
  ))

color_palette <- c("green", "yellow", "red")

#Add locations to the map
maxLossRiskMap <- add_trace(
  base_map,
  mode = "markers",
  lon = ~location_data$longitude,
  lat = ~location_data$latitude,
  marker = list(color = color_palette[match(location_data$maxLossRisk, c("LOW", "MEDIUM", "HIGH"))],
                size = 10*log(location_data$`Total Insured Value`/100000)),
  hovertext = ~paste0("Location: ", location_data$Location, "<br>",
                      "Total Insured Value: ", location_data$`Total Insured Value`, "<br>",
                      "Premium: ", location_data$Premium),
  legendgroup = ~location_data$maxLossRisk,
  name = "Insured Location"
)


# Display the interactive map
maxLossRiskMap