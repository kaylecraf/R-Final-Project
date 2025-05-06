library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(lubridate)
library(zoo)
library(pacman)
library(terra)
library(forecast)
library(leaflet)
library(scales) 
library(raster)

charleston <- read_csv("./data/CharlestonData.csv")
sandiego <- read_csv("./data/SanDiegoData.csv")



charleston$City <- "Charleston"
sandiego$City <- "San Diego"

#Charleston Tract data
charlestonpop <- read_csv("./data/CharlestonPop.csv")
SCTracts <- sf::read_sf("./data/ACS_2020_5YR_TRACT_45_SOUTH_CAROLINA.gdb", layer = "ACS_2020_5YR_TRACT_45_SOUTH_CAROLINA")
charlestonpop <- charlestonpop  %>%
  mutate(GEOID = str_sub(GEOID, -11))
charleston_tracts <- charlestonpop %>%
  left_join(SCTracts, by = "GEOID")

charleston_tracts$Total <- as.numeric(as.character(charleston_tracts$Total))

charleston_tracts_sf <- st_as_sf(charleston_tracts)
charleston_tracts_sf <- st_transform(charleston_tracts_sf, crs = 4326)

#San Diego tract data
# sandiegopop<-read_csv("./data/sandiegopop.csv")
# CATracts <- sf::read_sf("./data/ACS_2020_5YR_TRACT_06_CALIFORNIA.gdb", layer = "ACS_2020_5YR_TRACT_06_CALIFORNIA")
# sandiegopop <- sandiegopop  %>%
#   mutate(GEOID = str_sub(GEOID, -11))
# sandiego_tracts <- sandiegopop %>%
#   left_join(CATracts, by = "GEOID")
# 
# sandiego_tracts$Total <- as.numeric(as.character(sandiego_tracts$Total))
# 
# sandiego_tracts_sf <- st_as_sf(sandiego_tracts)
# sandiego_tracts_sf <- st_transform(sandiego_tracts_sf, crs = 4326)
# 
# sandiego_tracts_export <- sandiego_tracts_sf %>%
#   dplyr::select(GEOID, Total, Shape)
# 
# write_sf(sandiego_tracts_export, "./data/sandiego_tracts.gpkg") #making smaller for github

sandiego_tracts_sf <- sf::read_sf("./data/sandiego_tracts.gpkg") #pulling it back in

sea_level_data <- rbind(charleston, sandiego)

#making year month column and numerical dates
sea_level_data$yearmonth <- ym(paste(sea_level_data$Year, sea_level_data$Month))
charleston$yearmonth <- ym(paste(charleston$Year, charleston$Month))
sandiego$yearmonth <- ym(paste(sandiego$Year, sandiego$Month))


#Better plot
ggplot(sea_level_data, aes(x = yearmonth, y = Monthly_MSL, color = City)) +
  geom_line(size = 0.6) +
  theme_minimal() +
  labs(
    title = "Comparison of Historical Sea Level Rise",
    y = "Sea Level (mm)",
    x = "Date",
    color = "City"
  ) +
  scale_x_date(
    date_breaks = "60 months",      
    date_labels = "%b %Y",         
    expand = c(0.01, 0.01)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#finding linear trend for charleston
charleston$time <- charleston$Year + (charleston$Month - 1)/12

char_model <- lm(Monthly_MSL ~ time, data = charleston)
summary(char_model)
#found that MSL is increasing by 3.51mm per month or 42.12mm per year

#finding linear trend for san diego
sandiego$time <- sandiego$Year + (sandiego$Month - 1)/12

sd_model <- lm(Monthly_MSL ~ time, data = sandiego)
summary(sd_model)
#found that MSL is 2.23mm a month and 26.76mm a year

#Statistical testing to compare the time series
#Finding distributions
ggplot(charleston, aes(x = Monthly_MSL)) +
  geom_histogram(
    fill = "#3182bd",       # nice blue
    color = "white",        # white borders
    bins = 30,              # control number of bins
    alpha = 0.8             # slight transparency
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Monthly Mean Sea Level in Charleston",
    x = "Monthly Mean Sea Level (mm)",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "gray20"),
    panel.grid.minor = element_blank()
  ) #normal!

ggplot(sandiego, aes(x = Monthly_MSL)) +
  geom_histogram(
    fill = "#2ca25f",       # green tone
    color = "white",        # border around bars
    bins = 30,              # you can adjust this
    alpha = 0.8            # transparency
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Monthly Mean Sea Level in San Diego",
    x = "Monthly Mean Sea Level (mm)",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "gray20"),
    panel.grid.minor = element_blank()
  ) #uh... kinda not normal


#a little bit of stats!
t.test(charleston$Monthly_MSL, sandiego$Monthly_MSL)
wilcox.test(charleston$Monthly_MSL, sandiego$Monthly_MSL) #did not reject null

ks.test(charleston$Monthly_MSL, sandiego$Monthly_MSL) #rejected null!



#attemtping time series analysis
char_ts<- ts(charleston$Monthly_MSL, start = c(charleston$Year[1], charleston$Month[1]), frequency = 12)

char_zoo <- zoo(charleston$Monthly_MSL, charleston$yearmonth)

plot(char_ts, main = "Monthly Mean Sea Levels", ylab = "Sea Level (m)", xlab = "Year")
plot(char_zoo, main = "Monthly Mean Sea Levels", ylab = "Sea Level (m)", xlab = "Year")

char_forecast <- forecast(char_ts, h = 900)

plot(char_forecast, main = "Forecasting for until 2100") #uh i dont think that looks right

#find what the sea levels will be in 2100 using linear model
predict(char_model, newdata = data.frame(time = 2100.000)) #about 1 foot

predict(sd_model, newdata = data.frame(time = 2100.000)) #slightly less than 1 foot




#Charleston map stuff
sc_slr <- terra::rast("./data/SC_Central_connectRaster_1.tif") #1 foot of slr
plot(sc_slr) #quick look

sc_slr_crop <- crop(sc_slr, ext(charleston_tracts_sf))
sc_slr_wgs84 <- terra::project(sc_slr_crop, "EPSG:4326", method = "near")
sc_slr_lowres <- terra::aggregate(sc_slr_wgs84, fact = 5, fun = "mean")

#define the color palette for the raster
pal <- colorNumeric(
  palette = "Greens",
  domain = values(sc_slr_lowres),
  na.color = "transparent"
)

#define the color palette for the tracts
tract_pal <- colorQuantile(
  palette = "Blues", 
  domain = charleston_tracts_sf$Total,
  n = 5,
  na.color = "transparent"
)
#charleston leaflet map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(sc_slr_lowres, colors = pal, opacity = 0.6) %>%
  addPolygons(data = charleston_tracts_sf,
              fillColor = ~tract_pal(Total),  
              fillOpacity = 0.7,
              color = "#444444",
              weight = 1) %>%
  addLegend(pal = tract_pal,
            values = charleston_tracts_sf$Total,
            title = "Total Population",
            position = "topright") %>%
  addLegend(pal = pal,
            values = values(sc_slr_lowres),
            title = "1 ft SLR",
            position = "bottomright") %>%
  setView(lng = -79.94, lat = 32.78, zoom = 10)



#san diego map stuff
# ca_slr <- terra::rast("./data/CA_South_connectRaster_1_0.tif")
# ca_slr_lowres <- terra::aggregate(ca_slr, fact = 5, fun = "mean")
# 
# sandiego_slr <- terra::mask(ca_slr_lowres, sandiego_tracts_sf)
# 
# sd_slr_wgs84 <- terra::project(sandiego_slr, "EPSG:4326", method = "near")
# 
# plot(sd_slr_wgs84)
# 
# sd_slr_wgs84_fix <- sd_slr_wgs84 %>%
#   terra::crop(., sandiego_tracts_sf)
# 
# terra::writeRaster(sd_slr_wgs84_fix, "./data/sandiego_slr_1ft.tif", overwrite = TRUE) #saving a smaller version

sd_slr_wgs84_fix <- terra::rast("./data/sandiego_slr_1ft.tif") #pulling it all back in

sd_pal <- colorNumeric(
  palette = "Greens",
  domain = values(sd_slr_wgs84_fix),  
  na.color = "transparent"
)

sdtract_pal <- colorQuantile(palette = "Blues", 
                           domain = sandiego_tracts_sf$Total,
                           n = 5,
                           na.color = "transparent")
#whole san diego map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(sd_slr_wgs84_fix, colors = sd_pal, opacity = 0.6) %>%
  addPolygons(data = sandiego_tracts_sf,
              fillColor = ~sdtract_pal(Total),
              fillOpacity = 0.7,
              color = "#444444",    
              weight = 1) %>%
  addLegend(pal = sdtract_pal,
            values = sandiego_tracts_sf$Total,
            title = "Total Population",
            position = "topright") %>%
  addLegend(pal = sd_pal,
            values = values(sd_slr_wgs84_fix),
            title = "1 ft SLR",
            position = "bottomright") %>%
  setView(lng = -117.16, lat = 32.72, zoom = 10)


