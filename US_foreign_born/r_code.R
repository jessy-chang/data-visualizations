library(geojsonio)
library(broom)
library(data.table)
library(ggplot2)
library(sp)

# Geojson data source: https://eric.clst.org/tech/usgeojson/
spdf <- geojson_read("Data/gz_2010_us_040_00_500k.json",  what = "sp")

# Remove Alaska, Puerto Rico, Hawaii
spdf <- spdf[ !(spdf@data$NAME  %in% c("Alaska","Puerto Rico","Hawaii")), ]
spdf_fortified <- tidy(spdf, region = "STATE")

# Foreign Born data source: https://data.census.gov/cedsci/table?q=Foreign%20Born&t=Foreign%20Born&hidePreview=false&tid=ACSDP1Y2018.DP02
df <- fread("Data/ACSDP1Y2018.DP02_data_with_overlays_2020-05-31T172915.csv",
            select = c("GEO_ID", "NAME", "DP02_0092PE"))
df <- df[-1, ]
df[, DP02_0092PE := as.numeric(DP02_0092PE)]
setnames(df, old = 'DP02_0092PE', new = 'FOREIGN_BORN_PCT')
df[, STATE.abbr := state.abb[match(NAME, state.name)]]
df[is.na(STATE.abbr), STATE.abbr := 'DC']
df[NAME.x == 'Virginia' , STATE.abbr := 'VA']

# Merge numeric data with geocode data
df <- merge(spdf@data, df, by = "GEO_ID")
spdf_fortified <- merge(spdf_fortified, df, by.x = 'id', by.y = 'STATE')
spdf_fortified <- data.frame(spdf_fortified)

# Create state labels
centroids.df <- as.data.frame(coordinates(spdf))
names(centroids.df) <- c("Longitude", "Latitude")
pop.df <- data.frame(id = spdf$STATE, centroids.df)
pop.df <- merge(pop.df, df[, c('STATE','STATE.abbr')], by.x = 'id', by.y = 'STATE')


# Plot Choropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = FOREIGN_BORN_PCT, x = long, y = lat, group = group)) +
  geom_text(data=pop.df[!(pop.df$STATE.abbr %in% 'DC'),], aes(Longitude, Latitude, label = STATE.abbr), size=2) +
  scale_fill_gradient(high = "#4A235A", low = "#F4ECF7", guide = "colorbar") +
  labs(fill = "Foreign Born (%)") +
  theme_void() +
  theme(legend.title=element_text(size=8)) +
  coord_map()


