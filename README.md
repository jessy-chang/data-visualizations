# Data Visualizations

## U.S. Main Airline Routes
As of June 2014, the OpenFlights/Airline Route Mapper Route Database contains 67,663 routes between 3,321 airports on 548 airlines spanning the globe. This analysis focuses on the 8 major U.S. airports on 36 airlines.


### Analysis
The **Network Plot** shows the flight routes between the 8 U.S. major airports: 
* Hartsfield-Jackson Atlanta International Airport (ATL) had the most flight routes both from and to other destinations.
* The most common route was between Hartsfield-Jackson Atlanta International Airport (ATL) and O’Hare International Airport (ORD).
* There were more routes from Hartsfield-Jackson Atlanta International Airport (ATL) to John F. Kennedy International Airport (JFK) than JFK to ATL.

<img src="https://github.com/jessy-chang/data-visualizations/blob/main/US_airline_routes/Plot.png" width="600">

```r
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag)
library(data.table)
library(ggplot2)
library(readr)

# Kaggle data source:https://www.kaggle.com/divyanshrai/openflights-route-database-2014?select=routes.csv
data <- data.table(read_csv('routes.csv'))
names(data) <- gsub(" ", "_", names(data))
data <- data[Source_airport %in% c('ATL','LAX','ORD','DFW','DEN','JFK','SFO','SEA')]
data <- data[Destination_airport %in% c('ATL','LAX','ORD','DFW','DEN','JFK','SFO','SEA')]

df <- data.frame(table(data$Source_airport, data$Destination_airport))
names(df) <- c('Source_airport','Destination_airport','Freq')


# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


# color palette
mycolor <- viridis(8, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:8)]


# Base plot
chordDiagram(
  x = df, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.5, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], 20), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE,
      labels.cex = 0.6)
  }
)


```

### Links & Resources 
**Data Source**: https://www.kaggle.com/divyanshrai/openflights-route-database-2014?select=routes.csv   
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/US_airline_routes/Data    
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/US_airline_routes/r_code.R    

<br>
<br>

## U.S. Unemployment Rate
Due to the global pandemic of COVID-19, unemployment rate of the selected states are being evaluated to understand its impact on U.S. economy. Data is collected from the U.S. Bureau of Labor Statistics for March 2019 to March 2020. 


### Analysis
A **Horizon Graph** shows the change of employment rate for State of California, Florida, Illinois, New York, Texas, and Washington: 
* Monthly unemployment rate for these selected states mostly fluctuated within a 1% range.
* California, Florida, and Texas showed a larger increase in unemployment rate in March ‘20 compared to March ‘19.
* California and Illinois had a similar unemployment trend before 2020. However, Illinois seemed to be less impacted by the COVID-19 on its unemployment rate.

<img src="https://github.com/jessy-chang/data-visualizations/blob/main/US_unemployment_rate/image.png" width="600">

```r
library(readxl)
library(data.table)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(dplyr)


# General chart setting for lables
g_settings <- list(
  par.main.text = list(font = 2, # make it bold
                       just = 'left',
                       x = grid::unit(3, "cm")), 
  par.sub.text = list(font = 1, 
                      cex = 0.8,
                      pos = 3, 
                      x = grid::unit(5, "mm"),
                      y = grid::unit(0.5, "mm")))

g_time <- c("Mar'19", "Apr'19", "May'19", "Jun'19", "Jul'19", "Aug'19", "Sep'19", "Oct'19",
            "Nov'19", "Dec'19", "Jan'20", "Feb'20", "Mar'20")

# Same scale 
horizonplot(df_all, horizonscale = 0.5, 
            scales = list(x = list(at = seq(1,13,2), labels = g_time[seq(1,13,2)])),
            colorkey = TRUE, col.regions = hcl.colors(2 * 3, palette="Fall"), 
            layout = c(1, 6), par.settings=g_settings,
            main = 'Change of Unemployment Rate by State',
            sub="Source: U.S Bureau of Labor Statistics",
            ylab = 'Unemployment Rate (%)') +
  layer(lim <- current.panel.limits(),
        panel.text(lim$x[1], lim$y[1], round(lim$y[1],1), font = 2,
                   cex = 0.7, adj = c(-0.5,-0.5), col = "blue")) 
```



### Links & Resources 
**Data Source**: U.S. Bureau of Labor Statistics    
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/US_unemployment_rate/Raw%20Data    
**How to Plot a Horizon Graph:** http://rgraphgallery.blogspot.com/2013/05/rg-horizon-plot-time-series-data.html   
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/US_unemployment_rate/r_Code.R    

<br>
<br>

## Revenue of Apple
The statistic shows the quarterly revenue of Apple, broken down by geographical region. Apple generated 29.32 billion U.S. dollars in revenue in the Americas region in the fourth quarter of the 2019 financial year.

### Analysis
A **Radar Chart** shows the quarterly revenue of Apple in 2019 fiscal year, which started in October 2018. The unit in the chart is in billion U.S. dollars:
* Americas was Apple’s largest revenue generated region, followed by Europe.
* Performance in Q1 appeared to be the best in all regions, likely driven by the holiday season.
* In Americas and Europe, Q4 drove the second highest revenue likely due to major new product release in September.
* In Asian countries, Q2 to Q4 performance showed no apparent differences.

<img src="https://github.com/jessy-chang/data-visualizations/blob/main/apple_revenue/radarchart.png" width="600">

```r
library(readxl)
library(data.table)
library(fmsb)
library(ggplot2)


#################################################################################################
## Radar Chart ##################################################################################
#################################################################################################

# Color vector
colors_border=c(rgb(166/255,97/255,26/255,0.9), rgb(246/255,232/255,195/255,0.9), 
                rgb(128/255,205/255,193/255,0.9), rgb(1/255,133/255,113/255,0.9))
colors_in=c(rgb(166/255,97/255,26/255,0.4), rgb(246/255,232/255,195/255,0.4), 
            rgb(128/255,205/255,193/255,0.4), rgb(1/255,133/255,113/255,0.4))

# plot with default options:
radarchart(df, axistype=1, 
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,10), cglwd=0.8,
           #custom labels
           vlcex=0.8, 
           #add title
           title = '2019 Fiscal Year Apple Revenue by Region & by Quarter'
)


# Add a legend
legend(x=0.8, y=1.1, legend = c("Q1","Q2","Q3","Q4"), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1, pt.cex=3)


```

### Links & Resources 
**Data Source**: https://www.statista.com/statistics/382175/quarterly-revenue-of-apple-by-geograhical-region/   
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/apple_revenue/Data   
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/apple_revenue/r_code.R   

<br>
<br>

## U.S. Foreign-Born Population
Data was downloaded from the United States Census Bureau to understand the distribution of foreign-born population acoss different states. 

### Analysis
Below map shows the percentage of foreign-born population in each state:
* The coastal states had higher percentage of population who were foreign-born.
* California was the state with the highest foreign-born population, followed by New York State.

<img src="https://github.com/jessy-chang/data-visualizations/blob/main/US_foreign_born/Rplot.png" width="800">

```r
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

```
### Links & Resources 
**Census Data Source**: https://data.census.gov/cedsci/table?q=Foreign%20Born&t=Foreign%20Born&hidePreview=false&tid=ACSDP1Y2018.DP02   
**Geojson Data Source**: https://eric.clst.org/tech/usgeojson/   
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/US_foreign_born/Data   
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/US_foreign_born/r_code.R   

<br>
<br>

## Mobile Operating System Market Share
Google's Android is the leader in the mobile operating system market in Africa. In December 2019, it accounted for almost 84 percent of the mobile OS market, with Apple's iOS being the next major competitor, which has gained considerable market share in recent months. All the other operating systems had a market share of about five percent in December 2019.

### Analysis
The **Marimekko Chart** shows 2019 mobile operating system market share by region:
* Asia was the largest market, with over 50% of the total market share.
* Android had largest market share in most regions except for North America.
* Android and iOS combined took more than 98% of the market share in most regions except for Africa.

<img src="https://github.com/jessy-chang/data-visualizations/blob/main/mobile_os/Plot.png" width="600">

```r
library(data.table)
library(ggplot2)
library(dplyr)
library(latticeExtra)

#################################################################################################
## Marimekko/Mosaic Chart #######################################################################
#################################################################################################
# Append all data
df <- rbindlist(list(asia, europe, n_america, africa))
df[, (c('Android','iOS','Other')) := lapply(.SD, function(x){round(x, 2)}), .SDcols=c('Android','iOS','Other') ]

df[, segpct := c(59,17,12,12)]
df[, xmax := cumsum(df$segpct)]
df[, xmin := df$xmax - df$segpct]
df[, segpct := NULL]

dfm <- melt(df, id = c("Region", "xmin", "xmax"))

# Calculate ymin and ymax
dfm1 <- ddply(dfm, .(Region), transform, ymax = cumsum(value))
dfm1 <- ddply(dfm1, .(Region), transform, ymin = ymax - value)

# Position of text
dfm1$xtext <- with(dfm1, round(xmin + (xmax - xmin)/2, 2))
dfm1$ytext <- with(dfm1, round(ymin + (ymax - ymin)/2, 2))

# Plot the chart
ggplot(dfm1, aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax, fill = variable)) +
  geom_rect(colour = I("grey")) +
  geom_text(aes(x = xtext, y = ytext,
                label = ifelse(Region == "Asia", paste(variable," - ", value, "%", sep = ""), 
                               paste(value, "%", sep = ""))), size = 3) +
  geom_text(aes(x = xtext, y = 104, label = Region), size = 3) +
  theme_bw() + 
  labs(x = NULL, y = NULL, fill = NULL) + 
  ggtitle('Mobile Operating System Market Share') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "none", plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  scale_fill_manual(values=c("#b2df8a", "#1f78b4", "#a6cee3")) 


```

### Links & Resources 
**Data Source**: https://www.statista.com/statistics/1045247/share-of-mobile-operating-systems-in-africa-by-month/   
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/mobile_os/Data   
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/mobile_os/r_code.R   


## License 
I am providing code and resources in this repository to you under an open source license. Because this is my personal repository, the license you receive to my code and resources is from me and not my employer.
```
MIT License

Copyright (c) 2022 Jessy Chang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
