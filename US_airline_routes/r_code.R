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




