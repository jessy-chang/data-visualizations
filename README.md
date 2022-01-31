# Data Visualizations

## U.S. Unemployment Rate
Due to the global pandemic of COVID-19, unemployment rate of the selected states are being evaluated to understand its impact on U.S. economy. Data is collected from the U.S. Bureau of Labor Statistics for March 2019 to March 2020. 


### Analysis
A **Horizon Graph** shows the change of employment rate for State of California, Florida, Illinois, New York, Texas, and Washington: 
* Monthly unemployment rate for these selected states mostly fluctuated within a 1% range.
* California, Florida, and Texas showed a larger increase in unemployment rate in March ‘20 compared to March ‘19.
* California and Illinois had a similar unemployment trend before 2020. However, Illinois seemed to be less impacted by the COVID-19 on its unemployment rate.


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

<img src="https://github.com/jessy-chang/data-visualizations/blob/main/US_unemployment_rate/image.png" width="600">


### Links & Resources 
**Data Source**: U.S. Bureau of Labor Statistics    
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/US_unemployment_rate/Raw%20Data    
**How to Plot a Horizon Graph:** http://rgraphgallery.blogspot.com/2013/05/rg-horizon-plot-time-series-data.html   
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/US_unemployment_rate/r_Code.R    

<br>
<br>

## Revenue of Apple
Due to the global pandemic of COVID-19, unemployment rate of the selected states are being evaluated to understand its impact on U.S. economy. Data is collected from the U.S. Bureau of Labor Statistics for March 2019 to March 2020. 


### Analysis
A **Radar Chart** shows the quarterly revenue of Apple in 2019 fiscal year, which started in October 2018. The unit in the chart is in billion U.S. dollars:
* Americas was Apple’s largest revenue generated region, followed by Europe.
* Performance in Q1 appeared to be the best in all regions, likely driven by the holiday season.
* In Americas and Europe, Q4 drove the second highest revenue likely due to major new product release in September.
* In Asian countries, Q2 to Q4 performance showed no apparent differences.

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
<img src="https://github.com/jessy-chang/data-visualizations/blob/main/apple_revenue/radarchart.png" width="800">

### Links & Resources 
**Data Source**: https://www.statista.com/statistics/382175/quarterly-revenue-of-apple-by-geograhical-region/
**Downloaded Datasets**: https://github.com/jessy-chang/data-visualizations/tree/main/apple_revenue/Data
**Full R Code:** https://github.com/jessy-chang/data-visualizations/blob/main/apple_revenue/r_code.R
