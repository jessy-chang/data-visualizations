library(readxl)
library(data.table)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(dplyr)

####################################
## Data Preprocessing ##############
####################################

# User-defined fucntion to oerform data manipulation
clean_df <- function(file, State){
  df <- data.table(read_excel(file, skip = 11))
  df2 <- data.table()
  for (i in colnames(df)){
    row <- data.table(df[[i]])
    row[, Month := i]
    row[, Year := df[, 'Year']]
    df2 <- rbind(df2, row)
  }
  df2[, State := State]
  df2 <- df2[Month != 'Year']
  df2 <- na.omit(df2)
  df2 <- df2[order(Year)]
  df2 <- df2[nrow(df2) - (12:0)] ##only take data from Mar, 2019
  df2
}

# File directory
# Original Data Source: https://www.bls.gov/data/#unemployment
file1 <- 'Raw Data/SeriesReport-CA.xlsx'
file2 <- 'Raw Data/SeriesReport-FL.xlsx'
file3 <- 'Raw Data/SeriesReport-IL.xlsx'
file4 <- 'Raw Data/SeriesReport-NY.xlsx'
file5 <- 'Raw Data/SeriesReport-TX.xlsx'
file6 <- 'Raw Data/SeriesReport-WA.xlsx'

# Clean Data 
df1 <- clean_df(file1, 'CA')
df2 <- clean_df(file2, 'FL')
df3 <- clean_df(file3, 'IL')
df4 <- clean_df(file4, 'NY')
df5 <- clean_df(file5, 'TX')
df6 <- clean_df(file6, 'WA')


#################################################################################################
## Graph used in final deliverable ##############################################################
#################################################################################################
# Append all dataframe to one 
df_all <- ts(data.frame(df1[,V1],df2[,V1], df3[,V1], df4[,V1], df5[,V1], df6[,V1]), 
             names = sapply(list(df1,df2,df3,df4,df5,df6), function(x){unique(x[,State])}))


# Plot the horizon graph
# source: http://rgraphgallery.blogspot.com/2013/05/rg-horizon-plot-time-series-data.html

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








#################################################################################################
## Trying different Graphs ######################################################################
#################################################################################################

####################################
## Area Chart ######################
####################################
# Append all dataframe to one           
df_all <- rbindlist(list(df1, df2, df3, df4, df5, df6))


df_all[, Time := rep(1:13, 6)]
ggplot(df_all, aes(x=Time, y=V1, fill=State)) +
  geom_area()


####################################
## Horizon Graph ###################
####################################
# Append all dataframe to one 
df_all <- ts(data.frame(df1[,V1],df2[,V1], df3[,V1], df4[,V1], df5[,V1], df6[,V1]), 
             names = sapply(list(df1,df2,df3,df4,df5,df6), function(x){unique(x[,State])}))


# Plot the horizon graph
# source: http://rgraphgallery.blogspot.com/2013/05/rg-horizon-plot-time-series-data.html

# Same scale 
horizonplot(df_all, horizonscale = 0.5, colorkey = TRUE, 
            col.regions = hcl.colors(2 * 3, palette="Fall"), 
            layout = c(1, 6), par.settings=g_settings,
            main = 'Change of Unemployment Rate by State',
            sub="Source: U.S Bureau of Labor Statistics",
            ylab = 'Unemployment Rate (%)') +
  layer(lim <- current.panel.limits(),
        panel.text(lim$x[1], lim$y[1], round(lim$y[1],1), font = 2,
                   cex = 0.7, adj = c(-0.5,-0.5), col = "blue")) 


# Different scale
horizonplot(df_all, colorkey = TRUE, 
            col.regions = hcl.colors(2 * 3, palette="Fall"), 
            layout = c(1, 6), par.settings=my.settings,
            main = 'Change of Unemployment Rate by State',
            sub="Source: U.S Bureau of Labor Statistics",
            ylab = 'Unemployment Rate (%)') +
  layer(panel.scaleArrow(x = 0.99, digits = 1, col = "grey", srt = 90, cex = 0.7)) +
  layer(lim <- current.panel.limits(),
        panel.text(lim$x[1], lim$y[1], round(lim$y[1],1), font = 2,
                   cex = 0.7, adj = c(-0.5,-0.5), col = "blue")) 



####################################
## Line Graph ######################
####################################
# Append all dataframe to one 
df_all <- ts(data.frame(df1[,V1],df2[,V1], df3[,V1], df4[,V1], df5[,V1], df6[,V1]), 
             names = sapply(list(df1,df2,df3,df4,df5,df6), function(x){unique(x[,State])}))

# Same scale
xyplot(df_all, scales = list(y = "same"), strip = FALSE, layout = c(1, 6))

# Different scale
xyplot(df_all, strip = FALSE, layout = c(1, 6))


