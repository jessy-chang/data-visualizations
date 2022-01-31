library(readxl)
library(data.table)
library(fmsb)
library(ggplot2)

#################################################################################################
## Data Processing ##############################################################################
#################################################################################################
# Read excel file
file <- 'Data/statistic_id382175_apples-revenue-broken-down-by-geographical-region-2012-2019-by-quarter.xlsx'
raw_df <- read_excel(file, sheet = 'Data', skip = 4)
names(raw_df)[1] <- 'quarter_year'

# Only take 2019 data
df <- raw_df[which(raw_df$quarter_year %in% c("1Q '19", "2Q '19", "3Q '19", "4Q '19")),]
df <- df[,-1]
df <- df[, c(2, 1, 3, 4, 5)]
rownames(df) <- c('Q1','Q2','Q3','Q4')

# To use the fmsb package, add 2 lines to the dataframe: the max and min of each variable to show on the plot
df <- rbind(rep(40,5) , rep(0,5) , df)

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



