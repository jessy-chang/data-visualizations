library(readxl)
library(data.table)
library(ggplot2)
library(dplyr)
library(latticeExtra)

#################################################################################################
## Data Processing ##############################################################################
#################################################################################################

# Read excel file
file1 <- 'Data/North_America.xlsx'
file2 <- 'Data/Africa.xlsx'
file3 <- 'Data/Asia.xlsx'
file4 <- 'Data/Europe.xlsx'
file5 <- 'Data/Revenue.xlsx'

raw_df1 <- data.table(read_excel(file1, sheet = 'Data', skip = 4))
raw_df2 <- data.table(read_excel(file2, sheet = 'Data', skip = 4))
raw_df3 <- data.table(read_excel(file3, sheet = 'Data', skip = 4))
raw_df4 <- data.table(read_excel(file4, sheet = 'Data', skip = 4))
raw_df5 <- data.table(read_excel(file5, sheet = 'Data', skip = 4))

# User-defined function to clean data
data_append <- function(df, region){
  x <- copy(df)[, c(ncol(df)) := NULL]
  names(x)[1] <- 'date'
  num_cols <- names(x)[!(names(x) %in% 'date')]
  x[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  x <- x[c(nrow(x)),]
  x[, Other := 100-(Android+iOS)]
  # x[, Other := rowSums(x[, -c('date','Android','iOS')], na.rm = TRUE)]
  x[, Region := region]
  x <- x[, c('Android','iOS','Other','Region')]
  x
}

# Clean the data using user-defined function
n_america <- data_append(raw_df1, 'North America')
africa <- data_append(raw_df2, 'Africa')
asia <- data_append(raw_df3, 'Asia')
europe <- data_append(raw_df4, 'Europe')


# Calculate revenue by region in 2019
rev <- data.frame(t(raw_df5))
names(rev) <- c('2018','2019','2020')
rev <- rev[-1,]
rev$region <- rownames(rev)
rev <- data.table(rev)
rev[region %in% c('Greater China','Emerging Asia/Pacific','Mature Asia/Pacific','Japan'), region2 := 'Asia']
rev[region %in% c('Western Europe','Eurasia','Eastern Europe'), region2 := 'Europe']
rev[region %in% c('Middle East and North Africa','Sub-Saharan Africa'), region2 := 'Africa']
rev[region %in% c('North America'), region2 := 'North America']

rev[, (c('2018','2019','2020')) := lapply(.SD, as.character), .SDcols = c('2018','2019','2020')]
rev[, (c('2018','2019','2020')) := lapply(.SD, as.numeric), .SDcols = c('2018','2019','2020')]
rev <- rev[, lapply(.SD, sum), .SDcols = '2019', by = region2]
rev <- rev[!is.na(region2)]
rev[, pct := `2019`/sum(`2019`)]
rev[order(pct, decreasing = TRUE)]

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



