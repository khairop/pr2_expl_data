library(plyr)

# load data into R
nei = readRDS("./data/summarySCC_PM25.rds")
scc = readRDS("./data/Source_Classification_Code.rds")

# task 1 Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008? Using the base plotting system, make a plot showing
# the total PM2.5 emission from all sources for each of the years 1999, 
# 2002, 2005, and 2008.

# calculate total pm2.5 emissions for each year using ddply function
#the . in ddply is used for direct column naming
neisum <- ddply(nei, .(year), summarise, sumem = sum(Emissions))

# creating plots
png("plot1.png", width = 480, height = 480)

plot(neisum$year, neisum$sumem / 1000000 , xlab="Year", ylab = "Total PM2.5 Emissions (in millions)", pch = 19, col = "blue")

title(main = "Total PM2.5 Emissions Per Year")

dev.off()