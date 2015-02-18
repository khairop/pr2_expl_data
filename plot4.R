library(plyr)

# load data into R
nei = readRDS("./data/summarySCC_PM25.rds")
scc = readRDS("./data/Source_Classification_Code.rds")

# task 1 Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008? Using the base plotting system, make a plot showing
# the total PM2.5 emission from all sources for each of the years 1999, 
# 2002, 2005, and 2008.

#find all the relevant unique coal paramnerers in scc, in total 3 rows
coalarr <- grep("Coal", unique(scc$EI.Sector), value = T)

# logical array to find all the coal related values for EI sector, in total 99 rows are TRUE
sccsplit <- scc$EI.Sector %in% coalarr

# subsetting the data frame with the coal related values only
scccoal = subset(scc, sccsplit == T)$SCC

#filter out the emmisions for the coal sources, in total 28480 rows
neisplit <- nei$SCC %in% scccoal
neicoal <- subset(nei, neisplit == T)

# calculate total pm2.5 emissions for each year using ddply function
#the . in ddply is used for direct column naming
coalsum <- ddply(neicoal, .(year), summarise, sumem = sum(Emissions))

# creating plots
png("plot4.png", width = 480, height = 480)

plot(coalsum$year, coalsum$sumem, xlab="Year", ylab = "Total PM2.5 Emissions", pch = 19, col = "blue")
title(main = "Total PM2.5 Emissions Per Year from coal combustion sources")

dev.off()