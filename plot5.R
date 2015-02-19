library(plyr)

# load data into R
nei = readRDS("./data/summarySCC_PM25.rds")
scc = readRDS("./data/Source_Classification_Code.rds")

# task 1 Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008? Using the base plotting system, make a plot showing
# the total PM2.5 emission from all sources for each of the years 1999, 
# 2002, 2005, and 2008.

#find all the relevant unique motor related  paramnerers in scc, in total 2 rows
motorarr <- grep("On-Road", unique(scc$EI.Sector), value = T)

# logical array to find all the coal related values for EI sector, in total 1138 rows are TRUE
sccsplit <- scc$EI.Sector %in% motorarr

# subsetting the data frame with the coal related values only
sccmotor = subset(scc, sccsplit == T)$SCC

# subsetting nei dataset extracting values for baltimore county, 2096 rows
neibalt=subset(nei,nei$fips=="24510")

#filter out the emmisions for the on-road sources, in total 1119 rows
neisplit = neibalt$SCC %in% sccmotor
neimotor = subset(nei, neisplit == T)   #3468923 rows

# calculate total pm2.5 emissions for each year using ddply function
#the . in ddply is used for direct column naming
motorsum = ddply(neimotor, .(year), summarise, sumem = sum(Emissions))

# creating plots
png("plot5.png", width = 480, height = 480)

plot(motorsum$year, motorsum$sumem, xlab="Year", ylab = "Total PM2.5 Emissions", pch = 19, col = "blue")
title(main = "Total PM2.5 Emissions Per Year from On-Road sources in Baltimore")

dev.off()