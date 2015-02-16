library(plyr)

# load data into R
nei = readRDS("./data/summarySCC_PM25.rds")
scc = readRDS("./data/Source_Classification_Code.rds")

# task 1 Have total emissions from PM2.5 decreased in the United States
# from 1999 to 2008? Using the base plotting system, make a plot showing
# the total PM2.5 emission from all sources for each of the years 1999, 
# 2002, 2005, and 2008.

#filter out the baltimore City, Maryland
nei=subset(nei,nei$fips=="24510")

# calculate total pm2.5 emissions for each year using ddply function
#the . in ddply is used for direct column naming
neisum <- ddply(nei, .(type, year), summarise, sumem = sum(Emissions))

# creating plots
#png("plot3.png", width = 800, height = 600)

print(qplot(year, sumem, data=neisum,facets=.~type) + 
    geom_point(aes(color = type), size = 4) + labs(y = "Total PM2.5 Emissions", title = "Sources of PM2.5 Emissions Per Year in Baltimore"))

dev.copy(png,"./plot3.png",width=800,height=600)
dev.off()