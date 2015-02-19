library(plyr)
library(ggplot2)

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

# subsetting nei dataset extracting values for baltimore city, 2096 rows
neibalt=subset(nei,nei$fips=="24510")

# subsetting nei dataset extracting values for la city, 9320 rows
neila=subset(nei,nei$fips=="06037")

#filter out the emmisions for the on-road sources for Baltimore county
neisplit = neibalt$SCC %in% sccmotor
neimotor = subset(nei, neisplit == T)   
motorsumbalt = ddply(neimotor, .(year), summarise, sumem = sum(Emissions) / 1000)

#filter out the emmisions for the on-road sources for Los Angeles county
neisplit = neila$SCC %in% sccmotor
neimotor = subset(nei, neisplit == T)  
motorsumla = ddply(neimotor, .(year), summarise, sumem = sum(Emissions) / 1000)

#Binding the two data frame, using a new county variable
motorsumbalt$county = "Baltimore"
motorsumla$county  = "Los Angeles"
motorbind = rbind(motorsumla, motorsumbalt)

# creating plots using dev.copy function (Windows environment)



print(qplot(year, sumem, data = motorbind, color = county, geom = c("point","line")) + 
geom_point(size = 4)+labs(y = "Total PM2.5 Emissions (in thousands)", title = "Motor Vehicle Related PM2.5 Emissions Per Year in Baltimore and Los Angeles County"))

dev.copy(png,"./plot6.png", width = 800, height = 600)
dev.off()

