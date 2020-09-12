
'
Overall goal : The overall goal of this assignment is to explore the National Emissions Inventory 
database. The data that used for this assignment are for 1999, 2002, 2005, and 2008.

Prepare single plots for each question answered.
'

## Reading Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
NEI$year <- as.factor(as.Date(as.character(NEI$year), "%Y"))

# Getting idea of datasets
str(NEI)
str(SCC)   

## Q2.  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
 # (fips == "24510") from 1999 to 2008? 
 # Use the base plotting system to make a plot answering this question.

# Creating vars for emissions and date for Baltimore city
Emissions_Baltimore <- subset(NEI, fips == "24510", Emissions)
Dates_Baltimore <- subset(NEI, fips == "24510", year)

dim(Dates_Baltimore)
        # 2096 1
data.frame(table(NEI$fips)["24510"])
        # Gives 2096. Thus we have retireved all the records from Baltimore

# Plotting 
png("Plot2.png", width = 1040, height = 840)

plot( Dates_Baltimore$year, log10(Emissions_Baltimore$Emissions), 
        xlab = "Year", ylab = "Log 10 of Emission (tonnes)", main = "Baltimore pm2.5 emission chart" )
dev.off()

# Ans : pm25 Emissions seem to have decreased according to the plot
