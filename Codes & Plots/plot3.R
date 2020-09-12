
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


## Q3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 
# for Baltimore City? Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

# Creating vars for emissions and date for Baltimore city
Emissions_Baltimore <- subset(NEI, fips == "24510", Emissions)
Dates_Baltimore <- subset(NEI, fips == "24510", year)
Sub_NEI_Type <- subset(NEI, fips == "24510", type)

Sub_NEI_Type$type <- as.factor(Sub_NEI_Type$type)
str(Sub_NEI_Type)

TempDf <- data.frame("Emissions" = Emissions_Baltimore, "Year" = Dates_Baltimore, "Type" = Sub_NEI_Type$type)
head(TempDf)

# Plotting

png("Plot3.png", width = 1000, height = 440)
qplot(year, log10(Emissions), data = TempDf, geom = "boxplot", color = Type, facets = .~Type)
dev.off()

# Answer : According to the plots 3 types (Non-road, On-road & Point) have clearly
# decreased pm25 emissions. In Non-point however there seems to be no change. Median 
# appears to be the same in  1999 & 2008. Upperbounds seem have reduced above median in following years.

