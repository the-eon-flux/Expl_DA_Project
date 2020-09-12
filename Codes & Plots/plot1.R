
'
Overall goal : The overall goal of this assignment is to explore the National Emissions Inventory 
database. The data that used for this assignment are for 1999, 2002, 2005, and 2008.

Prepare single plots for each question answered.
'

## Reading Data
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")

        # Getting idea of datasets
        str(NEI)
        str(SCC)   

## Q1. : Have total emissions from PM2.5 decreased in the United States from 1999 to 
 # 2008? Using the base plotting system, make a plot showing the total PM2.5 emission 
 # from all sources for each of the years 1999, 2002, 2005, and 2008.
        
        NEI$year <- as.factor(as.Date(as.character(NEI$year), "%Y"))
        str(NEI)        
        levels(NEI$year)
        
        Sub_NEI_Em <- NEI$Emissions
        Sub_NEI_Dates <- NEI$year
        
        summary(sort(Sub_NEI_Em))
        
        sum(is.na(Sub_NEI_Em))
        # No missing values
        
        # Negative vals
        neg <- Sub_NEI_Em < 0.0
        sum(neg)
        # no negatives
        
        Count_Zero <- sum(Sub_NEI_Em == 0.0)
        Percent_Zero <- mean((Sub_NEI_Em == 0.0))
        # 5% values are 0. I guess for this question they don't matter.
        
# Plotting total emission for respective years
png("Plot1.png", width = 1020, height = 840)

with(NEI, boxplot(log10(Emissions) ~ year, ylab = "Log 10 of Emissions (Tonnes)", xlab = "Year"))

dev.off()

## Answer : According to the boxplot the median has decreased in the following years 
# after 1999. Also the 75th percentile of the rest of the 3 years is almost
# close to median of 1999 emission.
