
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

## Q5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

Lv2 <- unique(SCC$SCC.Level.Two)

Vehicle_Indices <- sapply(Lv2, function(str){
        sapply(strsplit(as.character(str), " "), function(SubStr) {
                if(sum(c( "Vehicles", "Vehicle", "vehicle", "VEHICLES", "vehicles") %in% SubStr)){return(TRUE)}else{
                        return(FALSE)}  } )
})

sum(Vehicle_Indices) # 5 levels at SCC.Level.Two with Vehicles

Lv2[Vehicle_Indices]
# Use these for subsetting SCC df and getting respective SCC ids.

Vehicle_SCC <- subset(SCC, SCC.Level.Two %in% Lv2[Vehicle_Indices], c(1,8))
Vehicle_NEI <- subset(NEI, SCC %in% Vehicle_SCC$SCC, c(2,4,6) )

# Merging with NEI df
MergedDf <- merge(Vehicle_NEI, Vehicle_SCC, by = "SCC")

# Plotting
png("Plot5.png", width = 1240, height = 440)
qplot(year, log10(Emissions), data = MergedDf, geom = "boxplot", color = year, facets = .~SCC.Level.Two)
dev.off()


# Answer : Almost all SCC's under 5 types of vehicle category have reduced the emission of pm2.5
