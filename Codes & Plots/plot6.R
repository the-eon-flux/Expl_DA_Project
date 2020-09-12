
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

## Q6. Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?        


# Figuring out which column of SCC has motor vehicle emission info
head(SCC)

# Level 2 seems to have something related to vehicle
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
Vehicle_City_NEI <- subset(NEI, fips %in% c("24510", "06037") & SCC %in% Vehicle_SCC$SCC  , c(1,2,4,6) )

# Merging with NEI df
MergedDf <- merge(Vehicle_City_NEI, Vehicle_SCC, by = "SCC")

City <- sapply(MergedDf$fips, function(f){ if(f == "06037"){ return("Los Angeles")}else(return("Baltimore"))})
MergedDf$City <- City

dim(MergedDf)
        # 2717  6        

# Plotting
png("Plot6.png", width = 840, height = 440)

qplot(year, log10(Emissions), data = MergedDf, geom = "boxplot", color= year, facets =  .~City )        

dev.off()


## Answer : We can see that Baltimore has seen reduction in motor vehicle emissions over time.
 # even though LA has also reduced but not to extent as Baltimore. 


