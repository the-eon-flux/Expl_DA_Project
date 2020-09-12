
'
Overall goal : The overall goal of this assignment is to explore the National Emissions Inventory 
database. The data that used for this assignment are for 1999, 2002, 2005, and 2008.

Prepare single plots for each question answered.
'

## Reading Data
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        head(NEI)
        head(SCC)
        
        str(NEI)
        str(SCC)        

## Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 
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
        with(NEI, boxplot(log10(Emissions) ~ year, ylab = "Log 10 Emissions (Tonnes)", xlab = "Year"))
        
        # According to the boxplot the median has decreased in the following years 
        # after 1999. Also the 75th percentile of the rest of the 3 years is almost
        # close to median of 1999 emission.
        
## Q2.  Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
 # (fips == "24510") from 1999 to 2008? 
 # Use the base plotting system to make a plot answering this question.
        
        Emissions_Baltimore <- subset(NEI, fips == "24510", Emissions)
        Dates_Baltimore <- subset(NEI, fips == "24510", year)
        
        View(data.frame(table(NEI$fips)["24510"]))
                # Gives 2096. Thus we have retireved all the records from Baltimore
        
        year(Dates_Baltimore)[[1]]
        plot( years(Dates_Baltimore$year), log10(Emissions_Baltimore$Emissions), 
                xlab = "Year", ylab = "Log 10 Emission (tonnes)", main = "Baltimore pm25 emission chart" )
        
 # Ans : pm25 Emissions seem to have decreased according to the plot
        
## Q3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
 # which of these four sources have seen decreases in emissions from 1999-2008 
 # for Baltimore City? Which have seen increases in emissions from 1999-2008? 
 # Use the ggplot2 plotting system to make a plot answer this question.

        Sub_NEI_Type <- subset(NEI, fips == "24510", type)
        Sub_NEI_Type$type <- as.factor(Sub_NEI_Type$type)
        str(Sub_NEI_Type)
        
        TempDf <- data.frame("Emissions" = Emissions_Baltimore, "Year" = Dates_Baltimore, "Type" = Sub_NEI_Type$type)
        head(TempDf)
        
        qplot(year, log10(Emissions), data = TempDf, geom = "boxplot", color = Type, facets = .~Type)
        
 # Answer : According to the plots 3 types (Non-road, On-road & Point) have clearly
 # decreased pm25 emissions. In Non-point however there seems to be no change. Median 
 # appears to be the same in  1999 & 2008. Upperbounds seem have reduced above median in following years.

## Q4. Across the United States, how have emissions from coal combustion-related 
 # sources changed from 1999-2008?

        head(SCC)
        
        # Combustion sources
        Lv1 <- unique(SCC$SCC.Level.One)
        
        Combustion_Indices <- sapply(Lv1, function(str){
                sapply(strsplit(as.character(str), " "), function(SubStr) {
                        if(sum(c( "Combustion", "combustion", "COMBUSTION") %in% SubStr)){return(TRUE)}else{
                                return(FALSE)}  } )
        })
        
        Lv1[Combustion_Indices]
        
        # Using the above Level 1 sources of combustion to find coal related combustion
        
        Sub_SCC <- subset(SCC, SCC.Level.One %in% Lv1[Combustion_Indices], c(1, 7, 8, 10 ))
        
        # Looking for Coal in Level 4 Column of combustion sources
        Lv4 <- as.list(unique(Sub_SCC$SCC.Level.Four))
        
        Coal_Indices <- sapply(Lv4, function(str){ 
                sapply(strsplit(as.character(str), " "), function(SubStr) {
                        if(sum(c( "Coal", "coal", "COAL") %in% SubStr)){return(TRUE)}else{
                                return(FALSE)}  } )
                })
        
        sum(Coal_Indices)
        
        # Getting the SCC id for Coal combustion related sources
        Lv4_sub <- unlist(Lv4[Coal_Indices])
        
        Coal_Combustion_SCC <- subset(Sub_SCC, SCC.Level.Four %in% Lv4_sub, c(1,2,4))
        Mode_Fuel <- paste(Coal_Combustion_SCC$SCC.Level.One, Coal_Combustion_SCC$SCC.Level.Four, sep = "_")
        Coal_Combustion_SCC$Mode <- Mode_Fuel
        # So now we have SCC ids for combustion of coal related pollution sources in this data frame
        
        # Merging with NEI df
        
        Coal_Combustion_NEI <- subset(NEI, SCC %in% Coal_Combustion_SCC$SCC, c(2,4,6))
        
        MergedDf <- merge(Coal_Combustion_NEI, Coal_Combustion_SCC, by = "SCC")
        
        View(head(MergedDf))
                
        dim(MergedDf)
                # 141   6
        
        
        qplot(year, log10(Emissions), data=MergedDf, geom= "boxplot", color = Mode, facets = .~Mode)
 # Answer : Emissions due to coal combustion have reduced in all the sources.
        
        
## Q5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
                
        head(SCC)
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
        
        # Merging with NEI df
        
        Vehicle_NEI <- subset(NEI, SCC %in% Vehicle_SCC$SCC, c(2,4,6) )
        MergedDf <- merge(Vehicle_NEI, Vehicle_SCC, by = "SCC")
        
        qplot(year, log10(Emissions), data = MergedDf, geom = "boxplot", color = year, facets = .~SCC.Level.Two)
 
 # Answer : Almost all SCC's under 5 types of vehicle category have reduced the emission of pm2.5
        
        
## Q6. Compare emissions from motor vehicle sources in Baltimore City with emissions
 # from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
 # Which city has seen greater changes over time in motor vehicle emissions?        
        
        
        head(SCC)
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
        
        # Merging with NEI df
        
        Vehicle_City_NEI <- subset(NEI, fips %in% c("24510", "06037") & SCC %in% Vehicle_SCC$SCC  , c(1,2,4,6) )
        MergedDf <- merge(Vehicle_City_NEI, Vehicle_SCC, by = "SCC")

        City <- sapply(MergedDf$fips, function(f){ if(f == "06037"){ return("Los Angeles")}else(return("Baltimore"))})
        MergedDf$City <- City
        
        dim(MergedDf)
                # 2717  6        
        
        head(MergedDf)
        
        qplot(year, log10(Emissions), data = MergedDf, geom = "boxplot", color= year, facets =  .~City )        

        # We can see that Baltimore has seen reduction in motor vehicle emissions over time.
        # even though LA has also reduced but not to extent as Baltimore. 
        
        
        