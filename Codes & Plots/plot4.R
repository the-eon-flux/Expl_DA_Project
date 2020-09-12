
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

Coal_Combustion_NEI <- subset(NEI, SCC %in% Coal_Combustion_SCC$SCC, c(2,4,6))

# Merging with NEI df
MergedDf <- merge(Coal_Combustion_NEI, Coal_Combustion_SCC, by = "SCC")

View(head(MergedDf))

dim(MergedDf)
# 141   6

# Plotting
png("Plot4.png", width = 840, height = 440)
qplot(year, log10(Emissions), data=MergedDf, geom= "boxplot", color = Mode, facets = .~Mode)
dev.off()

 # Answer : Emissions due to coal combustion have reduced in all the sources.
