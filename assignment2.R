# Module 4 - Assignment 2

setwd("C:/Users/hklau/Desktop/DataScienceClass/Module 4 - Exploratory Data Analysis/Assignment 2/")

filenamePM25 <- "summarySCC_PM25.rds"
filenameSCC <- "Source_Classification_Code.rds"

# Download data set
# This might take a while to download. It is prefered to download offline
downloadData <- function() {
    fileDownload <- "exdata-data-NEI_data.zip"
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    
    if (!file.exists(fileDownload)) {
        download.file(fileURL, destfile=(fileDownload))
        list.files(".")    
    }
    
    unzip(fileDownload)    
}

getData <- function() {
    # if file not exist, download from url
    if (!file.exists(filenamePM25) || !file.exists(filenameSCC)) {
        downloadData()
    }
    
    # if exist, prevent to read again for plot2.R, plot3.R and etc... due to long time
    if (!exists("NEI")) {
        NEI <<- readRDS(filenamePM25)    
    }
    
    # if exist, prevent to read again for plot2.R, plot3.R and etc... due to long time
    if (!exists("SCC")) { 
        SCC <<- readRDS(filenameSCC)
    }
}

summaryData <- function() {
    data_1999 <- NEI[NEI$year == "1999", ]
    data_2002 <- NEI[NEI$year == "2002", ]
    data_2005 <- NEI[NEI$year == "2005", ]
    data_2008 <- NEI[NEI$year == "2008", ]
    
    print(summary(data_1999))
    print(summary(data_2002))
    print(summary(data_2005))
    print(summary(data_2008))
    
    message("1999 (SUM): ", sum(data_1999$Emissions))
    message("2002 (SUM): ", sum(data_2002$Emissions))
    message("2005 (SUM): ", sum(data_2005$Emissions))
    message("2008 (SUM): ", sum(data_2008$Emissions))
}

getPlot1Data <- function() {
    
    # 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
    #    Using the base plotting system, make a plot showing the total PM2.5 emission from 
    #    all sources for each of the years 1999, 2002, 2005, and 2008.
    
    data <- with(NEI, aggregate(Emissions, by = list(year), sum))
    names(data) <- c("year", "Sum")
    return (data)    
}

plot1 <- function(data) {
    
    png(filename = "plot1.png", width = 480, height = 480, units = "px")
    
    plot(x = data$year, y = log(data$Sum), 
         ylab = "Emissions Log(PM25)", xlab = "Year", main = "Annual Emissions", 
         pch = 18, col = "red", type = "b")
    
    fit <- lm(log(Sum) ~ year, data)
    abline(fit, lwd = 2, lty = 2, col = "blue")
    
    legend("topright", 
           lty=c(1, 2),
           col = c("red", "blue"), 
           c("Emissions line (log)", "Regression line"))
    
    dev.off()
}

getPlot2Data <- function() {
    
    # 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
    #    Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
    #    to make a plot answering this question.

    data <- NEI[NEI$fips == "24510", ]
    data <- with(data, aggregate(Emissions, by = list(year), sum))
    names(data) <- c("year", "Sum")
    return (data)    
}

plot2 <- function(data) {
    
    png(filename = "plot2.png", width = 480, height = 480, units = "px")
    
    plot(x = data$year, y = log(data$Sum), 
         ylab = "Emissions Log(PM25)", xlab = "Year", 
         main = "Annual Emissions Baltimore City, Maryland", 
         pch = 18, col = "red", type = "b")
    
    fit <- lm(log(Sum) ~ year, data)
    abline(fit, lwd = 2, lty = 2, col = "blue")
    
    legend("topright", 
           lty=c(1, 2),
           col = c("red", "blue"), 
           c("Emissions line (log)", "Regression line"))
    
    dev.off()
}

getPlot3Data <- function() {
    
    # 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
    #    variable, which of these four sources have seen decreases in emissions from 1999-2008 
    #    for Baltimore City? Which have seen increases in emissions from 1999-2008? 
    #    Use the ggplot2 plotting system to make a plot answer this question.
    
    data <- NEI[NEI$fips == "24510", ]
    data <- with(data, aggregate(Emissions, by = list(year, type), sum))
    names(data) <- c("year", "type", "Sum")
    return (data)    
}

plot3 <- function(data) {
    
    # use width 640 to dispay year propery
    png(filename = "plot3.png", width = 640, height = 480, units = "px")
    
    #qplot(year, log(Sum), data = data, facets = . ~ type, 
    #      geom = c("point", "smooth"), method = "lm",
    #      main = "Baltimore Emissions Trend by Type (1999-2008)",
    #      xlab = "Year", ylab = "Emissions Log(PM25)")

    g <- ggplot(data, aes(x = year, y = log(Sum))) + 
        geom_point() + 
        geom_smooth(method = "lm") + 
        facet_grid(.~type) + 
        labs(title = "Baltimore Emissions Trend by Type (1999-2008)") + 
        labs(x = "year", y = "Emissions Log(PM25)")
        
    print(g)
    
    dev.off()
}

getPlot4Data <- function() {
    
    # 4. Across the United States, how have emissions from coal combustion-related sources 
    #    changed from 1999-2008?
    
    coal <- SCC[grep("Coal", SCC$EI.Sector), ]
    data <- NEI[NEI$SCC %in% coal$SCC, ]
    data <- with(data, aggregate(Emissions, by = list(year), sum))
    names(data) <- c("year", "Sum")
    return (data)    
}

plot4 <- function(data) {
    
    png(filename = "plot4.png", width = 480, height = 480, units = "px")
    
    #qplot(year, log(Sum), data = data, facets = . ~ type, 
    #      geom = c("point", "smooth"), method = "lm",
    #      main = "US Coal Combustion Emissions Trend (1999-2008",
    #      xlab = "Year", ylab = "Emissions Log(PM25)")
    
    g <- ggplot(data, aes(x = year, y = log(Sum))) + 
        geom_point() + 
        geom_smooth(method = "lm") + 
        labs(title = "US Coal Combustion Emissions Trend (1999-2008)") + 
        labs(x = "year", y = "Emissions Log(PM25)")
    
    print(g)
    
    dev.off()
}

getPlot5Data <- function() {
    
    # 5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
    vehicle <- SCC[grep("[Mm]obile|[Vv]ehicles", SCC$EI.Sector), ]
    #vehicle <- vehicle[!grepl("\\bNon\\b", vehicle$EI.Sector), ]
    data <- NEI[NEI$SCC %in% vehicle$SCC, ]
    data <- with(data, aggregate(Emissions, by = list(fips, year), sum))
    names(data) <- c("fips", "year", "Sum")
    return (data[data$fips == "24510",])    
}

plot5 <- function(data) {
    
    png(filename = "plot5.png", width = 480, height = 480, units = "px")
    
    #qplot(year, log(Sum), data = data, facets = . ~ type, 
    #      geom = c("point", "smooth"), method = "lm",
    #      main = "US Coal Combustion Emissions Trend (1999-2008",
    #      xlab = "Year", ylab = "Emissions Log(PM25)")
    
    g <- ggplot(data, aes(x = year, y = log(Sum))) + 
        geom_point() + 
        geom_smooth(method = "lm") + 
        labs(title = "Baltimore City Motor Vehicles Emissions Trend (1999-2008)") + 
        labs(x = "year", y = "Emissions Log(PM25)")
    
    print(g)
    
    dev.off()
}

getPlot6Data <- function() {
    
    # 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from 
    #    motor vehicle sources in Los Angeles County, California (fips == "06037"). 
    #    Which city has seen greater changes over time in motor vehicle emissions?
    
    vehicle <- SCC[grep("[Mm]obile|[Vv]ehicles", SCC$EI.Sector), ]
    #vehicle <- vehicle[!grepl("\\bNon\\b", vehicle$EI.Sector), ]
    data <- NEI[NEI$SCC %in% vehicle$SCC, ]
    data <- with(data, aggregate(Emissions, by = list(fips, year), sum))
    names(data) <- c("fips", "year", "Sum")
    return (data[data$fips == "24510" | data$fips == "06037", ])    
}

state_labeller <- function(var, value){
    value <- as.character(value)
    if (var=="fips") { 
        value[value=="24510"] <- " Baltimore City"
        value[value=="06037"]   <- "LA"
    }
    return(value)
}

plot6 <- function(data) {
    
    png(filename = "plot6.png", width = 480, height = 480, units = "px")
    
    #qplot(year, log(Sum), data = data, facets = . ~ type, 
    #      geom = c("point", "smooth"), method = "lm",
    #      main = "US Coal Combustion Emissions Trend (1999-2008",
    #      xlab = "Year", ylab = "Emissions Log(PM25)")
    
    g <- ggplot(data, aes(x = year, y = log(Sum))) + 
        geom_point() + 
        geom_smooth(method = "lm") + 
        facet_grid(.~fips, labeller=state_labeller) + 
        labs(title = "Annual Motor Vehicle Emissions") + 
        labs(x = "year", y = "Emissions Log(PM25)")
    
    print(g)
    dev.off()
}

getData()
summaryData()

# plot 1
data <- getPlot1Data()
plot1(data)

# plot 2
data <- getPlot2Data()
plot2(data)

# plot 3
library(ggplot2)
data <- getPlot3Data()
plot3(data)

# plot 4
data <- getPlot4Data()
plot4(data)

# plot 5
data <- getPlot5Data()
plot5(data)

# plot 6
data <- getPlot6Data()
plot6(data)



