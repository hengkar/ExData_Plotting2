# Module 4 - Assignment 2

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

getData()

# plot 3
library(ggplot2)
data <- getPlot3Data()
plot3(data)




