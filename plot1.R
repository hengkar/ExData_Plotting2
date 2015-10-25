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


getData()

# plot 1
data <- getPlot1Data()
plot1(data)




