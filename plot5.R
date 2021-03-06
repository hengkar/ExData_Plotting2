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

getData()

# plot 5
library(ggplot2)
data <- getPlot5Data()
plot5(data)




