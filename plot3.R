
## Load data, create plot, and save as PNG, per assignment specs
plot3 <- function(){
    ## Load data
    dataSet <- loadData()
    
    if(sum(dim(dataSet)) == 0){
        print("No data found.")
    } else {
        ## Pick up datetime values from data
        dateTime <- strptime(paste(as.Date(dataSet$Date, "%d/%m/%Y"), dataSet$Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
        
        ## Create parameters for saving plot as PNG
        png(filename = "plot3.png", width = 480, height = 480)
        
        ## Enforce number of rows and columns on the plot
        par(mfrow = c(1, 1))
        
        ## Create plot
        plot(x = dateTime, 
             y = dataSet$Sub_metering_1, 
             type = "l", 
             xlab = "", 
             ylab = "Energy sub metering")
        
        ## Add second line
        lines(dateTime, dataSet$Sub_metering_2, type = "l", col = "red")
        
        ## Add third line
        lines(dateTime, dataSet$Sub_metering_3, type = "l", col = "blue")
        
        ## Add legend
        legend("topright", 
               legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
               col = c("black","red","blue"), 
               lwd = 1)
        
        ## Save plot
        dev.off()
    }
}

## Load and return data (or empty dataset if data not found)
loadData <- function(){
    
    ## Verify that file exists or unzip it if necessary
    fileName <- "household_power_consumption.txt"
    zippedDataFileName <- "exdata-data-household_power_consumption.zip"
    successFlag <- findUnzipData(fileName, zippedDataFileName)
    
    if(!successFlag){
        dataSet <- data.frame()
    } else {
        ## Find the lines in the data for the wanted dates
        ## linesWanted <- grep("^[12]/2/2007", readLines(fileName))
        
        ## Uncomment above line, comment this line to generalize;
        ## this is hard-coded to save a whole bunch of time here
        linesWanted <- 66638:69517
        
        ## Load from the first to the last wanted line, then subset to exactly the lines wanted
        skipNum <- min(linesWanted) - 1
        nrowsNum <- max(linesWanted) - skipNum
        dataSet <- read.table(fileName, skip = skipNum, nrows = nrowsNum, sep = ";", na.strings = "?", header = FALSE)[linesWanted - skipNum, ]
        
        dataNames <- read.table(fileName, nrows = 1, sep = ";", stringsAsFactors = FALSE, header = FALSE)
        names(dataSet) <- dataNames
    }
    
    ## Return dataset
    return(dataSet)
}

## Check that the data file exists in the working directory. If not, check for
## zip file, unzip it, and check again. Returns logical success/failure flags
findUnzipData <- function(fileName, zippedDataFileName){
    
    if(!file.exists(fileName)){
        ## Data not found
        print(paste("File ", getwd(), "/", fileName, " not found.", sep = ""))
        
        if(file.exists(zippedDataFileName)){
            ## Zipped data found
            print(paste("Zip file ", zippedDataFileName, " found.", sep = ""))
            
            ## Unzip data
            print("Unzipping data...")
            unzip(zippedDataFileName)
            
            ## Check for data file after unzipping
            if(file.exists(fileName)){
                print("Data file found.")
                return(TRUE)
            } else {
                print("Data file not found.")
                return(FALSE)
            }
        } else {
            ## Zipped data not found.
            print(paste("Zip file ", zippedDataFileName, " not found.", sep = ""))
            return(FALSE)
        }
    } else {
        print("Data file found.")
    }
    return(TRUE)
}
