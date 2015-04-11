plot2 <- function() {
    
    # Extracting data from 1/2/2007 & 2/2/2007 from txt file
    # Initialize extracted data frame
    dataset <- data.frame()
    # Set counters for loops
    # i for row counter
    # dataset_prerow for number of rows in data frame from previous loop
    dataset_prenrow<-0
    i<-1
    # Start of loop
    while(i>0) {
        # Read in data based on counter i
        df <- read.table("household_power_consumption.txt", sep=";", skip=i, nrows=1000, col.names=c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        # Append data to dataset data frame if rows falls in date 1/2/2007 & 2/2/2007
        dataset <- rbind(dataset,df[df$Date %in% c("1/2/2007","2/2/2007"), ])
        # Break from loop when all required data is extracted
        if (nrow(dataset)!=0 && nrow(dataset)==dataset_prenrow)
            break
        # set counters for the next loop
        dataset_prenrow <- nrow(dataset)
        i <- i + 1000
    }
    # Convert and add new datetime to data frame
    dataset$DateTime <- strptime(paste(dataset$Date,dataset$Time), "%d/%m/%Y %H:%M:%S")
    
    # Plot 2
    # Set par parameters
    par(mfrow=c(1,1))
    par(mar=c(3.1,3.1,2.8,1.1))
    par(mgp = c(3, 0.4, 0))
    
    # Line Plot
    plot(dataset$DateTime, dataset$Global_active_power,
         type="l",
         xlab = "", ylab ="",
         main = "", cex.lab=0.8, cex.axis=0.8, tck=-0.02)
    mtext("Global Active Power (kilowatts)", side=2, line=1.8, cex=0.8)
    
}