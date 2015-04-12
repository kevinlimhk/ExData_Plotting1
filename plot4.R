plot4 <- function() {
    
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
    
    # Plot 4
    # Set par parameters
    # Set to 4 graphs
    par(mfrow=c(2,2))
    # Set margins for the plots
    par(mar=c(4.1,3.5,2,0.8))
    # Set the distance of axis values to axis lines
    par(mgp = c(3, 0.4, 0))
    
    # 1st Plot
    plot(dataset$DateTime, dataset$Global_active_power,
         type="l",
         xlab = "", ylab ="",
         main = "", cex.lab=0.8, cex.axis=0.6, tck=-0.02)
    # Adding text to y axis
    mtext("Global Active Power", side=2, line=1.8, cex=0.6)
    
    # 2nd Plot
    plot(dataset$DateTime, dataset$Voltage,
         type="l",
         xlab = "", ylab = "",
         main = "", cex.lab=0.8, cex.axis=0.6, tck=-0.02)
    # Adding texts to x and y axis
    mtext("Voltage", side=2, line=1.8, cex=0.6)
    mtext("datetime", side=1, line=1.6, cex=0.6)
    
    # 3rd Plot
    plot(dataset$DateTime, dataset$Sub_metering_1,
         type="l",
         xlab = "", ylab = "",
         main = "", cex.lab=0.8, cex.axis=0.7, tck=-0.02)
    # Adding text to y axis
    mtext("Energy sub metering", side=2, line=1.8, cex=0.6)
    # Adding lines for the other variables
    lines(dataset$DateTime, dataset$Sub_metering_2,
          type="l", col="red")
    lines(dataset$DateTime, dataset$Sub_metering_3,
          type="l", col="blue")
    # Adding legend
    legend("topright", cex=0.7,bty="n", y.intersp=0.8, x.intersp=0.2, lty=1, 
           col = c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    
    # 4th Plot
    plot(dataset$DateTime, dataset$Global_reactive_power,
         type="l",
         xlab = "", ylab = "",
         main = "", cex.lab=0.8, cex.axis=0.6, tck=-0.02)
    # Adding texts to x and y axis
    mtext("datetime", side=1, line=1.6, cex=0.6)
    mtext("Global_reactive_power", side=2, line=1.8, cex=0.6)
    
}
