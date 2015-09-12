plot4 <-function(fname="household_power_consumption.txt")
{
  #load the library required for reading selected records
  require(data.table)
  require(dplyr)
  # read in the data file converting to date while reading
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
  
  df <- read.csv(fname,header = TRUE,sep= ";",colClasses = c('myDate','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'),stringsAsFactors = FALSE, na.strings = c("?") )
  selectdf <- df[df$Date >= "2007-02-01" & df$Date < "2007-02-03",]
  colnames(selectdf) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

  datetimeData <- strptime(paste(as.character(selectdf$Date),selectdf$Time),"%Y-%m-%d %H:%M:%S")
  #set up the plot window to display four plots
  par(mfrow = c(2,2))
  #plot 1
  with(selectdf,plot(datetimeData,selectdf$Global_active_power,type = "l",xlab = "", ylab = "Global Active Power (kilowatts)" ))
  
  #plot 2
  with(selectdf,plot(datetimeData,selectdf$Voltage,type = "l",xlab = "datetime", ylab = "Voltage" ))
  
  #plot 3
  plot(datetimeData,selectdf$Sub_metering_1,type = "l",xlab = "", ylab = "Energy sub metering", col = "black")
  lines(datetimeData,selectdf$Sub_metering_2,type = "l",col = "Red")
  lines(datetimeData,selectdf$Sub_metering_3,type = "l",col = "Blue")
  legend("topright",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd=2,seg.len = .5,bty = "o")
  
  #plot 4
  with(selectdf,plot(datetimeData,selectdf$Global_reactive_power,type = "l",xlab = "datetime", ylab = "Global_reactive_power" ))
                     
  #save the graph to a png file
  dev.copy(png,file = "plot4.png")
  dev.off()
  
}