plot3 <-function(fname="household_power_consumption.txt")
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

  #create a file device for the plot
  png(file = "plot3.png");
  
  #with(selectdf,type = "n",xlab = "", ylab = "Energy sub metering" )
  plot(strptime(paste(as.character(selectdf$Date),selectdf$Time),"%Y-%m-%d %H:%M:%S"),selectdf$Sub_metering_1,type = "l",xlab = "", ylab = "Energy sub metering", col = "black")
  lines(strptime(paste(as.character(selectdf$Date),selectdf$Time),"%Y-%m-%d %H:%M:%S"),selectdf$Sub_metering_2,type = "l",col = "Red")
  lines(strptime(paste(as.character(selectdf$Date),selectdf$Time),"%Y-%m-%d %H:%M:%S"),selectdf$Sub_metering_3,type = "l",col = "Blue")
  legend("topright",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd=2,seg.len = 2.5,bty ="o")
  #save the graph to a png file
  #dev.copy(png,file = "plot3.png")
  dev.off()
  
}