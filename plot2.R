plot2 <-function(fname="household_power_consumption.txt")
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

  with(selectdf,plot(strptime(paste(as.character(Date),Time),"%Y-%m-%d %H:%M:%S"),selectdf$Global_active_power,type = "l",xlab = "", ylab = "Global Active Power (kilowatts)" ))
  
  #save the graph to a png file
  dev.copy(png,file = "plot2.png")
  dev.off()
  
}