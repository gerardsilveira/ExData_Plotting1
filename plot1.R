plot1 <-function(fname="household_power_consumption.txt")
{
  #load the library required for reading selected records
  require(data.table)
  
  # read in the data file converting to date while reading
  setClass('myDate')
  setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
  
  df <- read.csv(fname,header = TRUE,sep= ";",colClasses = c('myDate','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'),stringsAsFactors = FALSE, na.strings = c("?") )
  selectdf <- df[df$Date >= "2007-02-01" & df$Date < "2007-02-03",]
  
  #display the graph
  hist(selectdf$Global_active_power,main="Global Active Power",xlab = "Global Active Power (kilowatts)",col="Red")
  
  #save the graph to a png file
  dev.copy(png,file = "plot1.png")
  dev.off()
}