setwd("C:/Users/Haonan/Desktop/Study/R/exploratory project")
#data <- read.table("household_power_consumption.txt", sep = ";")
#user  system elapsed 
#328.06    1.11  342.22 
files <- file('./household_power_consumption.txt')
subsetted <- read.table(text = grep("^[1,2]/2/2007",readLines(files),value=TRUE), sep = ';', col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), na.strings = '?')
#subsetted$Date <- as.Date(subsetted$Date,"%Y-%m-%d")
library(lubridate)
subsetted$Date <- mdy(subsetted$Date)
#here need something to transform time into 

#plot 1
png("Plot1.png", width = 480, height = 480)
hist(subsetted$Global_active_power, col = "red", xlab = "Golbal Active Power(kilowatts)", main = "Global Active Power")
dev.off()

#plot2
datetime <- strptime(paste(subsetted$Date, subsetted$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
png("Plot2.png", width = 480, height = 480)
plot(datetime, subsetted$Global_active_power, xlab = " ", ylab = "Global Active Power (killowatts)", type = "l")
dev.off()

#Plot 3
png("Plot3.png", width = 480, height = 480)
plot(datetime, subsetted$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
lines(datetime,subsetted$Sub_metering_1, type = "l",  col = "black")
lines(datetime,subsetted$Sub_metering_2, type = "l",  col = "red")
lines(datetime,subsetted$Sub_metering_3, type = "l",  col = "blue")
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col = c("black","red","blue"), lty = 1)
dev.off()

#Plot 4
png("Plot4.png", width = 480, height = 480)
par(mfrow = c(2,2))
plot(datetime, subsetted$Global_active_power, xlab = " ", ylab = "Global Active Power", type = "l")
plot(datetime,subsetted$Voltage, ylab = "Voltage", type = "l")
plot(datetime, subsetted$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
lines(datetime,subsetted$Sub_metering_1, type = "l",  col = "black")
lines(datetime,subsetted$Sub_metering_2, type = "l",  col = "red")
lines(datetime,subsetted$Sub_metering_3, type = "l",  col = "blue")
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col = c("black","red","blue"), lty = 1, bty = "n", cex = 0.4)
plot(datetime, subsetted$Global_reactive_power, ylab = "Global_reactive_power", type = "l")
dev.off()


