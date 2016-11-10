setwd("C:/Users/Haonan/Desktop/Study/R/exploratory project/project 2")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#set up raw data

#Plot1
total <- with(NEI, tapply(Emissions, year, sum, na.rm = T)) #sum up emissions by year
total <- as.data.frame(total)
total[,2] <- c(1999,2002,2005,2008)                         #establish a new col for the future calling in plotting
total[,1] <- total[,1]/1000                                 #number is too big, so change the unit to kiloton
colnames(total) <- c("emission","year")                     #assign col names to make it readable
#plot(total$V2,total$total, col = "red", pch = 22, type = "b", lwd = 2, xlab = "Years", ylab = "Total Emissions (kt)", main = "US PM2.5 Emission")
#legend("topright", "Trend", col = "red", lty = 1)
png("Plot 1.png")
barplot(height = total$emission, names.arg = total$year, col = 3,, xlab = "Years", ylab = "Total Emissions (kt)", main = "US PM2.5 Emission")
dev.off()

#Plot2
rawbtm <- subset(NEI, fips == "24510")                      #select Baltimore city
btm <- with(rawbtm, tapply(Emissions,year,sum,na.rm = T))   #sum up emissions by year
btm <- as.data.frame(btm)
btm[,2] <- c(1999,2002,2005,2008)                           #establish a new col for the future calling in plotting
btm[,1] <- btm[,1]/1000                                     #number is too big, so change the unit to kiloton
colnames(btm) <- c("emission", "year")                      #assign col names to make it readable
#plot(btm$V2,btm$btm, col = "red", pch = 22, type = "b", lwd = 2, xlab = "Years", ylab = "Total Emissions (kt)", main = "Plot 2")
#legend("topright", "Trend", col = "red", lty = 1)
png("Plot 2.png")
barplot(height = btm$emission, names.arg = btm$year, col = 3,, xlab = "Years", ylab = "Total Emissions (kt)", main = "Baltimore PM2.5 Emission")
dev.off()

#Plot3
library(ggplot2)
#library(gridExtra)
library(reshape2)
rawbtm4 <- subset(NEI, fips == "24510")                     #select Baltimore city
btm4 <- with(rawbtm4, tapply(Emissions,list(year,type),sum,na.rm = T)) #sum up emissions by year and type
btm4 <- as.data.frame(btm4)
btm4[,5] <- c(1999,2002,2005,2008)                          #establish a new col for the future calling in plotting
colnames(btm4)[5]<- "year"                                  #assign col names to make it readable
btm4melt <- melt(btm4, id = "year")                         #melt the dataframe by year
colnames(btm4melt)[2] <- "Type"                             #assign col names to make it readable
png("Plot 3.png")
ggplot(btm4melt,aes(year,value,colour = Type)) + geom_line() + labs(title = "PM2.5 Emission by Type", x = "Years", y = "Total Emissions (t)")
dev.off()

#process <- function(x) {
#   plot(btm4$V5,x, col = "red", type = "b", xlab = "Years")
#    qplot(V5, x,data = btm4, geom = "line",col = "Trend", ylim = c(0,2200),xlab = "Years", ylab = "Total Emissions")#, main = btm4[n,6])
#    }
#png("Plot 3.png")
#fourplot <- apply(btm4[1:4],2,process)
#marrangeGrob(fourplot, nrow=2, ncol=2)
#dev.off()

#Plot 4
coal_SCC_No <- SCC$SCC[grep("Coal", SCC$EI.Sector)]         #Search content and find "Coal" word
rawcoal <- subset(NEI, SCC %in% coal_SCC_No)                #select in NEI which SCC contains "Coal" word
coal <- with(rawcoal, tapply(Emissions,year,sum,na.rm = T)) #sum up emissions by year
coal <- as.data.frame(coal)
coal[,2] <- c(1999,2002,2005,2008)                          #establish a new col for the future calling in plotting
coal[,1] <- coal[,1]/1000                                   #number is too big, so change the unit to kiloton 
colnames(coal) <- c("emission","year")                      #assign col names to make it readable
png("Plot 4.png")
ggplot(coal, aes(factor(year),emission)) + geom_bar(stat = "identity", color = 3) +labs(title = "Coal Combustion related PM2.5 Emission", x = "Years", y = "Total Emissions (kt)") 
dev.off()

#Plot 5
rawmotor <- subset(NEI,fips == "24510" & type == "ON-ROAD" )    #select Baltimore city and motor vehicle
motor <- with(rawmotor, tapply(Emissions,year,sum,na.rm = T))   #sum up emissions by year
motor <- as.data.frame(motor)
motor[,2] <- c(1999,2002,2005,2008)                             #establish a new col for the future calling in plotting
colnames(motor) <- c("emission","year")                         #assign col names to make it readable
png("Plot 5.png")
ggplot(motor, aes(factor(year),emission)) + geom_bar(stat = "identity", color = 3) +labs(title = "Motor Vehicle PM2.5 Emission", x = "Years", y = "Total Emissions (t)") 
dev.off()

#Plot 6
library(reshape2)
rawcity <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD") # select LA and BTM and motor vehicle
city <- with(rawcity, tapply(Emissions,list(year,fips),sum,na.rm = T))          #sum up emissions by year and type
city <- as.data.frame(city)
city[,3] <- c(1999,2002,2005,2008)                                  #establish a new col for the future calling in plotting
city[,1] <- city[,1]/1000                                           #number is too big, so change the unit to kiloton 
names(city) <- c("Los Angeles County", "Baltimore City", "year")    #assign col names to make it readable
citymelt <- melt(city, id = "year")                                 #melt city by year
colnames(citymelt)[2] <- "City"                                     #assign col names to make it readable
png("Plot 6.png")
ggplot(citymelt,aes(year, value, colour = City)) + geom_line() +labs(title = "Motor Vehicle PM2.5 Emission at LA and BTM", x = "Years", y = "Total Emissions (kt)") 
dev.off()


#process <- function(x) {
#   plot(btm4$V5,x, col = "red", type = "b", xlab = "Years")
#    qplot(V5, x,data = btm4, geom = "line",col = "Trend", ylim = c(0,2200),xlab = "Years", ylab = "Total Emissions")#, main = btm4[n,6])
#    }
#png("Plot 3.png")
#fourplot <- apply(btm4[1:4],2,process)
#marrangeGrob(fourplot, nrow=2, ncol=2)
#dev.off()
################ I've tried to wrote a function, it works but with some minor flaws can't be solved################

#Plot 4
coal_SCC_No <- SCC$SCC[grep("Coal", SCC$EI.Sector)]         #Search content and find "Coal" word
rawcoal <- subset(NEI, SCC %in% coal_SCC_No)                #select in NEI which SCC contains "Coal" word
coal <- with(rawcoal, tapply(Emissions,year,sum,na.rm = T)) #sum up emissions by year
coal <- as.data.frame(coal)
coal[,2] <- c(1999,2002,2005,2008)                          #establish a new col for the future calling in plotting
coal[,1] <- coal[,1]/1000                                   #number is too big, so change the unit to kiloton 
colnames(coal) <- c("emission","year")                      #assign col names to make it readable
png("Plot 4.png")
ggplot(coal, aes(factor(year),emission)) + geom_bar(stat = "identity", color = 3) +labs(title = "Coal Combustion related PM2.5 Emission", x = "Years", y = "Total Emissions (kt)") 
dev.off()

#Plot 5
rawmotor <- subset(NEI,fips == "24510" & type == "ON-ROAD" )    #select Baltimore city and motor vehicle
motor <- with(rawmotor, tapply(Emissions,year,sum,na.rm = T))   #sum up emissions by year
motor <- as.data.frame(motor)
motor[,2] <- c(1999,2002,2005,2008)                             #establish a new col for the future calling in plotting
colnames(motor) <- c("emission","year")                         #assign col names to make it readable
png("Plot 5.png")
ggplot(motor, aes(factor(year),emission)) + geom_bar(stat = "identity", color = 3) +labs(title = "Motor Vehicle PM2.5 Emission", x = "Years", y = "Total Emissions (t)") 
dev.off()

#Plot 6
library(reshape2)
rawcity <- subset(NEI, (fips == "24510" | fips == "06037") & type == "ON-ROAD") # select LA and BTM and motor vehicle
city <- with(rawcity, tapply(Emissions,list(year,fips),sum,na.rm = T))          #sum up emissions by year and type
city <- as.data.frame(city)
city[,3] <- c(1999,2002,2005,2008)                                  #establish a new col for the future calling in plotting
city[,1] <- city[,1]/1000                                           #number is too big, so change the unit to kiloton 
names(city) <- c("Los Angeles County", "Baltimore City", "year")    #assign col names to make it readable
citymelt <- melt(city, id = "year")                                 #melt city by year
colnames(citymelt)[2] <- "City"                                     #assign col names to make it readable
png("Plot 6.png")
ggplot(citymelt,aes(year, value, colour = City)) + geom_line() +labs(title = "Motor Vehicle PM2.5 Emission at LA and BTM", x = "Years", y = "Total Emissions (kt)") 
dev.off()
