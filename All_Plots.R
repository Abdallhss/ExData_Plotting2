#The data can be obtained through this URL
#https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip

#Read in the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(dplyr)
NEI_tbl <- tbl_df(NEI)
#Plot1
NEI_summary <- NEI_tbl %>% group_by(year) %>%
    summarise(total_PM25 =sum(Emissions)/1e6)
png(filename = "Plot1.png", width = 480, height = 480, units = "px")
barplot(total_PM25~year,data = NEI_summary,col = "blue",
        ylab = "Total PM2.5 Emissions [Gton]",
        xlab = "Year")
title("Total PM2.5 Emission in the US")
dev.off()
#Plot2
Baltimore <- subset(NEI_tbl,fips == "24510")
Baltimore_summary <- Baltimore %>% group_by(year) %>%
    summarise(total_PM25 =sum(Emissions))
png(filename = "Plot2.png", width = 480, height = 480, units = "px")
barplot(total_PM25~year,data = Baltimore_summary,col = "blue",
        ylab = "Total PM2.5 Emissions [ton]",
        xlab = "Year")
title("Total PM2.5 Emissions in Baltimore City")
dev.off()
#Plot3
Baltimore_summary <- Baltimore %>% group_by(type,year) %>%
    summarise(total_PM25 =sum(Emissions))
png(filename = "Plot3.png", width = 480, height = 480, units = "px")
ggplot(Baltimore_summary,aes(year,total_PM25)) +
    geom_line(aes(color = type),size=2)+
    labs(title = "Total PM2.5 Emissions in Baltimore City")+
    labs(x="Year") +
    labs(y="Total PM2.5 Emissions [ton]")
dev.off()

#Plot4
Coal_SCC <- SCC[grep("[Cc]oal",SCC$Short.Name),1]
NEI_coal <- subset(NEI_tbl,SCC %in% Coal_SCC)
NEI_coal_summary <- NEI_coal %>% group_by(year) %>%
    summarise(total_PM25 =sum(Emissions)/1e6)
png(filename = "Plot4.png", width = 480, height = 480, units = "px")
barplot(total_PM25~year,data = NEI_coal_summary,col = "blue",
        ylab = "Total PM2.5 Emissions [Gton]",
        xlab = "Year")
title("Total Coal-Related PM2.5 Emissions")
dev.off()
#Plot5
#motor vehicles emission is equivliant to on-road type
NEI_onroad_Balt <- subset(NEI_tbl,type == "ON-ROAD" & fips == "24510")
NEI_onroad_Balt_summary <- NEI_onroad_Balt %>% group_by(year) %>%
    summarise(total_PM25 =sum(Emissions))
png(filename = "Plot5.png", width = 480, height = 480, units = "px")
barplot(total_PM25~year,data = NEI_onroad_Balt_summary,col = "blue",
        ylab = "Total PM2.5 Emissions [ton]",
        xlab = "Year")
title("Total Vehicles PM2.5 Emissions in Baltimore City")
dev.off()
#Plot6
#motor vehicles emission is equivliant to on-road type
NEI_onroad_Balt_Los <- subset(NEI_tbl,type == "ON-ROAD" &
                                  fips %in% c("24510","06037"))
NEI_onroad_Balt_Los_summary <- NEI_onroad_Balt_Los %>% group_by(year,fips) %>%
    summarise(total_PM25 =sum(Emissions))
png(filename = "Plot6.png", width = 480, height = 480, units = "px")
ggplot(NEI_onroad_Balt_Los_summary,aes(year,total_PM25)) +
    geom_line(aes(color = fips),size=2)+
    labs(title = "Vehicle Emissions (Baltimore Vs. Los Angeles)")+
    labs(x="Year") +
    labs(y="Total PM2.5 Emissions [ton]")
dev.off()
