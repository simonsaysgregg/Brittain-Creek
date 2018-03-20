### Brittain Creek Henderson County, NC 
### Temperature and Depth data analysis
### Monitroing period September 8 - November 22

### Load r packages 
### not all packages required
#Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
#Statistical analysis
require("stats")        # Lots of stats stuff
#Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
#require("xlsx")         # Reads and writes to xlsx file
#Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read comma delimitated dataset
stream <- read.csv("CBC_Stream_all.csv")
## View to confirm proper read
#View(CBC_Stream_all.csv)

## Set date time fomat
stream$date <- as.POSIXct(stream$date, format = "%m/%d/%y %H:%M", tz = "est")
## Confirm
class(stream[,1])
##View
#View(stream)

## rename columns
colnames(stream) <- c("Date", 
                      "Temp", 
                      "Depth")
## Convert Units
stream.1 <- mutate(stream, 
                   Temp = (Temp - 32)/1.8, 
                   Depth = (Depth * 30.48))

########### subset dataset to remove periods of data collection
###### erronious increases in temperature 
stream.2 <- subset(stream.1, Date != as.POSIXct("2017-09-08") &
                     Date <= as.POSIXct("2017-10-25 13:00") | 
                     Date >= as.POSIXct("2017-10-25 19:00") & 
                     Date <= as.POSIXct("2017-11-22 3:00"))

### plot stream temperature and depth with 2 axis
ggplot(data = stream.2, aes(x = Date))+
  ggtitle("Brittain Creek")+
  geom_line(aes(y = Temp, colour = "Temperature (°C)"), size = 1.1)+
  geom_line(aes(y = Depth/2.5, colour = "Depth (cm)"), size = 1.1)+
  #geom_hline(aes(yintercept = 21, colour = "Trout Treshold (°C)"), size = 1.1)+
  scale_color_manual(values = c("#e41a1c", 
                                "#377eb8",
                                "#4daf4a"))+
  scale_y_continuous(sec.axis = sec_axis(~./2.5, name = "Depth (cm)"))+
  labs(y = "Temperature (°C)", x = "Date")+
  theme_classic()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, 
                                  hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.line = element_blank(),
        axis.text = element_text(size = 14))


## Create temperature-duration plot
## Entire monitoring period 
## excluding data collection times
## Use stream.2
## Select temperature variable: sort, rank, & round
stream.3 <- (stream.2) %>%
  select(Temp) %>%
  mutate(Temp = sort(Temp, decreasing = TRUE, na.last = TRUE),
         T.rank = rank(desc(Temp)),
         Temp = signif(Temp, digits = 2))
## View(stream.3)

## Add counter to data fram for in temp obersvations 
## grouped_by temperature
stream.3 <- stream.3 %>%
  group_by(Temp) %>%
  mutate(count = length(Temp))
##View(stream.3)

## Calculate duration (at temperature hrs) of temp observations
## 1 observation = 2-min duration
stream.3 <- stream.3 %>%
  group_by(count) %>%
  mutate(time = mean(count)*2/60,  ## Conversion to hours
         time = signif(time, digits = 3))
## View(stream.3)

## Select temp and duration variables
stream.temp <- (stream.3) %>%
  ungroup() %>%
  select("Temp", "time")
#View(stream.temp)

## Gather distict observations
## Should result in a single value per temperature
stream.temp <- distinct(stream.temp)
#View(stream.temp)

## Sum time to create cummulative duration exceedance of observation temperature
stream.temp <- (stream.temp) %>%
  mutate(cumdur = cumsum(time))
## View(stream.temp)

## Plot Brittain Creek Temperature-Durations
ggplot()+
  geom_point(data = stream.temp, aes(x = cumdur, y = Temp, shape = "Stream Temperature"))+ 
  ggtitle("Brittain Creek Temperature-Duration Plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_y_continuous(limits = c(5,35), 
                     expand = c(0,0)) +
  labs(x = "Duration (hrs)", y = "Temperature (°C)")



