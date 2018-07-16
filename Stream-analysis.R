### Brittain Creek Henderson County, NC 
### Temperature and depth data analysis
### Monitroing period September 8 - November 22
## data location in Working 
#CBC_BRC.DEL.csv

## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
require("grid")
## Statistical analysis
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("TTR")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read data file
# Data file has previous manipulations
stream <- read.csv("./Working/CBC_Stream_all.csv")
## View to confirm proper read
#View(stream)

## rename columns
colnames(stream) <- c("date", 
                       "temp", 
                       "depth")
# Confirm
# View(stream)

## Set date time fomat
stream$date <- mdy_hm(stream$date, tz = "est")
# Confirm class
#class(stream[,1])

## Convert Units
stream.1 <- mutate(stream, 
                   temp = (temp - 32)/1.8, 
                   depth = (depth * 30.48))

########### subset dataset to remove periods of data collection
###### erronious increases in temperature 
stream.2 <- subset(stream.1, date != as.POSIXct("2017-09-08") &
                     date <= as.POSIXct("2017-10-25 13:00") | 
                     date >= as.POSIXct("2017-10-25 19:00") & 
                     date <= as.POSIXct("2017-11-22 3:00"))

### plot stream temperature and depth with 
# plot1
plot1 <-ggplot(data = stream.2)+
  geom_line(aes(x = date, y = depth, color = "Depth"))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_color_manual(values = c("#377eb8"))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")
# Plot2
plot2 <-ggplot(data = stream.2)+
  geom_line(aes(x = date, y = temp, color = "Temperature"))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  labs(x = "Date", y = "Temperature (°C)")+
  scale_color_manual(values = c("#4daf4a",
                                "#e41a1c"))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

## below text for managing fonts
# theme(legend.position = "bottom", 
#       legend.title = element_blank(),
#       legend.text = element_text(size = 16),
#       plot.title = element_text(size = 20, 
#                                 hjust = 0.5),
#       axis.title = element_text(size = 18),
#       axis.line = element_blank(),
#       axis.text = element_text(size = 14))

## Create temperature-duration plot
## Entire monitoring period 
## excluding data collection times
## Use stream.2
## Select temperature variable: sort, rank, & round
stream.3 <- (stream.2) %>%
  select(temp) %>%
  mutate(temp = sort(temp, decreasing = TRUE, na.last = TRUE),
         T.rank = rank(desc(temp)),
         temp = signif(temp, digits = 3))
## View(stream.3)

## Add counter to data fram for in temp obersvations 
## grouped_by temperature
stream.3 <- stream.3 %>%
  group_by(temp) %>%
  mutate(count = length(temp))
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
  select("temp", "time")
#View(stream.temp)

## Gather distict observations
## Should result in a single value per temperature
stream.temp <- distinct(stream.temp)
#View(stream.temp)

## Sum time to create cummulative duration exceedance of observation temperature
stream.temp <- (stream.temp) %>%
  mutate(cumdur = cumsum(time))
## View(stream.temp)

## Plot Brittain Creek temperature-Durations
ggplot(data = stream.temp)+
  geom_point(aes(x = cumdur, y = temp, shape = "Brittain Creek"))+ 
  geom_smooth(aes(x = cumdur, y = temp, color = "Brittain Creek"), method = loess, se = FALSE)+ 
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_y_continuous(limits = c(0,35), 
                     expand = c(0,0)) +
  labs(x = "Duration (hrs)", y = "Temperature (°C)")






