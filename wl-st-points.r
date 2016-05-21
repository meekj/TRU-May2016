#!/usr/local/R-3.3.0/lib/R/library/littler/bin/r
## #!/usr/local/bin/r

## Generate monthly summary of Delaware River and Bay Daily Water Levels by Station
## Time ordered points, daily boxplot, and high / low predictions
## Jon Meek - May 2016


## Install the "littler" package for this CLI method
## Adjust first line to point to the version of littler you wish to use

## Sample command line:
##  ~/lab/R/noaa/wl-st-points.r --outdir /n2/r-reports --datadir ~/lab/R/noaa/data --month 201604

## Water level and prediction data obtained from https://tidesandcurrents.noaa.gov

RCSid <- '$Id: wl-st-points.r,v 1.5 2016/05/21 01:48:37 meekj Exp $'

library(ggplot2)
library(dplyr)
library(stringr)
library(knitr)
library(docopt)
library(XML)

Sys.setenv(TZ="UTC") # Water level data are UTC (but not tidal predictions, see below)

## Setup docopt command line options and help
##
cli <- "Usage: wl-st-points.r [--help --datadir <datadir> --month <month> --outdir <outdir>]

-h --help           Show this help text
--datadir <datadir> Data directory
--outdir <outdir>   Report directory, default is data directory
--month <month>     Month, format: 201602
"
options <- docopt(cli)          # Have docopt interpret the command line

DataDir <- options[["datadir"]] # Extract options from docopt
OutDir  <- options[["outdir"]]
Month   <- options[["month"]]

## Get the month name for report header
month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
Year      <- substr(Month, 1,4)
month_num <- substr(Month, 5,6)
MonthName <- month_names[as.integer(month_num)]


if (is.null(options[["outdir"]])) { # --outdir not specified
    cat("--outdir must be specified\n")
    q()
}
OutFile <- paste(OutDir, '/wl-st-points-', Month, '.html', sep = '') # Hardcoded file name prefix

setwd(OutDir) # This will put the 'figure' directory with temporary image file(s) in the same directory as the output
              # But be careful with running other programs that might write to the same directory at the same time
              # Adding a unique ID to the filename(s) is advisable

FigureWidth  <- 16 # Provides some control over the size of the plots, also see CSS block at bottom
FigureHeight <-  6

PointSize <- 0.8   # May need adjustment if figure size changes, or if few points will be plotted

opts_chunk$set(dev="png", dpi=96) # knitr settings, may be default

theme_jm1 <- theme_bw() + # A reasonable ggplot2 theme for HTML and copy/paste to word processing applications
    theme(
        plot.title  = element_text(size = rel(1.5), family = 'Helvetica', face = 'bold'),
        axis.title  = element_text(size = rel(1.5), colour = "black", face = 'bold'),
        axis.text.x = element_text(size = rel(1.5), lineheight = 0.9, colour = "black", vjust = 1, face = 'bold'),
        axis.text.y = element_text(size = rel(1.5), lineheight = 0.9, colour = "black", hjust = 1, face = 'bold'),
        strip.text.y = element_text(size = rel(1.7), colour = "black", face = 'bold'),
        legend.text = element_text(size = rel(1.3))
    )

## Delaware River static data from http://www.state.nj.us/drbc/library/documents/StreamMileageJuly2007.pdf

raw_station_data <- "Station,Dist,Name
8548989, 126.00 ,Newbold PA
8539094, 118.00 ,Burlington NJ
8546252, 106    ,Bridesburg PA
8545240,  99.00 ,Philadelphia PA
8551762,  61.00 ,Delaware City DE
8551910,  59.00 ,Reedy Point DE
8537121,  37.00 ,Ship John Shoal NJ
8555889,  10.00 ,Brandywine Shoal Light DE
8557380,   0.00 ,Lewes DE
"

## Get above Delaware River static data into a data frame
##
StationData <- read.csv(textConnection(raw_station_data), header=TRUE, stringsAsFactors = FALSE) # Get static data into data frame
StationData <- StationData %>% arrange(desc(Dist))                                               # Be sure that it is ordered North to South
StationData$Station <- as.character(StationData$Station)                                         # Make the station ID be a string rather than int

# Get the available files for the month, assume that only data for the water system of interest are there
Files <- Sys.glob(paste(DataDir, '/co-ops-wl-', Month, '*.csv', sep = ''))

wl <- NULL
for (f in Files) {                        # Read each station's measurement file
    station <- str_extract(f, '[0-9]{7}') # Get 7 digit NOAA station number from filename
    t <- read.csv(f)                      # Read data into temp data frame
    t$Station <- station                  # Add Station number column
    t$Date.Time <- as.POSIXct(strptime(as.character(t$Date.Time), format = "%Y-%m-%d %H:%M")) # Convert time/date string to POSIXct
    wl <- rbind(wl, t)                    # Add this station data to the collective data frame
}
wl$Date      <- as.Date(wl$Date.Time) # Generate a Date column
Stations     <- unique(wl$Station)    # Stations for which we have data
StationOrder <- StationData$Station   # Should add check that we actually have the data for each station


## Read tidal prediction data from XML files
##
Predictions <- NULL
for (station in Stations) {
    filename <- paste(DataDir, '/', station, '_annual.xml', sep = '')
    doc <- xmlParse(filename)

    t_station    <- xmlToDataFrame(getNodeSet(doc,"//stationid"), stringsAsFactors = FALSE) # Station ID
    t_prediction <- xmlToDataFrame(getNodeSet(doc,"//item"), stringsAsFactors = FALSE)      # Tidal predictions, high / low values

    station_number <- as.character(t_station[1]) # Add station ID column
    t_prediction$Station <- station_number

    t_prediction$predictions_in_ft <- as.numeric(t_prediction$predictions_in_ft) # Convert predictions to numeric
    t_prediction$predictions_in_cm <- as.numeric(t_prediction$predictions_in_cm)
    t_prediction$Time <- paste(t_prediction$date, t_prediction$time)             # Form a DateTime column for conversion

    Sys.setenv(TZ="US/Eastern") # Tidal predictions are in local time, maybe can be fixed at download
    t_prediction$Time <- as.POSIXct(strptime(t_prediction$Time, format = "%Y/%m/%d %I:%M %p")) # Convert prediction times to POSIXct
    Sys.setenv(TZ="UTC")

    Predictions <- rbind(Predictions, t_prediction) # Build the data frame with predictions for all stations
}

## Trim predictions to date range of interest
##
min_date <- min(wl$Date)
max_date <- max(wl$Date)
Predictions <- Predictions %>% filter(date >= min_date & date <= max_date)

## str(Predictions)   # Commented for CLI method, but leave for interactive developmet / testing

## Build the report
##

LineSize   <- 0.1 # Size constants for plots
JitterSize <- 0.4
PointSize  <- 2

## knitr_data vector will hold R/knitr/Rmarkdown blocks until rendered, usually once per loop or other logical block
## html_output will collect the rendered HTML for output at end 

knitr_data <- c(                                     # Report header, Markdown syntax
    "# Delaware River and Bay Daily Water Levels",   # HTML <H1>
    '## `r MonthName` `r Year`',                     #      <H2>
    'Predicted high and low water levels are in red' # Text in <p>
    )

html_output <- NULL # Buffer to hold final html data until it is written out

plot_number <- 0
for (station in StationOrder) { # Plot data for each station
    plotdata <- wl %>% filter(station == Station)
    plotdata$DateT <- as.POSIXct(strptime(paste(plotdata$Date, '12:00:00'), format = "%Y-%m-%d %H:%M:%S")) # 'Date' as POSIXct for boxplot + time ordered data points

    station_name <- as.character(StationData %>% filter(Station == station) %>% select(Name)) # Get current station name
    station_dist <- StationData %>% filter(Station == station) %>% select(Dist)               # and stream mile distance for plot label

    prediction_high <- Predictions %>% filter(station == Station & highlow == 'H')            # High & low water level predictions for current station
    prediction_low  <- Predictions %>% filter(station == Station & highlow == 'L')
    
    knitr_data <- c(knitr_data, "### `r station_name` - Stream Mile `r station_dist`")        # HTML plot label with embedded R variable values

##  plot_number <- plot_number + 1                                                            # Unique name for each block, not required for this rendering method
    plot_label <- paste('plot', plot_number, sep = '')                                        # All plots will be in 'plot0.png' temp file, overwritten for each plot 
    plot_header <- paste('```{r plot', plot_number,                                           # Plot header, will print warnings, but not code
                         ', echo=FALSE, message=FALSE, fig.width = FigureWidth, fig.height = FigureHeight}', sep = '')

    ## knitr_data <- NULL                    # For interactive development
    knitr_data <- c(knitr_data, plot_header) # Append to existing data, report header on first plot, NULL after that 

    knitr_data <-c(knitr_data,
                   "ggplot() +", # Daily measurements boxplot with no whiskers, time ordered measurements and high/low predictions
                   "geom_boxplot(data = plotdata, aes(group = DateT, x = DateT, y = Water.Level), coef = 0, outlier.size = 0, outlier.colour = 'NA', size = 0.3, colour = 'darkgray') +",
                   "geom_point(data = prediction_high, aes(x = Time, y = predictions_in_ft), colour  = 'red', size = PointSize, shape=19) +",
                   "geom_point(data = prediction_low,  aes(x = Time, y = predictions_in_ft), colour  = 'red', size = PointSize, shape=19) +",
                   "geom_line(data  = prediction_high, aes(x = Time, y = predictions_in_ft), colour  = 'red', size = LineSize) +",
                   "geom_line(data  = prediction_low,  aes(x = Time, y = predictions_in_ft), colour  = 'red', size = LineSize) +",
                   "geom_line(data  = plotdata, aes(x = Date.Time, y = Water.Level), colour  = 'dodgerblue',  size = LineSize) +",
                   "geom_point(data = plotdata, aes(x = Date.Time, y = Water.Level), colour  = 'dodgerblue',  size = JitterSize, shape=19) +",
                   "xlab('') + ylab('Water Level, feet') +",
                   "scale_x_datetime(date_minor_breaks = '1 day') +", # ggplot 2.0.0+ syntax
                   "theme_jm1")                                       # Theme for HTML readability

    ## eval(parse(text = knitr_data))                                 # For interactive development

    knitr_data  <- c(knitr_data, "```")                         # End R code section
    html_output <- c(html_output, knit2html(text = knitr_data)) # Render plot now so it uses current dataframe
    knitr_data  <- NULL                                         # Clear vector for next plot
}

## Compute monthly tidal range at each station
##
wl_range <- wl %>% group_by(Station) %>% summarise(Min = min(Water.Level), Max = max(Water.Level))

## Join StationData & wl_range data frames to pick up Stream Distance and Name columns
##
StationData$Station <- as.character(StationData$Station)      # Station must be same type to join data frames
wl_range <- left_join(wl_range, StationData, by = 'Station')
wl_range <- wl_range %>% mutate(Range = Max - Min)            # Add tidal range column

## Prepare a table for printing, in North up order
wl_range_presentation <- wl_range %>% select(Station, Name, Dist, Min, Max, Range) %>% arrange(desc(Dist))

PointSize <- 4 # Next plot has a small number of data points, need larger points

knitr_data <- c(knitr_data, "## Monthly Tidal Range") # Report section header


## Summary Plot 1

## plot_number <- plot_number + 1
plot_label  <- paste('plot', plot_number, sep = '')
plot_header <- paste('```{r plot', plot_number, ', echo=FALSE, message=FALSE, fig.width = FigureWidth, fig.height = FigureHeight}', sep = '')

knitr_data <- c(knitr_data, plot_header,
                "ggplot(wl_range) +",
                "geom_line(aes(x = Dist, y = Range), size=0.06) +",
                "geom_point(aes(x = Dist, y = Range), color = 'blue', size=PointSize, shape=19) +",
                "xlab('Stream Distance') + ylab('Monthly Tidal Range, feet') + theme_jm1",
                "```")
html_output <- c(html_output, knit2html(text = knitr_data)) # Render plot
knitr_data  <- NULL                                         # Clear vector buffer


## Summary Plot 2

## plot_number <- plot_number + 1
plot_label  <- paste('plot', plot_number, sep = '')
plot_header <- paste('```{r plot', plot_number, ', echo=FALSE, message=FALSE, fig.width = FigureWidth, fig.height = FigureHeight}', sep = '')

knitr_data <- c(knitr_data, plot_header,
                "ggplot(wl_range) +",
                "geom_errorbar(aes(x = Dist, ymin = Min, ymax = Max), color = 'blue', size=0.5) +",
                "xlab('Stream Distance') + ylab('Monthly Tidal Range') + theme_jm1",
                "```")
html_output <- c(html_output, knit2html(text = knitr_data)) # Render plot
knitr_data <- NULL                                          # Clear vector buffer

## Summary Table (xtable could be used if more control is desired)
##
knitr_data <- c(knitr_data, # Append table data
                "## Summary",
                "```{r table1, echo=FALSE, message=FALSE}",
                "kable(wl_range_presentation, digits = 2, row.names = TRUE)",
                "```",
                '***',
                "`r RCSid`")

knitr_data <- c(knitr_data, # Append CSS data, need to do it at end to override defaults
    '<style type="text/css">',
    'body {',
    'max-width: 1000px;',   # Make the plots wider, default is 800
    'margin: auto;',
    'padding: 1em;',
    'line-height: 20px ; ',
    '}',
    'table, th {',          # Customize tables a bit
    '   max-width: 95%;',
    '   border: 1px solid #ccc;',
    '   border-spacing: 15px 3px;',
    '}',
    '</style>'
    )

html_output <- c(html_output, knit2html(text = knitr_data)) # Render table & CSS

writeLines(html_output, OutFile) # Write the accumulated html to the output file

