#######################################
#####     Updated Low Flow    #####
#####         7-day mo avg        #####
#######################################

# This script calculates the 7-day moving average, zero flow and flow duration curve thresholds
# over the California dry season (June 1 to December 31)
# First section provides the output (csv file) of daily discharge and rolling mean

# Second section calculates the number of zero flow days, 7-day minimum, and flow duration curve thresholds
# Calculates the timing of each low flow metric

cat("\014") ;  rm(list=ls())

library(dataRetrieval); library(FlowScreen); 
library(dplyr); library(lubridate); library(tidyr); library(ggplot2);library(tools)
library(spatial); library(rgeos); library(raster); library(data.table)
library(RColorBrewer); library(rgdal); library(lfstat)

#State Shapefile for plots
pathfm = "C:/Work/Data/Shapefiles/California State"
usa <- readOGR(pathfm, layer="CA_State_TIGER2016")
usa <- spTransform(usa,CRS("+init=epsg:4326"))
ca = fortify(usa)

# Read in data for reference years
all.eff <- read.csv("C:/Work/Data/Functional Flows/EFF_All_Class_original_metrics.csv",header=T); head(all.eff)

# Get functional flows reference sites
tmet <- read.csv("C:\\Work\\Data\\Functional Flows\\Met files\\met_dryseason_point1_5day_15percentofyears.csv",header=T)
tmet = tmet[which(tmet$Stat == "Streamclass_annual"),]
sites= unique(tmet$ID)


# list to store data
seven.list <- list()

for(s in 1:length(sites)){
  
  #s=20
  site = sites[s]
  print(site)
  
  
  sf <- read.csv(paste0("C:\\Work\\Data\\USGS gages\\California Discharge\\Daily_Qavg_until_082021\\USGS_QAverage_",site,"_Discharge.csv"), header=T)
  sf <- sf[,c(3,4)]
  
  #calculate stats for the Zero-Flow days for each year and EFF period
  library(lfstat)
  sf$Date <- as.Date(sf$Date, "%Y-%m-%d")
  sf$MD <- substr(sf$Date,6,10)
  sf$year <- as.numeric(substr(sf$Date,1,4))
  sf$Wyear <- water_year(as.Date(sf$Date), origin=10)
  
  colnames(sf) <- c("Date","Discharge","MD","year","Wyear")
  
  # Flow duration curve thresholds
  qsf <- na.omit(sf)
  qts <- quantile(qsf$Discharge, c(0.02,0.06,0.10,0.14,0.18)) #sets percentiles 

  quants <- data.frame(names(qts),qts)
  colnames(quants) <- c("Threshold","Value")
  
  #Check for consecutive dates
  min <- min(sf$year);  max <- max(sf$year)
  
  wateryears <- seq(min,max,by=1)
  actyears <- unique(sf$year)
  
  #test to see how many missing observations  
  missing <- length(sf[is.na(sf$Discharge),])
  
  #data frame for rolling mean  
  x <- sf
  
  #Calculates moving average for different window size (n)
  x$roll7Mean <- RcppRoll::roll_mean(x$Discharge, n = 7L, fill = NA)
  x$test <- paste0(missing)
  x$site <- paste0(site)
  
  #remove rows with nas and days that do not have consecutive values
  all.sev = x[!is.na(x$roll7Mean),]
  all.sev <- na.omit(x)
  
  all.sev$year <- as.numeric(substr(all.sev$Date,1,4))
  all.sev$MD <- substr(all.sev$Date,6,10)
  all.sev$Wyear <- water_year(as.Date(all.sev$Date), origin=10)
  
  #write.csv(all.sev, paste0("C:\\Work\\Data\\Daily 7 day mo avg\\",site,"_SevenMoAvg.csv"),row.names=F)
  
  qts7 <- quantile(all.sev$roll7Mean, c(0.02,0.06,0.10,0.14,0.18))
  quants7 <- data.frame(names(qts7),qts7)
  colnames(quants7) <- c("Threshold","Value")
  
  
  # Data frame to store site's annual low flow metrics
  intermets <- data.frame(matrix(NA,nrow=length(wateryears),ncol=24))
  
  threshold.list <- list()
  
  for(y in 1:length(wateryears)){
    
    #y=6
    year = wateryears[y]
    
    #extract water year
    daily <- all.sev[which(all.sev$Wyear==year),]
    daily$Day <- seq_len(nrow(daily))
    
    
    if(nrow(daily)>=330){ # no more than 10% of missing data
      
      # to calculate low flow metrics over dry season, need next year's wy 
      nextyear <- all.sev[which(all.sev$Wyear == as.numeric(year)+1),]
      nextyear <- nextyear[order(as.Date(nextyear$Date, format="%Y-%m-%d")),]
      nextyear$Day <- seq_len(nrow(nextyear))+nrow(daily)
      
      #June 1 to Jan 1 for dry season timing 
      #Dry Season -- Calculates number of No Flow days during the dry season
      dry.tim <- as.Date(paste0("",year,"-06-01"),format="%Y-%m-%d")
      dry.end <- as.Date(paste0("",year,"-12-31"),format="%Y-%m-%d")
      
      # get entire ds (into next water year too)
      dry1 <- daily[which(daily$Date >= dry.tim),]
      dry2 <- nextyear[which(nextyear$Date <= dry.end),]
      dry <- rbind(dry1,dry2)
      
      # plots to check data
      library(ggplot2)
      t <- ggplot(daily)+
        geom_line(aes(x=Date,y=Discharge,color="Daily streamflow"))+
        #geom_line(aes(x=Date,y=roll7Mean,color="7-day mov avg"))+
        geom_line(data=dry,aes(x=Date,y=Discharge,color="Dry season"))+
        scale_color_manual(values=c("Daily streamflow"="black","7-day mov avg"="blue",
                                    "Dry season"="red"),name="")+
        theme_light()
      t <- t + theme(legend.position = c(0.8,0.88))
      print(t)
      
      # for some reason, not triggered when is the same value
      thres <- sapply(quants$Value,function(x){ nrow(dry[which(dry$Discharge <= as.numeric(x)),])})
      thres.days <- sapply(quants$Value,function(x){dry[which(dry$Discharge <= as.numeric(x)),]$Day[1]})
      
      thres7 <- sapply(quants7$Value,function(x){ nrow(dry[which(dry$roll7Mean <= as.numeric(x)),])})
      thres.days7 <- sapply(quants$Value,function(x){dry[which(dry$Discharge <= as.numeric(x)),]$Day[1]})
      
      library(dplyr)
      #Calculate sev day min, duration and start date
      SevDmin <- min(dry$roll7Mean)
      Sevdur <- ifelse(nrow(dry)>0, as.numeric(count(dry[which(dry$Discharge == SevDmin),])$n), NA)
      SevDay <- dry[which(dry$roll7Mean == SevDmin),]$Date[1]
      SevStart <- dry[which(dry$roll7Mean == SevDmin),]$Day[1]
      
      # number of zero flow (<0.1) within the dry season (raw)
      fzeros <- ifelse(nrow(dry)>0, as.numeric(nrow(dry[which(dry$Discharge < 0.1),])), NA)
      zdf <- dry[which(dry$Discharge < 0.1),]
      
      # if there are any zero flow days
      if(nrow(zdf)>0){
        
        zeros <- zdf %>%
          mutate(date = as.Date(Date, format = "%Y-%m-%d")) %>%
          arrange(Date) %>%
          group_by(id2 = cumsum(c(T, diff(Date) > 1)), .add = T) %>%
          mutate(num_con_days = ifelse(date == first(Date), last(Date) - Date + 1, 0))
        
        #zeros$Date <- as.Date(zeros$Date, format = "%Y-%m-%d")
        
        first <- zeros[which(zeros$num_con_days >= 5),]
        first <- zdf[1,]$Day
        
        # number of zero flow days after first five consecutive days
        fzeros1 <- nrow(zdf[which(zdf$Day >= first),])
        
        
        
      }  
      else{
        first <- NA
        last <- NA
        duration <- NA
        fzeros1 <- 0
      }
      
      print(fzeros1-fzeros)
      
      intermets[y,1] <- year
      intermets[y,2] <- fzeros1
      intermets[y,3] <- first
      intermets[y,4] <- SevDmin
      intermets[y,5] <- SevStart
      intermets[y,6] <- paste0(SevDay)
      
      # get threshold (discharge) and number of days below threshold (count of days)
      intermets[y,7] <- thres[1]
      intermets[y,8] <- thres[2]
      intermets[y,9] <- thres[3]
      intermets[y,10] <- thres[4]
      intermets[y,11] <- quants[1,2]
      intermets[y,12] <- quants[2,2]
      intermets[y,13] <- quants[3,2]
      intermets[y,14] <- quants[4,2]
      intermets[y,15] <- thres7[2]
      intermets[y,16] <- thres7[3]
      intermets[y,17] <- thres7[4]
      intermets[y,18] <- quants7[1,2]
      intermets[y,19] <- quants7[2,2]
      intermets[y,20] <- quants7[3,2]
      intermets[y,21] <- quants7[4,2]
      
    }
    else{next}
  }
  
  
  
  colnames(intermets) <- c("Year","Zero_Flow_Days","Zero_Flow_Start_Date_WY",
                           "Min_7_Day_Mov_Avg","Zero_date_check", "Min_7_Start_Date_WY",
                           "98%_Days","94%_Days","90%_Days","86%_Days",
                           "98%_Days7","94%_Days7","90%_Days7","86%_Days7")

  write.csv(intermets,paste0("C:\\Work\\Data\\Altered Low Flow Metrics Dry Season Consecutive\\",site,"_Annual_low_flow.csv"),row.names = F)
    
}
