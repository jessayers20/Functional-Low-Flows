###############################################
#####     Get COMID data for met file     #####
#####       soil and GDE datasets         #####
###############################################


# Get watershed boundaries from NHDplus tool box
# Extrac all PRISM/watershed data from original files (updated to 2021 from dataset)
# add soil porosity, available water capacity and groundwater depend ecosystem files
cat("\014") ;  rm(list=ls())

library(dataRetrieval); library(nhdplusTools); 
library(dplyr); library(lubridate); library(tidyr); library(ggplot2);library(tools)
library(spatial); library(rgeos); library(raster); library(rgdal)
library(sp); library(tidyverse); library(data.table)

# original fun flows met file
met1 <- list.files("D:\\CA_NHDPreds\\", pattern="*.csv")

library(stringr)
comids <- str_extract(met1, '.*(?=\\.csv)')

# Read in updated met file
upmet <- read.csv("C:\\Work\\Data\\Functional Flows\\Climate_mets_2006_2022.csv",header=T)
upmet <- rename(upmet, "WaYr"="Year")

# get all comids
altered <- read.csv("C:\\Work\\Data\\USGS gages\\California Discharge\\CA_altered_1980_25years_2.csv",header = T)
comids <- unique(c(altered$NHDV1_COMID,altered$NHDV2_COMID))

for(s in 1:length(comids)){
  
  # s = 1
  comid <- comids[s]
  
  # Read in file with NHD values (climate, watershed characteristics)
  # 1950-2015
  met <- read.csv(paste0("D:\\CA_NHDPreds\\",comid,".csv"))
  
  # If creating for ref gages only 
  tryCatch({
    met <- read.csv(paste0("D:\\CA_NHDPreds\\",comid,".csv"))
  },
  error = function(err){
    print("Error in evaluating a. Initializing it to NA")
    print(comid)
    met <<- data.frame(Characters=character(),
                       stringsAsFactors=FALSE)
  })
  
  #if first comid is empty, use second comid val
  if(nrow(met)==0){
    #comid <-  allbasins[which(allbasins$ID == site),]$NHDV2_COMID
    #comid <- refstations[which(refstations$ID == site),]$NHDV2_COMID
    next
  }
  
  
  
  # Isolate all climate vars so have the next year's climate vars too
  tmet = met[,c("COMID",  "WaYr" , "ppt_Jan.wy", "ppt_Feb.wy",
                "ppt_Mar.wy", "ppt_Apr.wy", "ppt_May.wy", "ppt_Jun.wy",
                "ppt_Jul.wy", "ppt_Aug.wy", "ppt_Sep.wy", "ppt_Oct.wy",
                "ppt_Nov.wy", "ppt_Dec.wy" ,
                "ppt_Jan.pwy", "ppt_Feb.pwy", "ppt_Mar.pwy", "ppt_Apr.pwy", "ppt_May.pwy", "ppt_Jun.pwy", "ppt_Jul.pwy", "ppt_Aug.pwy", "ppt_Sep.pwy",
                "tav_Jan.wy" ,"tav_Feb.wy",
                "tav_Mar.wy" ,"tav_Apr.wy", "tav_May.wy", "tav_Jun.wy",
                "tav_Jul.wy", "tav_Aug.wy", "tav_Sep.wy", "tav_Oct.wy",
                "tav_Nov.wy" ,"tav_Dec.wy", 
                "tav_Jan.pwy", "tav_Feb.pwy", "tav_Mar.pwy", "tav_Apr.pwy", "tav_May.pwy", "tav_Jun.pwy", "tav_Jul.pwy", "tav_Aug.pwy", "tav_Sep.pwy",
                "run_Jan.wy" , "run_Feb.wy",
                "run_Mar.wy",  "run_Apr.wy" ,"run_May.wy" , "run_Jun.wy" ,
                "run_Jul.wy",  "run_Aug.wy" , "run_Sep.wy",  "run_Oct.wy",  "run_Nov.wy",  "run_Dec.wy",
                "run_Jan.pwy", "run_Feb.pwy", "run_Mar.pwy", "run_Apr.pwy", "run_May.pwy", "run_Jun.pwy", "run_Jul.pwy", "run_Aug.pwy", "run_Sep.pwy"
                
  )]
  
  
  
  # read in updated data 
  # 2015-2022
  umet <- upmet[which(upmet$COMID == comid),]
  
  umet <- umet[,c("COMID",  "Year" , 
                  "ppt_Jan.wy", "ppt_Feb.wy",
                  "ppt_Mar.wy", "ppt_Apr.wy", "ppt_May.wy", "ppt_Jun.wy",
                  "ppt_Jul.wy", "ppt_Aug.wy", "ppt_Sep.wy", "ppt_Oct.wy",
                  "ppt_Nov.wy", "ppt_Dec.wy" ,"tav_Jan.wy" ,"tav_Feb.wy",
                  "tav_Mar.wy" ,"tav_Apr.wy", "tav_May.wy", "tav_Jun.wy",
                  "tav_Jul.wy", "tav_Aug.wy", "tav_Sep.wy", "tav_Oct.wy",
                  "tav_Nov.wy" ,"tav_Dec.wy", "run_Jan.wy" , "run_Feb.wy",
                  "run_Mar.wy",  "run_Apr.wy" ,   "run_May.wy" , "run_Jun.wy" ,
                  "run_Jul.wy",  "run_Aug.wy" , "run_Sep.wy",  "run_Oct.wy",  "run_Nov.wy",  "run_Dec.wy")]
  
  
  umet <- umet[which(umet$Year >= 2015),] #remove years with nas
  
  
  # Need to get previous years climate vars to calculate annual totals
  prevmet <- umet[which(umet$Year >= 2015),]
  
  # Need to subset water years
  prevmet$WaYr <- (prevmet$Year + 1) # minus one to get the correct previous year
  
  # set col names to previous wy
  colnames(prevmet) <- c("COMID","Year", "ppt_Jan.pwy", "ppt_Feb.pwy", "ppt_Mar.pwy", "ppt_Apr.pwy", "ppt_May.pwy", "ppt_Jun.pwy", "ppt_Jul.pwy", "ppt_Aug.pwy", "ppt_Sep.pwy", "ppt_Oct.pwy", "ppt_Nov.pwy", "ppt_Dec.pwy",
                         "tav_Jan.pwy", "tav_Feb.pwy", "tav_Mar.pwy", "tav_Apr.pwy", "tav_May.pwy", "tav_Jun.pwy", "tav_Jul.pwy", "tav_Aug.pwy", "tav_Sep.pwy","tav_Nov.pwy", "tav_Dec.pwy", 
                         
                         "run_Jan.pwy", "run_Feb.pwy", "run_Mar.pwy", "run_Apr.pwy", "run_May.pwy", "run_Jun.pwy", "run_Jul.pwy", "run_Aug.pwy", "run_Sep.pwy","run_Oct.pwy", "run_Nov.pwy", "run_Dec.pwy"
  )
  
  testmet <- merge(umet,prevmet,by=c("Year","COMID"))
  
  
  testmet <- testmet[,c("COMID",  "Year" , "ppt_Jan.wy", "ppt_Feb.wy",
                        "ppt_Mar.wy", "ppt_Apr.wy", "ppt_May.wy", "ppt_Jun.wy",
                        "ppt_Jul.wy", "ppt_Aug.wy", "ppt_Sep.wy", "ppt_Oct.wy",
                        "ppt_Nov.wy", "ppt_Dec.wy" ,
                        "ppt_Jan.pwy", "ppt_Feb.pwy", "ppt_Mar.pwy", "ppt_Apr.pwy", "ppt_May.pwy", "ppt_Jun.pwy", "ppt_Jul.pwy", "ppt_Aug.pwy", "ppt_Sep.pwy",
                        "tav_Jan.wy" ,"tav_Feb.wy",
                        "tav_Mar.wy" ,"tav_Apr.wy", "tav_May.wy", "tav_Jun.wy",
                        "tav_Jul.wy", "tav_Aug.wy", "tav_Sep.wy", "tav_Oct.wy",
                        "tav_Nov.wy" ,"tav_Dec.wy", 
                        "tav_Jan.pwy", "tav_Feb.pwy", "tav_Mar.pwy", "tav_Apr.pwy", "tav_May.pwy", "tav_Jun.pwy", "tav_Jul.pwy", "tav_Aug.pwy", "tav_Sep.pwy",
                        "run_Jan.wy" , "run_Feb.wy",
                        "run_Mar.wy",  "run_Apr.wy" ,"run_May.wy" , "run_Jun.wy" ,
                        "run_Jul.wy",  "run_Aug.wy" , "run_Sep.wy",  "run_Oct.wy",  "run_Nov.wy",  "run_Dec.wy",
                        "run_Jan.pwy", "run_Feb.pwy", "run_Mar.pwy", "run_Apr.pwy", "run_May.pwy", "run_Jun.pwy", "run_Jul.pwy", "run_Aug.pwy", "run_Sep.pwy"
                        
  )]
  

  
  names(tmet)[names(tmet) == 'WaYr'] <- 'Year'  
  
  # use new file for metrics post 2015, updated weather stations with PRISM
  tmet = tmet[which(tmet$Year<2015),]
  testmet = testmet[which(testmet$Year >= 2015),]
  
  # bind together
  allmet <- rbind(tmet, testmet)
  years <- unique(allmet$Year)
  
  # File to store next water year's data (nwy)
  yearslist <- list()
  
  for(y in 1:(length(years)-1)){
    
    year = years[y]
    
    #if(year < 2021){
    
    df.cur = allmet[which(allmet$Year == year),]
    
    com <- unique(allmet$COMID)
    cfil <- allmet
    df.next <- cfil[which(cfil$Year == (year+1)),]
    
    df.cur$ppt_Oct.nwy <- unique(df.next$ppt_Oct.wy)
    df.cur$tav_Oct.nwy <- unique(df.next$tav_Oct.wy)
    df.cur$ppt_Nov.nwy <- unique(df.next$ppt_Nov.wy)
    df.cur$tav_Nov.nwy <- unique(df.next$tav_Nov.wy)
    df.cur$ppt_Dec.nwy <- unique(df.next$ppt_Dec.wy)
    df.cur$tav_Dec.nwy <- unique(df.next$tav_Dec.wy)
    df.cur$ppt_Jan.nwy <- unique(df.next$ppt_Jan.wy)
    df.cur$tav_Jan.nwy <- unique(df.next$tav_Jan.wy)
    df.cur$ppt_Feb.nwy <- unique(df.next$ppt_Feb.wy)
    df.cur$tav_Feb.nwy <- unique(df.next$tav_Feb.wy)
    df.cur$ppt_Mar.nwy <- unique(df.next$ppt_Mar.wy)
    df.cur$tav_Mar.nwy <- unique(df.next$tav_Mar.wy)
    
    #df.prev <- cfil[which(cfil$WaYr ==(year-1)),]
    #df.cur$ppt_ann.pwy
    
    yearslist[[y]] <- df.cur
    
    rm(df.cur,cfil,df.next,com)
    # } else{next}
  }
  
  library(data.table)
  wyclim <- rbindlist(yearslist)
  
  
  ## Create summary/seasonal climate variables
  # PPT
  
  # WaYr annual & Seasons
  wyclim$ppt_ann.wy<-wyclim$ppt_Oct.wy + wyclim$ppt_Nov.wy + wyclim$ppt_Dec.wy + wyclim$ppt_Jan.wy + wyclim$ppt_Feb.wy + wyclim$ppt_Mar.wy + wyclim$ppt_Apr.wy + wyclim$ppt_May.wy + wyclim$ppt_Jun.wy + wyclim$ppt_Jul.wy + wyclim$ppt_Aug.wy + wyclim$ppt_Sep.wy
  wyclim$ppt_fall.wy<-wyclim$ppt_Oct.wy + wyclim$ppt_Nov.wy + wyclim$ppt_Dec.wy
  wyclim$ppt_wint.wy<-wyclim$ppt_Jan.wy + wyclim$ppt_Feb.wy + wyclim$ppt_Mar.wy
  wyclim$ppt_sprg.wy<-wyclim$ppt_Apr.wy + wyclim$ppt_May.wy + wyclim$ppt_Jun.wy
  wyclim$ppt_summ.wy<-wyclim$ppt_Jul.wy + wyclim$ppt_Aug.wy + wyclim$ppt_Sep.wy
  
  # Previous "annual" WaYr & Seasons. Note that previous water year Oct-Dec data NOT present here
  wyclim$ppt_ann.pwy<-wyclim$ppt_Jan.pwy + wyclim$ppt_Feb.pwy + wyclim$ppt_Mar.pwy + wyclim$ppt_Apr.pwy + wyclim$ppt_May.pwy + wyclim$ppt_Jun.pwy + wyclim$ppt_Jul.pwy + wyclim$ppt_Aug.pwy + wyclim$ppt_Sep.pwy
  wyclim$ppt_wint.pwy<-wyclim$ppt_Jan.pwy + wyclim$ppt_Feb.pwy + wyclim$ppt_Mar.pwy
  wyclim$ppt_sprg.pwy<-wyclim$ppt_Apr.pwy + wyclim$ppt_May.pwy + wyclim$ppt_Jun.pwy
  wyclim$ppt_summ.pwy<-wyclim$ppt_Jul.pwy + wyclim$ppt_Aug.pwy + wyclim$ppt_Sep.pwy
  
  # Running summs
  wyclim$ppt_sum1<-wyclim$ppt_ann.wy + wyclim$ppt_ann.pwy
  wyclim$ppt_sum2<-wyclim$ppt_ann.wy + wyclim$ppt_summ.pwy
  wyclim$ppt_sum3<-wyclim$ppt_ann.wy + wyclim$ppt_sprg.pwy
  wyclim$ppt_sum4<-wyclim$ppt_ann.wy +  wyclim$ppt_wint.pwy
  #RUN
  # WaYr annual & Seasons
  wyclim$run_ann.wy<-wyclim$run_Oct.wy + wyclim$run_Nov.wy + wyclim$run_Dec.wy + wyclim$run_Jan.wy + wyclim$run_Feb.wy + wyclim$run_Mar.wy + wyclim$run_Apr.wy + wyclim$run_May.wy + wyclim$run_Jun.wy + wyclim$run_Jul.wy + wyclim$run_Aug.wy + wyclim$run_Sep.wy
  wyclim$run_fall.wy<-wyclim$run_Oct.wy + wyclim$run_Nov.wy + wyclim$run_Dec.wy
  wyclim$run_wint.wy<-wyclim$run_Jan.wy + wyclim$run_Feb.wy + wyclim$run_Mar.wy
  wyclim$run_sprg.wy<-wyclim$run_Apr.wy + wyclim$run_May.wy + wyclim$run_Jun.wy
  wyclim$run_summ.wy<-wyclim$run_Jul.wy + wyclim$run_Aug.wy + wyclim$run_Sep.wy
  # Previous "annual" WaYr & Seasons. Note that previous water year Oct-Dec data NOT present here
  wyclim$run_ann.pwy<-wyclim$run_Jan.pwy + wyclim$run_Feb.pwy + wyclim$run_Mar.pwy + wyclim$run_Apr.pwy + wyclim$run_May.pwy + wyclim$run_Jun.pwy + wyclim$run_Jul.pwy + wyclim$run_Aug.pwy + wyclim$run_Sep.pwy
  wyclim$run_wint.pwy<-wyclim$run_Jan.pwy + wyclim$run_Feb.pwy + wyclim$run_Mar.pwy
  wyclim$run_sprg.pwy<-wyclim$run_Apr.pwy + wyclim$run_May.pwy + wyclim$run_Jun.pwy
  wyclim$run_summ.pwy<-wyclim$run_Jul.pwy + wyclim$run_Aug.pwy + wyclim$run_Sep.pwy
  # Running summs
  wyclim$run_sum1<-wyclim$run_ann.wy + wyclim$run_ann.pwy
  wyclim$run_sum2<-wyclim$run_ann.wy + wyclim$run_summ.pwy
  wyclim$run_sum3<-wyclim$run_ann.wy + wyclim$run_sprg.pwy
  wyclim$run_sum4<-wyclim$run_ann.wy +  wyclim$run_wint.pwy
  #TAV
  # WaYr annual & Seasons
  wyclim$tav_ann.wy<-wyclim$tav_Oct.wy + wyclim$tav_Nov.wy + wyclim$tav_Dec.wy + wyclim$tav_Jan.wy + wyclim$tav_Feb.wy + wyclim$tav_Mar.wy + wyclim$tav_Apr.wy + wyclim$tav_May.wy + wyclim$tav_Jun.wy + wyclim$tav_Jul.wy + wyclim$tav_Aug.wy + wyclim$tav_Sep.wy
  wyclim$tav_fall.wy<-wyclim$tav_Oct.wy + wyclim$tav_Nov.wy + wyclim$tav_Dec.wy
  wyclim$tav_wint.wy<-wyclim$tav_Jan.wy + wyclim$tav_Feb.wy + wyclim$tav_Mar.wy
  wyclim$tav_sprg.wy<-wyclim$tav_Apr.wy + wyclim$tav_May.wy + wyclim$tav_Jun.wy
  wyclim$tav_summ.wy<-wyclim$tav_Jul.wy + wyclim$tav_Aug.wy + wyclim$tav_Sep.wy
  # Previous "annual" WaYr & Seasons. Note that previous water year Oct-Dec data NOT present here
  wyclim$tav_ann.pwy<-wyclim$tav_Jan.pwy + wyclim$tav_Feb.pwy + wyclim$tav_Mar.pwy + wyclim$tav_Apr.pwy + wyclim$tav_May.pwy + wyclim$tav_Jun.pwy + wyclim$tav_Jul.pwy + wyclim$tav_Aug.pwy + wyclim$tav_Sep.pwy
  wyclim$tav_wint.pwy<-wyclim$tav_Jan.pwy + wyclim$tav_Feb.pwy + wyclim$tav_Mar.pwy
  wyclim$tav_sprg.pwy<-wyclim$tav_Apr.pwy + wyclim$tav_May.pwy + wyclim$tav_Jun.pwy
  wyclim$tav_summ.pwy<-wyclim$tav_Jul.pwy + wyclim$tav_Aug.pwy + wyclim$tav_Sep.pwy
  # Running summs
  wyclim$tav_sum1<-wyclim$tav_ann.wy + wyclim$tav_ann.pwy
  wyclim$tav_sum2<-wyclim$tav_ann.wy + wyclim$tav_summ.pwy
  wyclim$tav_sum3<-wyclim$tav_ann.wy + wyclim$tav_sprg.pwy
  wyclim$tav_sum4<-wyclim$tav_ann.wy +  wyclim$tav_wint.pwy
  
  vars <- met[1,-c(2:105)]
  
  # merge climate together 
  new <- merge(wyclim,vars)
  
  
  write.csv(new, paste0("D:\\CA_NHDPreds_1950_2021\\",comid,".csv"),row.names=F)
  #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  rm(totalveg,totalwet,gdetotal,met,final,new,comid,gdetotal,site,veg,wet,wyclim,maxawc,maxpor,meanawc,meanpor,minawc,minpor,totalveg,vars,tmet,umet,testmet,prevmet,
     df.awc,df.crop,nd.crop,basin,awc.crop,allmet,nldi_nwis,s,y,years,year)
  
}




# Merge all datasets together!!
# updated met file with next water year p/t with all FF metrics and new low flow metrics


cat("\014") ;  rm(list=ls())

options(max.print=9999999)

library(VSURF); library(varSelRF); library(caret); library(Boruta); library(gamlss)
library(randomForest);library(dplyr);library(data.table);library(foreach)
library(ranger);library(doParallel); registerDoParallel(cores=4); library(ggplot2)
library(magrittr)

# Select low flow metric --------------------------------------------------

## Load data with all FFM observations and associated watershed variables
# Get site ids and comids
met <- read.csv("C:\\Work\\Data\\Functional Flows\\data\\model_training\\met.csv")


#Use for ceff sites with more than 10 years of data available (201 vs. 219)
clus <-  read.csv("C:\\Work\\Data\\Functional Flows\\Streamflow_classification_5condays_dryseason.csv",header=T)
sites = unique(clus$site_no)

#Get next year's precipitation mets
ls.list <- list()

for (s in 1:length(sites)){
  
  site = sites[s]
  tmet <- met[which(met$ID == site),]

  comid <- unique(tmet$COMID)
  
  cfil <- read.csv(paste0("D:\\CA_NHDPreds_1950_2021\\",comid,".csv"),header=T)
  names(cfil)[names(cfil) == 'WaYr'] <- 'Year'  
  
    
  temp1 = read.csv(paste0("C:\\Work\\Data\\Altered Low Flow Metrics Dry Season Consecutive\\",site,"_Annual_low_flow.csv"),header=T) 
  temp1$site = paste0(site)
    
  avg <- mean(temp1$Zero_Flow_Days)
    
    
  ffmetrics <- tmet[,c("ID","Year","Value","Stat")]
  ffmetrics <- merge(ffmetrics,cfil)
  
  #get reference years
  years <- unique(ffmetrics$Year)
    
  # merge annual low flow metrics with climate/watershed variables as input in RF models
  lf <- temp1[,c("site","Year","Zero_Flow_Days")]
  lf$Metric <- paste0("Zero-flow") 
  colnames(lf) <- c("ID","Year","Value","Stat")
  lf2 <- merge(lf,cfil)
    
  lfd <- temp1[,c("site","Year","Zero_Flow_Start_Date_WY")]
  lfd$Metric <- paste0("Zero_Start")
  colnames(lfd) <- c("ID","Year","Value","Stat")
  lfd2 <- merge(lfd,cfil)
  
    
    #} else{
  minmag <- temp1[,c("site","Year","Min_7_Day_Mov_Avg")]
  minmag$Metric <- paste0("Min-7")
  colnames(minmag) <- c("ID","Year","Value","Stat")
  minmag <- merge(minmag,cfil)
    
  minday <- temp1[,c("site","Year","Min_7_Start_Date_WY")]
  minday$Metric <- paste0("Min-7_Start")
  colnames(minday) <- c("ID","Year","Value","Stat")
  minday <- merge(minday,cfil)
    
    #}
    
  lff <- rbind(lf2,lfd2,minmag,minday,ffmetrics)


  #set annual scale of zero flow vals
  ipvals <- temp1[,1:2]
  ipvals$Value <- ifelse(ipvals$Zero_Flow_Days >= 5, "Intermittent","Perennial")
  ipvals$Stat <- paste0("Streamclass_annual")
  ipvals$ID <- paste0(site)
  ipvals <- ipvals[c("ID","Year","Value","Stat")]
  classdf <- merge(ipvals,cfil)
    
  lff2 <- rbind(classdf,lff)
 # lff2 = lff2[which(lff2$Year %in% years),]

    
  ls.list[[s]] <- (lff2)
  rm(lf,lfd,lf2,lfd2,kvar,avg,occur,percent,temp1,lff,ipvals,classdf)
    
  }
  
  lowdf <- rbindlist(ls.list)
  

  write.csv(lowdf,"C:\\Work\\Data\\Functional Flows\\Met files\\met_dryseason_point1_5condays_15percentofyears_nonreferenceyears.csv",row.names=F)
  
  
  
  

newmets <- data.frame(rbindlist(allyears))
#newmets = newmets[,-c(275:280)]

#test = setdiff(met,newmets)

write.csv(newmets,"C:\\Work\\Data\\Functional Flows\\Met files\\nextmet.csv",row.names = F)











# Get files in proper order before update them
library(data.table)
setwd("C:\\Work\\Data\\COMID met files 2014-2022")
upfi <- list.files("C:/Work/Data/COMID met files 2014-2022")

#files <- rbindlist(lapply(upfi, function(x){read.csv(x, header=T)}))

FUN2 <- function(number){ # fun to unlist and format datasets
  
  #number=1012
  var  = unlist(strsplit(upfi[number],"_"))[3]  #within original temp, extracts into characters separated by / and extracts in the fourth position the characters in the position of 1 to 7
  year = as.integer(unlist(strsplit(upfi[number],"_"))[4])
  month = as.integer(substr(unlist(strsplit(upfi[number],"_"))[5],1,2))
  
  id = as.character(substr(upfi[number],1,11))
  
  if(month %in% c(10,11,12)){
    year <- as.numeric(year+1)
  }else{year <- year}
  
  Month = factor(month,levels=c("1","2","3","4","5","6","7","8","9","10","11","12"),
                 labels=c("Jan","Feb","Mar","Apr","May",
                          "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  
  var = ifelse(var == "ppt","ppt",
               ifelse(var=="runoff","run","tav"))
  
  temp=read.csv(upfi[number],header=T)
  
  temp$value <- temp$cum_avg_x_AreaSqKM/temp$cum_AreaSqKM  
  temp$Year <- year
  #temp1 = temp[which(temp$ComID == 8319319),]
  temp = temp[,c(1,7,6)]
  
  colnames(temp) <- c("COMID","Year","Value")
  temp$Var <- paste0(var,"_",Month,".wy")
  #temp$id <- paste0(id)
  write.csv(temp,paste0("C:\\Work\\Data\\Processed COMID met files 2014-2022\\",id,"_",var,"_",Month,"_",year,".csv"),row.names=F)
  
}

twoT = lapply(1:length(upfi), function(i){ FUN2(i)  }) #Use Lapply for a list of the same length as x(temp) using function (FUN1)



library(data.table); library(tidyr)
setwd("C:\\Work\\Data\\Processed COMID met files 2014-2022")
file_list <- list.files("C:/Work/Data/Processed COMID met files 2014-2022")

myMergedData <-   do.call(rbind, lapply( file_list, read.csv))

test<-spread(myMergedData,key="Var",value="Value")
test=test[-which(test$Year == 1950),]
#write.csv(test,"C:\\Work\\Data\\Functional Flows\\Climate_mets_2006_2022.csv",row.names=F)



