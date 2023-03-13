######################################################################
#####                  Model low-flow metrics                    #####
#####                                                           #####
######################################################################

# Reference gages to model low flow metrics using random forests
# minimum 7-day moving average and timing
# zero flow days is not predicted well enough to include in CEFF


cat("\014") ;  rm(list=ls())

options(max.print=9999999)

## Begin modeling
library(randomForest);library(dplyr);library(data.table);library(foreach); library(stats)
library(ranger);library(doParallel); registerDoParallel(cores=4); library(ggplot2)
library(ggplot2); library(tidyverse); library(VSURF); library(varSelRF); library(caret); library(Boruta)


## Load data with all FFM observations and associated watershed variables
drymets <- c("DS_Mag_90","DS_Mag_50","Min-7", "Min-7_Start","DS_Dur_WS","DS_Tim")
timing <- c("Min_7_Date","Zero-Flow","Zero_Date","DS_Tim","DS_Dur_WS")

# file for predictors
met <- read.csv("C:\\Work\\Data\\Functional Flows\\Met files\\met_dryseason_point1_5condays_15percentofyears_reference.csv",header=T)


# Use for ceff sites with more than 10 years of data available (201 vs. 219)
clus <-  read.csv("C:\\Work\\Data\\Functional Flows\\Streamflow_classification_5condays_dryseason.csv",header=T)
sites = unique(clus$site_no)

# loop through all functional flows here or just select which metrics using drymets
for(m in 1:length(drymets)){
#
m=3
curmet <- drymets[m]

#dir.create(paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Variable Selection\\Geology\\" ,curmet,""))
#dir.create(paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Model Performance\\Geology\\",curmet,""))

tmet<-subset(met,Stat==curmet)

cor.results <- matrix(NA,nrow=length(unique(tmet$ID)),5)



# Begin Modeling ----------------------------------------------------------

curmet.list <- list()

# Begin Modeling ----------------------------------------------------------
# selected a reduced set of predictors for modeling
new <- tmet[,c("ID","Year","Value", "DRAIN_SQKM",
                   
                   "ppt_Oct.wy", "ppt_Nov.wy", "ppt_Dec.wy",
                   "ppt_Oct.nwy", "ppt_Nov.nwy", "ppt_Dec.nwy", "ppt_Jan.wy","ppt_Feb.wy",
                   "ppt_Mar.wy", "ppt_Apr.wy", "ppt_May.wy", "ppt_Jun.wy", "ppt_Jul.wy","ppt_Aug.wy", "ppt_Sep.wy",
                   
                   "tav_Oct.wy" , "tav_Nov.wy" , "tav_Dec.wy",
                   "tav_Oct.nwy" , "tav_Nov.nwy" , "tav_Dec.nwy" , "tav_Jan.wy", "tav_Feb.wy","tav_Mar.wy" ,"tav_Apr.wy",
                   "tav_May.wy","tav_Jun.wy" ,"tav_Jul.wy" , "tav_Aug.wy","tav_Sep.wy",
                   
                   #"run_ann.wy","run_fall.wy","run_wint.wy",  "run_sprg.wy" , "run_summ.wy",
                   "run_sum1" , "run_sum2",   "run_sum3", "run_sum4" ,     
                   
                   "tav_sum1", "tav_sum2" , "tav_sum3",  "tav_sum4", 
                   "tav_sprg.wy", "tav_fall.wy", "tav_wint.wy", 
                   "tav_ann.wy",
                   
                   "ppt_sprg.wy", "ppt_fall.wy", "ppt_wint.wy",
                   "ppt_sum1", "ppt_sum2", "ppt_sum3", "ppt_sum4",
                   "ppt_ann.wy",
                   
                   
                   "ANN_CONS_WETDAYS", 
                   
                   "ET", "PET","RH_BASIN", "PPTAVG_BASIN" ,"T_AVG_BASIN", "T_MIN_BASIN" , "T_MAX_BASIN","AnnMinPrecip","AnnMaxPrecip",
                   
                   # watershed characteristcs
                   "SLOPE_PCT_30M","ELEV_MEAN_M_BASIN_30M","ELEV_MIN_M_BASIN_30M","ELEV_MAX_M_BASIN_30M","ElvRng","ECO3",
                   "SNOW_PCT_PRECIP", "FST32F_BASIN","LST32F_BASIN",  "CONTACT", "BFI_AVE","TOPWET",             
                   "PERDUN","RFACT","PERHOR",  "DEPTH_WATTAB" ,"SILTAVE", "CLAYAVE" ,"SANDAVE",
                   
                   "KFACT_UP",    "NO10AVE",  "NO200AVE", "NO4AVE" , "OMAVE", "PERMAVE" , "ROCKDEPAVE" , "BDAVE", "AWCAVE","WTDEPAVE" ,          
                   "quarternary", "sedimentary" ,"volcanic", "gneiss", "granitic", "ultramafic",
                   
                   "KO_pct", "CaO_pct", "FeO_pct", "MgO_pct", "P_pct","S_pct","SiO_pct" , "UCS" ,"Lperm", "RECHARGE",  "pmpe",
                   "BDMAX",  "PERMH_WS",  "KRUG_RUNOFF",
                   "HGA" ,"HGB" , "HGC","HGD",
                   
                   "HLR1","HLR2", "HLR3", "HLR4","HLR5", "HLR6","HLR7", "HLR8", "HLR9","HLR10", "HLR11","HLR12", "HLR13", "HLR14","HLR15",
                   "HLR16","HLR17","HLR18","HLR19", "HLR20"
                   
)]




new$Value = as.numeric(new$Value)


registerDoParallel(cores=4)

foreach(idx=1:length(sites))%dopar%{
  
  
  clist<-sites[-idx]
  train<-new[new$ID %in% clist,]
  test<-new[new$ID %in% sites[idx],]
  
  # x var needs to be altered for timing metrics (not scaled by DA)
  
  if(curmet %in% timing){
    train$Value <- train$Value
    test$Value <- test$Value
  } else{
    # add one bc often have zero flow vals
    train$Value <- (train$Value+1)/train$DRAIN_SQKM
    test$Value <- (test$Value+1)/test$DRAIN_SQKM
  }
  
  
  library(randomForest)
  
  rf.modcur <- randomForest(train$Value ~., train[,!(colnames(train) %in% c("ID","Year","Value","DRAIN_SQKM"))], ntrees=300)
  
  ### Save variable importance
  varimp<-as.data.frame(rf.modcur$importance)
  varimp$Variable = row.names(varimp)
  
  write.csv(varimp, file = paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Variable Selection\\Reduced (fixed)\\" ,curmet,"\\",unique(test$ID),".csv"),row.names=F)
  
  
  ### Predict to validation sites and save median, 10th, 25th, 75th, & 90th percentiles
  preds <- predict(rf.modcur, test, type="response",predict.all=T)
  predp50<-apply(preds$individual,1,median)
  predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
  predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
  predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
  predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
  rf.cor <- cor(predp50,test$Value)^2
  
  
  val<-data.frame(ID=test$ID,WY=test$Year,Obs=test$Value,AREA=test$DRAIN_SQKM,
                  p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90,
                  rf.cor ) # ,bayescor,bf.mod$yhat.test.mean)
  
  
  write.csv(val,paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Model Performance\\Reduced (fixed)\\",curmet,"\\",unique(test$ID),"_valpreds.csv"),row.names=FALSE)
  
  rm(preds,val)
}



# Model performance -------------------------------------------------------

files1 <- list.files(path = paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Variable Selection\\Reduced (fixed)\\",curmet,""))
files2 <- list.files(path=paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Model Performance\\Reduced (fixed)\\",curmet,"\\"))

sites = substr(files2,1,8)
r.results <- matrix(NA,nrow=length(sites),4)

for(s in 1:length(sites)){
  #dat1 <- read.csv(paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Variable Selection\\model performance vars by site\\" ,curmet,"\\",site,".csv"),header=T)
  site = sites[s]
  
  dat2 <- read.csv(paste0("C:\\Work\\CEFF Metrics and Models\\Results\\Model Performance\\Reduced (fixed)\\",curmet,"\\",site,"_valpreds.csv"),header=T)
  #corbayes = unique(dat2$bayescor)
  
  rfcor = cor(dat2$Obs,dat2$p50)^2
  
  #rfcor = cor(dat2$Obs,dat2$p50)^2
  
  r.results[s,1] <- site
  #  r.results[s,2] <- as.numeric(cor(dat2$Obs,dat2$p50))
  r.results[s,3] <- paste0(curmet)
  r.results[s,4] <- rfcor
  #  r.results[s,5] <- corbayes
  
}

colnames(r.results) <- c("ID","R","Metric","R2")

all.results <- data.frame(r.results)
all.results$R2 = as.numeric(all.results$R2)

allbasins <- read.csv("C:/Work/Data/USGS gages/EFF_Reference_Gages.csv")
allbasins$ID <- as.character(substr(allbasins$ID,2,length(allbasins$ID)))

allcor <- left_join(all.results, allbasins,by="ID")
allcor <- allcor[complete.cases(allcor$LATITUDE),]

#Transform lat/long data for EFF data
pts = allcor[,c("LONGITUDE","LATITUDE")]
coordinates(pts) = ~LONGITUDE+LATITUDE
projection(pts) <- CRS("+init=epsg:4326")
Long = allcor$Long 
Lat = allcor$Lat 


classes_R = seq(-1,1,0.1) # Only when I will have negative values I will be able to put -1 as initial value of cuts
n_cuts = length(classes_R)-1
block1 = rev(colorRampPalette(c("lightblue2","blue","darkblue"),bias=0.5)(n_cuts/2-1))
block3 = colorRampPalette(c("yellow2","red","darkred"),bias=0.5)(n_cuts/2-1)
block2 = colorRampPalette(c("white","white"))(2)
my.palette_R = c(block1,block2,block3)

#allcor = allcor[which(allcor$ID %in% c("11115500","11116000")),]

R_values =  
  ggplot()+
  geom_polygon(data=ca,aes(x=long,y=lat,group=group),colour="grey10",fill="grey90",size=0.6)+
  geom_point(data=allcor, aes(x=LONGITUDE, y=LATITUDE,fill=R2),shape=21,size=6)+
  scale_shape_manual(values=c(21,24),name="")+
  scale_color_manual(values =c("grey50","grey50"),name="")+
  #  scale_fill_manual(values= c("white","red"),name="")+
  scale_fill_gradientn(colors =my.palette_R, breaks=round(seq(-1,1,0.1),3), limits=c(-1,1),
                       guide = guide_colorbar(barwidth = 1, barheight = 20), oob = scales::squish, name="R2") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        # legend.title=element_blank(),R_values
        strip.background = element_rect(fill="grey30", size=0.5, linetype="solid"), # Pr #308fd8  Ob #4598ba
        #   panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_text(size=rel(1.25),color="white"))


ggsave(paste0("R_Cor_Obs_",curmet,"_streamclass_longtermavg.png"), plot=R_values, device="png", path = "C:\\Work\\CEFF Metrics and Models\\Figures\\Model Performance Correlation Maps",
       width = 9, height = 9, dpi=300, limitsize=TRUE)





# For fast model predictions ----------------------------------------------




#Split data into training and testing
spec = c(train = .8, test = .2) # can add third with validation =.2

g = sample(cut(
  seq(nrow(new)), 
  nrow(new)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(new, g)
train <- data.frame(res[[1]]); test <- data.frame(res[[2]])



# x var needs to be altered for timing metrics (not scaled by DA)

if(curmet %in% timing){
  xtrain <- train$Value
  xtest <- test$Value
} else{ 
  xtrain <- train$Value/train$DRAIN_SQKM
  xtest <- test$Value/test$DRAIN_SQKM
}



mod = train[,!(colnames(train) %in% c("Value","DRAIN_SQKM"))]

library(randomForest)
#rf.modcur <- randomForest(xtrain ~., mod, ntrees=200)
### Save variable importance
#varimp<-as.data.frame(rf.modcur$importance)
#varImpPlot(rf.modcur)

# Run Ranger forest model (shorter)
rf2 <-ranger(xtrain~.,mod, num.trees=200,importance = "impurity",quantreg=TRUE)
varimp <- data.frame(importance(rf2))

# this part just creates the data.frame for the plot part
library(dplyr)
varimp$Variable = rownames(varimp)
varimp = data.frame(varimp[order(-varimp[,1]),])
varimp = varimp[1:20,]
colnames(varimp) <- c("Importance","Variable")


# this is the plot part, be sure to use reorder with the correct measure name
variableimp <- ggplot(varimp, aes(x=reorder(Variable, Importance), y=Importance)) + 
  geom_point(pch=21,color="blue",fill=NA) +
  geom_segment(aes(x=Variable,xend=Variable,y=0,yend=Importance),color="blue") +
  scale_color_manual() +
  ylab("IncNodePurity") +
  xlab("Variable Name") +
  coord_flip()+
  labs(title=paste0(curmet))+
  theme(panel.background = element_blank(),
        axis.text = element_text(size=15),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill="grey30", size=2, linetype="solid"), # Pr #308fd8  Ob #4598ba
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.major = element_blank(),
        strip.text = element_text(size=rel(3),color="white"),
        legend.position = "none")

ggsave(paste0("Variable_Importance_",curmet,"_reduced.png"),variableimp, device="png", 
       path = "C:\\Work\\CEFF Metrics and Models\\Figures\\Variable Selection",
       width = 6, height = 6, dpi=300, limitsize=TRUE)

### Predict to validation sites and save median, 10th, 25th, 75th, & 90th percentiles
preds<-predict(rf2,data = test, type = "quantiles",quantiles = c(0.5,0.9))
predp50<-apply(preds$predictions,1,median)

#preds<-predict(rf.modcur,test,predict.all=TRUE)
#predp50<-apply(preds$individual,1,median)
cur.vals<-data.frame(Obs=xtest,p50=predp50)#preds$predictions[1:length(xtest),1])


#predp50<-apply(preds$individual,1,median)
#predp10<-apply(preds$individual,1,function(x) quantile(x,probs=c(0.1,0.25))

R.rf.cur <- cor(cur.vals$p50,cur.vals$Obs); print(R.rf.cur)

lims <- range(c(xtest,cur.vals$p50))
xlim = quantile(lims,probs=0.7)
ylim = quantile(lims, probs=0.95)

one <- 
  ggplot(cur.vals)+
  geom_point(aes(x=p50, y=Obs),size=3,color="black",fill="blue",pch=21)+
  scale_x_continuous(name="Observations (scaled by DA)", limits=lims, expand = c(0,0)) +
  scale_y_continuous(name="Predictions (scaled by DA)", limits=lims, expand = c(0,0)) +
  geom_abline(slope=1, intercept=0)+
  geom_text(aes(label=paste0("R=",round(R.rf.cur,4)), x=xlim,y=ylim), size=8, hjust = 0.09, vjust = 1.9,color="#a50f15")+
  theme(panel.background = element_blank(),
        # axis.title = element_text(size=20,margin=margin(t=0, r=20,b=0,l=0)),
        axis.text = element_text(size=15),
        # panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # panel.spacing = unit(0.9, "lines"),
        strip.background = element_rect(fill="grey30", size=2, linetype="solid"), # Pr #308fd8  Ob #4598ba
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.major = element_line(colour="grey70", size=0.5),
        strip.text = element_text(size=rel(3),color="white"),
        legend.position = "none")

print(one)

ggsave(paste0("Mine_Climate_WSChars_Variables_",curmet,"_reduced.png"),one, device="png", 
       path = "C:\\Work\\CEFF Metrics and Models\\Figures\\Variable Selection",
       width = 6, height = 6, dpi=300, limitsize=TRUE)

}

# Plot intermitent and perennial ------------------------------------------


library(rgdal); library(raster)

#State Shapefile
pathfm = "C:/Work/Data/Shapefiles/California State"
usa <- readOGR(pathfm, layer="CA_State_TIGER2016")
usa <- spTransform(usa,CRS("+init=epsg:4326"))
ca = fortify(usa)

setwd("C:\\Work\\Data\\Low Flow Metrics Ref Period 7-day")
files <- list.files()

lowvals <- function(low){
  site = substr(unlist(strsplit(as.character(files[low]),"/"))[[1]],1,8)  #within original temp, extracts into characters separated by / and extracts in the fourth position the characters in the position of 1 to 7
  temp1=read.table(files[low],sep=",",header=TRUE)
  temp1$site = paste0(site)
  
  avg <- mean(temp1$Zero_Flow_Days)
  
  if(avg>15){
    lf <- temp1[,c("site","Year","Zero_Flow_Days","Zero_Flow_Start_Date_WY")]
    lf$Metric <- paste0("Intermittent")
  } else{
    lf <- temp1[,c("site","Year","Min_7_Day_Mov_Avg","Min_7_Start_Date_WY")]
    lf$Metric <- paste0("Perennial")
  }
  
  colnames(lf) <- c("site","Year","Value","Start Date","Metric")
}


lowdf = lapply(1:length(files), function(i){ lowvals(i)  }) #applies function to length of list
lowdf <- rbindlist(lowdf)


#counts <- peaks %>% group_by(high) %>% summarize(count=n())
allbasins <- read.csv("C:/Work/Data/USGS gages/EFF_Reference_Gages.csv")
allbasins$site <- as.character(substr(allbasins$ID,2,length(allbasins$ID)))

avg <- lowdf[,c(1,2,4,6)] %>%
  group_by(ID) %>%
  summarize(Mean = as.numeric(mean(Value)), Metric=unique(Metric))

allvals <- merge(data.frame(avg),allbasins, by.x=c("ID"), by.y = c("site")) 


# plot all vals 
Mu_Ds = 
  ggplot(allvals, aes(x=LONGITUDE,y=LATITUDE))+
  geom_polygon(data=ca,aes(x=long,y=lat,group=group),colour="black",fill="grey")+
  #  geom_point(data= mon.con, aes(x=Long, y = Lat),fill="white", color="black",shape=21, size = 0.75) +
  geom_point(aes(fill = Metric),color="grey50",size=3,pch=21) + 
  # scale_fill_gradientn(colors =my.palette_R, breaks=round(seq(0,0.2,0.02),2), limits=c(0,0.2),
  #                     guide = guide_colorbar(barwidth = 1, barheight = 8), name="NDVI") +
  scale_fill_manual(values = c("Intermittent"="red","Perennial"="blue"))+  #Scale for high/low months
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.7,0.8),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        #strip.background = element_rect(fill="grey30", size=0.3, linetype="solid"), # Pr #308fd8  Ob #4598ba
        #panel.border = element_rect(colour = "black", fill=NA),
        strip.text = element_text(size=rel(1),color="white"))

ggsave("Inter_Perennial_Gage_Locations.png", plot=Mu_Ds, device="png", path = "C:\\Work\\CEFF Metrics and Models\\Figures\\Annual Mets",
       width = 8, height = 8, dpi=300, limitsize=TRUE)


# Principal components analysis
library(ggbiplot)

tmet = tmet[-c(204:244)]
tmet2 = tmet[,-which(colnames(tmet) %in% c("HLR1","HLR2", "HLR3", "HLR4","HLR5", "HLR6","HLR7", "HLR8", "HLR9","HLR10", "HLR11","HLR12", "HLR13", "HLR14","HLR15",
                                           "HLR16","HLR17","HLR18","HLR19", "HLR20",
                                           "PCT", "HGAC", "HGAD","HGBC", "HGBD","HGCD","HGVAR",
                                           "anorthositic", "intermediate" 
))]

mtcars.pca <- prcomp(tmet2[,c(8:164)], center = TRUE,scale. = TRUE)
all.pca <- ggbiplot(mtcars.pca, varname.size = 5, alpha=0.01)
ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4), varname.size = 5, alpha=0.01)



ggsave(paste0("PCA_all.png"),all.pca, device="png", 
       path = "C:\\Work\\CEFF Metrics and Models\\Figures\\Variable Selection",
       width = 10, height = 6, dpi=300, limitsize=TRUE)


mtcars.pca2 <- prcomp(reduced[,2:90], center = TRUE,scale. = TRUE)
reducted.pca <- ggbiplot(mtcars.pca2, varname.size = 5, alpha=0.1,scale=T)
ggbiplot(mtcars.pca2,ellipse=TRUE,choices=c(3,4), varname.size = 5, alpha=0.01)


ggsave(paste0("PCA_reduced.png"),reducted.pca, device="png", 
       path = "C:\\Work\\CEFF Metrics and Models\\Figures\\Variable Selection",
       width = 10, height = 6, dpi=300, limitsize=TRUE)

reduced = reduced[complete.cases(reduced),]
x=(reduced$Value/reduced$DRAIN_SQKM)+1

library(gamlss)
mod1 <- gamlss(x ~ ., data = reduced[,-c(1:2)], family = GA(mu.link="log"))
#Model to selection for Mu parameter
mod2<-stepGAICAll.B(mod1,what="mu", k=log(length(x)))
mod.mu <- summary(mod2)

mu.pars = substring(mod2$call[2],14)

sregr = data.frame(summary(mod2))
