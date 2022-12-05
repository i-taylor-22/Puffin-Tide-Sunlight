# Set current working directory.
setwd("C:/Users/Bella Taylor/Desktop/Honours Project/Data")

# Get and print current working directory.
print(getwd())

#install  packages
install.packages("readxl")
install.packages("zoo")
install.packages("Rtools")             
install_github("cran/weathermetrics")  
install.packages("ggcorrplot") 
install.pacakes ("glmmTMB")
install.packages("blmeco")
install.packages("AICcmodavg")
install.packages("performance")



# load packages
library(readxl)
library(zoo) 
library(weathermetrics)               
library(ggcorrplot) #load package
library(ggplot2)      #Download 'ggplot2'
library(tidyverse)
library(lme4)
library(merTools)
library(glmmTMB) # fitting generalised linear mixed models
library(bbmle) # general maximum likelihood estimation
library(ggthemes)
library(showtext)
library(blmeco)
library(AICcmodavg)
library(performance)


#cite packages
citation(package = "weathermetrics")  
citation(package = "ggcorrplot")
citation(package = "ggplot2")
citation(package = "glmmTMB")
citation(package = "AICcmodavg")
citation(package = "performance")






#load data
csvdata <- read.csv("Extracted photo data from cameras.csv")  # print(data)
#print(ncol(csvdata)) # Columns
#print(nrow(csvdata)) # Rows

#create a subset of the photo data
csvdata2=csvdata[, c("Camera_ID","Location","Date","Time","Temperature","Number_of_puffins_in_frame","Number_of_uncertain_puffins_in_frame","Number_of_puffins_in_sure_area","Number_of_puffins_in_relatively_sure_area","Number_of_puffins_in_least_sure_area") ]
str(csvdata2$Date)

#Examine the incorrect date (from camera at 171 cliffs in 2022)
test<- csvdata2[ which(csvdata2$Location=="171 cliffs"),]
test$year<-str_sub(test$Date,-4,-1)
test$month<-str_sub(test$Date,1,2)
day<- str_sub(test$Date,1,5)
day<- str_sub(day,-2,-1)
test$day<-day

table(test$year,test$month)
names(test)

test$Date<- as.POSIXct(test$Date, format="%m/%d/%Y", tryFormats = c( "%m/%d/%Y"))
test$julian<-as.POSIXlt(test$Date)$yday
#see that data from 2022 and 2023 is not where it should 
ggplot(test, aes(julian, Temperature)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(test$julian,test$year)
#the 9th of june is the 160 julian day
#the first value of julian date for 2022 is 342 - should be 160 (342-160=182)
test[which(test$julian>300),14]<- test[which(test$julian>300),14]-182
table(test$julian,test$year)
#the first value of julian date for 2023 is 0 -  it should be 183 which is the latest value it takes once modified
test[which(test$julian<47),14]<- test[which(test$julian<47),14]+183
table(test$julian,test$year)
test[which(test$year==2023),11]<- 2022
#check that it has worked 
table(test$julian,test$year)

#see that data from 2022 and 2023 is now where it should
ggplot(test, aes(julian, Temperature)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#get the test data back to the main dataset 
csvdata2[ which(csvdata2$Location=="171 cliffs"),]<- test[,c(1:10)] 
csvdata2$Date<- as.POSIXct(csvdata2$Date, format="%m/%d/%Y", tryFormats = c( "%m/%d/%Y"))
#not sure why it needs the +1 but when checking the dates for consistency it look like they were missing one day.
csvdata2$julian<-as.POSIXlt(csvdata2$Date)$yday
table(csvdata2$julian)
csvdata2$year<-format(as.Date(csvdata2$Date, format="%m/%d/%Y"),"%Y")
csvdata2$month<-format(as.Date(csvdata2$Date, format="%m/%d/%Y"),"%m")
csvdata2$day<-format(as.Date(csvdata2$Date, format="%m/%d/%Y"),"%d")
csvdata2$Time
csvdata2$DATE<-""
names(csvdata2)
for(i in 1:dim(csvdata2)[1]){
  csvdata2[i,15]<-paste(
    csvdata2[i,14],"/",
    csvdata2[i,13],"/",
    csvdata2[i,12]," ",
    csvdata2[i,4], sep="" ) }
as.POSIXct("4/27/2016 11:59:59 AM", format="%m/%d/%Y %I:%M:%S %p")
as.POSIXct("4/27/2016 11:59:59 AM", format="%d/%m/%Y %I:%M %p")
csvdata2[,15]<-as.POSIXct(csvdata2[,15], format="%d/%m/%Y %I:%M %p",tz="GMT")
library(lubridate)
csvdata2[,15]<-csvdata2[,15]+days(1)
#check that dates are ok
View(csvdata2)

#remove columns that I created to modify your dataset
csvdata2[,3] <-csvdata2[,15]
csvdata2<-csvdata2[,-c(11:15)]
View(csvdata2)


#load sunlight data
sunlight_filename = "C:/Users/Bella Taylor/Desktop/Honours Project/Data/Sunlight data.xlsx"
sunlight_df <- read_excel(sunlight_filename) #read sunlight excel file
#load tide data
tide_filename = "C:/Users/Bella Taylor/Desktop/Honours Project/Data/Tide data.xlsx"
tide_df <- read_excel(tide_filename)  #read tide excel file




#create a subset of sunlight
sunlight_df_GMT=sunlight_df[, c("Date","Sunrise_BST","Sunset_BST","Solar_noon_BST") ]
tac<-paste(ymd(strptime(as.character(sunlight_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(sunlight_df$Sunrise_BST), format = "%H:%M:%S"))
head(tac)
head(sunlight_df_GMT)

sunlight_df_GMT$Sunrise_BST <-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")-hours(1)  # subtract 1 hour to convert BST to GMT
head(sunlight_df_GMT)


tac<-paste(ymd(strptime(as.character(sunlight_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(sunlight_df$Sunset_BST), format = "%H:%M:%S"))
sunlight_df_GMT$Sunset_BST <-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")-hours(1) # subtract 1 hour to convert BST to GMT


tac<-paste(ymd(strptime(as.character(sunlight_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(sunlight_df$Solar_noon_BST), format = "%H:%M:%S"))
sunlight_df_GMT$Solar_noon_BST <-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")-hours(1) # subtract 1 hour to convert BST to GMT

# colnames(sunlight_df_GMT)[colnames(sunlight_df_GMT)=="Sunrise_BST"]     <- "Sunrise_GMT"
# colnames(sunlight_df_GMT)[colnames(sunlight_df_GMT)=="Sunset_BST"]      <- "Sunset_GMT"
# colnames(sunlight_df_GMT)[colnames(sunlight_df_GMT)=="Solar_noon_BST"]  <- "Solar_noon_GMT"
colnames(sunlight_df_GMT)    <- c("Date"  ,"Sunrise_GMT" , "Sunset_GMT","Solar_noon_GMT")

#Repeat the same logic for the tide data (but just change the date format, the tide is already in GMT)
names(tide_df)
tac<-paste(ymd(strptime(as.character(tide_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(tide_df$AM_high_GMT), format = "%H:%M:%S"))
tide_df$AM_high_GMT <-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")

tac<-paste(ymd(strptime(as.character(tide_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(tide_df$AM_low_GMT), format = "%H:%M:%S"))
tide_df$AM_low_GMT<-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")

tail(tide_df$AM_low_GMT)
tail(tac)

tac<-paste(ymd(strptime(as.character(tide_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(tide_df$PM_high_GMT), format = "%H:%M:%S"))
tide_df$PM_high_GMT<-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")

head(tide_df$PM_high_GMT)
head(tac)


tac<-paste(ymd(strptime(as.character(tide_df$Date), "%Y-%m-%d")), 
           format(as.POSIXct(tide_df$PM_low_GMT), format = "%H:%M:%S"))
tide_df$PM_low_GMT<-as.POSIXct(tac, format="%Y-%m-%d %H:%M:%S",tz="GMT")

tail(tide_df$PM_high_GMT)
tail(tac)
head(tide_df)



#cite weathermetrics
citation(package = "weathermetrics")  

print( fahrenheit.to.celsius(csvdata2$Temp) )   #print converted temperature in farenheight to celsius (where appropriate)

#print temperature in farenheight to celsius (where relevant)
csvdata2$TempC = ifelse( csvdata2$Temp>=41 & csvdata2$Temp<=88,  fahrenheit.to.celsius(csvdata2$Temp), csvdata2$Temp )

names(csvdata2)


#Convert the csvdata2 time from 12hrs to 24hrs (this has already been done in the date column but I need it in a separate column for later)
csvdata2$Time24hrs=format(as.POSIXct(csvdata2$Time,format='%I:%M %p'),format="%H:%M")


#create subset of full photo data to manipulate only the last 3 columns
csvdata3=csvdata2
#remove NA values from the csvdata2 dataset (if NA is in 'Number_of_puffins_in_frame' or 'Number_of_uncertain_puffins_in_frame')
toremove<-which(is.na(csvdata2$Number_of_puffins_in_frame)) #set which column the data is to be removed from
datawithoutNAPuffinfram<-csvdata2[-toremove,]               #create new dataset with NAs removed from this column

#Note for lines 115-117 = If a NA value is in 'Number_of_puffins_in_frame' then it will also be NA in 'Number_of_uncertain_puffins_in_frame'; SO only have to set the chosen column as 'Number_of_puffins_in_frame'


#remove NA values from the csvdata3 dataset (if NA in 'Number_of_puffins_in_sure_area','Number_of_puffins_in_relatively_sure_area', or 'Number_of_puffins_in_least_sure_area')
toremove<-which(is.na(csvdata3$Number_of_puffins_in_sure_area))
datawithoutNAPuffinAcross3SectionsOfFram<-csvdata3[-toremove,]

#Note for lines 122-124 = If a NA value is in 'Number_of_puffins_in_sure_area' then it will also be NA in 'Number_of_puffins_in_relatively_sure_area' and 'Number_of_puffins_in_least_sure_area'; SO only have to set the chosen column as 'Number_of_puffins_in_sure_area'



head(tide_df)
tide_df$D<- ymd(strptime(as.character(tide_df$Date), "%Y-%m-%d"))
sunlight_df_GMT$D<-ymd(strptime(as.character(sunlight_df_GMT$Date), "%Y-%m-%d"))


#merge tide and sunlight_df_GMT data files
Tide_and_sunlight_df <- merge( tide_df, sunlight_df_GMT, by.x=c("D"), by.y=c("D") )
#Check it worked
head(sunlight_df_GMT)
head(tide_df)
head(Tide_and_sunlight_df)
Tide_and_sunlight_df<-Tide_and_sunlight_df[,-c(1,11)]
names(Tide_and_sunlight_df)<-c("Date","AM_high_GMT","AM_high_height_m", 
                               "AM_low_GMT","AM_low_height_m" , "PM_high_GMT",     
                               "PM_high_height_m", "PM_low_GMT","PM_low_height_m" , 
                               "Sunrise_GMT" ,     "Sunset_GMT"    ,   "Solar_noon_GMT") 

names(datawithoutNAPuffinfram)
names(Tide_and_sunlight_df)
str(datawithoutNAPuffinfram)
Tide_and_sunlight_df$D<- ymd(strptime(as.character(Tide_and_sunlight_df$Date), "%Y-%m-%d"))
datawithoutNAPuffinfram$D<-ymd(strptime(as.character(datawithoutNAPuffinfram$Date), "%Y-%m-%d"))

#merge tide+sunlight data file with datawithoutPuffinfram; if you don't create a column before that is char it doesn't merge properly 
Puffins_and_everything_df_wholeFrame <- merge( datawithoutNAPuffinfram, Tide_and_sunlight_df, by.x=c("D"),by.y=c("D") )
Puffins_and_everything_df_wholeFrame

str(Puffins_and_everything_df_wholeFrame) #check stucture of dataframe

#format Time24hrs as a POSIX class (I've already done this but I need it in a separate column for later on) 
Puffins_and_everything_df_wholeFrame$POSIXtime24hrs = as.POSIXct(Puffins_and_everything_df_wholeFrame$Time24hrs, format="%H:%M", tz="GMT")

#create copy of Puffins_and_everything_df_wholeFrame
Puffins_and_everything_df_wholeFrame2=Puffins_and_everything_df_wholeFrame

#Delete any rows from the tide data which are missing data (column value as "NA")
Puffins_and_everything_df_wholeFrame2 <- Puffins_and_everything_df_wholeFrame2[ -which(is.na(Puffins_and_everything_df_wholeFrame2$AM_high_GMT)), ]
Puffins_and_everything_df_wholeFrame2 <- Puffins_and_everything_df_wholeFrame2[ -which(is.na(Puffins_and_everything_df_wholeFrame2$AM_low_GMT)), ]
Puffins_and_everything_df_wholeFrame2 <- Puffins_and_everything_df_wholeFrame2[ -which(is.na(Puffins_and_everything_df_wholeFrame2$PM_high_GMT)), ]
Puffins_and_everything_df_wholeFrame2 <- Puffins_and_everything_df_wholeFrame2[ -which(is.na(Puffins_and_everything_df_wholeFrame2$PM_low_GMT)), ]


#adjust plot margins
par(mar = c(1, 1, 1, 1))

#check for outliers in 'Number of puffins in frame'
ggplot(Puffins_and_everything_df_wholeFrame2) +
  aes(x = "", y = Number_of_puffins_in_frame) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


#check 'Number of puffins in frame' for normality
qqnorm(Puffins_and_everything_df_wholeFrame2$Number_of_puffins_in_frame)
qqline(Puffins_and_everything_df_wholeFrame2$Number_of_puffins_in_frame)

 

#Explore the data and see if things make sense and provide some plots  
Puffins_and_everything_df_wholeFrame2$Julian<-as.POSIXlt(Puffins_and_everything_df_wholeFrame2$Date.x)$yday
names(Puffins_and_everything_df_wholeFrame2)
data<-Puffins_and_everything_df_wholeFrame2

#table(data$Camera,data$Location)
d <- data %>% 
  group_by(c(Julian )) %>% 
  summarise(
    VAR1 = mean(TempC))
names(d)<-c("julian","meanT")

ggplot(d, aes(julian, meanT)) +
  geom_point() 


data$year<-year(data$Date.x)
data$hour<-hour(data$Date.x)
tad<-data.frame(c("00", "05",
                  "10", "15",
                  "20", "25",
                  "30", "35",
                  "40", "45",
                  "50", "55"),
                lab=names(table(trunc(minute(data$Date.x)/5)))
)
names(tad)<-c("interval","lab")
data$interval<-trunc(minute(data$Date.x)/5)
data$interval2<-data$interval
#get data formatted for regular time intervals. 
names(data)
for(i in 1:dim(data)[1]){
  data[i,30]<-tad[which(tad[,2]==data[i,29]),1]
}
data$TimeInteval<-paste(hour(data$Date.x),":",data$interval2,":00",sep="")

data$TimeInteval3<-as.POSIXlt(data$TimeInteval, tz = "", format="%H:%M:%S",
                              tryFormats = c("%H:%M:%S"),
                              optional = FALSE)
head(data)

data$Date
d <- data %>% 
  group_by(c(year )) %>% 
  summarise(
    VAR1 = max(Number_of_puffins_in_frame))

d
d <- data %>% 
  group_by(c(data$TimeInteval )) %>% 
  summarise(
    VAR1 = max(Number_of_puffins_in_frame))
names(d)<-c("time","N")
str(d)
d$time<-format(d$time,format="%H:%M:%S") 
ggplot(d, aes(time, N)) +                   #plot time of day against number of puffins
  geom_point()

#plot height of PM low tide against number of puffins
ggplot(Puffins_and_everything_df_wholeFrame2, aes(PM_low_height_m, Number_of_puffins_in_frame)) +
  geom_point() +
  labs(x="Height of PM low tide (m)", y="Number of puffins")

#plot julian date against number of puffins
ggplot(Puffins_and_everything_df_wholeFrame2, aes(Julian, Number_of_puffins_in_frame)) +
  geom_point() +
  labs(x="Day (Julian)", y="Number of puffins")

#plot height of AM tide height against number of puffins
ggplot(Puffins_and_everything_df_wholeFrame2, aes(AM_high_height_m, Number_of_puffins_in_frame)) +
  geom_point() +
  labs(x="Height of AM high tide (m)", y="Number of puffins")




str (Puffins_and_everything_df_wholeFrame2)  #check structure & class of the entire dataframe
Puffins_and_everything_df_wholeFrame2$Location <- as.factor(Puffins_and_everything_df_wholeFrame2$Location)  #change class to factor
class(Puffins_and_everything_df_wholeFrame2$Location) #check class (make sure its a factor)
Puffins_and_everything_df_wholeFrame2$Camera_ID <- as.factor(Puffins_and_everything_df_wholeFrame2$Camera_ID) #change class to factor
class(Puffins_and_everything_df_wholeFrame2$Camera_ID) #check class (make sure its a factor) 



#Convert the Time24hrs from POSIX to numeric class, and create a new column for this
Puffins_and_everything_df_wholeFrame2$Time24hrs_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$POSIXtime24hrs) %% 86400) / 3600


#Convert the sunrise_GMT from POSIX to numeric class and create new column as sunrise hours past midnight
Puffins_and_everything_df_wholeFrame2$Sunrise_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$Sunrise_GMT) %% 86400) / 3600


#Convert the sunset_GMT from POSIX to numeric class and create a new column as sunset hours past midnight
Puffins_and_everything_df_wholeFrame2$Sunset_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$Sunset_GMT) %% 86400) / 3600

#Convert the sunset_GMT from POSIX to numeric class and create a new column as solar noon hours past midnight
Puffins_and_everything_df_wholeFrame2$Solar_noon_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$Solar_noon_GMT) %% 86400) / 3600


#create a subset of the Puffins_and_everything df with just the hrs past midnight sunlight data
sunlight_hpm_df=Puffins_and_everything_df_wholeFrame2[, c("Sunrise_HPM", "Sunset_HPM", "Solar_noon_HPM") ]

#create a correlation matrix for the sunlight variables
cor(sunlight_hpm_df, method = "spearman")

#visualize correlation matrix
ggcorrplot(cor(sunlight_hpm_df))


#create a new column with time since sunrise
Puffins_and_everything_df_wholeFrame2$Time_since_sunrise = Puffins_and_everything_df_wholeFrame2$Time24hrs_HPM - Puffins_and_everything_df_wholeFrame2$Sunrise_HPM


#Convert the AM_High_tide from POSIX to numeric class and create new column with time of AM high tide in hours past midnight
Puffins_and_everything_df_wholeFrame2$AM_HighTide_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$AM_high_GMT) %% 86400) / 3600

#Convert the AM_low_tide from POSIX to numeric class and create new column with time of AM low tide in hours past midnight
Puffins_and_everything_df_wholeFrame2$AM_LowTide_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$AM_low_GMT) %% 86400) / 3600

#Convert the PM_high_tide from POSIX to numeric class and create new column with time of PM high tide in hours past midnight
Puffins_and_everything_df_wholeFrame2$PM_HighTide_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$PM_high_GMT) %% 86400) / 3600

#Convert the PM_low_tide from POSIX to numeric class and create new column with time of PM low tide in hours past midnight
Puffins_and_everything_df_wholeFrame2$PM_LowTide_HPM = (as.numeric(Puffins_and_everything_df_wholeFrame2$PM_low_GMT) %% 86400) / 3600


#create a subset of the Puffins_and_everything df with just the hrs past midnight tide and tide height data
tide_hpm_and_height_df=Puffins_and_everything_df_wholeFrame2[, c("AM_HighTide_HPM","AM_high_height_m", "AM_LowTide_HPM","AM_low_height_m", "PM_HighTide_HPM","PM_high_height_m", "PM_LowTide_HPM", "PM_low_height_m") ]


#create a correlation matrix for the tide variables
cor(tide_hpm_and_height_df, method = "spearman")

#visualize correlation matrix
ggcorrplot(cor(tide_hpm_and_height_df))

#create a new column with time AM high tide
Puffins_and_everything_df_wholeFrame2$Time_since_AM_HighTide = Puffins_and_everything_df_wholeFrame2$Time24hrs_HPM - Puffins_and_everything_df_wholeFrame2$AM_HighTide_HPM



#Run zero inflated negative binomial  mixed linear model for the chosen tide and sunlight variables, with the number of puffins as the response variable
model0 <- glmmTMB(Number_of_puffins_in_frame ~ 1 + (1|Location) + (1|TempC) + (1|Julian) + (1|POSIXtime24hrs),
                  data=Puffins_and_everything_df_wholeFrame2,
                  ziformula=~1,
                  family=nbinom2)
summary(model0)


model1 <- glmmTMB(Number_of_puffins_in_frame ~ Time_since_AM_HighTide + Time_since_sunrise + Time_since_AM_HighTide*Time_since_sunrise + (1|Location) + (1|TempC) + (1|Julian) + (1|POSIXtime24hrs),
                  data=Puffins_and_everything_df_wholeFrame2,
                  ziformula=~1,
                  family=nbinom2)


model2 <- glmmTMB(Number_of_puffins_in_frame ~ Time_since_AM_HighTide + Time_since_sunrise + (1|Location) + (1|TempC) + (1|Julian) + (1|POSIXtime24hrs),
                  data=Puffins_and_everything_df_wholeFrame2,
                  ziformula=~1,
                  family=nbinom2)


model3 <- glmmTMB(Number_of_puffins_in_frame ~ Time_since_AM_HighTide + (1|Location) + (1|TempC) + (1|Julian) + (1|POSIXtime24hrs),
                  data=Puffins_and_everything_df_wholeFrame2,
                  ziformula=~1,
                  family=nbinom2)

model4 <- glmmTMB(Number_of_puffins_in_frame ~ Time_since_AM_HighTide + (1|Location) + (1|TempC) + (1|Julian) + (1|POSIXtime24hrs),
                  data=Puffins_and_everything_df_wholeFrame2,
                  ziformula=~1,
                  family=nbinom2)


#run ANOVA
anova(model0,model1,model2,model3,model4, best="Chisq")


#define list of models
models <- list(model0, model1, model2, model3, model4)

#assign model names
mod.names <- c("nullmodel", "Tide+Sunrise+Tide*Sunrise", "Tide+Sunrise", "Tide", "Sunrise")

#calculate AIC and delta AIC of each model
aictab(cand.set=models, modnames=mod.names)

#cite Rstudio
RStudio.Version()


#test the goodness of fit of the 'best' models according to the delta AIC values
r2(model1)
r2(model2)


#test for homogeneity of variance in model1 (the model that has both additive aspects and interactions)
resmodel1<-resid(model1)
plot(fitted(model1),resmodel1)+
  abline(0,0)
qqnorm(resmodel1)
qqline(resmodel1)

hist(resmodel1,breaks=37)





#plot height of AM tide height against number of puffins (for practice)

d <- Puffins_and_everything_df_wholeFrame2 %>% 
  group_by(c(AM_high_height_m )) %>% 
  summarise(
    VAR1 = mean(Number_of_puffins_in_frame),
    VAR2=sd(Number_of_puffins_in_frame))

names(d)<-c("height" ,"mean","sd" )

ggplot(d, aes(height, mean)) +
  geom_point() +
  labs(x="Height of AM high tide (m)", y="Number of puffins")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), position=position_dodge(width=0.5))+
  ylim(-4,4)





#plot number of puffins against time since high tide

d <- Puffins_and_everything_df_wholeFrame2 %>% 
  group_by(c(Time_since_AM_HighTide )) %>% 
  summarise(
    VAR1 = mean(Number_of_puffins_in_frame),
    VAR2=sd(Number_of_puffins_in_frame))

names(d)<-c("HighTide" ,"mean","sd" )

ggplot(d, aes(HighTide, mean)) +
  geom_point() +
  labs(x="Time since AM high tide (Hours)", y="Mean number of puffins")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), position=position_dodge(width=0.5))+
  ylim(-4,15)



#plot number of puffins against time since sunrise

d <- Puffins_and_everything_df_wholeFrame2 %>% 
  group_by(c(Time_since_sunrise )) %>% 
  summarise(
    VAR1 = mean(Number_of_puffins_in_frame),
    VAR2=sd(Number_of_puffins_in_frame))

names(d)<-c("Sunrise" ,"mean","sd" )

ggplot(d, aes(Sunrise, mean)) +
  geom_point() +
  labs(x="Time since sunrise (Hours)", y="Mean number of puffins")+
  geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd), position=position_dodge(width=0.5))+
  ylim(-4,15)


