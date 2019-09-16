#Edit dates 24 November 2016, 31 January 2017, 14 March 2017, 24 August 2017
#Caryl S. Benjamin and Patrick Lawrence Cadelina

#This code will only work if the following conditions are satisfied
#Additional functions will be provided in Github to correct limitations 
     #(e.g.reduce gps interval to 1 second,etc.) prior to running this code

#Data folder should contain 2 subfolders and 1 *.xlsx file
        #Folder 1: GPS - contains all GPS files (*.gpx); locations should be in decimal degrees format, interval should be 1 second 
        #Folder 2: PICTURES - contains all goPro picture files
        #startstop.xlsx - start and end times for each transect encoded in Sheet1;
        #               - dates and times encoded as 03-23-1981 13-05-30 equivalent to "March 23, 1981 1:05:30 PM", format should be text
#add 1 empty folder called forCPCE

#Data Processing steps: 
#1. load startstop file and convert to date/time formats
#2. generate dataframe of exif information
#3. generate single dataframe of gps data from several gpx files
#4. check dataframes: startstop, exif and gps
#5. apply time correction to exif dataframe
#6. subset gps dataframe according to startstop
#7. geotagging: lon/lat are merged to the list of pictures using the date and time
#8. subsampling over user-specified distance
#9. mapping of sampled points


#all lines in the script that require editing/user attention are specified with ##EDIT

setwd(" ") #set working directory for Folder 1
#1. load startstop file
{
  install.packages("readxl") #run once for each computer used in data processing
  library(readxl)
  pathtostartstop<- paste0(getwd(),"/startstop.xlsx")    
  
  startstop<-read_excel(pathtostartstop, sheet = "Sheet1") # Excel value format must be in 'Text'.
  
  tz<-Sys.timezone()
  startstop$START<-as.POSIXct(strptime(as.character(startstop$START), "%Y-%m-%d %H-%M-%S",tz=tz))
  startstop$STOP<-as.POSIXct(strptime(as.character(startstop$STOP), "%Y-%m-%d %H-%M-%S",tz=tz))
  
  head(startstop) #start and/or stop columns will have <NA> values if formats in as.POSIXct don't match
                  #note that the format in the sample data is 03-23-1981 13-05-30 equivalent to "March 23, 1981 1:05:30 PM"   
  str(startstop)  #start and stop should be POSIXct format
  
} #done step #1

#2. exif extraction
{
#run installation of packages once only for each computer used in data processing
install.packages("devtools") 
devtools::install_github("paleolimbot/exifr") #Trying 'exiftool' on the console...close exiftool window
install.packages("lubridate")
install.packages("iterators")
install.packages("plotKML") #includes the readGPX function
install.packages("chron")
install.packages("data.table")
install.packages("reshape") #for manipulating the nested lists from readGPX into dataframes
install.packages("stringr") #for subsetting text similar to left and right functions in excel
install.packages("dplyr")

library(exifr) #close exiftool window
library(lubridate)
library(iterators)
  
  pictures<- paste0(getwd(),"/abPICS") ##EDIT path to pictures folder ##

setwd(pictures)
files <- list.files(pattern = ".JPG", recursive=T) #list all files in the folder with .JPG in their filenames
                                                   #recursive=T means it will also check subfolders
                                
exif<-exifr(files, exiftoolargs = "-FileName -DateTimeOriginal") #generates a dataframe with the requested information
                                                                 #this takes some time depending on the number of picture files

exif<-exif[,-1] #remove first column (File location)

exif<- read.csv("exif_correct.csv")##~thesus
exif<-exif[, c(3, 7)] ##~thesus
colnames(exif)<-c("FILENAME","DATE_TIME")

exif$DATE_TIME<-as.POSIXct(strptime(as.character(exif$DATE_TIME), "%Y-%m-%d %H:%M:%S", tz=tz)) ##~thesus edited

attr(exif$DATE_TIME,"tzone")
head(exif) 
str(exif) #Filename should be in chr format while DATE_TIME in POSIXct
} #done step #2
  
#3. reading gps (.gpx format) data 
{
library(plotKML) #PlotKML library for readGPX function
library(reshape)

GPSdir<-" "  ##EDIT set path to GPS folder##    
setwd(GPSdir)

GPX<- lapply(list.files(".", pattern="*.gpx"), function(x)readGPX(x, tracks=T)) #reads .gpx files and lists them
GPS<- lapply(GPX, '[[', "tracks") #extracts 'tracks' list within the nested list
allGPS<- melt(GPS, id.vars =c("lon", "lat", "time")) #creates a dataframe with time, lon, lat, and elevation

allGPS<- read.csv("GPStime.csv")

allGPS<- allGPS[, c("lon", "lat", "time")] #subset dataframe to essential data only
allGPS$time<- gsub("T|Z", " ", allGPS$time) #removes 't' and 'z' characters in time column
allGPS$time <- as.POSIXct(allGPS$time, "%Y-%m-%d %H:%M:%S",tz=tz) #edit: tz declares which timezone the data belong
attributes(allGPS$time)$tzone <- tz #changes the timezone of file to system timezone
                                      #make sure computer time zone is same as study site 
attr(allGPS$time, "tzone") #check if the time zone reflected
str(allGPS)
colnames(allGPS)<- c("LAT", "LON", "DATE_TIME", "DEPTH", "wTEMP") #rename column names ##~thesus edited
} #done step #3       
        
#4. check dataframes: startstop, exif and allGPS
{
 #all columns should have data and not NAs
 #formats: date/time should be POSIXct   
 #Be sure that time zones of allGPS, startstop, and exif are the same       
 head(startstop)
 str(startstop)
 attr(startstop$START,"tzone")
 
 startstop$DURATION<-as.numeric(startstop$STOP-startstop$START)
 hist(startstop$DURATION)
 startstop #edit csv file if there are unrealistic durations e.g. negative values
 
 head(exif)
 str(exif)
 attr(exif$DATE_TIME,"tzone")
 
 head(allGPS)
 str(allGPS)
 attr(allGPS$DATE_TIME,"tzone")

} #done step #4

#5. apply time correction to exif dataframe

{
#count monitoring days from startstop
days<-length(unique(as.Date(startstop$START)))

cor<-data.frame()

for (i in 1:days){
    cor[i,1] <- readline(paste("How much time in seconds should I add to the current picture taken times for DAY",i,"?"))
}
##EDIT input time in seconds in the console below
####T3RK**: script above for 'days' may be inaccurate if survey day = 1, CSB edited, please test again <- t3rk: works wonders! :)
library(lubridate)
cor<-seconds(cor[,1]) #change format of first column to seconds

#correction for exif file per day

#create a new column with just the date
exif$dates<-format(exif$DATE_TIME,"%Y-%m-%d")
head(exif)

new_exif<-data.frame()
for (z in 1:days){
        
     d<-sort(unique(exif$dates))
     d[z]
     sub<-exif[exif$dates==d[z],]
     sub$mod_dt<-sub$DATE_TIME+cor[z,1]
     new_exif<-rbind(new_exif,sub)
}

head(new_exif)
#edit the exif dataframe to revert back to previous columns
exif<-new_exif[,-c(2,3)]
colnames(exif)<-c("FILENAME","DATE_TIME")

#check: FILENAME should be chr while DATE_TIME should be POSIXct
head(exif)
str(exif)
exif$DATE_TIME<- as.POSIXct(exif$DATE_TIME, tz=Sys.timezone())
attr(exif$DATE_TIME,"tzone")

}#done step #5

#6. subset gps dataframe according to startstop
{
#check if time interval for gps is 1 second

for (a in 2:nrow(allGPS)){
        allGPS$interval<-allGPS$DATE_TIME[a]-allGPS$DATE_TIME[a-1]
}
unique(allGPS$interval) #if output is not 1, run gps interval function (to be added in github)

#subsetting the allGPS file according to the time intervals in the startstop file

gpssubset = data.frame()                                            #create new dataframe

for (x in 1:nrow(startstop)){                                       #for each transect
        
        int <- new_interval(startstop$START[x], startstop$STOP[x])      #interval covered by transect: time period
        gpssub<-allGPS[allGPS$DATE_TIME %within% int, ]                 #subset the gps file and save as gpssub
        
        if (nrow(gpssub)>0) {
                
                startstop$numgps[x]<-nrow(gpssub)
                
                gpssub$TRANSECT<-startstop$TRANSECT[x] #create a new column called transect and assign the transect from startstop
                gpssubset<-rbind(gpssubset,gpssub)
        } else
        { startstop$numgps[x]<-0}
        
        startstop$DURATION<-as.numeric(startstop$STOP-startstop$START)
        startstop$nogps<-startstop$numgps-startstop$DURATION
}
#check startstop
startstop #returns the numgps column which is the number of gps points captured within the 
                #time interval
#check the newly created dataframe with the subset of gps points
head(gpssubset)
str(gpssubset)

}#done step #6

#7. geotagging: lon/lat are merged to the list of pictures using the date and time
{

total<-merge(exif,gpssubset)
head(total)
str(total)

output<-"geotagged.csv"
write.csv(total, output, row.names=F) #creates a new csv in the specified folder of a list of geotagged pictures
}#done step #7

#8. subsampling over user-specified distance
{

library(geosphere)
distance<-2 # Desired distance between points (in meters). 


head(total)
subsetdropcam = data.frame()                  #create new dataframe
# unique(alldropcam$TRANSECT)

for (i in 1:max(total$TRANSECT)){   #loop over all transects
  
  sub<-total[total$TRANSECT==i,]  #subset to transect
  
  if (nrow(sub)>1) {
    
    sub$DISTANCE[1]<-0
    sub$INCLUDE[1]<-"y" #include the first picture at all times
    lastgoodrow<-1
    
    for (r in 2:nrow(sub)){
      
      #code for cumulative distance and subsetting
      sub$DISTANCE[r]<-distVincentyEllipsoid((sub[lastgoodrow,3:4]), sub[r,3:4])#distance between current and last yes
      if (sub$DISTANCE[r]>=distance) {
        sub$INCLUDE[r]<-"y"
        lastgoodrow<-r
      } 
      else                            {
        sub$INCLUDE[r]<-"n"
      }
      
    }#end of for loop within transect
    
    # print(i)
    subsetdropcam<-rbind(subsetdropcam,sub)
  }
  
  
}#end of for loop for all transects

incset<-subsetdropcam[subsetdropcam$INCLUDE=="y",]
}
sink<- " " #directory where pictures will be copied to 
pictures<- " " #directory where the pictures will be copied from
if(dir.exists(sink)==FALSE) { dir.create(sink)} #creates that directory if it doesn't exist yet

#copies files from source folder to sink folder
for(i in 1:nrow(incset)){
  SourceFile<- paste0(pictures,"/",incset$FILENAME[i])
  SinkFile<- paste0(sink,"/",incset$FILENAME[i])
  file.copy(SourceFile, SinkFile, copy.date = TRUE)
}


###############################################################################################
###############################################################################################


#9. mapping of sampled points
{
 #lon lat is the point at the center of the map of the desired area
 #zoom should be between 10-15, bigger values correspond to higher zoom
  
 data<- incset[, 3:4]
        
 lon<-mean(data$LON)
 lat<-mean(data$LAT)
 zoom<-11 #EDIT 
 title<-" " #Write optional title for the figure
        
 library(ggmap)
 attach(data)
 
 dataMap <- get_map( c(lon = lon, lat = lat) # points to the center of the figure
                     , zoom = zoom # larger is closer
                     , maptype = "satellite" # map type
                     , source = "google"
 )
 
 p <- ggmap(dataMap)
 
 windows()
 print(p)
 
 p <- p + labs(title = title)
 
 p <- p + 
         scale_shape_identity() +
         geom_point(data = data,aes(x = LON, y = LAT, colour = "red", shape = 15)) +
         guides(colour=FALSE) +
         theme(legend.title=element_blank()) +
         ylab("Latitude") +
         xlab("Longitude")
 
 print(p)
 detach(data)
} #done step 9