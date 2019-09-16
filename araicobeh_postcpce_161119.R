#Edit dates 24 November 2016, 31 January 2017, 14 March 2017, 24 August 2017
#Caryl S. Benjamin and Patrick Lawrence P. Cadeliña

#ARAICoBeH script for post-CPCe processing
#post-cpce data in xlsx format from Governor Generoso are used as an example
#folder should contain all *.xlsx files generated using CPCe as well as a *.csv file from the precpce code

#Data Processing steps: 
#1. merge excel files from CPCe and create 1 dataframe, attach location data, create a csv file
#2. Generate map with live coral cover data 
#3. Generate map with community structure data

#1 merge excel files from CPCe and create 1 dataframe, attach location data, create a csv file
{

#name excel files with numbers
        
setwd(" ")
        #spreadsheet with results is always called Data Summary

# install.packages("readxl")            
library(readxl)
filelist <- list.files(".", pattern = "*.xlsx") #list all excel files in the folder


cpce<-data.frame()
for (file in filelist) { 
        print(file)
        
        dat<-read_excel(file, sheet = "Data Summary",col_names=F)
        ##EDIT
        first<- 6 #row number of row with TRANSECT NAME
        last<- 70 #row number of last row with relevant data on percent cover (usually Tape, Wand and Shadow (TWS) before NOTES) 
        dat<-dat[first:last,]
        colnames(dat)<-dat[1,]
        dat <- dat[!is.na(names(dat))]
                #remove cells with NAs (for the section headings)
              
        # dat<-dat[complete.cases(dat),] #*#T3rk edit: commented this out since this would delete data in case a transect column have NAs
        dat <- dat[rowSums(is.na(dat)) <=(ncol(dat)-2), ] #*# This will remove non-essential rows. An alternative for complete.cases within the for-loop. 
        
       # colnames(dat)
        n <- dat$'TRANSECT NAME'
        
        # transpose all but the first column (name)
        dat.trans <- as.data.frame(t(dat[,-1]))
        colnames(dat.trans) <- n
        dat.trans$myfactor <- factor(row.names(dat.trans))
        
        #str(dat.trans) # Check the column types
        
        #remove unecessary columns
        dat<-dat.trans[,-c(2:4,15,53)]
        cpce<-rbind(cpce,dat)
        
}
cpce<-cpce[complete.cases(cpce), ] # This will remove transect data with NAs
cpce<- cpce[!duplicated(cpce$`TRANSECT NAME`), ] # Removes duplicated transect name and data. 
cpce[-1]<- apply(cpce[-1], 2, function(x) as.numeric(as.character(x))) # Converts the data frame from factor to numeric


## Merge GPS data
setwd(" ") #set path for
loc<-read.csv("geotagged.csv") #reads the geotagged.csv output from precpce code
myvars <- c("FILENAME", "LON", "LAT","TRANSECT")
loc <- loc[myvars]

cpce$FILENAME<-paste0(cpce$`TRANSECT NAME`,".JPG")
cpce$`TRANSECT NAME`<-NULL

gpscpce<-merge(loc,cpce)
gpscpce.fn<-" "#desired filename for saving .csv for merged datasets of gps information and cpce data
write.csv(gpscpce,gpscpce.fn,row.names=F)
} #done step #1

#2. Generate map with live coral cover data
{
        library(ggplot2)
        library(rgdal)
        library(grid)
        library(gridExtra)
        ##Load base map. Extent of the map is dependent on the mean of longitude and latitude plus difference set by the value 'margin'
        library(raster)
        library(viridis)
        
        
        ## load base map for Philippines
        ph<- getData("GADM", country="PHL", level=0)
        
        ## sets margin of the map (for aesthetic purposes)
        {
                margin <- 0.02
                xmin<- min(gpscpce$LON) - margin
                xmax<- max(gpscpce$LON) + margin
                ymin<- min(gpscpce$LAT) - margin
                ymax<- max(gpscpce$LAT) + margin
        }
        
        ## set base map with land, lat, lon
        base.map<- ggplot() +
                geom_polygon(data=ph, aes(long,lat, group=group), colour="grey80", fill="grey80") + 
                coord_map(xlim=c(xmin,xmax), ylim=c(ymin,ymax)) +
                theme(
                        text = element_text(size = 20),
                        panel.background=element_blank(),
                        panel.border=element_blank(),
                        panel.grid.major=element_blank(),
                        panel.grid.minor=element_blank(),
                        plot.background=element_blank())
        base.map
        
        
        ## add the base map (landmass) with hard coral cover data 
        hcc.map<- base.map +
                geom_point(data=gpscpce, aes(x=LON, y=LAT, color = CORAL..C.), size=0.7, shape = 15) +
                theme_bw() +
                scale_color_viridis(option="viridis", limits=c(0,100)) + 
                labs(title = "Hard coral cover:  ", x="Longitude", y="Latitude", color="Hard coral \ncover (%)") +
                theme(
                        plot.title = element_text(size=12, hjust=0.5),
                        axis.title.x = element_text(size=10),
                        axis.title.y = element_text(size=10),
                        panel.grid.major = element_line(colour="#f0f0f0"),
                        axis.line = element_line(colour="black"),
                        axis.ticks = element_line(),
                        axis.text = element_text(size=8),
                        axis.text.y = element_text(angle=90, hjust=0.5),
                        legend.text = element_text(size=9),
                        legend.title = element_text(size=10),
                        panel.grid.minor = element_blank())
        hcc.map
} #done step #2

#3. Generate map with community structure data

## Group 
## for this example, we used the data 
{
        tn<- #input column number for transect
        maxcol<- (ncol(gpscpce)+1) #determine number of columns of the gpscpce data set + 1
        for (i in 1:1719){
                if (gpscpce[i,tn]>=1 & gpscpce[i,tn]<=17){gpscpce[i,maxcol]<-"Guntao"}
                if (gpscpce[i,tn]>=18 & gpscpce[i,tn]<=30){gpscpce[i,maxcol]<-"N. Bay"}
                if (gpscpce[i,tn]>=31 & gpscpce[i,tn]<=42){gpscpce[i,maxcol]<-"S. Bay"}
                if (gpscpce[i,tn]>=43 & gpscpce[i,tn]<=63){gpscpce[i,maxcol]<-"Miniloc"}
                if (gpscpce[i,tn]>=64 & gpscpce[i,tn]<=77){gpscpce[i,maxcol]<-"South Cadlao"}
                if (gpscpce[i,tn]>=78 & gpscpce[i,tn]<=88){gpscpce[i,maxcol]<-"North Cadlao"}
        }        
}


library(scatterpie)
{
        ### Consolidate data per site by acquiring the mean per site.
        cpcemean<- NULL
        colnames(gpscpce)[maxcol]<- "SITE" #renames the last column to sote
        
        alt<- gpscpce[, c(maxcol, 2:11)] #rearangges the data frame to SITE, then Major benthic categories only
        for(x in 1:length(unique(alt$SITE))){
                a<- alt[alt$SITE==unique(alt$SITE)[x], ]
                c<- as.data.frame(t(colMeans(a[, -1])))
                c$SITE<- a$SITE[x]
                cpcemean<- as.data.frame(rbind(cpcemean, c))
                row.names(cpcemean)<- NULL
                a<- NULL
        }
        cpce_major<- cpcemean[,c(11,1:10)]
        #cpce_major<- cpcemean[, c(1,2,4:11, ncol(cpcemean))] ## Plot only major benthic categories 
        
        borders<- data.frame(site=c("Guntao", "N.Bay", "S.Bay", "Miniloc", "S.Guntao", "N.Guntao"), 
                             xmin=c(119.23,119.385, 119.37, 119.305, 119.33,119.335), xmax=c(119.275,119.425, 119.417, 119.35, 119.39, 119.377), 
                             ymin=c(11.1, 11.0978, 11.0405, 11.1096, 11.165, 11.215), ymax=c(11.15,11.1511, 11.0978,11.165,11.215, 11.26))
        
        colnames(cpce_major)<-c("SITE", "LON", "LAT", "Hard corals", "Soft corals", "Algae", "Seagrass beds", "Other Invertebrates", "Coralline algae", "Dead corals", "Abiotic")
        fxngrps<- colnames(cpce_major)[4:11]
        #Plot pie charts
        cbPalette <- c("#D55E00", "#E69F00", "#CC79A7", "#e5d8bd", "#0072B2", "#999999","#009E73", "#56B4E9" )
        
        ##Input arbitrary LON & LAT Values for the pie charts
        cpce_major$LON<- c(119.35761, 119.405, 119.3658, 119.3370, 119.26, 119.39) 
        cpce_major$LAT<- c(11.19, 11.119,11.23, 11.14257,11.13626, 11.06504)
        
        pie<- # loads the base map (from previous step) and incorporates mean values per benthic category per site. 
                base.map + 
                geom_scatterpie(aes(x=LON, y=LAT, group=SITE, r=0.01), cols = fxngrps, data=cpce_major, color=NA) +
                geom_rect(data = borders, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1) +
                scale_fill_manual(values=cbPalette) +
                labs(title = "Benthic Community Structure: ", x="Longitude", y="Latitude") +
                guides(fill=guide_legend(title="Major benthic\ncategories")) +
                theme(
                        plot.title = element_text(size=12, hjust=0.5),
                        plot.margin = unit(c(0.01,0.001,0.01,0.1),"cm"),
                        axis.title.x = element_text(size=10),
                        axis.title.y = element_text(size=10),
                        panel.grid.major = element_line(colour="#f0f0f0"),
                        axis.line = element_line(colour="black"),
                        axis.ticks = element_line(),
                        axis.text = element_text(size=8),
                        axis.text.y = element_text(angle=90, hjust=0.5),
                        legend.text = element_text(size=8),
                        legend.key.size = unit(0.15,"in"),
                        legend.title = element_text(size=9),
                        panel.border = element_rect(colour="black", fill=NA, size=1), 
                        panel.grid.minor = element_blank())
        pie
        
}#done step #5


