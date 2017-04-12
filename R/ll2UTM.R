#' @title Convert lat/longs into UTMS
#' @export
#' @description Using a standard input convert lat/longs into UTMS using the readxl and rgdal package
#' @param file_type Currently only supporting excel and csv. Defaults to excel
#' @param create_csv Export a csv? Defauls to TRUE
#' @examples
#' #x <- ll2utm(create_csv=FALSE)
#' #ll2utm(create_csv=TRUE)

ll2utm <- function(file_type = "excel", create_csv = TRUE){
message("Excel or csv file need to have the following columns: Station,Trawl,Number,Status,Lat,Long and the lat long to be in this format: 49 40 109. Each file needs to have a common UTM zone.")

file_path <- file.choose()

#olc <- read.csv("E:/BCCF/TILLS/To be imported to TILLS/Lat-Long_to_UTM/Okanagan Lake Coordinates.csv")

if(file_type == "excel"){olc = as.data.frame(readxl::read_excel(file_path,1))}

if(file_type == "csv"){olc = read.csv(file_path)}


## Convert to decimal
## I think this is the right format
## Will often need to re-jig the substr for the specific format
olc$Lat2 <- as.numeric(substr(olc$Lat, 1,2)) +
                (as.numeric(paste(substr(olc$Lat, 4,5),
                   substr(olc$Lat, 7,9),
                   sep="."))/60)

olc$Long2 <- (-1*(as.numeric(substr(olc$Long, 1,3)) +
  (as.numeric(paste(substr(olc$Long, 5,6),
                    substr(olc$Long, 8,10),
                    sep="."))/60)))



## Not sure what this does here?
olc <- as.data.frame(olc)

##Start points
LatLong <- olc[,c("Lat2","Long2")]
names(LatLong) <- c("X","Y")

# Convert it to a sp object
sp::coordinates(LatLong) <- ~ Y + X # longitude first

# Add a coordinate reference system
sp::proj4string(LatLong) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


UIinput <- function(){
  cat ("Need to run line by line")
  #Ask for user input
  x <- readline(prompt = "Please enter the UTM zone: ")

  #Can it be converted?
  x <- as.numeric(x)

  #If it can't, be have a problem
  if(x>11 | x<10){

    stop("Outside the range of acceptable values")

  }

  #If it can be, return - you could turn the if into an if/else to make it more
  #readable, but it wouldn't make a difference in functionality since stop()
  #means that if the if-condition is met, return(x) will never actually be
  #evaluated.
  return(x)
}


## Interative function called to specify UTM zone
CRSinput<-paste("+proj=utm +zone=",UIinput()," ellps=WGS84",sep="")

# Project using spTransform
utms <- as.data.frame(sp::spTransform(LatLong, CRS(CRSinput))) ##UTM zones!
colnames(utms)<-c("Easting","Northing") ## Unsure why this step has to happen

utms$Easting<-round(utms$Easting)
utms$Northing<-round(utms$Northing)


if(create_csv==TRUE){
## Write out the data
write.csv(cbind(olc,
                 utms,row.names = NULL),
           file=paste0(gsub("\\..*","",sub('.*\\\\', '', file_path)),"_convUTM.csv"),
          row.names = FALSE)
} else{return(cbind(olc, utms,row.names = NULL))}

}
