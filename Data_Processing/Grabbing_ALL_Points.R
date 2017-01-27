## Catrina Nowakowski
## Summer 2016 - November 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements

## This script is to grab the closest grid point to the stations and make a dataframe for that variable and a .CSV for each variable


## Note:
# Use variable list made from the Process_NETCDF_Data_For_Use


#############################################################################################
## Reads in Files

## Variables
Variable_Names <- read.csv("Var_Name_List.csv", header = FALSE, stringsAsFactors = FALSE)

## Loop through the list to load each variable file:
for(The_Var_Name in Variable_Names$V1){
  file_name <- paste0(The_Var_Name, ".csv")
  a <- read.csv(file_name, header = TRUE)
  assign(The_Var_Name, a)
}


## Locations of points
Locations <- read.csv("Paired_Gridded_Points.csv", header = TRUE)

Locations <- unique(Locations)

Locations$Grid_Lat <- round(Locations$Grid_Lat, 5)


print('loded data')
#############################################################################################
## Sets up for the functions
drops <- c("Lat","Long", "X")

#############################################################################################
## A Funtion to grab the points!

Point_Grabber <- function(Variable_Char, Point) {
  
  #############################################################################################
  ## Grabs all days for one station
  Variable <- get(Variable_Char)
  Point <- Point
  Variable$Lat <- round(Variable$Lat, 5)
  
  Variable_Point <- Variable[Variable$Lat == Locations$Grid_Lat[Point],]
  
  #############################################################################################
  ## Formats for that station and point
  
  ## Drops Lat and Long and "X"
  Variable_Point <- Variable_Point[ , !(names(Variable_Point) %in% drops)]
  
  ## Transpose
  Variable_Point <- t(Variable_Point)
  Variable_Point <- as.data.frame(Variable_Point, header = FALSE)
  
  ## Formats data Frame
  Variable_Point$Date <- dates
  Variable_Point$Lat <- Locations$Sta_Lat[Point]
  Variable_Point$Long <- Locations$Sta_Long[Point]
  Variable_Point$Sta <- Locations$Sta[Point]
  Variable_Point$Old_New <- Locations$Old_New[Point]
  # Variable_Point$Region_W <- 1
  # Variable_Point$Region_C <- 0
  # Variable_Point$Region_E <- 0
  colnames(Variable_Point)[1] <- Variable_Char
  
  return(Variable_Point)
  ## End Function
} 


#############################################################################################
## Outside loop that goes through each variable:
## Variable Names:

## Make a List of the Points
Points <- Locations$Sta


for(i in Variable_Names$V1){
  var_name <- rep(NA, length(Locations$Distance))
  
  #ID Year
  year <- unlist(strsplit(i, "_", fixed = TRUE))
  year <- year[length(year)]
  

  if(year != 2004 | year != 2008){
    ## Make singular water shead data frame:
    date_st <- paste0(year, "-04-01")
    date_end <- paste0(year, "-11-01")
    dates <- seq(as.Date(date_st), as.Date(date_end), by="days")
    
  }else if(year == 2004 | year == 2008){
    ## Make singular water shead data frame:
    date_st <- paste0(year, "-03-31")
    date_end <- paste0(year, "-10-31")
    dates <- seq(as.Date(date_st), as.Date(date_end), by="days")
    
  }
  
  for(j in 1:length(Points)){
    
    a <- Point_Grabber(i,j)
    var_name[j] <- paste(i, "_",j,sep="")
    assign(var_name[j], a)
  }
  
  #############################################################################################
  ## Mearge to one file:
  Final_File <- merge(get(var_name[1]), get(var_name[2]), all = TRUE)
  
  for(k in 3:34){
    Final_File <- merge(Final_File, get(var_name[k]), all = TRUE)
  }
  
  #############################################################################################
  ## Write CSV
  ## To write a CSV file for input to Excel one might use
  
  file_name <- paste0(i, "_Points_O_N.csv",sep="")
  
  write.table(Final_File, file = file_name, sep = ",", col.names = NA,
              qmethod = "double")  
  print('csv written')
}



#############################################################################################
## Write CSV
## To write a CSV file for input to Excel use

var_name_list <- rep(NA, length(Variable_Names$V1))

for(i in 1:length(Variable_Names$V1)){
  var_name_list[i] <- paste0(Variable_Names$V1[i], "_Points_O_N",sep="")
}

file_name <- c("Point_Variable_Names.csv")

write.table(var_name_list, file = file_name, sep = ",", col.names = NA,
            qmethod = "double")  
print('csv written')
