## Catrina Nowakowski
## Summer 2016 - November 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements

## This script process NETCDF data output from CMAQ bidi 
## Format:
## Set Up lists of variables and File names
## Function to pull them from NETCDF (Pull_Var)
## Function to convert Var to data frame (Mat_to_Data_Frame)
## Loop to apply Pull_Var and use same Loop to apply Mat_to_Data_Frame *applys to all variables 
# previously applied to Pull_Var because a list of variables was created in the Pull_Var loop
## CSV FILES WILL BE FOUND IN WORKING DIRECTORY

## Note used lat_long file again, checked out and it all lines up correct!

#############################################################################################
## Load the longitude and Latitude for all files
print("Reading Latitude and Longitude...")

Lat_Long <- read.csv("longitude_latitude.csv", header = TRUE)

#############################################################################################
## Making list of Variables and File names
## To find variable names:
# print("Opening NetCDF")
# nc <- nc_open(File)
# print(nc)
# print("Close file")
# nc_close(nc)


var_names <- c("Radiation", "Tmax", "Tmin", "Precipitation", "R_humidity", "Windspeed", 
               "Dry_Oxidized_ND", "Dry_Reduced_ND", "Wet_Oxidized_ND", "Wet_Reduced_ND", 
               "Wet_Organic_ND")

file_names <- c("site_weather_dep_20020101_to_20021231.nc", "site_weather_dep_20030101_to_20031231.nc", 
                "site_weather_dep_20040101_to_20041231.nc", "site_weather_dep_20050101_to_20051231.nc", 
                "site_weather_dep_20060101_to_20061231.nc")

#############################################################################################
## Function to Pull NETCDF Variables 

## GSP's guide to netCDF format data and the 'R' package 'ncdf'
## https://www.image.ucar.edu/GSP/Software/Netcdf/


Pull_Var <- function(File, Variable){
  library(ncdf4)
  
  print("Opening NetCDF")
  
  nc <- nc_open(File)

  print("Grab Variable")
  Var <- ncvar_get(nc, Variable)
  
  print("Close file")
  nc_close(nc)
 
  return(Var)
  
}

#############################################################################################
## Making a function to turning Each variable in to a data frame that I can use
## CURRENTLY FORMATED TO PULL OUT THE LAKE ERIE AREA, FOR WHOLE COUNTRY OMIT THE REFINE SECTION
## CURRENTLY FORMATED TO GRAB DAYS 121 THROUGH 300, CHANGE OR OMIT SECTION TO NEEDS

Mat_to_Data_Frame <- function(Var, Lat_Long) {
  
  Var_Name <- Var
  Var <- get(Var)
  
  ## Stack the variable so all of the columns are under one another in order by day
  print("Stacking...")
  Var <- matrix(Var, ncol = 1) 
  
  
  ## Puts in to to day by each column
  print("Splitting...")
  Var <- split(Var, 1:137241 )
  
  ## Makes it back in to a data frame
  print("To Data Frame...")
  Var <- as.data.frame(Var)
  Var <- t(Var)
  print("To Data Frame again...")
  Var <- as.data.frame(Var)
  
  
  ## NA values to 0
#   print("NA to Zero...")
#   Var[is.na(Var)] <- "0"
# I am going to do this later when there are less variables to change  

  
  #############################################################################################
  ## Renaming Data Frames:
  print("Renaming Data Frames...")
  
  # Counter
  i <- 1
  
  # Loop to chang column name
  for(j in 1:365){
    names(Var)[i]<-  paste0("day_",j)
    i <- i + 1
  }
  
  #############################################################################################
  ## Grabing days 121 through 300:
  Var <- Var[,121:300]
  
  
  #############################################################################################
  ## Adding Lat and Lon
  print("Adding Latitude and Longitude Columns")
  
  Var$Lat <-Lat_Long$Latitude
  Var$Long <-Lat_Long$Longitude
  
  #############################################################################################
  ## Refineing to Area of Intrest
  Lat_Start  <- 40.27952566
  Lat_End    <- 44.3709869
  
  Long_Start <- -85.869140625
  Long_End   <- -77.673339
  
  
  Var <- Var[Var$Lat  > Lat_Start   &
               Var$Lat  < Lat_End     &
               Var$Long > Long_Start  &   
               Var$Long < Long_End, ] 
  
  print("writing csv") 
  ## To write a CSV file for input to Excel one might use
  
  The_CSV_Name <- paste0(Var_Name, ".csv")
  
  write.table(Var, file = The_CSV_Name, sep = ",", col.names = NA,
              qmethod = "double")
  
  return(Var)
  
  print("END")
}



#############################################################################################
## Loops through each file and then loops through each variable applys Pull_Var function
## and saves them as seperate varialbes in the format: Variable_Year.  
## Additionaly it creats a variable name list
## Appling Mat_to_Data_Frame function to all variables to produce a CSV file with just one var 
## and just for the area of intrest and date range
## LOOK IN WORK DIRETORY FOR THE CSV FILE FOR EACH VARIABLE

#initiate all_var_name list:
all_var_name <- rep(NA, length(var_names)*length(file_names))

for(The_File in file_names){

#Tracks the Year  
i = 2002
#Tracks the variable name list
j = 1

for(The_Variable in var_names){
  
  Var <- Pull_Var(The_File, The_Variable)
  assign(paste0(The_Variable, "_", i), Var)
  all_var_name[j] <- paste0(The_Variable, "_", i)
  
  print(all_var_name[j])
  
###############################################
## Appling Mat_to_Data_Frame function to all variables to produce a CSV file with just one var 
## and just for the area of intrest and date range
## LOOK IN WORK DIRETORY FOR THE CSV FILE FOR EACH VARIABLE
## Adding in the second funtion here because I used too much space with each variable

Var <- all_var_name[j]
a <- Mat_to_Data_Frame(Var, Lat_Long)
assign(all_var_name[j], a)  
  
j = j + 1  
}
i = i + 1
}

#############################################################################################


#############################################################################################
## Write a CSV file for input to Excel 
print("writing csv") 
The_CSV_Name <- "Var_Name_List.csv"

write.table(all_var_name, file = The_CSV_Name, sep = ",", col.names = NA,
            qmethod = "double")
