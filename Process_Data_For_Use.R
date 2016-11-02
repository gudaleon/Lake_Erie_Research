#############################################################################################
## Making list of Variables and File names
## To check variable names use print(nc)

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
## Loops through each file and then loops through each variable applys Pull_Var function
## and saves them as seperate varialbes in the format: Variable_Year.  
## Additionaly it creats a variable name list

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
  j = j + 1
  
}
i = i + 1
}



#############################################################################################
## Turning the variable in to a data frame that I can use


## Stack the variable so all of the columns are under one another in order by day
print("Stacking...")
Radiation <- matrix(Radiation, ncol = 1) 


## Puts in to to day by each column
print("Splitting...")
Radiation <- split(Radiation, 1:137241 )

## Makes it back in to a data frame
print("To Data Frame...")
Radiation <- as.data.frame(Radiation)
Radiation <- t(Radiation)
print("To Data Frame again...")
Radiation <- as.data.frame(Radiation)


## NA values to 0
print("NA to Zero...")
Radiation[is.na(Radiation)] <- "0"

#############################################################################################
## Renaming Data Frames:
print("Renaming Data Frames...")

# Counter
i <- 1

# Loop to chang column name
for(j in 1:365){
  names(Radiation)[i]<-  paste0("day_",j)
  i <- i + 1
}

#############################################################################################
## Grabing days 121 through 300:
Radiation <- Radiation[,121:300]

#############################################################################################
## Load the longitude and Latitude for all files
print("Reading Latitude and Longitude...")

Lat_Long <- read.csv("longitude_latitude.csv", header = TRUE)

#############################################################################################
## Adding Lat and Lon
print("Adding Latitude and Longitude Columns")

Radiation$Lat <-Lat_Long$Latitude
Radiation$Long <-Lat_Long$Longitude

#############################################################################################
## Refineing to Area of Intrest
Lat_Start  <- 40.27952566
Lat_End    <- 44.3709869

Long_Start <- -85.869140625
Long_End   <- -77.673339


Radiation <- Radiation[Radiation$Lat  > Lat_Start   &
                         Radiation$Lat  < Lat_End     &
                         Radiation$Long > Long_Start  &   
                         Radiation$Long < Long_End, ] 

print("writing csv") 
## To write a CSV file for input to Excel one might use
write.table(Radiation, file = "Radiation.csv", sep = ",", col.names = NA,
            qmethod = "double")


print("END")


