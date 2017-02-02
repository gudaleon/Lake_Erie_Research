## Catrina Nowakowski
## November 27, 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements


## This file puts all of the WRF and CMAQ data in to one data frame


#######################################################################################################################################
## Load the variable name file

Data_Name <- read.csv("Var_Name_List_watershed.csv", header = FALSE, stringsAsFactors = FALSE)

drops <- c("X")

## Read in stations
for(i in 1:length(Data_Name$V1)){
  #Make File Name
  File_Name <- paste0(Data_Name$V1[i], "_by_water_shed.csv")
  #Read File
  a <- read.csv(File_Name, header = TRUE)
  #Date Format
  a$Date <- as.Date(a$Date)
  #Format Watershed
  a$Water_Shed <- as.numeric(a$Water_Shed)
  #Drop the x column
  a <- a[ , !(names(a) %in% drops)]
  #Rename the variable column, rn it has the year in it an stuff..
  Var_name <- unlist(as.vector(strsplit(Data_Name$V1[i], "_200")))
  colnames(a) <- c("Date", paste0(Var_name[1], "_WS"), "Water_Shed") 
  #Assign the data to the correct variable name
  assign(paste0(Data_Name$V1[i], "_by_water_shed"), a)
  #Save the correct variable name 
  Data_Name$V1[i] <- paste0(Data_Name$V1[i], "_by_water_shed")
}

rm(a)


## Rbind each variable so all the dates are in one
# To do this make lists for each variable by taking the first 11 variables in the Data Name list and doing strsplit
# Then itterate though that list

Var_Names <- c("Radiation", "Tmax", "Tmin", "Precipitation", "R_humidity", "Windspeed", "Dry_Oxidized_ND", "Dry_Reduced_ND",
               "Wet_Oxidized_ND", "Wet_Reduced_ND", "Wet_Organic_ND")

for(i in 1:length(Var_Names)){
  a <- merge(get(paste0(Var_Names[i], "_2002_by_water_shed")), get(paste0(Var_Names[i], "_2003_by_water_shed")) , all = TRUE)
  
  for(j in 2004:2009){
    a <- merge( a , get(paste0(Var_Names[i], "_", j, "_by_water_shed")) , all = TRUE)
  }
  assign(paste0(Var_Names[i], "_WS"), a)
}

## Deleats extra variables 
rm(list = Data_Name$V1)
rm(a)


## Now I want to merge each file by the date and by the water shed 
Var_Names <- c( "Tmin", "Precipitation", "R_humidity", "Windspeed", "Dry_Oxidized_ND", "Dry_Reduced_ND",
               "Wet_Oxidized_ND", "Wet_Reduced_ND", "Wet_Organic_ND")

WRF_CMAQ <- merge(Radiation_WS, Tmax_WS, by = c("Date", "Water_Shed"))

for(i in 1:length(Var_Names)){
  WRF_CMAQ <- merge( WRF_CMAQ , get(paste0(Var_Names[i], "_WS")) , by = c("Date", "Water_Shed") )
}
