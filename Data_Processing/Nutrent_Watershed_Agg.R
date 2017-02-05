## Catrina Nowakowski
## Summer 2016 - November 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements

## This script takes the watersheds and aggragates the variables across each and then writes a CSV file for each variable that includes the
# new number, date, and watershed id

## Note:
# Run the process_NETCDF_Data_For_Use.R file first
# This has the dates changed to 04-01-2002 and 10-31-2002 
# for the variable names you eddited it, if you run the 40 min code again it you have to fix the file


#############################################################################################
## Reads in the variable

Variable_Names <- read.csv("Names_Variables_Nutrients.csv", header = TRUE, stringsAsFactors = FALSE)


## Loop through the list to load each variable file:
for(The_Var_Name in Variable_Names$V1){
  file_name <- paste0(The_Var_Name, ".csv")
  a <- read.csv(file_name, header = TRUE)
  a$Date <- as.Date(a$Date)
  assign(The_Var_Name, a)
}


Water_Sheds <- read.csv("Water_Sheds_Hand.csv")


#############################################################################################
## Identifie the watersheds 


Each_Water_Shead <- unique(Water_Sheds$Water_Shed)

## pull out the lat and long for that water shed
Names_Water_Sheds <- matrix(data = NA, nrow = length(Each_Water_Shead), ncol = 1)

for(i in 1:length(Each_Water_Shead)){
  single_water_sheds <- Water_Sheds[Water_Sheds$Water_Shed == Each_Water_Shead[i],]
  
  ## Test remove ectra 10 rows
  drops <- c("X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9", "X.10", 
             "X.11", "X.12", "X.13", "X.14", "X.15", "X.16", "X.17", "X.18",
             "FID_1","FID_2", "TNMID", "METASOURCE", "SOURCEDATA", "SOURCEORIG", "SOURCEFEAT", 
             "LOADDATE", "GNIS_ID", "X", "NAME", "HUC8", "AREAACRES", "AREASQKM", "STATES")
  single_water_sheds <- single_water_sheds[ , !(names(single_water_sheds) %in% drops)]
  
  single_water_sheds$Lat <- round(single_water_sheds$Lat, digits = 4)
  single_water_sheds$Long <- round(single_water_sheds$Long, digits = 4)
  
  
  
  ## End test
  assign(paste0("Water_Shed_", i), single_water_sheds)
  Names_Water_Sheds[i] <- paste0("Water_Shed_", i)
}


## Drops extra column from the variable:
for(i in Variable_Names$V1){
  drops <- c("X")
  a <- get(i)
  a <- a[ , !(names(a) %in% drops)]
  
  a$Lat <- round(a$Lat, digits = 4)
  a$Long <- round(a$Long, digits = 4)
  
  assign(i,a)
}




## Read in og lat long
Lat_Long <- read.csv("longitude_latitude.csv", header = TRUE)
names(Lat_Long) <- c("Long", "Lat")
Lat_Long$Lat <- round(Lat_Long$Lat, digits = 4)
Lat_Long$Long <- round(Lat_Long$Long, digits = 4)

test <- merge(Lat_Long, Water_Shed_7, by = c("Lat", "Long"), all = TRUE) ## This is looking good
test <- test[!is.na(test$Water_Shed), ]
test <- test[!is.na(test$Lat), ]

Water_Shed_7_Test <- Water_Shed_7[!is.na(Water_Shed_7$Long),]


#############################################################################################
## Function For just one water shed

agg_by_water_shead <- function(Variable, water_shed, year){
  
  # # A check on the function
  # Variable <- get("DRNN_2003")
  # water_shed <- Water_Shed_1
  # year = 2003
  
  
  ##Merge the data
  water_shed_merge <- merge(Variable, water_shed, by = c("Lat", "Long"))
  # water_shed_merge <- water_shed_merge[!is.na(water_shed_merge$day_91), ]
  
  ## Save for data frame id in end
  Water_Shed <- water_shed_merge$Water_Shed[1]

  ## Agg over the watershed 
  Agged_watershed <- aggregate(list(data = water_shed_merge[ , 3]), list(Date = cut(water_shed_merge$Date, "day")), mean)
  
  ## format stuff

  Agged_watershed$Date <- as.Date(Agged_watershed$Date)
  Agged_watershed$Water_Shed <- Water_Shed
  
  return(Agged_watershed)
}

#############################################################################################
## For variables loop through all of the water sheds
for(i in Variable_Names$V1){
  Variable <- get(i)
  year <- unlist(strsplit(i, "_", fixed = TRUE))
  year <- year[length(year)]
  
  ####################################################
  ## Round off some issues 
  Variable$Lat <- round(Variable$Lat, digits = 4)
  Variable$Long <- round(Variable$Long, digits = 4)
  
  
  ############################################################
  ## For just one variable loop through all of the water sheds
  
  ## Initialize data frame:
  Final_Data <- data.frame(Date = as.Date(NA), Data = NA, Water_Shed = NA)
  
  for(j in Names_Water_Sheds){
    
    function_return <- agg_by_water_shead(Variable, get(j), year)
    colnames(function_return)[2] <- i
    colnames(Final_Data)[2] <- i
    
    Final_Data <- rbind(Final_Data, function_return)
    
    
  }
  
  #############################################################################################
  ## Write CSV
  
  file_name <- paste(i, "_by_water_shed.csv",sep="")
  
  write.table(Final_Data, file = file_name, sep = ",", col.names = NA,
              qmethod = "double")
  
  
}
