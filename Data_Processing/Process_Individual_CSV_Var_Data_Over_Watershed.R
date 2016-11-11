## Figure out the dates for each year!



#############################################################################################
## Reads in files

Water_Sheds <- read.csv("Water_Sheds_Hand.csv")
Variable_Names <- read.csv("all_var_name.csv")

## Format the list

## Loop through the list to load each variable file:
for(The_Var_Name in Variable_Names){
  file_name <- paste0(The_Var_Name, ".csv")
  a <- read.csv(file_name, header = TRUE)
  assign(The_Var_Name, a)
}

#############################################################################################
## Drops extra variables:
drops <- c("FID_1","FID_2", "TNMID", "METASOURCE", "SOURCEDATA", "SOURCEORIG", "SOURCEFEAT", 
           "LOADDATE", "GNIS_ID", "X", "NAME", "HUC8", "AREAACRES", "AREASQKM", "STATES")
Water_Sheds <- Water_Sheds[ , !(names(Water_Sheds) %in% drops)]

## Drops extra variables:

for(i in Variable_Names){
  drops <- c("X")
  a <- get(i)
  a <- a[ , !(names(a) %in% drops)]
  assign(i,a)
}

#############################################################################################
## Identify the water sheds
Each_Water_Shead <- unique(Water_Sheds$Water_Shed)

## pull out the lat and long for that water shed
Names_Water_Sheds <- matrix(data = NA, nrow = length(Each_Water_Shead), ncol = 1)

for(i in 1:length(Each_Water_Shead)){
  single_water_sheds <- Water_Sheds[Water_Sheds$Water_Shed == Each_Water_Shead[i],]
  
  ## Test remove ectra 10 rows
  drops <- c("X.1", "X.2", "X.3", "X.4", "X.5", "X.6", "X.7", "X.8", "X.9", "X.10", 
             "X.11", "X.12", "X.13", "X.14", "X.15", "X.16", "X.17", "X.18" )
  single_water_sheds <- single_water_sheds[ , !(names(single_water_sheds) %in% drops)]
  
  
  ## End test
  assign(paste0("Water_Shed_", i), single_water_sheds)
  Names_Water_Sheds[i] <- paste0("Water_Shed_", i)
}


for(i in Names_Water_Sheds){
  a <- get(i)
  
  a$Lat <- round(a$Lat, digits = 4)
  a$Long <- round(a$Long, digits = 4)
  
  assign(i, a)
  
}


#############################################################################################
## Function For just one water shed

agg_by_water_shead <- function(Variable, water_shed){
  
  ## Merge the data
  water_shed_merge <- merge(Variable, water_shed)
  
  ## Save for data frame id in end
  Water_Shed <- water_shed_merge$Water_Shed[1]
  
  ## Drop out the identifers to aggragate
  drops <- c("Lat", "Long", "X", "NAME", "AREAACRES", "AREASQKM", "STATES", "HUC8", "Water_Shed")
  water_shed_merge <- water_shed_merge[ , !(names(water_shed_merge) %in% drops)]
  
  ## aggragate each column (this gives the average for the day for the water shed)
  water_shed_agg <- colMeans(water_shed_merge)
  water_shed_agg <- as.data.frame(water_shed_agg)
  
  
  ## Make singular water shead data frame:
  dates <- seq(as.Date("2007-05-01"), as.Date("2007-10-27"), by="days")
  Water_Shed_Data <- data.frame(Date = dates, the_data = water_shed_agg, Water_Shed = Water_Shed)
  
  
  
  return(Water_Shed_Data)
}

#############################################################################################
## For variables loop through all of the water sheds
for(i in Variable_Names){
  Variable <- get(i)
  
  ####################################################
  ## Round off some issues 
  Variable$Lat <- round(Variable$Lat, digits = 4)
  Variable$Long <- round(Variable$Long, digits = 4)
  
  
  ############################################################
  ## For just one variable loop through all of the water sheds
  
  ## Initialize data frame:
  Final_Data <- data.frame(Date = as.Date(NA), Radiation = NA, Water_Shed = NA)
  
  for(j in Names_Water_Sheds){
    
    function_return <- agg_by_water_shead(Variable, get(j))
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