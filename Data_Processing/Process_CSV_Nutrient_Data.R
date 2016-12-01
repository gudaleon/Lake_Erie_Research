## Catrina Nowakowski
## Summer 2016 - November 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements


## What does this file do?



## Note used lat_long file again, checked out and it all lines up correct!

#############################################################################################
## Make a table of the locations for lat and long to make sense of this row column stuff
## Ellens email explaining the table:
# So the main issue is that the .csv files will only have row and col.  I think we have gone through 
# this before, but the numbering runs from the lower left (row=1,col=1) to upper right (row=299,col=459)
# And so gridid’s should run from 1 through 137241.  You may already have this file, but I am having a 
# file titled “beld…” put up that should provide additional links with lat/long, etc.

Lat_Long_Position<- c(1:137241)
Lat_Long_Position <- split(Lat_Long_Position, 1:299)
Lat_Long_Position <- as.data.frame(Lat_Long_Position)
Lat_Long_Position <- t(Lat_Long_Position)
## Note V1 is col1, X1 is row1  [row, col]

## How to use:
# from the nutrient file for example I want to know the lat and long of col 15 row 20:
# Location <- Lat_Long_Position[20, 15]
# Lat <- Lat_Long$Lat[Location]
# Long <- Lat_Long$Long[Location]

## Next use this key to add a lat and long column to the nutrients and then make dataframes with date, nutrient, lat, long
## Format date column
## Pull the dates and area I want

#############################################################################################
## Load the longitude and Latitude for all files
print("Reading Latitude and Longitude...")

Lat_Long <- read.csv("longitude_latitude.csv", header = TRUE)

#############################################################################################
## Making list of File names
file_names <- c("2003_ncd_bygrids_20030101__to_20031231.csv", "2004_ncd_bygrids_20040101__to_20041231.csv",
                "2005_ncd_bygrids_20050101__to_20051231.csv")
year <-2003

#############################################################################################
## Open Files
 
for(i in file_names){
  ## Read in file, IT TAKES FOREVER!
  a <- read.csv(i, header = TRUE)
  assign(paste0("Nutrient_", year), a)
  rm(a)
  
  ##Save the Name of the file
  file_var_name <- paste0("Nutrient_", year)
  
  ##Makes List of variable names
  var_names <- names(get(file_var_name))
  
  ## Loops through and makes each variable a seperate thing CHANGE THIS TO WHAT YOU WROTE ABOVE 
  for(var in var_names){
    a <- get(file_var_name)
    assign(var, a[var_name])
  }
  
  ## removes the whole matrix to make sure there is enough space
  rm(a)
  rm(get(file_var_name))
  
  
  ## FINAL END LOOP
  year = year + 1
}

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

 
#############################################################################################
print("writing csv") 
## To write a CSV file for input to Excel use

The_CSV_Name <- "Var_Name_List.csv"

write.table(all_var_name, file = The_CSV_Name, sep = ",", col.names = NA,
            qmethod = "double")