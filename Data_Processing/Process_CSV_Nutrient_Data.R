## Catrina Nowakowski
## Summer 2016 - November 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements


## Made a key in this file to deal with the lat and long col row thing that Ellen explained in an email once upon a time


## Note used lat_long file again, checked out and it all lines up correct!
#############################################################################################
## Load the longitude and Latitude for all files
print("Reading Latitude and Longitude...")

Lat_Long <- read.csv("longitude_latitude.csv", header = TRUE)

#############################################################################################
## Make a table of the locations for lat and long to make sense of this row column stuff
## Ellens email explaining the table:
# So the main issue is that the .csv files will only have row and col.  I think we have gone through 
# this before, but the numbering runs from the lower left (row=1,col=1) to upper right (row=299,col=459)
# And so gridid’s should run from 1 through 137241.  You may already have this file, but I am having a 
# file titled “beld…” put up that should provide additional links with lat/long, etc.

Lat_Long_Position<- c(1:length(Lat_Long$Longitude))
Lat_Long_Position <- split(Lat_Long_Position, 1:299)
Lat_Long_Position <- as.data.frame(Lat_Long_Position)
Lat_Long_Position <- t(Lat_Long_Position)
## Note V1 is col1, X1 is row1  [row, col]

## How to use:
# from the nutrient file for example I want to know the lat and long of col 15 row 20:
# Location <- Lat_Long_Position[20, 15]

Location <- Lat_Long_Position[1, 300]
Lat <- Lat_Long$Lat[Location]
Long <- Lat_Long$Long[Location]

## Next use this key to add a lat and long column to the nutrients and then make dataframes with date, nutrient, lat, long
## Format date column
## Pull the dates and area I want


#############################################################################################
## Making list of File names
file_names <- c("2003_ncd_bygrids_20030101__to_20031231.csv")  #, "2004_ncd_bygrids_20040101__to_20041231.csv",
                #"2005_ncd_bygrids_20050101__to_20051231.csv")
year <-2003

#############################################################################################
## Open Files
 
for(i in file_names){
  ## Read in file, IT TAKES FOREVER!
  a <- read.csv(i, header = TRUE)
  print("Read CSV...")
  
  ##Makes List of variable names
  var_names <- names(a)
  print("Listed Names")
  
  a$DATE <- as.numeric(a$DATE)
  
  ##Grabs Dates april 1st - oct 30th
  if(year == 2003){
    a <- a[a$DATE >= 91 & a$DATE <= 304, ]
    date_st <- paste0(year, "-04-01")
    date_end <- paste0(year, "-10-31")
    dates <- seq(as.Date(date_st), as.Date(date_end), by="days")
    Day = as.numeric(c(91:304))
    date_key <- data.frame(Dates = dates, Day = Day ) 
    
  }else if(year == 2004){
    a <- a[a$DATE >= 92 & a$DATE <= 305, ]
    date_st <- paste0(year, "-04-01")
    date_end <- paste0(year, "-10-31")
    dates <- seq(as.Date(date_st), as.Date(date_end), by="days")
    Day = as.numeric(c(92:305))
    date_key <- data.frame(Dates = dates, Day = Day ) 
    
  }else if(year == 2005){
    a <- a[a$DATE >= 91 & a$DATE <= 304, ]
    date_st <- paste0(year, "-04-01")
    date_end <- paste0(year, "-10-31")
    dates <- seq(as.Date(date_st), as.Date(date_end), by="days")
    Day = as.numeric(c(91:304))
    date_key <- data.frame(Dates = dates, Day = Day ) 
    
  }
  print("IDed Year")
  
  
  ## Make date Column
  a$Date <- NA
  a$Date <- as.Date(a$Date)
  
  ## Fill in the date column
  for(day in 1:length(91:304)){
    a$Date[a$DATE == date_key$Day[day] ] <- date_key$Dates[day]
  }
  
  ## Pull just the dates we want
  a <- a[!is.na(a$Date), ]
  
  ## I think I don't need this now
  
  # ## Cut down on data by the row and columns
  # # Lat_Long$Location <-  1:length(Lat_Long$Latitude)
  # 
  # ## What I learned with this is the largest location is 93520 and the lowest is 68682 so I can pick the colums that are inbetween these
  # # by looking at the summary of the lat_long_position matrix
  # # Lat_Long <- Lat_Long[Lat_Long$Latitude > 40 & Lat_Long$Latitude < 45, ]
  # # Lat_Long <- Lat_Long[Lat_Long$Longitude >-86 & Lat_Long$Longitude < -77, ]
  # 
  # ## what I have determined from looking is to Keep after V230 and drop 315 and after
  # 
  # ## So in "a" I should pull when col is greater than 229 and less than 315 
  # 
  # a <- a[a$col > 229 & a$col < 315, ]

  
  ## Makes a vector that repeats 1:299 exactly 459 times, idk why I can't just do it in one line...
  one_3rd <- rep(c(1:299), 153)
  row_key <- rep(one_3rd, 3)
  
  col_key <- rep(1:459, each = 299)
  
  
  ## Finishes key
  Lat_Long$col <- col_key
  Lat_Long$row <- row_key
  
  ## Merge the key with the data
  a <- merge.data.frame(a, Lat_Long)
  
  
  ## Old loop from before I used the merge function 
  # for(j in 1:length(a$DATE) ){
  #   # col <-a$col[j]
  #   # row <-a$row[j]
  #   
  #   Location <- Lat_Long_Position[a$row[j], a$col[j]]
  #   
  #   a$Lat[j] <- Lat_Long$Latitude[Location]
  #   a$Long[j] <- Lat_Long$Longitude[Location]    
  # }
  
  
  print("added Lat and Long Columns")
  
  ## Refineing to Area of Intrest
  Lat_Start  <- 40.27952566
  Lat_End    <- 44.3709869
  
  Long_Start <- -85.869140625
  Long_End   <- -77.673339
  
  
  a <- a[a$Lat  > Lat_Start   &
         a$Lat  < Lat_End     &
         a$Long > Long_Start  &   
         a$Long < Long_End, ] 
  print("Selected Area Of Interest")
  

  ## Loops through and makes each variable a seperate thing                
  for(var in var_names){
    b <- data.frame(a[var], a["Date"], a["Latitude"], a["Longitude"])
    
    # Write CSV
    The_CSV_Name <- paste0(var, "_", year, ".csv")
    
    write.table(b, file = The_CSV_Name, sep = ",", col.names = NA,
                qmethod = "double")
  }
  
  
  ## removes the whole matrix to make sure there is enough space
  rm(a)
  rm(b)
  
  
  ## FINAL END LOOP
  year = year + 1
}


