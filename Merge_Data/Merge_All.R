## Files to merge:
# Nutrient_ws
# Hydro
# WRF_CMAQ_Pnt
# WRF_CMAQ_ws
# Chlorophyll 
## Dont forget to include 2007 nutrient

## Read Chlorophyll data in
LEC_Chlor <- read.csv("LEC_Chlor_All.csv", header = TRUE, stringsAsFactors = FALSE)
LEC_Chlor$Date <- as.Date(LEC_Chlor$Date)
drops <- c("Phosphrous", "Station", "Year", "X", "Long", "Lat")
LEC_Chlor <- LEC_Chlor[ , !(names(LEC_Chlor) %in% drops)]
colnames(LEC_Chlor) <- c("Date", "Chlor", "Sta")
LEC_Chlor$Sta <- as.numeric(LEC_Chlor$Sta)
LEC_Chlor <- LEC_Chlor[!is.na(LEC_Chlor$Chlor), ]
# LEC_Chlor$Long <- round(LEC_Chlor$Long, 4)
# LEC_Chlor$Lat <- round(LEC_Chlor$Lat, 4)

## Function to make station ids for the data that is by watershed
tag_sta_numb <- function(data){
  
a <- data
  
a$Sta <- NA
b <- a
c <- a
d <- a
e <- a
f <- a
g <- a
h <- a


a$Sta[a$Water_Shed == 34 ] <- 3
a$Sta[a$Water_Shed == 78 ] <- 7
a$Sta[a$Water_Shed == 56  ] <- 5
a$Sta[a$Water_Shed == 910  ] <- 9
a$Sta[a$Water_Shed == 1112  ] <- 11
a$Sta[a$Water_Shed == 1516  ] <- 15
a$Sta[a$Water_Shed == 1718  ] <- 17

b$Sta[b$Water_Shed == 34  ] <- 4
b$Sta[b$Water_Shed == 78  ] <- 8
b$Sta[b$Water_Shed == 56  ] <- 6
b$Sta[b$Water_Shed == 910  ] <- 10
b$Sta[b$Water_Shed == 1112  ] <- 12
b$Sta[b$Water_Shed == 1516  ] <- 16
b$Sta[b$Water_Shed == 1718  ] <- 18

c$Sta[c$Water_Shed == 34  ] <- 60
d$Sta[d$Water_Shed == 34  ] <- 59
e$Sta[e$Water_Shed == 34  ] <- 58
f$Sta[f$Water_Shed == 34  ] <- 91
g$Sta[g$Water_Shed == 34  ] <- 61

c$Sta[c$Water_Shed == 56  ] <- 92

c$Sta[c$Water_Shed == 1112  ] <- 43
d$Sta[d$Water_Shed == 1112  ] <- 36
e$Sta[e$Water_Shed == 1112  ] <- 78
f$Sta[f$Water_Shed == 1112  ] <- 32

c$Sta[c$Water_Shed == 910  ] <- 42
d$Sta[d$Water_Shed == 910  ] <- 73
e$Sta[e$Water_Shed == 910  ] <- 37
f$Sta[f$Water_Shed == 910  ] <- 38
g$Sta[g$Water_Shed == 910  ] <- 31
h$Sta[h$Water_Shed == 910  ] <- 30


c$Sta[c$Water_Shed == 1516  ] <- 1515
d$Sta[d$Water_Shed == 1516  ] <- 1010

c$Sta[c$Water_Shed == 1718  ] <- 99
d$Sta[d$Water_Shed == 1718  ] <- 63

test <- merge(a,b, all = TRUE)
test <- merge(test, c, all = TRUE)
test <- merge(test, d, all = TRUE)
test <- merge(test, e, all = TRUE)
test <- merge(test, f, all = TRUE)
test <- merge(test, g, all = TRUE)
test <- merge(test, h, all = TRUE)

test <- test[ !is.na(test$Sta), ]

return(test)

}

##########################################################################################
## Read in GLNPO Data

## This paper on page five is how I decided to do the average of the top 10 meters
## A Band-Ration algorithm for Retrieving open-lake chlorophyll values from satellite observations of the Great Lakes

# Read in data
GLNPO <- read.csv("GLNPO_Data.csv", header = TRUE)

# Choose date range
GLNPO <- GLNPO[GLNPO$Year <= 2012 & GLNPO$Year >= 2002, ]
GLNPO <- GLNPO[GLNPO$Month <= 10 & GLNPO$Month >= 4, ]

# Format data
GLNPO$Date <- as.Date(GLNPO$Date)
drops <- c("X", "DO", "SB_DO", "dissolved_oxygen", "Year", "Month", "Day", "Lat", "Long")
GLNPO <- GLNPO[, !names(GLNPO) %in% drops]

# Isolate the chlor readings 
GLNPO <- GLNPO[!is.na(GLNPO$Chlor), ]
GLNPO <- GLNPO[GLNPO$Depth <= 10.49, ]


# average the chlorophyll by day
## Agg over the watershed 
GLNPO <- aggregate(list(Chlor = GLNPO[ , 3]), by = GLNPO[, c("Date", "Sta")], mean)


Nutrient_ws <- tag_sta_numb(Nutrient_ws)
WRF_CMAQ_WS <- tag_sta_numb(WRF_CMAQ_WS)


## Right now this just does the dates where all of the data is accounted for, could add more to the date column to cover the entire range in each
# data frame later... 
# Also don't forget to add 2007

Chlor <- rbind(GLNPO, LEC_Chlor)

df <- merge(WRF_CMAQ_pnt,Hydro , by = c("Date", "Sta", "Old_New"))
df <- merge(df, Nutrient_ws, by = c("Date", "Sta"))
df <- merge(df, WRF_CMAQ_WS, by = c("Date", "Sta", "Water_Shed"))
df <- merge(df, Chlor, by = c("Date", "Sta"), all = TRUE) #"Lat", "Long"

test <- df[!is.na(df$Chlor), ]
test <- test[!is.na(test$Wet_Organic_ND_WS), ]


## Adding the data from 2007
## This will probalby mean redoing all of the names and using r bind

Data_2007 <- read.csv("Data_Frame_Lat_Long.csv")

Chlor_2007 <- Data_2007[!is.na(Data_2007$Chlor), ]



## Figure out how to add biweek
## Add a column that counts how often a station occurs? Maybe do not include a 


# 
# ## IDs   #LEC W
# IDs <- c(3, 4, 5, 6, 
#          ## GLNPO Stations West
#          58, 59, 60, 61, 91, 92, 
#          
#          #LEC C
#          7, 8, 9, 10, 11, 12,
#          ## GLNPO Stations Central 
#          30, 31, 32, 36, 37, 38, 42, 43, 73, 78, 
#          
#          #LEC E
#          15, 16, 17, 18, 
#          ##GLNPO Stations East
#          99, 1010, 1515, 63)

## Figure out the chlorophyll data

## Set up the stats stuff 