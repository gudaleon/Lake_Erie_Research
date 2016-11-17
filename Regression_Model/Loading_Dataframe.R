## Catrina Nowakowski 
## From July 2016 to November 2016
## UConn Civil and Environmental Engineering Department

## Currently updating this script form the summer. 


###################################################################################################
## Librarys
library(foreign)
library(nlme)
library(ggplot2)
library(usdm)
library(gstat)


###################################################################################################
## Reading in files
Data <- read.csv("Data_Frame_Lat_Long.csv")
# LEC_Chlor_Data <- read.csv("LEC_Chlor_Data.csv")

###################################################################################################
## Format Data Frame
Data$Date <- as.Date(Data$Date, "%Y-%m-%d")



# drops <- c("X")
# df <- df[ , !(names(df) %in% drops)]
###################################################################################################
## Adjusting Variables
Data$Chlor_Log <- log(Data$Chlor)
Data$Chlor_Log[Data$Chlor_Log == -Inf] <- NA


## Adding and Averaging 
Data$Total_Wet_InorD_water_shed <- (Data$Wet_Reduced_ND_by_water_shed + Data$Wet_Oxidized_ND_by_water_shed)
Data$Total_Dry_InorD_water_shed <- (Data$Dry_Reduced_ND_by_water_shed + Data$Dry_Oxidized_ND_by_water_shed)

Data$Total_Wet_D_water_shed <- (Data$Wet_Reduced_ND_by_water_shed + Data$Wet_Organic_ND_by_water_shed +Data$Wet_Oxidized_ND_by_water_shed)


Data$Total_D_water_shed <- (Data$Wet_Reduced_ND_by_water_shed + Data$Wet_Organic_ND_by_water_shed +Data$Wet_Oxidized_ND_by_water_shed +
                              Data$Dry_Reduced_ND_by_water_shed + Data$Dry_Oxidized_ND_by_water_shed  )

Data$Total_Wet_InorD <- (Data$Wet_Reduced_ND  + Data$Wet_Oxidized_ND )
Data$Total_Dry_InorD <- (Data$Dry_Reduced_ND  + Data$Dry_Oxidized_ND )

Data$Total_Wet_D <- (Data$Wet_Reduced_ND  + Data$Wet_Organic_ND  +Data$Wet_Oxidized_ND )

Data$Total_D <- (Data$Wet_Reduced_ND  + Data$Wet_Organic_ND  + Data$Wet_Oxidized_ND  +
                   Data$Dry_Reduced_ND  + Data$Dry_Oxidized_ND)

Data$Total_D_Inor <- (Data$Wet_Reduced_ND  + Data$Wet_Oxidized_ND + Data$Dry_Reduced_ND  + Data$Dry_Oxidized_ND )

Data$Phos_A_Tot <- (Data$AMP_07_by_water_shed +Data$AOP_07_by_water_shed)

Data$Nitro_A_Tot <- (Data$ANH3_07_by_water_shed + Data$ANO3_07_by_water_shed +Data$AON_07_by_water_shed)

Data$Nitro_A_Inorganic <- (Data$ANH3_07_by_water_shed + Data$ANO3_07_by_water_shed)

Data$Taverage <- (Data$Tmax +Data$Tmin)/2

Data$Taverage_by_water_shed <- (Data$Tmax_by_water_shed +Data$Tmin_by_water_shed)/2


## Put windspeed at 5 mph

###################################################################################################
## Grab Chlor

## MAKE SURE YOU CHOOSE LOG OR NO LOG
# df <- Data[!is.na(Data$Chlor), ]

df <- Data[!is.na(Data$Chlor_Log), ]



## Change all NA values to zero
#df$Q_cfs[is.na(df$Q_cfs)] <- 0


## Take out high chlor 
#df <- df[df$Chlor < 15,]


###################################################################################################
## Lagging Variables


lag <- function(lag, Variable, df){
  ## Date Format
  Data$Date <- as.Date(Data$Date, "%Y-%m-%d")
  
  Sta_id <- c(3,4,5,6,7,8,9,10,11,12,15,16,17,18)
  j = 1
  Sta_names <- matrix(NA, length(Sta_id))
  for(i in Sta_id){
    
    Sta <- i
    
    ## Grab the water shed 
    All_Dates_All_Var <- Data[Data$Sta == Sta, ]
    
    ## Grab the initial dates
    Chlor <- All_Dates_All_Var[!is.na(All_Dates_All_Var$Chlor), ]
    Chlor_Ini_Dates <- Chlor$Date
    
    ## Make the new Lag date
    Chlor_alter_dates <- as.Date(Chlor_Ini_Dates) + lag
    Chlor_alter_dates <- data.frame(Date = Chlor_alter_dates)
    
    ## Pull one variable for the lag dates
    alter_Dates_All_Var <- merge(Chlor_alter_dates, All_Dates_All_Var)
    alter_Dates_One_Var <- alter_Dates_All_Var[[Variable]]
    
    ## attach the initial date to the lagged variable
    new_var <- data.frame(Date = Chlor_Ini_Dates, new = alter_Dates_One_Var, Sta = Sta)
    names(new_var) <- c("Date", paste0(Variable, "_Lag"), "Sta")
    
    assign(paste0(Variable, "_", Sta), new_var)
    Sta_names[j] <- paste0(Variable, "_", Sta)
    
    j = j+1
    
  }
  
  new_var <- merge(get(Sta_names[1]), get(Sta_names[2]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[3]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[4]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[5]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[6]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[7]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[8]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[9]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[10]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[11]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[12]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[13]), all = TRUE)
  new_var <- merge(new_var, get(Sta_names[14]), all = TRUE)
  
  new_var <- data.frame(new_var)
  
  df <- merge(df, new_var)
  drops <- c("X")
  df <- df[ , !(names(df) %in% drops)]
  
  names(df)[names(df) == paste0(Variable, "_Lag")] <- paste0(Variable, "_", lag, "L")
  
  return(df)
}

###################################################################################################
## Lagging Variables:
df <- lag(-1, "Q_cfs", df)
df <- lag(-2, "Q_cfs", df)
df <- lag(-3, "Q_cfs", df)
df <- lag(-4, "Q_cfs", df)
df <- lag(-5, "Q_cfs", df)
df <- lag(-6, "Q_cfs", df)
df <- lag(-7, "Q_cfs", df)

df <- lag(-7, "Precipitation", df)

df <- lag(-3, "Precipitation_by_water_shed", df)
df <- lag(-4, "Precipitation_by_water_shed", df)
df <- lag(-5, "Precipitation_by_water_shed", df)

df <- lag(-3, "Windspeed", df)
df <- lag(-4, "Windspeed", df)
df <- lag(-5, "Windspeed", df)

df <- lag(-1, "Water_Temp_C", df)
df <- lag(-6, "Water_Temp_C", df)
df <- lag(-7, "Water_Temp_C", df)

df <- lag(-1, "Taverage", df)
df <- lag(-7, "Taverage", df)

df <- lag(-6, "Taverage_by_water_shed", df)
df <- lag(-7, "Taverage_by_water_shed", df)

df <- lag(-1, "R_humidity_by_water_shed", df)
df <- lag(-6, "R_humidity_by_water_shed", df)

df <- lag(-1, "R_humidity", df)
df <- lag(-2, "R_humidity", df)

df <- lag(-1, "outsensible_Wm2", df)
df <- lag(-3, "outsensible_Wm2", df)
df <- lag(-4, "outsensible_Wm2", df)
df <- lag(-5, "outsensible_Wm2", df)
df <- lag(-6, "outsensible_Wm2", df)
df <- lag(-7, "outsensible_Wm2", df)

df <- lag(-1, "Outlatent_Wm2", df)
df <- lag(-2, "Outlatent_Wm2", df)
df <- lag(-3, "Outlatent_Wm2", df)
df <- lag(-4, "Outlatent_Wm2", df)
df <- lag(-5, "Outlatent_Wm2", df)
df <- lag(-6, "Outlatent_Wm2", df)
df <- lag(-7, "Outlatent_Wm2", df)

df <- lag(-1, "ET_mm", df)
df <- lag(-2, "ET_mm", df)
df <- lag(-3, "ET_mm", df)
df <- lag(-4, "ET_mm", df)
df <- lag(-5, "ET_mm", df)
df <- lag(-6, "ET_mm", df)
df <- lag(-7, "ET_mm", df)

df <- lag(-1, "Radiation_by_water_shed", df)
df <- lag(-2, "Radiation_by_water_shed", df)
df <- lag(-3, "Radiation_by_water_shed", df)
df <- lag(-4, "Radiation_by_water_shed", df)
df <- lag(-5, "Radiation_by_water_shed", df)

df <- lag(-3, "Radiation", df)
df <- lag(-4, "Radiation", df)
df <- lag(-5, "Radiation", df)
df <- lag(-6, "Radiation", df)
df <- lag(-7, "Radiation", df)

df <- lag(-2, "Net.heat.flux_Wm2", df)
df <- lag(-3, "Net.heat.flux_Wm2", df)
df <- lag(-4, "Net.heat.flux_Wm2", df)
df <- lag(-5, "Net.heat.flux_Wm2", df)
df <- lag(-6, "Net.heat.flux_Wm2", df)
df <- lag(-7, "Net.heat.flux_Wm2", df)

df <- lag(-1, "net.downward.radiation_Wm2", df)
df <- lag(-2, "net.downward.radiation_Wm2", df)
df <- lag(-3, "net.downward.radiation_Wm2", df)
df <- lag(-4, "net.downward.radiation_Wm2", df)
df <- lag(-5, "net.downward.radiation_Wm2", df)
df <- lag(-6, "net.downward.radiation_Wm2", df)
df <- lag(-7, "net.downward.radiation_Wm2", df)

df <- lag(-2, "Wet_Oxidized_ND_by_water_shed", df)
df <- lag(-3, "Wet_Oxidized_ND_by_water_shed", df)
df <- lag(-6, "Wet_Oxidized_ND_by_water_shed", df)
df <- lag(-7, "Wet_Oxidized_ND_by_water_shed", df)

df <- lag(-3, "Wet_Organic_ND_by_water_shed", df)
df <- lag(-6, "Wet_Organic_ND_by_water_shed", df)

df <- lag(-1, "Wet_Reduced_ND", df)
df <- lag(-4, "Wet_Reduced_ND", df)
df <- lag(-6, "Wet_Reduced_ND", df)

df <- lag(-4, "Wet_Reduced_ND_by_water_shed", df)
df <- lag(-5, "Wet_Reduced_ND_by_water_shed", df)
df <- lag(-6, "Wet_Reduced_ND_by_water_shed", df)
df <- lag(-7, "Wet_Reduced_ND_by_water_shed", df)

df <- lag(-1, "ANO3_07_by_water_shed", df)
df <- lag(-4, "ANO3_07_by_water_shed", df)
df <- lag(-5, "ANO3_07_by_water_shed", df)
df <- lag(-6, "ANO3_07_by_water_shed", df)
df <- lag(-7, "ANO3_07_by_water_shed", df)

df <- lag(-1, "AOP_07_by_water_shed", df)
df <- lag(-4, "AOP_07_by_water_shed", df)

df <- lag(-1, "AMP_07_by_water_shed", df)
df <- lag(-3, "AMP_07_by_water_shed", df)
df <- lag(-4, "AMP_07_by_water_shed", df)
df <- lag(-5, "AMP_07_by_water_shed", df)
df <- lag(-6, "AMP_07_by_water_shed", df)

df <- lag(-4, "ANH3_07_by_water_shed", df)
df <- lag(-5, "ANH3_07_by_water_shed", df)
df <- lag(-6, "ANH3_07_by_water_shed", df)

df <- lag(-2, "Dry_Reduced_ND_by_water_shed", df)
df <- lag(-4, "Dry_Reduced_ND_by_water_shed", df)
df <- lag(-5, "Dry_Reduced_ND_by_water_shed", df)
df <- lag(-6, "Dry_Reduced_ND_by_water_shed", df)
df <- lag(-7, "Dry_Reduced_ND_by_water_shed", df)

df <- lag(-2, "Dry_Reduced_ND", df)
df <- lag(-3, "Dry_Reduced_ND", df)
df <- lag(-5, "Dry_Reduced_ND", df)
df <- lag(-6, "Dry_Reduced_ND", df)
df <- lag(-7, "Dry_Reduced_ND", df)

df <- lag(-2, "Dry_Oxidized_ND", df)
df <- lag(-7, "Dry_Oxidized_ND", df)

df <- lag(-1, "Dry_Oxidized_ND_by_water_shed", df)
df <- lag(-6, "Dry_Oxidized_ND_by_water_shed", df)

df <- lag(-5, "Total_Dry_InorD", df)
df <- lag(-6, "Total_Dry_InorD", df)
df <- lag(-7, "Total_Dry_InorD", df)

df <- lag(-6, "Total_Dry_InorD_water_shed", df)

df <- lag(-3, "Total_Wet_InorD_water_shed", df)

df <- lag(-1, "Nitro_A_Inorganic", df)
df <- lag(-3, "Nitro_A_Inorganic", df)
df <- lag(-4, "Nitro_A_Inorganic", df)
df <- lag(-6, "Nitro_A_Inorganic", df)

df <- lag(-1, "Phos_A_Tot", df)
df <- lag(-4, "Phos_A_Tot", df)

df <- lag(-5, "Total_D", df)

df <- lag(-1, "SM1_mm", df)
df <- lag(-2, "SM1_mm", df)

df <- lag(-5, "SM2", df)

df <- lag(-3, "SM3", df)
df <- lag(-4, "SM3", df)

###################################################################################################
## Get Rid of all NA values left in df

df[is.na(df)] <- 0

###################################################################################################
## Initialize Data Frame
df_i <- data.frame(Region_W = df$Region_W, Region_C = df$Region_C, Region_E = df$Region_E, Chlor = df$Chlor_Log,
                   Long = df$Long, Lat = df$Lat, BiWeek = df$BiWeek)


###################################################################################################
## variable Names

var_names <- names(df)
drops <- c("Chlor", "Chlor_Log", "Date", "Region_W", "Region_C", "Region_E", "Lat", "Long", "Water_Shed", 
           "Year", "Day", "Month", "BiWeek", "Sta", "Phosphrous", 
           "SWE_mm")


var_names <- var_names[!var_names %in% drops]

# ## Unpaired GLS Log transform
# var_names <- c("Outlatent_Wm2_-7L", "Net.heat.flux_Wm2_-5L", "outsensible_Wm2_-7L", "Radiation_by_water_shed_-5L", "ANO3_07_by_water_shed_-1L")
