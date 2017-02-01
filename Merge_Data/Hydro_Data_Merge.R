## Catrina Nowakowski
## November 27, 2016

## Project:  Prediction of harmful water quality parameters combining weather, air quality and ecosystem models with in-situ measurements


## List of file names
file_names <- c("Hydro_Sta3_a.csv", "Hydro_Sta4.csv","Hydro_Sta5.csv","Hydro_Sta6_a.csv","Hydro_Sta7.csv","Hydro_Sta8_a.csv",
                "Hydro_Sta9.csv","Hydro_Sta10_a.csv","Hydro_Sta11_a.csv","Hydro_Sta12.csv","Hydro_Sta15.csv","Hydro_Sta16_a.csv",
                "Hydro_Sta17.csv","Hydro_Sta18_a.csv")

## List of Var names
var_names <- c("Hydro_Sta3", "Hydro_Sta4", "Hydro_Sta5", "Hydro_Sta6", "Hydro_Sta7", "Hydro_Sta8", 
               "Hydro_Sta9", "Hydro_Sta10", "Hydro_Sta11", "Hydro_Sta12", "Hydro_Sta15", "Hydro_Sta16", "Hydro_Sta17", "Hydro_Sta18")

## IDs
IDs <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18) 



## Read in stations
for(i in 1:length(var_names)){
  a <- read.csv(file_names[i], header = TRUE)
  ## Add station ID
  a$Sta <- IDs[i]
  assign(var_names[i], a)
}

## Makes a list of the new station names: 
New_Ids <- c(58, 59, 60, 61, 91, 92, 30, 31, 32, 36, 37, 38, 42, 43, 73, 78, 99, 1010, 1515, 63)

New_var_names <- matrix(data = NA, nrow = length(New_Ids), ncol = 1)

for(i in 1:length(New_Ids)){
  New_var_names[i] <- paste0("Hydro_Sta", New_Ids[i])
}

## Assigning the data to the new station

## West Basin
Hydro_Sta58 <- Hydro_Sta3
Hydro_Sta58$Sta <- 58

Hydro_Sta59 <- Hydro_Sta4
Hydro_Sta59$Sta <- 59

Hydro_Sta60 <- Hydro_Sta3
Hydro_Sta60$Sta <- 60

Hydro_Sta61 <- Hydro_Sta3
Hydro_Sta61$Sta <- 61

Hydro_Sta91 <- Hydro_Sta3
Hydro_Sta91$Sta <- 91

Hydro_Sta92 <- Hydro_Sta5
Hydro_Sta92$Sta <- 92

## Central Basin
Hydro_Sta42 <- Hydro_Sta10
Hydro_Sta42$Sta <- 42

Hydro_Sta73 <- Hydro_Sta10
Hydro_Sta73$Sta <- 73

Hydro_Sta37 <- Hydro_Sta10
Hydro_Sta37$Sta <- 37

Hydro_Sta38 <- Hydro_Sta10
Hydro_Sta38$Sta <- 38

Hydro_Sta31 <- Hydro_Sta10
Hydro_Sta31$Sta <- 31

Hydro_Sta30 <- Hydro_Sta10
Hydro_Sta30$Sta <- 30

Hydro_Sta32 <- Hydro_Sta11
Hydro_Sta32$Sta <- 32

Hydro_Sta78 <- Hydro_Sta11
Hydro_Sta78$Sta <- 78

Hydro_Sta36 <- Hydro_Sta11
Hydro_Sta36$Sta <- 36

Hydro_Sta43 <- Hydro_Sta11
Hydro_Sta43$Sta <- 43

## East Basin
Hydro_Sta1515 <- Hydro_Sta16
Hydro_Sta1515$Sta <- 1515

Hydro_Sta1010 <- Hydro_Sta16
Hydro_Sta1010$Sta <- 1010

Hydro_Sta63 <- Hydro_Sta18
Hydro_Sta63$Sta <- 63

Hydro_Sta99 <- Hydro_Sta18
Hydro_Sta99$Sta <- 99

## Make a new station List
New_var_names <- as.vector(New_var_names)
Sta_Names <- c(var_names, New_var_names)



## IDs   #LEC W
IDs <- c(3, 4, 5, 6, 
         ## GLNPO Stations West
         58, 59, 60, 61, 91, 92, 
         
         #LEC C
         7, 8, 9, 10, 11, 12,
         ## GLNPO Stations Central 
         30, 31, 32, 36, 37, 38, 42, 43, 73, 78, 
         
         #LEC E
         15, 16, 17, 18, 
         ##GLNPO Stations East
         99, 1010, 1515, 63)


## Drop stuff
# sta 17 has an x
drops <- c("X")
Hydro_Sta17 <- Hydro_Sta17[ , !(names(Hydro_Sta17) %in% drops)]

## Merge variables 
Hydro <-merge(Hydro_Sta3, Hydro_Sta4, all = TRUE)

for(i in 1:(length(Sta_Names)-2)){
Hydro <-merge(Hydro, get(Sta_Names[(i+2)]), all = TRUE)
}

## Add basins and old new label:
Hydro$Region_W <- 0
Hydro$Region_C <- 0
Hydro$Region_E <- 0

Hydro$Old_New <- 0

Hydro$Region_W[Hydro$Sta == IDs[1]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[2]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[3]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[4]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[5]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[6]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[7]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[8]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[9]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[10]] <- 1

Hydro$Old_New[Hydro$Sta == IDs[5]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[6]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[7]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[8]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[9]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[10]] <- 1

Hydro$Region_C[Hydro$Sta == IDs[11]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[12]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[13]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[14]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[15]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[16]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[17]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[18]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[19]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[20]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[21]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[22]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[23]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[24]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[25]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[26]] <- 1


Hydro$Old_New[Hydro$Sta == IDs[17]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[18]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[19]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[20]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[21]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[22]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[23]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[24]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[25]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[26]] <- 1


Hydro$Region_E[Hydro$Sta == IDs[27]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[28]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[29]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[30]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[31]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[32]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[33]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[34]] <- 1

Hydro$Old_New[Hydro$Sta == IDs[31]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[32]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[33]] <- 1
Hydro$Old_New[Hydro$Sta == IDs[34]] <- 1



## Add date
Hydro$Date <- as.Date(paste(Hydro$Year, Hydro$Month, Hydro$Day, sep = "."), format = "%Y.%m.%d")

## Select Date range:
Hydro$Month <- as.numeric(Hydro$Month)
Hydro <- Hydro[Hydro$Month > 3 & Hydro$Month < 10, ]


