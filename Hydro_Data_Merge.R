## List of file names
file_names <- c("Hydro_Sta3_a.csv", "Hydro_Sta4.csv","Hydro_Sta5.csv","Hydro_Sta6_a.csv","Hydro_Sta7.csv","Hydro_Sta8_a.csv",
                "Hydro_Sta9.csv","Hydro_Sta10_a.csv","Hydro_Sta11_a.csv","Hydro_Sta12.csv","Hydro_Sta17.csv","Hydro_Sta18_a.csv")

## List of Var names
var_names <- c("Hydro_Sta3", "Hydro_Sta4", "Hydro_Sta5", "Hydro_Sta6", "Hydro_Sta7", "Hydro_Sta8", 
               "Hydro_Sta9", "Hydro_Sta10", "Hydro_Sta11", "Hydro_Sta12", "Hydro_Sta17", "Hydro_Sta18")

## IDs
IDs <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 17, 18)

## Read in stations
for(i in 1:length(var_names)){
  a <- read.csv(file_names[i], header = TRUE)
  ## Add station ID
  a$Sta <- IDs[i]
  assign(var_names[i], a)
}

## Drop stuff
# sta 17 has an x
drops <- c("X")
Hydro_Sta17 <- Hydro_Sta17[ , !(names(Hydro_Sta17) %in% drops)]

## Merge variables 
Hydro <-merge(Hydro_Sta3, Hydro_Sta4, all = TRUE)

for(i in 1:(length(var_names)-2)){
Hydro <-merge(Hydro, get(var_names[(i+2)]), all = TRUE)
}

## Add basins
Hydro$Region_W <- 0
Hydro$Region_C <- 0
Hydro$Region_E <- 0

Hydro$Region_W[Hydro$Sta == IDs[1]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[2]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[3]] <- 1
Hydro$Region_W[Hydro$Sta == IDs[4]] <- 1

Hydro$Region_C[Hydro$Sta == IDs[5]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[6]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[7]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[8]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[9]] <- 1
Hydro$Region_C[Hydro$Sta == IDs[10]] <- 1

Hydro$Region_E[Hydro$Sta == IDs[11]] <- 1
Hydro$Region_E[Hydro$Sta == IDs[12]] <- 1

## Add date
Hydro$Date <- as.Date(paste(Hydro$Year, Hydro$Month, Hydro$Day, sep = "."), format = "%Y.%m.%d")


