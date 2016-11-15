## Catrina Nowakowski 
## From July 2016 to November 2016
## UConn Civil and Environmental Engineering Department

## Currently updating this script form the summer. 

## What if I do the p-val thing and then at the end test how many variables to use with BIC?? this just
# BIC is not going to work becasue it will not konck out p-val issues and will favor correlations between 
# variables
## Can I compair BIC after every iteration, that way if it is higher after slecting a new variable then 
# it cuts the code off? After testing I think it may be better to use both BIC and ACI, BIC is harsh

# Var i, best p
# save bic i 1
# Var i,j,
# save bic i, j 1
# see if bic i, j i is < i 1

###################################################################################################
## GLS
## With and With out Log

## This script uses a Stepwise variable selection method designed by me.  Runs an iteration to select
# a variable based off of the ACI value then saves the best variable and repeats the process holding the
# slected variable constant and adds a new variable.  This code does 5 variables. After five initial 
# variables are slected it retries every variable while holding the four other variables constant to 
# check to see if a different variable is now more significant.  

## Then a final regression is run and analysed using:
## ... 

## What if an error occurs?


## Loading the dataframe from a seperate script
## The script does:
## ... 
source("Loading_Dataframe.R")


## Preserves the initial list of variable names for the second iteration through the variables
## It just makes it easier later on rather then reconstructing an eddited list. 
var_names_2 <- var_names

## Iniciates a tracker for the p-values
## This will be used to reflect on all of the model runs and choose the best ACI value

## Change to reflect ACI
p_val_tracker_i <- matrix(NA, length(var_names))
###############################
## For Variable i

## Loops through each variable being analized for the regression
for(i in 1:length(var_names)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_i <- df[[var_names[i]]]
  
  ## Makes a regression model using GLS
  ## Region_W, Region_C, and Region_E are catorgorical variables (values will either be 1 or 0)
  ## Exponential correlation was determined using the lowest ACI value of a model with 5 variables
  ## Lat and Long are used with the correlation and it is broken up biweekly to not alow for overlap in 
  # station readings
  # This is writen over every iteration
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i,
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
  ## Produces a summery of the output, seperates the ACI Value and I don't think I need the corr table...
## FIX FOR ACI
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_i[i] <- p_val$`p-value`[2]
## Ends the loop   
}

## Picks the p-value closest to zero and finds its location in the tracker vector
ii <-  which.min(abs(p_val_tracker_i - 0))
## Stors the value
ii_Val <- p_val_tracker_i[ii]
## Stors the name
ii_name <- var_names[ii]

## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
df_i$variable_i <- df[[var_names[ii]]]

## Removes the variable name that was slected from the list so it will not be pulled twice
var_names <- var_names[!var_names == ii_name]




## Initiates the tracker for the next variable Loop
p_val_tracker_j <- matrix(NA, length(var_names))
###############################
## For j

for(j in 1:length(var_names)){
  
  
  df_i$variable_j <- df[[var_names[j]]]
  
  
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j ,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_j[j] <- p_val$`p-value`[3]
}
jj <- which.min(abs(p_val_tracker_j - 0))
jj_Val <- p_val_tracker_j[jj]
jj_name <- var_names[jj]


df_i$variable_j <- df[[var_names[jj]]]


var_names <- var_names[!var_names == jj_name]

p_val_tracker_k <- matrix(NA, length(var_names))
###############################
## For K

for(k in 1:length(var_names)){
  
  df_i$variable_k <- df[[var_names[k]]]
  
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_k[k] <- p_val$`p-value`[4]
}
kk <- which.min(abs(p_val_tracker_k - 0))
kk_Val <- p_val_tracker_k[kk]
kk_name <- var_names[kk]


df_i$variable_k <- df[[var_names[kk]]]

var_names <- var_names[!var_names == kk_name]

p_val_tracker_l <- matrix(NA, length(var_names))
###############################
## For L

for(l in 1:length(var_names)){
  
  df_i$variable_l <- df[[var_names[l]]]
  
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_l[l] <- p_val$`p-value`[5]
}

ll <- which.min(abs(p_val_tracker_l - 0))
ll_Val <- p_val_tracker_l[ll]
ll_name <- var_names[ll]


df_i$variable_l <- df[[var_names[ll]]]

var_names <- var_names[!var_names == ll_name]

p_val_tracker_m <- matrix(NA, length(var_names))
###############################
## For M

for(m in 1:length(var_names)){
  
  df_i$variable_m <- df[[var_names[m]]]
  
  
  
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_m[m] <- p_val$`p-value`[6]
  
}

mm <- which.min(abs(p_val_tracker_m - 0))
mm_Val <- p_val_tracker_m[mm]
mm_name <- var_names[mm]


df_i$variable_m <- df[[var_names[mm]]]



var_names_2 <- var_names_2[!var_names_2 == jj_name]
var_names_2 <- var_names_2[!var_names_2 == kk_name]
var_names_2 <- var_names_2[!var_names_2 == ll_name]
var_names_2 <- var_names_2[!var_names_2 == mm_name]

p_val_tracker_i_2 <- matrix(NA, length(var_names_2))
###############################
## For i 2

for(i in 1:length(var_names_2)){
  
  
  df_i$variable_i <- df[[var_names_2[i]]]
  
  
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_i_2[i] <- p_val$`p-value`[2]
  
}

ii_2 <- which.min(abs(p_val_tracker_i_2 - 0))
ii_2_Val <- p_val_tracker_i_2[ii_2]
ii_2_name <- var_names_2[ii_2]

df_i$variable_i <- df[[var_names[ii_2]]]

var_names_2 <-var_names_2[!var_names_2 == ii_2_name]
var_names_2[length(var_names_2)+1] <- jj_name


p_val_tracker_j_2 <- matrix(NA, length(var_names_2))
###############################
## For j 2

for(j in 1:length(var_names_2)){
  
  
  df_i$variable_j <- df[[var_names_2[j]]]
  
  
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_j_2[j] <- p_val$`p-value`[3]
  
}

jj_2 <- which.min(abs(p_val_tracker_j_2 - 0))
jj_2_Val <- p_val_tracker_j_2[jj_2]
jj_2_name <- var_names_2[jj_2]

df_i$variable_j <- df[[var_names_2[jj_2]]]


var_names_2 <- var_names_2[!var_names_2 == jj_2_name]

var_names_2[length(var_names_2)+1] <- kk_name

p_val_tracker_k_2 <- matrix(NA, length(var_names_2))
###############################
## For k 2

for(k in 1:length(var_names_2)){
  
  
  df_i$variable_k <- df[[var_names_2[k]]]
  
  
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_k_2[k] <- p_val$`p-value`[4]
  
}

kk_2 <- which.min(abs(p_val_tracker_k_2 - 0))
kk_2_Val <- p_val_tracker_k_2[kk_2]
kk_2_name <- var_names_2[kk_2]

df_i$variable_k <- df[[var_names_2[kk_2]]]


var_names_2 <- var_names_2[!var_names_2 == kk_2_name]

var_names_2[length(var_names_2)+1] <- ll_name

p_val_tracker_l_2 <- matrix(NA, length(var_names_2))
###############################
## For l 2

for(l in 1:length(var_names_2)){
  
  
  df_i$variable_l <- df[[var_names_2[l]]]
  
  
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_l_2[l] <- p_val$`p-value`[5]
  
}

ll_2 <- which.min(abs(p_val_tracker_l_2 - 0))
ll_2_Val <- p_val_tracker_l_2[ll_2]
ll_2_name <- var_names_2[ll_2]

df_i$variable_l <- df[[var_names_2[ll_2]]]


var_names_2 <- var_names_2[!var_names_2 == ll_2_name]

var_names_2[length(var_names_2)+1] <- mm_name

p_val_tracker_m_2 <- matrix(NA, length(var_names_2))
###############################
## For m 2

for(m in 1:length(var_names_2)){
  
  
  df_i$variable_m <- df[[var_names_2[m]]]
  
  
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek))  
  
  
  Summary_Output <- summary(regress_model)
  p_val <-  as.data.frame(Summary_Output$tTable)
  corr_table <- as.data.frame(Summary_Output$corBeta)
  
  p_val_tracker_m_2[m] <- p_val$`p-value`[6]
  
}

mm_2 <- which.min(abs(p_val_tracker_m_2 - 0))
mm_2_Val <- p_val_tracker_m_2[mm_2]
mm_2_name <- var_names_2[mm_2]

df_i$variable_m <- df[[var_names_2[mm_2]]]


regress_model_final <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                           data = df_i, 
                           na.action="na.exclude", 
                           control = list(singular.ok = TRUE),
                           corr = corExp(form =~ Lat + Long | BiWeek)
                           
)  

# corAR1 659.7716 265.0276
# corExp 640.406 230.1096
# corGaus 641.7344 229.6906
# corSpher 643.8627 232.0682
### correlation = corSymm(form = ~1|Subject)

###################################################################################################
## Generate Plots
all_var <- df_i
drops = c("Region_W", "Region_C", "Region_E", "Lat", "Long", "BiWeek")
all_var <- all_var[ , !(names(all_var) %in% drops)]

plot(all_var)


















###################################################################################################
## VIF
library(car)
print("VIF: ")
Vif_Numbers <- vif(gls(Chlor~ variable_i + variable_j + variable_k + variable_l + variable_m,
                       
                       data = df_i, na.action="na.exclude") )
print(Vif_Numbers)

###################################################################################################
## Checking the Regression
library(cvTools)
Chlor = df$Chlor_Log

The_Cross_Val <- cvFit(regress_model_final, data = df_i, y = Chlor, K = length(df$Region_W))
print(The_Cross_Val)
## CVfit a function that takes a tenth of my data and sets it asid while it runs my model 
## and it calculates the residuals for the 10% then it will randomly slect another 10% 
## and redo all cross validation

## outputs mean bias lower the better


## FInd a way to plot and set K = n


###################################################################################################
Summary_Output <- summary(regress_model_final)
print(Summary_Output)

###################################################################################################
corelation_data_frame <- data.frame(variable_i = df_i$variable_i, variable_j = df_i$variable_j,
                                    variable_k = df_i$variable_k, variable_l = df_i$variable_l,
                                    variable_m = df_i$variable_m)
names(corelation_data_frame)<- c(ii_name, jj_name, kk_name, ll_name, mm_name)
corelation <- cor(corelation_data_frame)
print(corelation)

###################################################################################################
## Residuals
regress_model_resid <- residuals(regress_model_final)
plot(regress_model_resid, main = "Regression model Residuals", ylab = "Residuals"); abline( 0,0, col = "red")

###################################################################################################
## QQ Plots

qqnorm(regress_model_resid, main = "Regression Residuals QQ plot"); qqline(regress_model_resid, col = "red")


###################################################################################################
## Grab vs the predicted
x <-  as.data.frame(df_i$Chlor)
names(x) <- c("Samples")

y <-  as.data.frame(predict(regress_model_final))
names(y) <- c("Predicted")
y_no_na <- y[!is.na(y)]

RMSE <- round(sqrt( sum( (y$Predicted - x$Samples)^2 , na.rm = TRUE ) / length(y_no_na)), 3)

Correla <-  round(cor(x$Samples, y$Predicted, use="complete"), 3)

one_to_one <- qplot(x,y) + 
  geom_abline(intercept = 0, colour = "red", size = 1) + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 2.5, y = 0, size = 5,label =paste0("R2: ", Correla )) +
  annotate("text", x = 2.5, y = .5, size = 5,label =paste0("RMSE: ", RMSE )) +
  xlab("Sample Chlorophyll") + ylab("Predicted Chlorophyll")
show(one_to_one)


###################################################################################################
## Testing:

dat <- data.frame(Long_x = df$Long, Lat_y = df$Lat, resids = regress_model_resid )

dat <- dat[!is.na(dat$resids),]
coordinates(dat)<-c('Long_x','Lat_y')
# a <- bubble(dat,zcol='resids')
# plot(a)

var.mod<-variogram(resids~1,data=dat, cloud = TRUE) #, alpha=c(0,45,90,135))
plot(var.mod)
var.mod<-variogram(resids~1,data=dat) #, alpha=c(0,45,90,135))
plot(var.mod)
