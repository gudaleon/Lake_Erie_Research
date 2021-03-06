## Catrina Nowakowski 
## 12-6-2016
## UConn Civil and Environmental Engineering Department

## New Script

## df_i is a very bad variable name
###############################################################################################################

## Split data randomly in to n groups
## Regress on n-1 groups n times
# Fit the omitted group to the new model - store all in same data frame (fitted n-1, measured n-1, fitted omit, measured omit)

## Plot the n-1 with the omitted 

## Iniate n and df, df is so I don't mess df_i up
# if n is a factor of 112 then comment out the two remander sections, if not be sure to include them
n = 112
df_i <- as.data.frame(df_i)
df <- as.data.frame(df_i)

## iniates names vectors
sample_name <- matrix(data = NA, nrow = 1, ncol = n)
set_name <- matrix(data = NA, nrow = 1, ncol = n)
regress_name <- matrix(data = NA, nrow = 1, ncol = n)
prediction_names <- matrix(data = NA, nrow = 1, ncol = n)
measured_val_names <- matrix(data = NA, nrow = 1, ncol = n)
RMSE <- matrix(data = NA, nrow = 1, ncol = n)
Correla <- matrix(data = NA, nrow = 1, ncol = n)
one_to_one <- matrix(data = NA, nrow = 1, ncol = n)


## Calculates the sample size
sample_size <- floor((length(df_i$Chlor))/n)


## Makes the sample dataframes to each be regressed on
for(i in 1:n){
  
  if(i <= n-1){
    df_sample_location <- sample(length(df$Chlor), size = sample_size)
    df_sample <- df[df_sample_location, ]
    rownames(df_sample) <-NULL
    
    assign(paste0("df_", i), df_sample)
    df <- df[-df_sample_location, ]
    rownames(df) <- 1:nrow(df)
    sample_name[i] <- paste0("df_", i)
    
    
  }else if(i == n){
    assign(paste0("df_", i), df)
    sample_name[i] <- paste0("df_", i)
    rm(df_sample)
    rm(df)
  }
  
}

## Merge all of the data back together but the one group that is ment to be left out
for(i in 1:n){
id_list <- 1:n
id_list <- id_list[-i]

df_set <- rbind( get( sample_name[id_list[1]] ), get(sample_name[id_list[2]]))

for(j in 3:(n-1)){
  df_set <- rbind(df_set, get(sample_name[id_list[j]]) )
}
assign(paste0("df_set_", i), df_set)  
set_name[i] <- paste0("df__set", i)

rm(df_set)

}

## Runs regression models with each set
i = 1
for(df in set_name){
regression <- gls_different_num_var(5, get(df))
assign(paste0("regression_", df), regression)
regress_name[i] <- paste0("regression_", df)

i = i+1
}

## Predict the fitted values and the omitted values
for(i in 1:n){
  predicted_values <- predict(get(regress_name[i]), get(sample_name[i]) )
  
  # if(i <= n-1){
  # 
  # a <- length(predicted_values)
  # for(j in 1:remander){
  #     predicted_values[(a+j)] <-NA
  #   }
  #   
  # }
  assign(paste0("prediction_", i), predicted_values)
  prediction_names[i] <- paste0("prediction_", i)
  
}


## Put all predictions in one data frame
Predictions <- cbind.data.frame( get( prediction_names[1] ), get(prediction_names[2]) )

for(i in 3:(n)){
  Predictions <- cbind.data.frame(Predictions,  get( prediction_names[i] ) )
}
names(Predictions) <- prediction_names


## Add measured values to prediction dataframe
for(i in 1:n){
  a <- get(sample_name[i])
  a <- a$Chlor
  
  # if(i <= n-1){
  #   remander <-  (length(df_i$Chlor) - sample_size*(10))  ## not quite sure why I need -1 
  # 
  #   a_length <- length(a)  
  #   for(j in 1:remander){
  #     a[(a_length+j)] <-NA
  #   }
  #   
  # }
  
  assign(paste0("Chlor_", i), a)
  measured_val_names[i] <- paste0("Chlor_", i)
}


for(i in 1:(n)){
  Predictions <- cbind.data.frame(Predictions,  get( measured_val_names[i] ) )
}
names(Predictions) <- c(prediction_names, measured_val_names)


###################################################################################################
## Merging all of the predictions in to one data frame

predict <- rbind.data.frame(prediction_1, prediction_2)
for(i in 3:n){
  predict <- rbind.data.frame(predict, get(prediction_names[i]) )
}
names(predict) <- c("Log_Predictions")


###################################################################################################
## Merging all of the observations in to one data frame
obser <- rbind.data.frame(Chlor_1, Chlor_2)
for(i in 3:n){
  obser <- rbind.data.frame(obser, get(measured_val_names[i]) )
}
names(obser) <- c("Log_Observations")


###################################################################################################
## Binding the Predictions and observations into the same dataframe
One_To_One_df <- cbind(obser, predict)
One_To_One_df$Observations <- exp(One_To_One_df$Log_Observations)
One_To_One_df$Predictions <- exp(One_To_One_df$Log_Predictions)

###################################################################################################
## Making all of the Plots!!
# x is sample and Y is predicted
library(ggplot2)
pdf("D:/OneDrive/Documents/School-Catrina/Research/Code Post Conferance/Regression Model/CV_Plots.pdf")

#########################
## Grab vs the predicted
x <-  as.data.frame(df_i$Chlor)
names(x) <- c("Samples")

y <-  as.data.frame(predict(regress_model_final))
names(y) <- c("Predicted")
y_no_na <- y[!is.na(y)]

RMSE <- round(sqrt( sum( (y$Predicted - x$Samples)^2 , na.rm = TRUE ) / length(y_no_na)), 3)

Correla <-  round(cor(x$Samples, y$Predicted, use="complete"), 3)

one_to_one <- qplot(((x)),(y)) + 
  geom_abline(intercept = 0, colour = "red", size = 1) + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 2.5, y = 0, size = 5,label =paste0("R2: ", Correla )) +
  annotate("text", x = 2.5, y = .5, size = 5,label =paste0("RMSE: ", RMSE )) +
  xlab("Sample Chlorophyll ln(ug/L)") + ylab("Predicted Chlorophyll ln(ug/L)")
print(one_to_one)


#########################
## Grab vs the predicted One to one
x <-  as.data.frame(One_To_One_df$Observations)
names(x) <- c("Samples")

y <-  as.data.frame(One_To_One_df$Predictions)
names(y) <- c("Predicted")
y_no_na <- y[!is.na(y)]

RMSE <- round(sqrt( sum( (y$Predicted - x$Samples)^2 , na.rm = TRUE ) / length(y_no_na)), 3)

Correla <-  round(cor(x$Samples, y$Predicted, use="complete"), 3)

one_to_one <- qplot((x),(y)) + 
  geom_abline(intercept = 0, colour = "red", size = 1) + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 20, y = 0, size = 5,label =paste0("R2: ", Correla )) +
  annotate("text", x = 20, y = 2.5, size = 5,label =paste0("RMSE: ", RMSE )) +
  xlab("Sample Chlorophyll (ug/L)") + ylab("Predicted Chlorophyll (ug/L)")
print(one_to_one)


#########################
## Grab vs the predicted One to one with out points over 30
One_To_One_df_Alter <- One_To_One_df[One_To_One_df$Observations < 30, ]

x <-  as.data.frame(One_To_One_df_Alter$Observations)
names(x) <- c("Samples")

y <-  as.data.frame(One_To_One_df_Alter$Predictions)
names(y) <- c("Predicted")
y_no_na <- y[!is.na(y)]

RMSE <- round(sqrt( sum( (y$Predicted - x$Samples)^2 , na.rm = TRUE ) / length(y_no_na)), 3)

Correla <-  round(cor(x$Samples, y$Predicted, use="complete"), 3)

one_to_one <- qplot((x),(y)) + 
  geom_abline(intercept = 0, colour = "red", size = 1) + 
  geom_smooth(method = "lm", se = FALSE) +
  annotate("text", x = 15, y = 0, size = 5,label =paste0("R2: ", Correla )) +
  annotate("text", x = 15, y = 2.5, size = 5,label =paste0("RMSE: ", RMSE )) +
  xlab("Sample Chlorophyll (ug/L)") + ylab("Predicted Chlorophyll (ug/L)")
print(one_to_one)





#########################
## Group Plots
# 
# 
# for(i in 1:n){
# x <- Predictions[, (n+i)]
# y <- Predictions[, i]
# 
# RMSE[i] <- round(sqrt( sum( (y - x)^2 , na.rm = TRUE ) / length(y)), 3)
# 
# Correla[i] <-  round(cor(x, y, use="complete"), 3)
# 
# one_to_one <- qplot((10^x),(10^y)) +
#   geom_abline(intercept = 0, colour = "red", size = 1) +
#   geom_smooth(method = "lm", se = FALSE) +
#   annotate("text", x = 1, y = 0, size = 5,label =paste0("R2: ", Correla[i] )) +
#   annotate("text", x = 1, y = .5, size = 5,label =paste0("RMSE: ", RMSE[i] )) +
#   xlab("Sample Chlorophyll") + ylab("Predicted Chlorophyll") +
#   ggtitle(paste("Group", i))
# print(one_to_one)
# 
# }
# show(AIC_BIC_Plot)
# print(Vif_Numbers)
# print(Summary_Output)
# print(corelation)
# plot(corelation_data_frame)

## END PDF
dev.off()

mean_RMSE <- mean(RMSE)
print("RMSE")
print(mean_RMSE)
mean_Correla <- mean(Correla)
print("R^2")
print(mean_Correla)


#############################################################################################
print("writing csv") 
## To write a CSV file for input to Excel one might use

One_To_One_df$in_sample_chlor <- exp(df_i$Chlor)
One_To_One_df$in_sample_predic <- exp(predict(in_sample)

The_CSV_Name <- "Sample_Predicted.csv"

write.table(One_To_One_df, file = The_CSV_Name, sep = ",", col.names = NA,
            qmethod = "double")
