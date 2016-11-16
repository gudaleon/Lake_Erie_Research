## df_i is a very bad variable name

#source("Loading_Dataframe.R")

###############################################################################################################
## Function to run regression


gls_different_num_var <- function(num_of_var, df){

## Checks to see how many variables are nessary and slections appropriate function to run to make GLS model
if(num_of_var == 1){
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i,
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
} else if(num_of_var == 2){
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j ,
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
} else if(num_of_var == 3){
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k,
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
} else if(num_of_var == 4){
  regress_model <- gls(Chlor~ Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l,
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
} else if(num_of_var == 5){
  regress_model <- gls(Chlor~Region_W +Region_C +Region_E + variable_i + variable_j + variable_k + variable_l + variable_m,
                       data = df_i, na.action = na.omit, control = list(singular.ok = TRUE),
                       corr = corExp(form =~ Lat + Long | BiWeek)) 
  
}
  return(regress_model)
  ## END FUNCTION
}


###############################################################################################################
## Function to produce sample output

produce_sum_output <- function(regress_model, iteration, iterator){
  ## Produces a summery of the output, seperates the P-Value 
  
  if(iteration == 1){
    Summary_Output <- summary(regress_model)
    p_val <-  as.data.frame(Summary_Output$tTable)
    
    i = iterator
    p_val_tracker_i[i] <- p_val$`p-value`[4]
    return(p_val_tracker_i)
    
  } else if(iteration == 2){
    Summary_Output <- summary(regress_model)
    p_val <-  as.data.frame(Summary_Output$tTable)
  
    j = iterator
    p_val_tracker_j[j] <- p_val$`p-value`[5]
    return(p_val_tracker_j)
    
  } else if(iteration == 3){
    Summary_Output <- summary(regress_model)
    p_val <-  as.data.frame(Summary_Output$tTable)
   
    k = iterator
    p_val_tracker_k[k] <- p_val$`p-value`[6]
    return(p_val_tracker_k)
    
  } else if(iteration == 4){
    Summary_Output <- summary(regress_model)
    p_val <-  as.data.frame(Summary_Output$tTable)
  
    l = iterator
    p_val_tracker_l[l] <- p_val$`p-value`[7]
    return(p_val_tracker_l)
    
  } else if(iteration == 5){
    Summary_Output <- summary(regress_model)
    p_val <-  as.data.frame(Summary_Output$tTable)
   
    m = iterator
    p_val_tracker_m[m] <- p_val$`p-value`[8]
    return(p_val_tracker_m)
    
  }

  ## END FUNCTION
}



###############################################################################################################
## Function to find the best variable


best_var <- function(p_Val_tracker, var_names, df_i, df, iteration){
  ## identifys the best variable, removes it from the list, adds it to the df_i
  if(iteration == 1){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    ii <-  which.min(p_val_tracker_i)
    ## Stors the value
    ii_Val <- p_val_tracker_i[ii]
    ## Stors the name
    ii_name <- var_names[ii]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_i <- df[[var_names[ii]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == ii_name]
    
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("ii_name", ii_name, pos = 1)
   
    
  } else if(iteration == 2){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    jj <-  which.min(abs(p_val_tracker_j - 0))
    ## Stors the value
    jj_Val <- p_val_tracker_j[jj]
    ## Stors the name
    jj_name <- var_names[jj]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_j <- df[[var_names[jj]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == jj_name]
    
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("jj_name", jj_name, pos = 1)
    
    
  } else if(iteration == 3){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    kk <-  which.min(abs(p_val_tracker_k - 0))
    ## Stors the value
    kk_Val <- p_val_tracker_k[kk]
    ## Stors the name
    kk_name <- var_names[kk]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_k <- df[[var_names[kk]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == kk_name]
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("kk_name", kk_name, pos = 1)
    
    
  } else if(iteration == 4){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    ll <-  which.min(abs(p_val_tracker_l - 0))
    ## Stors the value
    ll_Val <- p_val_tracker_l[ll]
    ## Stors the name
    ll_name <- var_names[ll]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_l <- df[[var_names[ll]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == ll_name]
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("ll_name", ll_name, pos = 1)
    
    
  } else if(iteration == 5){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    mm <-  which.min(abs(p_val_tracker_m - 0))
    ## Stors the value
    mm_Val <- p_val_tracker_m[mm]
    ## Stors the name
    mm_name <- var_names[mm]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_m <- df[[var_names[mm]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == mm_name]
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("mm_name", mm_name, pos = 1)
    
  } else if(iteration == 11){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    ii <-  which.min(p_val_tracker_i)
    ## Stors the value
    ii_Val <- p_val_tracker_i[ii]
    ## Stors the name
    ii_name <- var_names[ii]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_i <- df[[var_names[ii]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == ii_name]
    
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("ii_2_name", ii_name, pos = 1)
    
    
  } else if(iteration == 22){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    jj <-  which.min(abs(p_val_tracker_j - 0))
    ## Stors the value
    jj_Val <- p_val_tracker_j[jj]
    ## Stors the name
    jj_name <- var_names[jj]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_j <- df[[var_names[jj]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == jj_name]
    
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("jj_2_name", jj_name, pos = 1)
    
    
  } else if(iteration == 33){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    kk <-  which.min(abs(p_val_tracker_k - 0))
    ## Stors the value
    kk_Val <- p_val_tracker_k[kk]
    ## Stors the name
    kk_name <- var_names[kk]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_k <- df[[var_names[kk]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == kk_name]
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("kk_2_name", kk_name, pos = 1)
    
    
  } else if(iteration == 44){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    ll <-  which.min(abs(p_val_tracker_l - 0))
    ## Stors the value
    ll_Val <- p_val_tracker_l[ll]
    ## Stors the name
    ll_name <- var_names[ll]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_l <- df[[var_names[ll]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == ll_name]
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("ll_2_name", ll_name, pos = 1)
    
    
  } else if(iteration == 55){
    ## Picks the p-value closest to zero and finds its location in the tracker vector
    mm <-  which.min(abs(p_val_tracker_m - 0))
    ## Stors the value
    mm_Val <- p_val_tracker_m[mm]
    ## Stors the name
    mm_name <- var_names[mm]
    
    ## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
    df_i$variable_m <- df[[var_names[mm]]]
    
    ## Removes the variable name that was slected from the list so it will not be pulled twice
    var_names <- var_names[!var_names == mm_name]
    
    assign("var_names", var_names, pos = 1)
    assign("df_i", df_i, pos = 1)
    assign("mm_2_name", mm_name, pos = 1)
    
  }
  
  ## END FUNCTION
}

## Save for second section
var_names_2 <- var_names
## Itorator for AIC and BIC
AIC_BIC_It <- 1
## Initiates the tracker for the next variable Loop
AIC_Track <- matrix(NA, 10)
BIC_Track <- matrix(NA, 10)

###############################################################################################################
## For Variable i

## Initiates the tracker for the next variable Loop
p_val_tracker_i <- matrix(NA, length(var_names))

## Loops through each variable being analized for the regression
for(i in 1:length(var_names)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_i <- df[[var_names[i]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(1, df_i)

  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_i <- produce_sum_output(regress_model, 1, i)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_i, var_names, df_i, df, 1)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(1, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It


###############################################################################################################
## For Variable j

## Initiates the tracker for the next variable Loop
p_val_tracker_j <- matrix(NA, length(var_names))

## Loops through each variable being analized for the regression
for(j in 1:length(var_names)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_j <- df[[var_names[j]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(2, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_j <- produce_sum_output(regress_model, 2, j)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_j, var_names, df_i, df, 2)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(2, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It


###############################################################################################################
## For Variable k

## Initiates the tracker for the next variable Loop
p_val_tracker_k <- matrix(NA, length(var_names))

## Loops through each variable being analized for the regression
for(k in 1:length(var_names)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_k <- df[[var_names[k]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(3, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_k <- produce_sum_output(regress_model, 3, k)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_k, var_names, df_i, df, 3)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(3, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It


###############################################################################################################
## For Variable l

## Initiates the tracker for the next variable Loop
p_val_tracker_l <- matrix(NA, length(var_names))

## Loops through each variable being analized for the regression
for(l in 1:length(var_names)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_l <- df[[var_names[l]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(4, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_l <- produce_sum_output(regress_model, 4, l)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_l, var_names, df_i, df, 4)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(4, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It


###############################################################################################################
## For Variable m

## Initiates the tracker for the next variable Loop
p_val_tracker_m <- matrix(NA, length(var_names))

## Loops through each variable being analized for the regression
for(m in 1:length(var_names)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_m <- df[[var_names[m]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(5, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_m <- produce_sum_output(regress_model, 5, m)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_m, var_names, df_i, df, 5)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(5, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It

###############################################################################################################
## SECOND TIME THROUGH THE VARIALBES: What this section does is runs through each variable in each position one more time to check to see that that is still
# the best one to use 

#################################################################
## Remove Current variable names in use from the list
var_names_2 <- var_names_2[!var_names_2 == jj_name]
var_names_2 <- var_names_2[!var_names_2 == kk_name]
var_names_2 <- var_names_2[!var_names_2 == ll_name]
var_names_2 <- var_names_2[!var_names_2 == mm_name]

###############################################################################################################
## For Variable i

## Initiates the tracker for the next variable Loop
p_val_tracker_i <- matrix(NA, length(var_names_2))

## Loops through each variable being analized for the regression
for(i in 1:length(var_names_2)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_i <- df[[var_names_2[i]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(1, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_i <- produce_sum_output(regress_model, 1, i)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_i, var_names_2, df_i, df, 11)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(5, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It

###############################################################################################################
## For Variable j

## Initiates the tracker for the next variable Loop
p_val_tracker_j <- matrix(NA, length(var_names_2))

## Loops through each variable being analized for the regression
for(j in 1:length(var_names_2)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_j <- df[[var_names_2[j]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(2, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_j <- produce_sum_output(regress_model, 2, j)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_j, var_names_2, df_i, df, 22)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(5, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It

###############################################################################################################
## For Variable k

## Initiates the tracker for the next variable Loop
p_val_tracker_k <- matrix(NA, length(var_names_2))

## Loops through each variable being analized for the regression
for(k in 1:length(var_names_2)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_k <- df[[var_names_2[k]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(3, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_k <- produce_sum_output(regress_model, 3, k)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_k, var_names_2, df_i, df, 33)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(5, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It

###############################################################################################################
## For Variable l

## Initiates the tracker for the next variable Loop
p_val_tracker_l <- matrix(NA, length(var_names_2))

## Loops through each variable being analized for the regression
for(l in 1:length(var_names_2)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_l <- df[[var_names_2[l]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(4, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_l <- produce_sum_output(regress_model, 4, l)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_l, var_names_2, df_i, df, 44)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(5, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It

###############################################################################################################
## For Variable m

## Initiates the tracker for the next variable Loop
p_val_tracker_m <- matrix(NA, length(var_names_2))

## Loops through each variable being analized for the regression
for(m in 1:length(var_names_2)){
  
  ## Adds the variable to the regression data frame, each iteration this is updated
  df_i$variable_m <- df[[var_names_2[m]]]
  
  ## Runs model using a function I made to use proper number of variables
  regress_model <- gls_different_num_var(5, df_i)
  
  ## Produces a summery of the output, seperates the P-Value
  p_val_tracker_m <- produce_sum_output(regress_model, 5, m)
}


## Picks the p-value closest to zero and finds its location in the tracker vector
## Stores the value
## Stores the name
## Adds the best variable that was found above in the data frame to be used in the rest of the itterations
## Removes the variable name that was slected from the list so it will not be pulled twice
best_var(p_Val_tracker_m, var_names_2, df_i, df, 55)

####################
## Check AIC and BIC
regress_model <- gls_different_num_var(5, df_i)
quick_sum <- summary(regress_model)
AIC_Track[AIC_BIC_It] <- quick_sum$AIC
BIC_Track[AIC_BIC_It] <- quick_sum$BIC
AIC_BIC_It <- 1 + AIC_BIC_It




#################################################################
## Run the Final Regression Model
regress_model_final <- gls_different_num_var(5, df_i)
  
  
  
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
Chlor = df$Chlor

The_Cross_Val <- cvFit(regress_model_final, data = df_i, y = Chlor )
print(The_Cross_Val)


# 
# library(cvTools)
# Chlor = df$Chlor_Log
# 
# The_Cross_Val <- cvFit(regress_model_final, data = df_i, y = Chlor, K = length(df$Region_W))
# print(The_Cross_Val)



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

####################
## Check AIC and BIC
ACI_BIC <- cbind(AIC_Track, BIC_Track)
ACI_BIC <- as.data.frame(ACI_BIC)
colnames(ACI_BIC) <- c("ACI", "BIC")

ACI_BIC$x <- 1:length(ACI_BIC$ACI)

## FINISH THIS HEre

AIC_BIC_Plot <- ggplot(ACI_BIC, aes(x)) +
                  geom_line(aes(y = ACI), color = "red") +
                  geom_point(aes(y = ACI), color = "red") +
                  geom_line(aes(y = BIC), color = "blue") +
                  geom_point(aes(y = BIC), color = "blue")
                 
show(AIC_BIC_Plot)

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