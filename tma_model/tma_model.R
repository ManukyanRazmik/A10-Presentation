# PROJECT: TEG6s HN Analysis of A10 Parameter

# Date: 23-Feb-2024
# R version environment: R - 4.3.1

# ************************************************
# Function name: tma.Model
# Description: This program fit Mixed Effect Model to Time to MA data

#------------PARAMETERS----------
# df :           DataFrame containing exploratory and target variables
# assay :        assay type
# fixed :        fixed effects (vector)
# random :       random effect (vector)
# y :            dependent variable (vector)
# return.model : if TRUE, returns the model. Default FALSE


#-------------RETURN-------------
# JSON containg predictions and pdf of predictions

require('lme4')


# Define the function to calculate statistics
tma.Model <- function(df, assay = 'CFFH', fixed = c("INDICATION", "TIMEPOINT", "COAG_STAT"), random = c('SUBJECT_ID'), y = c('Delta'), return.model = F) {
  
  temp.df <- df %>%
    filter(ASSAY == assay)
  
  data.model <- subset(temp.df, select = c(fixed, random, y))
  
  model <- lmer(Delta ~ INDICATION + TIMEPOINT + COAG_STAT + (1 | SUBJECT_ID), data = data.model)
  if (return.model == T){
    return(model)
  }
  
  Delta_pred <- predict(model)
  
  density <- density(Delta_pred)
  
  result <- list(
    delta_pred = Delta_pred,
    x.dense = density$x,
    y.dense = density$y
  )
  
  # Convert the list to JSON format
  json_result <- toJSON(result)
  
  # Return the JSON object
  return(json_result)
}