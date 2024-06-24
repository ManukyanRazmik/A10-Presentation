# PROJECT: TEG6s HN Analysis of A10 Parameter

# Date: 23-Feb-2024
# R version environment: R - 4.3.1

# ************************************************
# Function name: tma.Liklihood
# Description: This program calculates the probability of Time to MA being less then threshold + epsilon

#------------PARAMETERS----------
# df :         DataFram
# col.name :   column with necessary data
# e_max :      maximum value of epsilon. Default value is 10
# thd :        threshhold for Time to MA. Default is 10
# assay :      assay type for filtering
# coag :       coagulation type for filtering. Default is NULL 
# timepoint :  timepoint for filtering. Default is NULL
# indication : indication for filtering. Default is NULL

#-------------RETURN-------------
# JSON with likelihood values

require('jsonlite')

tma.Liklihood <- function(df, col.name, e_max = 10, thd = 10,
            assay = 'CFFH', coag = NULL, timepoint = NULL, indication = NULL){
  
  # Filtering dataset
  if(!is.null(assay)){
    df <- df[df$ASSAY == assay, ]
  }
  
  if(!is.null(coag)){
    df <- df[df$COAG_STAT == coag, ]
  }
  
  if(!is.null(timepoint)){
    df <- df[df$TIMEPOINT == timepoint, ]
  }
  
  if(!is.null(indication)){
    df <- df[df$INDICATION == indication, ]
  }
  
  data = df[[col.name]]
  
  # Defining epsilons
  e_values <- seq(0, e_max, by = 0.1)
  
  likelihood_values <- sapply(e_values, function(e) {
                                         mean(data <= thd + e)
                                        }
                              )
  result = list(e_val = e_values,
                liklihood = likelihood_values)
  
  json_result <- toJSON(result)
  
  # Return the JSON object
  return(json_result)
}
