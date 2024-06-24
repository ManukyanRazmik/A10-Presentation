# PROJECT: TEG6s HN Analysis of A10 Parameter

# Date: 23-Feb-2024
# R version environment: R - 4.3.1

# ************************************************
# Function name: tma.tma.Cum
# Description: This program calculates cumulative function of Time to MA values

#------------PARAMETERS----------
# df:          DataFrame
# column.name: column containing necessary data
# thd:         threshold to calculate the exceeding probability. Default is 10
# assay:       assay type to be filtered. Default is NULL.
# coag:        coagulation type to be filtered. Default is NULL.
# timepoint:   timepoint to be filtered. Default is NULL.
# indication:  indication type to be filtered. Default is NULL.


#-------------RETURN-------------
# length, mean, standard deviation and confidence intervals for the mean

require('jsonlite')


tma.Cum <- function(df, col.name, thd = 10, assay = 'CFFH',
  coag = NULL, timepoint = NULL, indication = NULL) {
  
  # Filtering the data
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
  
  # Extract the values from the specified column
  x <- df[[col.name]]
  
  # Compute the empirical cumulative distribution function (CDF) for X
  cdf <- ecdf(x)   
  y <- cdf(x)
  
  # Compute the CDF value at the specified threshold
  cdf.thd <- cdf(thd)
  
  # Calculate quantiles
  quantiles <- seq(0, 1, by = 0.1)
  quantile_values <- quantile(cdf, probs = quantiles)
  
  # Create a list containing the results
  result <- list(x = x, y = y, y.thd = cdf.thd, 
                 quantiles = quantile_values, cdf.quantiles = quantiles)
  
  # Convert the list to JSON format
  json_result <- toJSON(result)
  
  # Return the JSON object
  return(json_result)
}
