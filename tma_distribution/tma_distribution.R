# PROJECT: TEG6s HN Analysis of A10 Parameter

# Date: 23-Feb-2024
# R version environment: R - 4.3.1

# ************************************************
# Function name: tma.Stats
# Description: This program added calculates statistics for the given dataset

#------------PARAMETERS----------
# col.data - DataFrame column

#-------------RETURN-------------
# length, mean, standard deviation and confidence intervals for the mean


# *************************************************
# Function name: tma.Distribution
# Description: This program returns JSON containing statistics density function,
#              probability being greater then threshold along with main data
               

#------------PARAMETERS----------
# df:          DataFrame
# column.name: column containing necessary data
# thd:         threshold to calculate the exceeding probability. Default is 10
# assay:       assay type to be filtered. Default is NULL.
# coag:        coagulation type to be filtered. Default is NULL.
# timepoint:   timepoint to be filtered. Default is NULL.
# indication:  indication type to be filtered. Default is NULL.

#-------------RETURN-------------
# JSON with statistics


require('jsonlite')

# Define the function to calculate statistics
tma.Stats <- function(col.data) {
  
  # Length of data
  n <- length(col.data)
  
  # Data mean 
  mean.delta <- mean(col.data)
  
  # Data median
  median.delta <- median(col.data)
  
  # Data standard deviation
  std.delta <- sd(col.data)
  
  # Standard error
  se.delta <- std.delta / sqrt(n)
  z.critical <- qnorm(0.975)
  margin.of.error <- z.critical * se.delta
  
  # Confidence intervals for the mean
  ci.delta <- c(mean.delta - margin.of.error, mean.delta + margin.of.error)
  
  return(c(n, mean.delta, median.delta, std.delta, ci.delta[1], ci.delta[2]))
}



# Define the function to calculate distribution and return JSON
tma.Distribution <- function(df, column.name = 'Delta', assay = NULL, thd = 10, 
        coag = NULL, timepoint = NULL, indication = NULL) {
  
  # Checking filters
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

  # Extract column data
  col.data <- df[[column.name]]
 

  # Calculate density estimation
  density <- density(col.data)

  # Calculate probability greater than thd
  probability_greater_than_thd <- mean(col.data > thd)
  
  # Using Stats function to calculate statistics
  stats <- tma.Stats(col.data)

  result <- list(
    data = col.data,
    x.dense = density$x,
    y.dense = density$y,
    thd = thd,
    prob.thd = probability_greater_than_thd,
    n = stats[1],
    mean = stats[2],
    median = stats[3],
    std = stats[4],
    ci.min = stats[5],
    ci.max = stats[6],
    timepoint = timepoint,
    indication = indication,
    assay = assay
  )

  # Convert the list to JSON format
  json_result <- toJSON(result)

  # Return the JSON object
  return(json_result)
}
