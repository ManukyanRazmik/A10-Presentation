# Function to read the data from the path, create new column delta
# 
# Args:
#   n (numeric): The number for which factorial is to be calculated.
# 
# Returns:
#   numeric: The factorial of the input number.
# 
# Example:
#   factorial(5)  # Returns 120


library('dplyr')
library('ggplot2')

tma.Process <- function(path) {
  
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  df <- readRDS(path)
  assay.df <- df %>%
  mutate(
    Delta = TMA_1 - R_1
    ) %>% 
  filter(!is.na(Delta))
  
  return(assay.df)
}

