# PROJECT: TEG6s HN Analysis of A10 Parameter

# Date: 23-Feb-2024
# R version environment: R - 4.3.1
# Function name: tma.Process
# Description: This program added Time to MA feature to the preprocessed data and cleans it

#------------PARAMETERS----------
# path - path to RDS file

#-------------RETURN-------------
# Processed and cleaned DataFrame

tma.Process <- function(path) {                        # should be addapted to read any file
  
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  
  # Reading data fro path
  df <- readRDS(path)
  
  # Adding Delta colun, removing nans and inconsistent values
  assay.df <- df %>%
    mutate(Delta = TMA_1 - R_1) %>%
    
    filter(!is.na(Delta) &
             COAG_STAT %in% c('Hypo', 'Normal', 'Hyper'))  # will be in configs
  
  return(assay.df)
}
