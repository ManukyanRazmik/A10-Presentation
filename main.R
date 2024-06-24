# Get the current directory
current_directory <- getwd()
# Set the working directory to the current directory
setwd(current_directory)

# Importing necessary libraries
libs = c("dplyr", "ggplot2", "ggpmisc", "lme4", "Matrix", "cowplot", "writexl", "tibble", "lmerTest")
lapply(libs, require, character.only = TRUE)

# Importing necessary modules
source('data_processing/tma_data_processing_new.R')
source('tma_distribution/tma_distribution.R')
source('tma_model/tma_model.R')
source('tma_liklihood/tma_liklihood.R')
source('tma_cumulative/tma_cumulative.R')

# Defining path to processed data 
main.path <- "Y:/HAE_TEG Publications/"
base.path <- "TEG 6s HN Analysis of A10 Parameter/Biometrics/Data/"
dataset.path <- "Final Analysis Dataset/"
file.name <- "final_analysis_dataset.rds"
path <- paste0(main.path, base.path, dataset.path, file.name)

save.path <- "plots/"

# Cleaning data
data = tma.Process(path = path)

# filt.data = data %>%
#   mutate(Delta = TMA_1 - R_1) %>%
#   
#   filter(!is.na(Delta) &
#            COAG_STAT %in% c('Hypo', 'Normal', 'Hyper'))

# Deriving values for plotting distribution

# Defining points necessary for plotting violins

min_x <- min(data$Delta)
max_x <- max(data$Delta)

assays <- unique(data$ASSAY)

coags <- unique(data$COAG_STAT)
timepoints <- unique(data$TIMEPOINT)
indications <- unique(data$INDICATION)

cols = c('blue', 'green', 'red')


# Plotting for Coagulation

for (assay in assays) {
  temp.list.coag <- list()
  temp.list.time <- list()
  temp.list.indi <- list()

  dist.delta = fromJSON(tma.Distribution(df = data,
                                       column.name = 'Delta',
                                       assay = assay))
  print(c(dist.delta$ci.max, dist.delta$ci.min))
  # Fitting model to data
  model.delta = fromJSON(tma.Model(df = data,
                                 assay = assay))

  model.sum <- tma.Model(df = data,
                       assay = assay, return.model = T)
  
  
  # Saving model results
  summary_df <- as.data.frame(summary(model.sum)$coefficients)
  summary_df <- rownames_to_column(.data = summary_df, var = "Fixed effects")
  write_xlsx(summary_df, paste0("model_results/model_summary_", assay, ".xlsx"), col_names = TRUE)
  
  

  # Deriving data to plot the likelihood
  lik.delta = fromJSON(tma.Liklihood(data, 'Delta', assay = assay, e_max = 20))

  # Defining data to plot CDF
  cdf.delta <- fromJSON(tma.Cum(data, 'Delta', assay = assay))

  # Table for quantiles
  table_data <- data.frame(
    "CDF" = paste0(cdf.delta$cdf.q * 100, "%"),
    "X" = round(cdf.delta$q, 2)
    )

                      
  # DISTRIBUTION                      
  distr.plot <- ggplot() +
    # PDF 
    geom_line(aes(x = dist.delta$x, y = dist.delta$y), color = "blue") +
    # Fitted model
    geom_line(aes(x = model.delta$x.dense, y = model.delta$y.dense), color = "brown") +
    # Probability being >threshold
    geom_vline(xintercept = 10, linetype = "dashed", color = "red") +
    geom_area(aes(x = dist.delta$x[dist.delta$x >= 10], y = dist.delta$y[dist.delta$x >= 10]), fill = "skyblue", alpha = 0.2) +
    
    ggplot2::annotate(geom = "text", x = model.delta$x.dense[which.max(model.delta$y.dense)]+2, y = max(model.delta$y.dense)-0.001, label =  'Fitted Model', color = "brown",
                      size = 4, vjust = -1) +
    
    ggplot2::annotate(geom = "text", x = dist.delta$mean + 3, y = 0.02,
                      label = substitute(P[Delta > 10] == val, list(val = round(dist.delta$prob.thd, 4))),
                      size = 5, color = "black") +
    
    ggplot2::annotate(geom = "text", x = dist.delta$thd-2, y = max(dist.delta$y) - 0.01, label =  expression(paste(Delta, " = 10")), color = "red",
                      size = 4, vjust = -1) +
    
    ggplot2::annotate(geom = "text", x = min(dist.delta$x) - 2, y = max(dist.delta$y), label = paste("N =", dist.delta$n),
                      hjust = 0, vjust = 1, size = 4, color = "black") +
    
    ggplot2::annotate(geom = "text", x = max(dist.delta$x) - 8, y = max(dist.delta$y) - 0.03,
                      label = paste("Mean =", round(dist.delta$mean, 2)), hjust = 0, vjust = 1, size = 4, color = "black") +
    
    ggplot2::annotate(geom = "text", x = max(dist.delta$x) - 8, y = max(dist.delta$y) - 0.035,
                      label = paste("Std =", round(dist.delta$std, 2)), hjust = 0, vjust = 1, size = 4, color = "black") +
    
    geom_vline(xintercept = dist.delta$mean, linetype = "dashed", color = "grey") +
    geom_errorbarh(aes(xmin = dist.delta$ci.min, xmax = dist.delta$ci.max, y = 0.001),
                   height = 0.002, color = 'black') +
    geom_text(aes(x = dist.delta$ci.min, y = 0.001, label = paste("CI:", "95%")),
              vjust = -1, color = "black", size = 4) +
    labs(x = expression(Delta), y = "Density", title = paste0("Distribution of ", assay)) +
    # theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "grey95"),
          panel.background = element_rect(fill = "grey95"))
    

  #LIKELIHOOD
  
  likelihood.plot <- ggplot() +
    geom_line(aes(x = lik.delta$e_val, y = lik.delta$liklihood)) +
    labs(x = expression(epsilon),
         y = expression(P(paste(Delta, " > ", 10, " + ", epsilon))),
         title = bquote(paste("Likelihood of ", Delta, " > ", 10, " + ", epsilon, " for Assay ", .(assay)))) +
    # theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "grey95"),
          panel.background = element_rect(fill = "grey95"))
    
  # CUMULATIVE
  
  cum.plot <- ggplot() +
    geom_line(aes(x = cdf.delta$x, y =1 - cdf.delta$y), color = "black", linewidth = 0.5) +
    annotate(geom = 'table',
             x = max(cdf.delta$x),
             y = min(cdf.delta$y) + 0.1,
             label = list(table_data)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0.03)) +  # Set expand argument to include 0
    scale_x_continuous() +
    labs(title = paste0("1 - CDF of ", assay),
         x = 'X',
         y = expression(paste("P(", Delta, " > X (Time) "))) +
    coord_cartesian(ylim = c(0, 1), xlim = c(min(cdf.delta$x), max(cdf.delta$x))) + 
    # theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "grey95"),
          panel.background = element_rect(fill = "grey95"))


  for (coag in coags) {
    x.lab <- ''
    data_coag <- fromJSON(tma.Distribution(data, assay = assay, coag = coag))
    col <- 'green'
    i <-  2
    if (coag == 'Hypo'){
      col <- 'blue'
      i <- 3
      x.lab <- "Time to MA"
    }
    if (coag == 'Hyper'){
      col <- 'red'
      i <- 1
    }
    temp <- list('Data' = data_coag, 'coag' = coag, 'col' = col, 'xlab'=x.lab)
    temp.list.coag[i] <- list(temp)
  }
  
  coag.plots <- lapply(X = temp.list.coag, function(x) {
    ggplot() +
      geom_line(aes(x = x$Data$x.dense, y = x$Data$y.dense), color = "blue")  +
      geom_area(aes(x = x$Data$x.dense, y = x$Data$y.dense), fill = x$col, alpha = 0.2) +
      geom_jitter(aes(x = x$Data$data, y = -0.02), width = 0.2, height = 0.01, alpha = 0.2, color = x$col) +
      
      geom_boxplot(aes(x = x$Data$data, y = -0.02), width = 0.02, coef=0, outlier.size=-1,size=0.6,color="black", alpha = 0.5) +
      
      geom_vline(xintercept = x$Data$mean,linetype = "dashed", color = "grey", size = 0.9) +
      
      geom_vline(xintercept = x$Data$median, linetype = "dashed", color = "brown", size = 1.1) +
     
      ggplot2::annotate(geom = "text", x = min_x, y = max(x$Data$y), label = paste("N =", x$Data$n),
                        hjust = 0, vjust = 1, size = 4, color = "black") +
      
      
      labs(x = x$xlab, y = "Probability", title = paste0("Density Plot, Assay: ", assay, ", Coagulation: ", x$coag)) +
      xlim(min_x,max_x) +
      # theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "grey95"),
            panel.background = element_rect(fill = "grey95"))
  })

  for (i in seq_along(timepoints)) {
    temp <- list("Data" = fromJSON(tma.Distribution(data, assay = assay, timepoint = timepoints[i])), 'time' = timepoints[i], 'col' = cols[i])
    temp.list.time <- append(x = temp.list.time, values = list(temp))
  }
  
  # Get the overall minimum and maximum values for x$Data$x.dense
  
  time.plots <- lapply(X = temp.list.time, function(x) {
    
    #Check, if the plot is the final to add x label
    x.lab <- ifelse(temp.list.time[[length(temp.list.time)]]$time == x$time, "Time to MA","")
    ggplot() +
      geom_line(aes(x = x$Data$x.dense, y = x$Data$y.dense), color = "blue")  +
      geom_area(aes(x = x$Data$x.dense, y = x$Data$y.dense), fill = x$col, alpha = 0.2) +
      geom_jitter(aes(x = x$Data$data, y = -0.02), width = 0.2, height = 0.01, alpha = 0.2, color = x$col) +
      
      geom_boxplot(aes(x = x$Data$data, y = -0.02), width = 0.02, coef=0, outlier.size=-1,size=0.6,color="black", alpha = 0.5) +
      
      geom_vline(xintercept = x$Data$mean,linetype = "dashed", color = "grey", size = 0.9) +
      
      geom_vline(xintercept = x$Data$median, linetype = "dashed", color = "brown", size = 1.1) +
      
      ggplot2::annotate(geom = "text", x = min_x, y = max(x$Data$y), label = paste("N =", x$Data$n),
                        hjust = 0, vjust = 1, size = 4, color = "black") +
      
      labs(x = x.lab, y = "Probability", title = paste0("Density Plot with Boxplot, Assay: ", assay, ", Timepoint: ", x$time)) +
      xlim(min_x,max_x) +
      # theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "grey95"),
            panel.background = element_rect(fill = "grey95"))
  })
  
  
  for (i in seq_along(indications)) {
    temp <- list("Data" = fromJSON(tma.Distribution(data, assay = assay, indication = indications[i])), 'indication' = indications[i], 'col' = cols[i])
    temp.list.indi <- append(x = temp.list.indi, values = list(temp))
  }
  indic.plots <- lapply(X = temp.list.indi, function(x) {
    
    #Check, if the plot is the final to add x label
    x.lab <- ifelse(temp.list.indi[[length(temp.list.indi)]]$indication == x$indication, "Time to MA","")
    
    ggplot() +
      geom_line(aes(x = x$Data$x.dense, y = x$Data$y.dense), color = "blue")  +
      geom_area(aes(x = x$Data$x.dense, y = x$Data$y.dense), fill = x$col, alpha = 0.2) +
      geom_jitter(aes(x = x$Data$data, y = -0.02), width = 0.2, height = 0.01, alpha = 0.2, color = x$col) +
      
      geom_boxplot(aes(x = x$Data$data, y = -0.02), width = 0.02, coef=0, outlier.size=-1,size=0.6,color="black", alpha = 0.5) +
      
      geom_vline(xintercept = x$Data$mean,linetype = "dashed", color = "grey", size = 0.9) +
      
      geom_vline(xintercept = x$Data$median, linetype = "dashed", color = "brown", size = 1.1) +
      
      ggplot2::annotate(geom = "text", x = min_x, y = max(x$Data$y), label = paste("N =", x$Data$n),
                        hjust = 0, vjust = 1, size = 4, color = "black") +
      
      
      labs(x = x.lab, y = "Probability", title = paste0("Density Plot with Boxplot, Assay: ", assay, ", Indication: ", x$indication)) +
      xlim(min_x,max_x) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "grey95"),
            panel.background = element_rect(fill = "grey95"))
  })
  
  #Saving plots
  
  combined_plots <- plot_grid(plotlist = indic.plots, ncol = 1, align = 'h')
  ggsave(paste0(save.path, "combined_indication_plots_",assay,".jpeg"), combined_plots, width = 8, height = 10)
  
  combined_plots <- plot_grid(plotlist = time.plots, ncol = 1, align = 'h')
  ggsave(paste0(save.path,"combined_timepoint_plots_",assay,".jpeg"), combined_plots, width = 8, height = 10)
  
  combined_coag_plots <- plot_grid(plotlist = coag.plots, ncol = 1, align = 'h')
  ggsave(paste0(save.path, "combined_coag_plots_",assay,".jpeg"), combined_coag_plots, width = 8, height = 10)
  
  ggsave(paste0(save.path, "dist_", assay, ".jpeg"), plot = distr.plot, width = 10, height = 6, dpi = 300)
  ggsave(paste0(save.path, "liklihood_", assay, ".jpeg"), plot = likelihood.plot, width = 10, height = 6, dpi = 300)
  ggsave(paste0(save.path, "cumulative_", assay, ".jpeg"), plot = cum.plot, width = 10, height = 6, dpi = 300)
  
}
