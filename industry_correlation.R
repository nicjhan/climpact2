## Correlation between climate and industry data

# define in environment
assign("wheat_plot_y_label", "Wheat Yield", envir=.GlobalEnv)

# draw the correlation between temperature and industry data
draw.correlation <- function(progress, user.file, sector.file, plot.title, detrendCheck){
  
  # Change directory where output should be stored
  curwd <- getwd()
  setwd(corrdir)
  
  create.correlation.plot(progress, user.file, sector.file, plot.title, detrendCheck)
  
  setwd(curwd)
}

# create the plot
create.correlation.plot <- function(progress, user.file, sector.file, plot.title, detrendCheck){
  
  climate.data <- read.user.file(user.file)
  # assume the sector data is a csv file for now
  sector.data <- read.csv(sector.file)
  
  # Increment progress bar.
  progress$inc(0.1)
  
  # match on common data, if not throw error!
  temp_per_year <-  climate.data %>% group_by(year) %>% summarise(avg_tmax = mean(tmax, na.rm = TRUE), avg_tmin = mean(tmin, na.rm = TRUE), avg_t = mean(c(tmax,tmin), na.rm = TRUE))
  common_years <- intersect(temp_per_year$year, sector.data$Year)
  if (length(common_years) == 0){
    return("Error: not able to make a correlation, since there is not data in common!")
  }
  
  # selection
  sector.data <- sector.data %>% filter(Year %in% common_years)
  temp_per_year <- temp_per_year %>% filter(year %in% common_years)
  temp_per_year_sector <- cbind(temp_per_year, wheat = sector.data$Wheat.yield..t.ha., wheatD = sector.data$Detrended.Wheat.yield..t.ha.)
  
  # depending on detrend, select column
  wheatCol <- ifelse(detrendCheck, "wheatD", "wheat")
  progress$inc(0.2)
  
  # Calculate correlation (default = pearson)
  correlation <- cor(temp_per_year_sector[,-1]) # skip year
  correlationDF <- data.frame(correlation)
  
  # plot tmin vs wheat
  corr_min_tmp_vs_wheat <- correlationDF[rownames(correlationDF) %in% "avg_tmin", wheatCol]
  create_save_scatter_plot(paste0("corr_tmin_",wheatCol,".jpg"), temp_per_year_sector, "avg_tmin", wheatCol, plot.title, "Average min temperature", wheat_plot_y_label, as.character(corr_min_tmp_vs_wheat))
  progress$inc(0.2)
  
  # plot tmax vs wheat
  corr_max_tmp_vs_wheat <- correlationDF[rownames(correlationDF) %in% "avg_tmax", wheatCol]
  create_save_scatter_plot(paste0("corr_tmax_",wheatCol,".jpg"), temp_per_year_sector, "avg_tmax", wheatCol, plot.title, "Average max temperature", wheat_plot_y_label, as.character(corr_max_tmp_vs_wheat))
  progress$inc(0.2)
  
  # plot t vs wheat
  corr_tmp_vs_wheat <- correlationDF[rownames(correlationDF) %in% "avg_t", wheatCol]
  create_save_scatter_plot(paste0("corr_t_",wheatCol,".jpg"), temp_per_year_sector, "avg_t", wheatCol, plot.title, "Average temperature", wheat_plot_y_label, as.character(corr_tmp_vs_wheat))
  progress$inc(0.2)
  
  create_save_corrplot("corrplot.jpg", correlation)
  progress$inc(0.1)
  
  # all ok
  return("")
}

# create ggplot and save to jpg
create_save_scatter_plot <- function(filename, df, x, y, plot.title, x.label, y.label, correlation){
  p <- ggplot(df, aes_string(x, y)) + 
    geom_point(shape=1) + 
    geom_smooth(method=lm) +
    ggtitle(paste(plot.title, "( correlation =", correlation , ")")) + xlab(x.label) + ylab(y.label)
  ggsave(filename, plot=p, width = 8, height = 6)
}

# Correlation plot
# See https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html for options
create_save_corrplot <- function(filename, correlation){
  jpeg(file=filename, width = 1024, height = 768)
  corrplot(correlation, method="number")
  dev.off()
}
