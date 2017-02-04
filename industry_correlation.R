## Correlation between climate and industry data

# define in environment
assign("wheat_plot_y_label", "Wheat Yield (t/ha)", envir=.GlobalEnv)

# draw the correlation between temperature and industry data
draw.correlation <- function(progress, user.file, sector.file, plot.title, detrendCheck){
  
  # Change directory where output should be stored
  curwd <- getwd()
  setwd(corrdir)
  
  error <- create.correlation.plots(progress, user.file, sector.file, plot.title, detrendCheck)
  
  setwd(curwd)
  return(error)
}

# create the correlation plots and save them to disk
create.correlation.plots <- function(progress, user.file, sector.file, plot.title, detrendCheck){
  
  climate.data <- read.user.file(user.file)
  # assume the sector data is a csv file for now
  sector.data <- read.csv(sector.file)
  
  # Increment progress bar.
  progress$inc(0.1)
  
  # match on common data, if not throw error!
  temp_per_year <-  climate.data %>% group_by(year) %>% summarise(avg_tmax = mean(tmax, na.rm = TRUE), avg_tmin = mean(tmin, na.rm = TRUE), avg_t = mean(c(tmax,tmin), na.rm = TRUE), above30 = sum(tmax > 30, na.rm = TRUE))
  common_years <- intersect(temp_per_year$year, sector.data$Year)
  if (length(common_years) == 0){
    return("Error: not able to make a correlation, since there is not data in common!")
  }
  
  # Get sector column name..assume it is the 2nd column!
  sectorColumnName <- colnames(sector.data)[2]
  
  # add detrend column
  sector.data[,getDetrendedColumnName(sectorColumnName)] <- calculateDeTrendValues(sector.data, 1, 2)
  
  # selection
  sector.data <- sector.data %>% filter(Year %in% common_years)
  temp_per_year <- temp_per_year %>% filter(year %in% common_years)
  temp_per_year_sector <- cbind(temp_per_year, sector.data[,2], sector.data[,3])
  colnames(temp_per_year_sector) <- c("year", "avg_tmax", "avg_tmin", "avg_t", "above30", sectorColumnName, getDetrendedColumnName(sectorColumnName))
  
  # depending on detrend, select column
  sectorCol <- ifelse(detrendCheck, getDetrendedColumnName(sectorColumnName), sectorColumnName)
  
  # Calculate correlation (default = pearson)
  correlation <- cor(temp_per_year_sector[,-1]) # skip year
  correlationDF <- data.frame(correlation)
  
  # plot tmin vs wheat
  create_save_scatter_plot(paste0("corr_tmin_",sectorCol,".jpg"), temp_per_year_sector, "avg_tmin", sectorCol, plot.title, "Average min temperature", wheat_plot_y_label)
  progress$inc(0.2)
  
  # plot tmax vs wheat
  create_save_scatter_plot(paste0("corr_tmax_",sectorCol,".jpg"), temp_per_year_sector, "avg_tmax", sectorCol, plot.title, "Average max temperature", wheat_plot_y_label)
  progress$inc(0.2)
  
  # plot t vs wheat
  create_save_scatter_plot(paste0("corr_t_",sectorCol,".jpg"), temp_per_year_sector, "avg_t", sectorCol, plot.title, "Average temperature", wheat_plot_y_label)
  progress$inc(0.2)
  
  # plot above30 vs wheat
  create_save_scatter_plot(paste0("corr_above30_",sectorCol,".jpg"), temp_per_year_sector, "above30", sectorCol, plot.title, "Days above 30°C", wheat_plot_y_label)
  progress$inc(0.2)
  
  create_save_corrplot("corrplot.jpg", correlation)
  progress$inc(0.1)
  
  # all ok
  return("")
}

# get detrended column name given the sector column name
getDetrendedColumnName <- function(sectorColumnName){
  return(paste("Detrended", sectorColumnName, sep = "."))
}

# calculate detrend values baded on given dataframe and sector and year column
calculateDeTrendValues <- function(df, yearColumn, sectorColumn){
  result <- c()
  lineair_model <- lm(df[,sectorColumn] ~ df[,yearColumn])
  regressionCoefficient <- lineair_model[[1]][2]
  totalRows <- nrow(df)
  for(i in 1:nrow(df)){
    currentValue <- df[i,sectorColumn] 
    averageRow <- ifelse(nrow(df) %% 2 == 1, mean((seq(1:nrow(df)))), mean((seq(1:(nrow(df) +1)))))
    value <- currentValue - (i-averageRow) * regressionCoefficient
    result <- append(result, value)
  }
  return(result)
}

# create scatter plot with trend line, and save the plot to jpg file
create_save_scatter_plot <- function(filename, df, x, y, plot.title, x.label, y.label){
  annotateX <- min(df[,x]) + (max(df[,x]) - min(df[,x])) / 8
  annotateY <- max(df[,y])
  lm.sector <- lm(data = df, paste(y, "~", x, sep = ""))
  rsquared <- round(summary(lm.sector)$r.squared, 2)
  coefficients <- round(summary(lm.sector)$coefficients, 2)
  annotateText <- paste("Y=", coefficients[2], "X +", coefficients[1], "\nR2:", as.character(rsquared))
  
  p <- ggplot(df, aes_string(x, y)) + 
    geom_point(colour ='red') +                             # points with red color
    geom_smooth(method=lm, se = FALSE, colour = "black") +  # draw lineair regression line without confidence interval
    ggtitle(plot.title) + xlab(x.label) + ylab(y.label) +
    annotate("text", label = annotateText, x = annotateX, y = annotateY)
  ggsave(filename, plot=p, width = 8, height = 6)
}

# Correlation plot
# See https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html for options
create_save_corrplot <- function(filename, correlation){
  jpeg(file=filename, width = 1024, height = 768)
  corrplot(correlation, method="number")
  dev.off()
}
