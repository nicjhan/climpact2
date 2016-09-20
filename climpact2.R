
# extraQC code, taken from the "rclimdex_extraqc.r" package, 
# Quality Control procedures programed by Enric Aguilar (C3, URV, Tarragona, Spain) and 
# and Marc Prohom, (Servei Meteorologic de Catalunya). Edited by nherold to output to .csv (Jan 2016).
allqc <- function (master, output, outrange = 4)
{
	output <- paste(output, "/", ofilename, sep = "")
	# fourboxes will produce boxplots for non-zero precip, tx, tn, dtr using the IQR entered previously
	# the plot will go to series.name_boxes.pdf
	# outliers will be also listed on a file (series.name_outliers.txt)
	fourboxes(master, output, save = 1, outrange)
	
	# Will plot a histogram of the decimal point to see rounding problems, for prec, tx, tn
	# The plot will go to series.name_rounding.pdf. Needs some formal arrangements (title, nice axis, etc)
	roundcheck(master, output, save = 1)
	
	# will list when tmax <= tmin. Output goes to series.name_tmaxmin.txt
	tmaxmin(master, output)
	
	# will list values exceeding 200 mm or temperatures with absolute values over 50. Output goes to 
	# series.name_toolarge.txt
	humongous(master, output)
	
	# 'Annual Time series' constructed with boxplots. Helps to identify years with very bad values
	# Output goes to series.name_boxseries.pdf
	boxseries(master, output, save = 1)
	
	# Lists duplicate dates. Output goes to series.name_duplicates.txt	
	duplivals(master, output)
	
	# The next two functions (by Marc Prohom, Servei Meteorologic de Catalunya) identify consecutive tx and tn values with diferences larger than 20
	# Output goes to series.name_tx_jumps.txt and series.name_tn_jumps.txt. The first date is listed. 
	jumps_tx(master, output)
	jumps_tn(master, output)
	
	# The next two functions (by Marc Prohom, Servei Meteorologic de Catalunya)identify 
	# series of 3 or more consecutive identical values. The first date is listed. 
	# Output goes to series.name_tx_flatline.txt  and series.name_tx_flatline.txt
	flatline_tx(master, output)
	flatline_tn(master, output)
}

# Given a user's RClimdex text file path, read in, convert -99.9 to NA and
# return contents as array of 6 columns.
read.user.file <- function(user.file) {
	temp.filename = paste(user.file,".temporary",sep="")
	raw.table = readLines(user.file)
	newtext = gsub(",","\t",raw.table)
	cat(newtext,file=temp.filename,sep="\n")

	data <- tryCatch(read.table(temp.filename,header=F,col.names=c("year","month","day","prcp","tmax","tmin"),colClasses=rep("real",6)),
			error= function(c) {
				     print(paste("INPUT FILE NOT FORMATTED CORRECTLY.\n\n",c$message,sep="")) })

	# Replace -99.9 data with NA
	if(!is.null(data)) { data$prcp[data$prcp==-99.9]=NA ; data[data$tmax==(-99.9),"tmax"]=NA ; data[data$tmin==(-99.9),"tmin"]=NA }

	return(data)
}

# creates a list of metadata
create.metadata <- function(latitude,longitude,base.year.start,base.year.end,dates,ofilename) {
	return(list(lat=latitude,lon=longitude,base.start=base.year.start,base.end=base.year.end,year.start=as.numeric(format(dates[1],format="%Y")),year.end=as.numeric(format(dates[length(dates)],format="%Y")),dates=dates,ofile=ofilename)) 
}

# Preps data and creates the climdex.input object based on the R package climdex.pcic
create.climdex.input <- function(user.data,metadata) {
	date.seq <- data.frame(list(time=seq(metadata$dates[1],metadata$dates[length(metadata$dates)],by="day")))
	data_raw = data.frame(list(time=as.Date(metadata$dates,format="%Y-%m-%d"),prec=user.data[,4],tmax=user.data[,5],tmin=user.data[,6]))
	merge_data = merge(data_raw,date.seq,all=TRUE)
	
	days <- as.Date(as.character(merge_data[,1],format="%Y-%m-%d"))-as.Date("1850-01-01")
	seconds <- as.numeric(days*24*60*60)
	ts.origin = "1850-01-01"                # arbitarily chosen origin to create time-series object with. This needs to be made global 
	pcict.dates <- as.PCICt(seconds,cal="gregorian",origin=as.character(ts.origin))
	
	date.months <- unique(format(as.character((merge_data[,1]),format="%Y-%m")))
	date.years <- unique(format(as.character((merge_data[,1]),format="%Y")))
	assign('date.months',date.months,envir=.GlobalEnv)
	assign('date.years',date.years,envir=.GlobalEnv)

        # create a climdex input object
	cio <- climdexInput.raw(tmin=merge_data[,4],tmax=merge_data[,3],prec=merge_data[,2],tmin.dates=pcict.dates,tmax.dates=pcict.dates,prec.dates=pcict.dates,base.range=c(metadata$base.start,metadata$base.end),prec.qtiles=prec.quantiles,
				temp.qtiles=temp.quantiles,quantiles=quantiles)

	# add diurnal temperature range
	cio@data$dtr = cio@data$tmax - cio@data$tmin

	return(cio)
}


# This function runs QC functionality on the user specified input data. It requres as input;
#    - metadata: output of create.metadata()
#    - data: output of convert.user.file
#    - graphics: boolean for whether running with graphics or not (determines whether progress bars, message windows etc. are shown).
QC.wrapper <- function(metadata, user.data, user.file) {

	# Check base period is valid when no thresholds loaded
	if(is.null(quantiles)) {
        if(metadata$base.start < format(metadata$dates[1],format="%Y") | metadata$base.end > format(metadata$dates[length(metadata$dates)],format="%Y") | metadata$base.start > metadata$base.end) {
            cat("Base period must be between ", format(metadata$dates[1],format="%Y")," and ",format(metadata$dates[length(metadata$dates)],format="%Y"),". Please correct.")
            return()
        }
	}

	# Check for ascending order of years
	if(!all(user.data$year == cummax(user.data$year))) {
        cat("Years are not in ascending order, please check your input file.")
        return()
	}

    ##############################
	# Create climdex object
	# NICK: After this point all references to data should be made to the climdex input object 'cio'. One exception is the allqc function, 
	# which still references the INPUT to the climdex.input function.
	assign("latitude",  metadata$lat, envir = .GlobalEnv)
	assign("longitude", metadata$lon, envir = .GlobalEnv)

	cio <<- create.climdex.input(user.data,metadata)
	print("climdex input object created.",quote=FALSE)

    ##############################
	# Calculate and write out thresholds
	tavgqtiles <- get.outofbase.quantiles(cio@data$tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(metadata$base.start,metadata$base.end),temp.qtiles=temp.quantiles,prec.qtiles=NULL)
	cio@quantiles$tavg$outbase <<- tavgqtiles$tmax$outbase	# while this says tmax it is actually tavg, refer to above line.

	# heat wave thresholds
	tavg <- (cio@data$tmax + cio@data$tmin)/2
	Tavg90p <- suppressWarnings(get.outofbase.quantiles(tavg,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(metadata$base.start,metadata$base.end),n=15,temp.qtiles=0.9,prec.qtiles=NULL,
	                                                                min.base.data.fraction.present=0.1))
	TxTn90p <- suppressWarnings(get.outofbase.quantiles(cio@data$tmax,cio@data$tmin,tmax.dates=cio@dates,tmin.dates=cio@dates,base.range=c(metadata$base.start,metadata$base.end),n=15,temp.qtiles=0.9,prec.qtiles=NULL,
	                                                                min.base.data.fraction.present=0.1))
    tn90p <- TxTn90p$tmin$outbase
    tx90p <- TxTn90p$tmax$outbase
    tavg90p <- Tavg90p$tmax$outbase

	# write to file
    thres <- c(cio@quantiles$tmax$outbase,cio@quantiles$tmin$outbase,cio@quantiles$tavg$outbase,cio@quantiles$prec,as.list(tn90p),as.list(tx90p),as.list(tavg90p))#,cio@dates,cio@data)#$tmin,cio@data$tmax,cio@data$prec)
	nam1 <- paste(outthresdir, paste(ofilename, "_thres.csv", sep = ""),sep="/")
	write.table(as.data.frame(thres), file = nam1, append = FALSE, quote = FALSE, sep = ", ", na = "NA", col.names = c(paste("tmax",names(cio@quantiles$tmax$outbase),sep="_"),paste("tmin",names(cio@quantiles$tmin$outbase),sep="_"),
	paste("tavg",names(cio@quantiles$tavg$outbase),sep="_"),paste("prec",names(cio@quantiles$prec),sep="_"),"HW_TN90","HW_TX90","HW_TAVG90"),row.names=FALSE) 
	
    # write raw tmin, tmax and prec data for future SPEI/SPI calcs
	yeardate2 <- format(cio@dates,format="%Y")
	dates <-format(cio@dates[1:50],format="%Y-%m-%d")
	base.dates <- dates[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)]
	thres2 <- list(dates=base.dates,tmin=cio@data$tmin[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)],tmax=cio@data$tmax[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)],
	prec=cio@data$prec[which(yeardate2 >= metadata$base.start & yeardate2 <= metadata$base.end)])
	nam2 <- paste(outthresdir, paste(ofilename, "_thres_spei.csv", sep = ""),sep="/")
    write.table(as.data.frame(thres2), file = nam2, append = FALSE, quote = FALSE, sep = ", ", na = "NA", col.names = c("Base_period_dates","Base_period_tmin","Base_period_tmax","Base_period_prec"),row.names=FALSE)

    ##############################
	# Set some text options
	if(metadata$lat<0) lat_text = "°S" else lat_text = "°N"
	if(metadata$lon<0) lon_text = "°W" else lon_text = "°E"
	Encoding(lon_text) <- "UTF-8"	# to ensure proper plotting of degree symbol in Windows (which uses Latin encoding by default)
	Encoding(lat_text) <- "UTF-8"
	title.station <- paste(ofilename, " [", metadata$lat,lat_text, ", ", metadata$lon,lon_text, "]", sep = "")
	assign("title.station", title.station, envir = .GlobalEnv)
#	assign("ofilename", ofilename, envir = .GlobalEnv)

	##############################

	nam1 <- paste(outlogdir, paste(ofilename, "_prcpPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	
	prcp <- cio@data$prec[cio@data$prec >= 1 & !is.na(cio@data$prec)]

	if(length(prcp) > 30)
	{
		hist(prcp, main = paste("Histogram for Station:", ofilename, " of PRCP>=1mm", sep = ""),breaks = c(seq(0, 40, 2),max(prcp)), xlab = "", col = "green" , freq = FALSE)
		lines(density(prcp, bw = 0.2, from = 1), col = "red")
	}
	pplotts(var = "prcp", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()
	nam1 <- paste(outlogdir, paste(ofilename, "_tmaxPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	pplotts(var = "tmax", type = "l", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()
	nam1 <- paste(outlogdir, paste(ofilename, "_tminPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	pplotts(var = "tmin", type = "l", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()
	nam1 <- paste(outlogdir, paste(ofilename, "_dtrPLOT.pdf", sep = ""), sep = "/")
	check_open(nam1)
	pdf(file = nam1)
	pplotts(var = "dtr", type = "l", tit = ofilename,cio=cio,metadata=metadata)
	dev.off()

	##############################
	# Call the ExtraQC functions.
	print("TESTING DATA, PLEASE WAIT...",quote=FALSE)

	allqc(master = paste(user.file,".temporary",sep=""), output = outqcdir, outrange = 3) #stddev.crit)   # extraQC is called here. NOTE the default outrange=3 in original verson.

	##############################	
	# Write out NA statistics.
	write.NA.statistics(cio)

	##############################	
	# Remove temporary file
	system(paste("rm ",user.file,".temporary",sep=""))

} # end of QC.wrapper()

# Check for required packages and install if necessary
package.check <- function() {
	gui.packages <- c("bitops","Rcpp","caTools","PCICt","SPEI","climdex.pcic")
	new.packages <- gui.packages[!(gui.packages %in% installed.packages()[,"Package"])]

	# Install/update packages needed for ClimPACT2 GUI.
	if(length(new.packages)) {
	        print("******************************")
	        print(paste("Installing the following required packages...",new.packages,sep=""))
	        install.packages(new.packages) 
	}

	print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
}

# Define global variables. The use of global variables is undesirable from a programming point of view
# but for historical purposes is still used in this code.
global.vars <- function() {
	# Nullify objects globally to avoid warning messages.
	reading.pb <<- process.pb <<- pb <<- orig.name.user <<- qc.yes <<- outthresdir <<- quantiles <<- cio <<- ofilename <<- infor1 <<- orig.name <<- title.station <<- outlogdir <<- thres.calc <<- 
	add.data <<- add.data.name <<- out <<- ref.start <<- ts.end <<- basetmin <<- basetmax <<- baseprec <<- start.but <<- cal.but <<- ttmp <<- outqcdir <<- NULL
	
	version.climpact <<- software_id
	
	# Global variables
	running.zero.allowed.in.temperature <<- 4
	temp.quantiles <<- c(0.05,0.1,0.5,0.9,0.95)
	prec.quantiles <<- c(0.05,0.1,0.5,0.9,0.95,0.99)
	barplot_flag    <<- TRUE
	loaded <<- FALSE
	min_trend     <<- 10	# minimum number of data points for plotting a linear trend
}

