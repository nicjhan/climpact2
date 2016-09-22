
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

    return("")
}

# A function that should be called before any .csv file is written. It appends some basic information that should be stored in each file for 
# the user's record.
write_header <- function(filename,header="")
{
	if(is.null(filename)) { stop("Filename not passed to function 'write_header'") }
	
	header = cbind("Description: ",header)
	tmp = try(write.table(header, sep=",", file = filename, append = FALSE, row.names=FALSE,col.names = FALSE))
	# Check if file is open
	if(class(tmp)=="try-error") { tkmessageBox(message=paste("Error encountered, please check that the file ",filename," is not currently open, then select OK to try again.",sep=""),icon='warning');  write_header(filename) }

	first_lines = cbind(c("Station: ","Latitude: ","Longitude: ","ClimPACT2_version: ","Date_of_calculation: "),c(ofilename,latitude,longitude,version.climpact,toString(Sys.Date())))
	write.table(first_lines, sep=",", file = filename, append = TRUE, row.names=FALSE,col.names = FALSE)

}


check_open <- function(filename)
{
	write.table("test text", sep=",", file = filename, append = FALSE, row.names=FALSE,col.names = FALSE)
}

# Plots boxplots. Needs only station and save
fourboxes <- function(station, output, save = 0, outrange)
{
	# add save option
	if (save == 1)
	{ 
		nombre <- paste(output, "_boxes.pdf", sep = "")
		check_open(nombre)
		pdf(file = nombre)
	}
	
	datos <- read.table(station, col.names = c("year", "month", "day", "pc", "tx", "tn"),na.strings = "-99.9")
	datos$tr <- datos$tx - datos$tn
	prec <- subset(datos, datos$pc > 0)
	par(mfrow = c(2, 2))
	
	# we open a file for writing outliers. First time is not append; rest is append
	filena <- paste(output, "_outliers.csv", sep = "")
	
	# for each of precip, tmax, tmin, dtr:
	#   produce boxplots: IQR for default is 3 for temp and 5 for precip
	#     can be entered as parameter when calling the function. Precip will always be 2 units more than temp
	#   write outliers out
	# if no data's available, 'no data available' is printed on a blank panel instead
	write_header(filena,"Outliers shown in *boxseries.pdf")
	write.table(cbind("Date","Prec","TX","TN","DTR"), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

	if (any(!is.na(prec$pc)))
	{
		respc <- boxplot(prec$pc ~ prec$month, main = "NON ZERO PREC", col = "blue", range = outrange + 2)

		# write precip outliers
		write.table("Prec up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in length(respc$names)) #unique(prec$month))	#1:12)
		{
			prov <- subset(datos,datos$month == as.numeric(respc$names[a]) & datos$pc > respc$stats[5, a])#a])
			date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
			write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		} else
		{
			plot.new()
			text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
		}
	
	if (any(!is.na(datos$tx)))
	{
		restx <- boxplot(datos$tx ~ datos$month, main = "TX", col = "red", range = outrange)
		
		# write tmax outliers
		write.table("TX up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tx > restx$stats[5, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file= filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		write.table("TX low",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tx < restx$stats[1, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr),sep=",", file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
	} else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (any(!is.na(datos$tn)))
	{
		restn <- boxplot(datos$tn ~ datos$month, main = "TN", col = "cyan", range = outrange)
		
		# write tmin outliers
		write.table("TN up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tn > restn$stats[5, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		write.table("TN low",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tn < restn$stats[1, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
	} else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (any(!is.na(datos$tr)))
	{
		restr <- boxplot(datos$tr ~ datos$month, col = "yellow", main = "DTR", range = outrange)
		
		# write dtr outliers
		write.table("DTR up",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tr > restr$stats[5, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
		write.table("DTR low",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE)
		for (a in 1:12)
		{
		  prov <- subset(datos, datos$month == a & datos$tr < restr$stats[1, a])
		  date.tmp = paste(prov$year,prov$month,prov$day,sep="-")
		  write.table(cbind(date.tmp,prov$pc,prov$tx,prov$tn,prov$tr), sep=",",file = filena, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
		}
	} else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (save == 1) dev.off()
	
	rm(datos)
}

# Plots histograms showing rounding. Needs station and you can delimit the period with first and last year
roundcheck <- function(station,output,fyear = 1000,lyear = 3000,save = 0)
{
	if (save == 1)
	{ 
		nombre<-paste(output,'_rounding.pdf',sep="")
		check_open(nombre)
		pdf(file=nombre)
	}
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	par(mfrow=c(1,3))
	my<-subset(datos,datos$year >= fyear & datos$year <= lyear)
	ispc=subset(my$pc,my$pc > 0)
	hist(ispc %% 1,col='blue',main='NON ZERO PREC ROUNDING',breaks=c(seq(0,1.0,0.0999999)),xlab="")
	hist(my$tx %% 1,col='red',main='TX ROUNDING',breaks=c(seq(0,1.0,0.0999999)),xlab="")
	hist(my$tn %% 1,col='cyan',main='TN ROUNDING',breaks=c(seq(0,1.0,0.0999999)),xlab="")
	
	if (save == 1) { dev.off() }
	rm(datos)
}

tmaxmin <- function(station,output)
{
	filena = paste(output,'_tmaxmin.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	maxmin = subset(datos,(datos$tx-datos$tn)<=0)
	date.tmp = paste(maxmin$year,maxmin$month,maxmin$day,sep="-")
	write_header(filena,"Dates where TN>TX")
	write.table(cbind("Date","Prec","TX","TN"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,maxmin$pc,maxmin$tx,maxmin$tn),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	
	# If no data (i.e. no TN>TX) in variable print message
	if(length(maxmin)==0) { write.table("NO TN > TX FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)
}

humongous <- function(station,output)
{
	filena = paste(output,'_toolarge.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	grande<-subset(datos,(datos$tx > 50 | datos$tx < -50 | datos$tn > 50 | datos$tn < -50 | datos$pc > 200 | datos$pc < 0))
	date.tmp = paste(grande$year,grande$month,grande$day,sep="-")
	write_header(filena,"Dates where precipitation > 200 mm or abs(temperature) > 50 degrees.")
	write.table(cbind("Date","Prec","TX","TN"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,grande$pc,grande$tx,grande$tn),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no data (i.e. no large values) in variable print message
	if(length(grande)==0) { write.table("NO EXCESSIVE VALUES FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(list=ls())
}

boxseries <- function(station, output, save = 0)
{
	if (save == 1)
	{
		nombre <- paste(output, "_boxseries.pdf", sep = "")
		check_open(nombre)
		pdf(file = nombre)
	}
	
	datos <- read.table(station, col.names = c("year", "month", "day", "pc", "tx", "tn"),na.strings = "-99.9")
	datos$tr <- datos$tx - datos$tn
	prec <- subset(datos, datos$pc > 0)
	par(mfrow = c(2, 2))
	
	if (any(!is.na(prec$pc))) respc <- boxplot(prec$pc ~ prec$year, main = "NON ZERO PREC", col = "blue", range = 4) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	if (any(!is.na(datos$tx))) restx <- boxplot(datos$tx ~ datos$year, main = "TX", col = "red", range = 3) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	if (any(!is.na(datos$tn))) restn <- boxplot(datos$tn ~ datos$year, main = "TN", col = "cyan", range = 3) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	if (any(!is.na(datos$tr))) restr <- boxplot(datos$tr ~ datos$year, col = "yellow", main = "DTR", range = 3) else
	{
		plot.new()
		text(x = 0.5, y = 0.5, "NO DATA AVAILABLE", adj = c(0.5, NA))
	}
	
	if (save == 1) dev.off()
	
	rm(datos)  # we don't want to delete everyting...
}

duplivals <- function(station,output)
{
	filena = paste(output,'_duplicates.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	isdupli<-cbind(datos$year,datos$month,datos$day)
	duplicate.dates = subset(isdupli, duplicated(isdupli)== TRUE)
	date.tmp = paste(duplicate.dates[,1],duplicate.dates[,2],duplicate.dates[,3],sep="-")
	write_header(filena,"Dates that have been used more than once in your input file.")
	write.table(cbind("Dates_duplicated"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(date.tmp,sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no data (i.e. no large values) in variable print message
	if(length(date.tmp)==0) { write.table("NO DUPLICATE DATES FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}

jumps_tx <- function(station, output)
{
	filena = paste(output, '_tx_jumps.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	diftx <-abs(round(diff(datos$tx, lag=1, differences=1),digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftx)$lengths),c.type=rle(diftx)$values)
	x <-na.omit(x)
	names(x) <-c("id","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tx)
	Z <- z[,6:10]
	names(z) <-c("id","year","month","day","tx")
	jumps <- merge(z, x, by="id", all.x=F, all.y=T)
	jumps <- subset(jumps, (jumps$val>=20))
	jumps <- jumps[,7:11]
	jumps <- jumps[,-4]
	names(jumps) = c("year","month","day","tx")
	date.tmp = paste(jumps$year,jumps$month,jumps$day,sep="-")
	write_header(filena,"Dates where the change in TX is > 20 degrees.")
	write.table(cbind("Date","TX"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,jumps$tx),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	
	# If no issues found in variable, print message
	if(length(jumps$tx)==0) { write.table("NO LARGE TX JUMPS FOUND",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }
	
	rm(datos)  # we don't want to delete everyting...
}

jumps_tn <- function(station,output)
{
	filena = paste(output, '_tn_jumps.csv',sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"),na.strings='-99.9')
	diftn <-abs(round(diff(datos$tn, lag=1, differences=1), digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftn)$lengths),c.type=rle(diftn)$values)
	x <-na.omit(x)
	names(x) <-c("id","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tn)
	Z <- z[,6:10]
	names(z) <-c("id","year","month","day","tn")
	jumps <- merge(z, x, by="id", all.x=F, all.y=T)
	jumps <- subset(jumps, (jumps$val>=20))
	jumps <- jumps[,7:10]
	names(jumps) = c("year","month","day","tn")
	date.tmp = paste(jumps$year,jumps$month,jumps$day,sep="-")
	write_header(filena,"Dates where the change in TN is > 20 degrees.")
	write.table(cbind("Date","TN"),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,jumps$tn),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(jumps$tn)==0) { write.table("NO LARGE TN JUMPS FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}

flatline_tx <- function(station,output)
{
	filena = paste(output, '_tx_flatline.csv', sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"), na.strings='-99.9')
	diftx <-abs(round(diff(datos$tx, lag=1, differences=1), digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftx)$lengths),c.size=rle(diftx)$lengths,c.type=rle(diftx)$values)
	x <-x[x$c.type==0,]
	x <-na.omit(x)
	names(x) <-c("id","dup","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tx)
	z_1 <- z[,6:10]
	names(z_1) <-c("id","year","month","day","tx") 
	flat <- merge(z_1, x, by="id", all.x=F, all.y=T)
	flat <- subset(flat, (flat$dup>=3))
	flat <- flat[,2:6]

	date.tmp = paste(flat$year,flat$month,flat$day,sep="-")
	write_header(filena,"Dates where TX values have been repeated more than 4 times.")
	write.table(cbind("Date","TX","Number of duplicates"),sep=",",append=TRUE,file=filena,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,flat$tx,flat$dup+1),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(flat$tx)==0) { write.table("NO REPEATED TX FOUND",sep=",", file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }
	
	rm(datos)  # we don't want to delete everyting...
}

flatline_tn <- function(station,output)
{
	filena = paste(output, '_tn_flatline.csv', sep='')
	datos<-read.table(station,col.names=c("year","month","day","pc","tx","tn"), na.strings='-99.9')
	diftx <-abs(round(diff(datos$tn, lag=1, differences=1), digits=1))
	x <-data.frame(c.ndx=cumsum(rle(diftx)$lengths),c.size=rle(diftx)$lengths,c.type=rle(diftx)$values)
	x <-x[x$c.type==0,]
	x <-na.omit(x)
	names(x) <-c("id","dup","val")
	z <-data.frame(id=row(datos), year=datos$year, month=datos$month, day=datos$day, day=datos$tn)
	z_1 <- z[,6:10]
	names(z_1) <-c("id","year","month","day","tn") 
	flat <- merge(z_1, x, by="id", all.x=F, all.y=T)
	flat <- subset(flat, (flat$dup>=3))
	flat <- flat[,2:6]
	date.tmp = paste(flat$year,flat$month,flat$day,sep="-")
	write_header(filena,"Dates where TX values have been repeated more than 4 times.")
	write.table(cbind("Date","TN","Number of duplicates"),sep=",",append=TRUE,file=filena,row.names=FALSE,col.names=FALSE)
	write.table(cbind(date.tmp,flat$tn,flat$dup+1),sep=",",append=TRUE,file=filena,quote=FALSE,row.names=FALSE,col.names=FALSE)

	# If no issues found in variable, print message
	if(length(flat$tn)==0) { write.table("NO REPEATED TN FOUND", sep=",",file = filena, append = TRUE, row.names = FALSE, col.names = FALSE) }

	rm(datos)  # we don't want to delete everyting...
}
# End of Prohom and Aguilar code.


# pplotts
# plots QC'ed data (TX, TN, PR) into pdf files.
pplotts <- function(var = "prcp", type = "h", tit = NULL,cio,metadata)
{
	# set bounds for the plot based on available data. dtr and prcp have
	# floors of 0 by definition (assuming tmax and tmin have been qc'd)
	
	if(var == "dtr")
	{
	#  ymax <- max(data[, "tmax"] - data[, "tmin"], na.rm = TRUE)
		ymax <- max(cio@data$dtr,na.rm = TRUE)
		ymin <- 0
	} else if (var == "prcp")
	{
		ymax <- max(cio@data$prec, na.rm = TRUE)
		ymin <- 0
	} else
	{
		ymax <- max(cio@data[[var]], na.rm = TRUE) + 1
		ymin <- min(cio@data[[var]], na.rm = TRUE) - 1
	}
	if(var == "prcp") { var1 = "prec" } else { var1 = var }

	# set default y scales if proper ones can't be calculated
	# but do we really want to try to plot if there's no data available at all?
	if (is.na(ymax) | is.na(ymin) | (ymax == -Inf) | (ymin == -Inf))
	{
		ymax <- 100
		ymin <- -100
		warning(paste("Warnings have been generated because there is no available data for","one or more of tmax, tmin or precip. Check the plots in /qc to confirm this."))
	}
	
	par(mfrow = c(4, 1))
	par(mar = c(3.1, 2.1, 2.1, 2.1))

	year.start = as.numeric(format(metadata$dates[1],format="%Y"))
	year.end = as.numeric(format(metadata$dates[length(metadata$dates)],format="%Y"))
	for(i in seq(year.start, year.end, 10))
	{
		at <- rep(1, 10)
		# if (i > yeare)
		for(j in (i + 1):min(i + 9, year.end + 1))
		{
			if(leapyear(j)) at[j - i + 1] <- at[j - i] + 366 else
			  at[j - i + 1] <- at[j - i] + 365
		}
		
		tmp.dates <- format(cio@dates,format="%Y")
		ttmp <- cio@data[[var1]][tmp.dates>=i & tmp.dates <= min(i + 9, year.end)]
		plot(1:length(ttmp), ttmp, type = type, col = "blue",
		  xlab = "", ylab = "", xaxt = "n", xlim = c(1, 3660), ylim = c(ymin, ymax))
		abline(h = 0)
		tt <- seq(1, length(ttmp))
		if(!is.null(ttmp)) tt <- tt[is.na(ttmp) == TRUE] #else print(paste(var,"is null."))
		axis(side = 1, at = at, labels = c(i:(i + 9)))
		for(k in 1:10) abline(v = at[k], col = "yellow")
		lines(tt, rep(ymin, length(tt)), type = "p", col = "red")
		title(paste("Station: ", tit, ", ", i, "~", min(i + 9, year.end), ",  ", var1, sep = ""))
	}
}

# Creates an array of strings, each string containing a folder in the path to the user's file.
# Globally assigns two variables: the array of strings and the final string (i.e. the file name)
# This should be improved in the future (global variables should not be relied on)
# The 'graphics' parameter indicates whether a progress bar is drawn to the screen via tcltk
get.file.path <- function(tmpfile) {
	outdirtmp<-strsplit(tmpfile,"/")[[1]]
	file.name=outdirtmp[length(outdirtmp)]
	e=strsplit(file.name,"\\.")[[1]]
	ofilename=substr(file.name,start=1,stop=nchar(file.name)-nchar(e[length(e)])-1)
	assign('outdirtmp',outdirtmp,envir=.GlobalEnv)
	assign('ofilename',ofilename,envir=.GlobalEnv)
}

# This function calls the major routines involved in reading the user's file, creating the climdex object and running quality control
load.data.qc <- function(user.file, tmpfile, latitude, longitude, station.entry, base.year.start,base.year.end)
{
	get.file.path(tmpfile)

	user.data <- read.user.file(user.file)
	error <- draw.step1.interface(user.data, user.file, latitude, longitude, station.entry, base.year.start, base.year.end)
    return(error)
}

get.qc.dir <- function()
{
    return(outqcdir)
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

# Create directories for output. Requires get.file.path to be called beforehand. That functionality was moved to a separate function
# so that the directory could be modified by the user in the ClimPACT2 GUI.
# Undesirably these are currently kept as global variables.
create.dir <- function(user.file) {
	# create directory names
	if(length(outdirtmp)<=2) {
		dirsplit<-strsplit(user.file,":")[[1]][1]
		outinddir<-paste(dirsplit,"indices",sep=":/")
		outlogdir<-paste(dirsplit,"qc",sep=":/")
		outjpgdir<-paste(dirsplit,"plots",sep=":/")
		outtrddir<-paste(dirsplit,"trend",sep=":/")
		outthresdir<-paste(dirsplit,"thres",sep=":/")  # to save *_thres.csv files   
		outqcdir<-paste(dirsplit,"qc",sep=":/")   # save results from extraqc
	} else{
		outdir<-outdirtmp[1]
		for(i in 2:(length(outdirtmp)-1))
		outdir<-paste(outdir,outdirtmp[i],sep="/")
		outinddir<-paste(outdir,"indices",sep="/")
		outlogdir<-paste(outdir,"qc",sep="/")
		outjpgdir<-paste(outdir,"plots",sep="/")
		outtrddir<-paste(outdir,"trend",sep="/")
		outqcdir<-paste(outdir,"qc",sep="/")    # save results from extraqc
		outthresdir<-paste(outdir,"thres",sep="/")   # to save *_thres.csv files 
	}
	
	# Create subdirectories if non-existent
	if(!file.exists(paste(outinddir,ofilename,sep="/"))) { dir.create(outinddir,showWarnings=FALSE) ; dir.create(paste(outinddir,ofilename,sep="/")) }
	if(!file.exists(paste(outlogdir,ofilename,sep="/"))) { dir.create(outlogdir,showWarnings=FALSE) ; dir.create(paste(outlogdir,ofilename,sep="/")) }
	if(!file.exists(paste(outjpgdir,ofilename,sep="/"))) { dir.create(outjpgdir,showWarnings=FALSE) ; dir.create(paste(outjpgdir,ofilename,sep="/")) }
	if(!file.exists(paste(outtrddir,ofilename,sep="/"))) { dir.create(outtrddir,showWarnings=FALSE) ; dir.create(paste(outtrddir,ofilename,sep="/")) }
	if(!file.exists(paste(outqcdir,ofilename,sep="/")))  { dir.create(outqcdir,showWarnings=FALSE) ; dir.create(paste(outqcdir,ofilename,sep="/")) }
	if(!file.exists(paste(outthresdir,ofilename,sep="/"))) { dir.create(outthresdir,showWarnings=FALSE) ; dir.create(paste(outthresdir,ofilename,sep="/")) }
	
	# modify subdirectory names
	outinddir <- paste(outinddir,ofilename,sep="/")
	outlogdir <- paste(outlogdir,ofilename,sep="/")
	outjpgdir <- paste(outjpgdir,ofilename,sep="/")
	outtrddir <- paste(outtrddir,ofilename,sep="/")
	outqcdir <- paste(outqcdir,ofilename,sep="/")
	outthresdir <- paste(outthresdir,ofilename,sep="/")
	
	# save the directory as global variable for use somewhere else.
	assign("outinddir",outinddir,envir=.GlobalEnv)
	assign("outlogdir",outlogdir,envir=.GlobalEnv)
	assign("outjpgdir",outjpgdir,envir=.GlobalEnv)
	assign("outtrddir",outtrddir,envir=.GlobalEnv)
	assign("outqcdir", outqcdir, envir=.GlobalEnv)
	assign("outthresdir",outthresdir,envir=.GlobalEnv)
}


# return True (T) if leapyear, esle F
leapyear <- function(year)
{
  remainder400 <- trunc(year - 400 * trunc(year / 400));
  remainder100 <- trunc(year - 100 * trunc(year / 100));
  remainder4   <- trunc(year - 4 * trunc(year / 4));
  if (remainder400 == 0) leapyear <- TRUE else
  {
    if (remainder100 == 0) leapyear <- FALSE else
    {
      if(remainder4 == 0) leapyear <- TRUE else leapyear <- FALSE;
    }
  }
}

write.NA.statistics <- function(cio) { 
	naprec = array(NA,dim=c(length(unique(cio@date.factors$annual))))
	naprec = tapply.fast(cio@data$prec,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
	natx = tapply.fast(cio@data$tmax,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
	natn = tapply.fast(cio@data$tmin,cio@date.factors$annual,function(x) { return(sum(is.na(x))) })
	
	nam1 <- paste(outqcdir, paste(ofilename, "_nastatistics.csv", sep = ""), sep = "/")
	write_header(nam1)
	# Suppress warning about column names in files
	suppressWarnings(write.table(cbind.data.frame(unique(cio@date.factors$annual),naprec,natx,natn), file = nam1, sep=",",append = TRUE, quote = FALSE, row.names = FALSE, col.names = c("Year","Prec","TX","TN")))
}

# creates a list of metadata
create.metadata <- function(latitude,longitude,base.year.start,base.year.end,dates,ofilename) {
	return(list(lat=latitude,lon=longitude,base.start=base.year.start,base.end=base.year.end,year.start=as.numeric(format(dates[1],format="%Y")),year.end=as.numeric(format(dates[length(dates)],format="%Y")),dates=dates,ofile=ofilename)) 
}

# returns a date time-series from user data, removes any non-gregorian dates and corresponding data in the process
check.and.create.dates <- function(user.data) {
	yyymmdd <- paste(user.data[,1],user.data[,2],user.data[,3],sep="-")
	user.dates <- as.Date(yyymmdd,format="%Y-%m-%d")

	year <- user.data$year[!is.na(user.dates)]
	month <- user.data$month[!is.na(user.dates)]
	day <- user.data$day[!is.na(user.dates)]
	prcp <- user.data$prcp[!is.na(user.dates)]
	tmax <- user.data$tmax[!is.na(user.dates)]
	tmin <- user.data$tmin[!is.na(user.dates)]
	user.data2 <- data.frame(year=year,month=month,day=day,precp=prcp,tmax=tmax,tmin=tmin)

	user.data2$dates <- user.dates[!is.na(user.dates)]

	return(user.data2)
}

# This function draws the "Step 1" interface that lets user enter station metadata and run QC on their file.
draw.step1.interface <- function(user.data, user.file, latitude, longitude, station.entry, base.year.start, base.year.end) {
    ofilename <<- station.entry
    outdirtmp[length(outdirtmp)] <<- ofilename

    assign("base.year.start",base.year.start,envir=.GlobalEnv)
    assign("base.year.end",base.year.end,envir=.GlobalEnv)

    user.data <- check.and.create.dates(user.data)
    create.dir(user.file)
    metadata <- create.metadata(latitude,longitude,base.year.start,base.year.end,user.data$dates,ofilename)
    assign("metadata",metadata,envir=.GlobalEnv)
    error <- QC.wrapper(metadata,user.data,user.file)
    return(error)
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
            return(paste("Base period must be between ", format(metadata$dates[1],format="%Y")," and ",format(metadata$dates[length(metadata$dates)],format="%Y"),". Please correct."))
        }
	}

	# Check for ascending order of years
	if(!all(user.data$year == cummax(user.data$year))) {
        return("Years are not in ascending order, please check your input file.")
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

    print("Got here")
    print(ofilename)
    print(outlogdir)

    # extraQC is called here. NOTE the default outrange=3 in original verson.
	error <- allqc(master = paste(user.file,".temporary",sep=""), output = outqcdir, outrange = 3) #stddev.crit)

	##############################	
	# Write out NA statistics.
	write.NA.statistics(cio)

	##############################	
	# Remove temporary file
	#system(paste("rm ",user.file,".temporary",sep=""))

    return(error)

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

