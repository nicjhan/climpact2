# ClimPACT2

## What is it?

ClimPACT2 is an R software package that calculates the ET-SCI indices as well as additional climate extremes indices from data stored in text or netCDF files. It directly incorporates the R packages climdex.pcic and climdex.pcic.ncdf developed by the Pacific Climate Impacts Consortium (PCIC). Three methods of using the software allow the user to calculate indices on a station text file via a Graphical User Interface, to batch process multiple station text files in parallel and to calculate the indices on netCDF data in parallel.

##  Where can I get it?

ClimPACT2 is available on github @ https://github.com/ARCCSS-extremes/climpact2

## How do I use the GUI?

The ClimPACT2 GUI is an app that runs within a browser. It is developed using Shiny by Rstudio. The GUI Can be run on a personal computer without connecting to the Internet or by visiting the web server at http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:3838/

To run the app from your computer:

```{bash}
git clone https://github.com/nicjhan/climpact2-app.git
cd climpact2-app
R
```

```{r}
install.packages('shiny')
install.packages('shinythemes')
install.packages('markdown')
install.packages('servr')
install.packages('dplyr')
install.packages('corrplot')
install.packages('ggplot2')
install.packages('shinyjs')
library(shiny)
runApp('./')
```

(it will automatically start up a web browser)


## How do I calculate the indices on netCDF datasets? (Linux/MacOS only)

Software you will need before proceeding:
    -R (version 3.0.2 or later). You will need administrator privileges 
 on your computer or the ability to install R libraries.
    -netCDF
    -PROJ4 development files (libproj-dev package on Ubuntu)
    -udunits development files (libudunits2-dev package on Ubuntu)

1) Download and extract the following file to your computer:
   https://github.com/ARCCSS-extremes/climpact2/archive/master.zip
   This will create a directory named "climpact2-master".

2) Cd to the climpact2-master directory created in step 1, open R and run 
   "source('installers/climpact2.ncdf.installer.r')" to install the required R packages.
   You may be asked whether you would like to make a personal library, in 
   most cases the answer should be 'yes'. Once complete, quit R by typing
   "q()". This step only needs to be done once.

3) Modify the climpact2.ncdf.wrapper.r file to suit your needs (see manual
   for optional parameters to specify). Then execute by running 
   "Rscript climpact2.ncdf.wrapper.r" from the Linux command line. Depending
   on the size of your data and the number of cores selected, this process
   can take anywhere from one to twelve hours.

          Notes on netCDF data format:
          - Files must be CF compliant.
          - There must be no 'bounds' attributes in your latitude or 
            longitude variables.
          - Your precipitation variable must have units of "kg m-2 d-1",
            not "mm/day". These are numerically equivalent.
          - Your minimum and maximum temperature variables must be 
            uniquely named.
          - ncrename, ncatted and ncks from the NCO toolset can help 
            you modify your netCDF files.
            http://nco.sourceforge.net/


## How do I batch process multiple station (.txt) files?
  
Software you will need before proceeding:
    -R (version 3.0.2 or later). You will need administrator privileges 
 on your computer or the ability to install R libraries.

1) Download and extract the following file to your computer:
     https://github.com/ARCCSS-extremes/climpact2/archive/master.zip
     This will create a directory named "climpact2-master".

2) Cd to the climpact2-master directory created in step 1, open R and run 
   "source('installers/climpact2.batch.installer.r')" to install the required R packages.  You may be asked whether you would like to make a personal library, in 
   You may be asked whether you would like to make a personal library, in 
   most cases the answer should be 'yes'. Once complete, quit R by typing
   "q()". This step only needs to be done once.
   
3) From the terminal run the following command, replacing the flags
   with the folder where your station text files are kept, a metadata file
   containing the file name of each station text file along with relevant 
   station information, the beginning and end years of the base period, and
   the number of cores to use in processing, respectively. See the user guide for more information.

Rscript climpact2.batch.stations.r ./sample_data/ ./sample_data/climpact2.sample.batch.metadata.txt 1971 2000 4
  
## Common problems

  - If you experience trouble installing R packages in Windows, try to disable your antivirus software temporarily.

## Documentation
  
 Documentation exists in the form of this README file, the official ClimPACT2 user guide (available with this software) as well as the source code itself.

## Contact
  
Software issues contact Nicholas Herold : nicholas.herold@unsw.edu.au
  
All other issues contact Lisa Alexander : l.alexander@unsw.edu.au
  
## Credits
  
Oversight: World Meteorological Organisation (WMO), the Expert Team on Sector-specific Climate Indices (ET-SCI).

Design and documentation: Lisa Alexander and Nicholas Herold.

GUI: Nicholas Herold, James Goldie, Lisa Alexander, Enric Aguilar, Marc Prohom, the Pacific Climate Impacts Consortium (David Bronaugh, James Hiebert), Hongang Yang, Yang Feng and Yujun Ouyang.

NetCDF calculation: Pacific Climate Impacts Consortium (David Bronaugh, James Hiebert) and Nicholas Herold.

Batch processing: Nicholas Herold.
  
