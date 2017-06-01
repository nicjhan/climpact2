
### About ClimPACT2
ClimPACT2 is an R software package that calculates the [ET-SCI](https://www.wmo.int/pages/prog/wcp/ccl/opace/opace4/ET-SCI-4-1.php) indices, as well as additional climate extremes indices. It directly incorporates the R climdex.pcic package (available on CRAN) to perform most of the calculations. Climdex.pcic is available thanks to the efforts of the Pacific Climate Impacts Consortium (PCIC). ClimPACT2 is developed at the University of New South Wales.

### About this App
This is a web app which can also be run on a personal computer without connecting to the Internet. It interfaces with the ClimPACT2 software and can be used to generate indices based on data and parameters provided by the user. This involves two steps which must be completed in order:

1. **Load and check input data**.
2. **Calculate climate indices**.

Use the navigation bar at the top of the page to perform these steps.

### How to Run this App
 To run the app locally on your computer with R installed:

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
The app will automatically start in your default browser.

Or, connect to the ClimPACT2 server at http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:3838/

### Further Documentation and License
Please refer to the official ClimPACT2 GitHub page at https://github.com/ARCCSS-extremes/climpact2
