# climpact2-app

A web or desktop app for ClimPACT2. Can be run on a personal computer without connecting to the Internet or by visiting the web server at http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:3838/

# Install

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

# Install server on Ubuntu (not necessary when running locally)


Follow instructions here: https://www.rstudio.com/products/shiny/download-server/

Then:

```{bash}
sudo su --c "R -e \"install.packages('rmarkdown', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('shinythemes', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('servr', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('corrplot', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('Rcpp', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('caTools', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('PCICt', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('SPEI', repos='http://cran.rstudio.com/')\""
sudo su --c "R -e \"install.packages('climdex.pcic', repos='http://cran.rstudio.com/')\""
```

Make sure TCP port 3838 is open/accessible then visit: http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:3838/

# Update app on server (not necessary when running locally)

```{bash}
cd /srv/shiny-server/
sudo git clone https://github.com/nicjhan/climpact2-app.git
```

Edit /etc/shiny-server/shiny-server.conf to look like below, notice that 'site_dir' is commented out.
```
# Define a location at the base URL
location / {

	app_dir /srv/shiny-server/climpact2-app;

    # Host the directory of Shiny Apps stored in this directory
    # site_dir /srv/shiny-server;
...
```

Restart the server:
```{bash}
sudo systemctl restart shiny-server
```

To see the logs:
```{bash}
cat /var/log/shiny-server.log
```


