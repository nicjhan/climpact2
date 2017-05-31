
# Overview

The GUI is a web app which uses [Shiny](http://shiny.rstudio.com) by [Rstudio](https://www.rstudio.com). It uses a client-server pattern. The appearance and interactive behaviour of the app is all controlled on the client side. On the server side control messages are received, ClimPACT2 processing is done and results sent back. All of the client code is within the 'ui/' directory, while the server code can be found in 'server/' and 'Climpact2.R'.

Looking at the files in the 'ui/' directory you can see that they primarily deal with widget configuration and layout. They intentionally don't contain any lower level functionality.

In server/server.R ...

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
