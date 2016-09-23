# climpact2-app

A web or desktop app for ClimPACT2. Can be run on a personal computer without connecting to the Internet or by visiting the web server at http://ec2-52-43-242-182.us-west-2.compute.amazonaws.com:3838/

# Install

To run the app from your computer:

```{bash}
$ git clone https://github.com/nicjhan/climpact2-app.git
$ cd climpact2-app
$ R
```

```{r}
> install.packages('shiny')
> install.packages('shinythemes')
> install.packages('markdown')
> install.packages('servr')
> source('startapp.R')
```

(it will automatically start up a web browser)

