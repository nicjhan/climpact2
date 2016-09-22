
library(shiny)
library(servr)

servr::httw(port=4199, browser=FALSE, daemon=TRUE)
shiny::runApp('./')

