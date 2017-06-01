library(shiny)
library(shinythemes)
library(shinyjs)

library(markdown)
library(servr)
library(dplyr)
library(corrplot)
library(ggplot2)
source("climpact2.R")
package.check()
source("ancillary/climpact2.etsci-functions.r")
source("server/server.R")
source("server/sector_correlation.R")

try(servr::httw(host='0.0.0.0', port=4199, browser=FALSE, daemon=TRUE))

ui <- tagList(
    useShinyjs(),
    navbarPage(title="", id="mainNavbar", theme = shinytheme("cerulean"),
      source(file.path("ui", "front_page_tab.R"),  local = TRUE)$value,
      source(file.path("ui", "getting_started.R"),  local = TRUE)$value,
      source(file.path("ui", "load_and_check.R"),  local = TRUE)$value,
      source(file.path("ui", "calculate_indices.R"),  local = TRUE)$value,
      source(file.path("ui", "sector_data_correlation.R"),  local = TRUE)$value
  )
)

shinyApp(ui, climpact.server)
