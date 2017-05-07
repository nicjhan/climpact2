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
source("sector_correlation.R")
source("server/server.R")

try(servr::httw(host='0.0.0.0', port=4199, browser=FALSE, daemon=TRUE))

# FIXME: use session to get url. Removes the need for a global variable.
if (Sys.info()["nodename"] == 'ip-172-31-0-164') {
    file_url <- "\"http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:4199/"
} else {
    file_url <- "\"http://localhost:4199/"
}

ui <- tagList(
    useShinyjs(),
    navbarPage(title="", id="mainNavbar", theme = shinytheme("cerulean"),
      source(file.path("ui", "front_page_tab.R"),  local = TRUE)$value,
      source(file.path("ui", "getting_started.R"),  local = TRUE)$value,
      source(file.path("ui", "load_and_check.R"),  local = TRUE)$value,
      source(file.path("ui", "calculate_indices.R"),  local = TRUE)$value
  )
)

shinyApp(ui, climpact.server)
