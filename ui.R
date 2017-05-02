
library(shinythemes)
library(markdown)
library(shinyjs)

if (Sys.info()["nodename"] == 'ip-172-31-0-164') {
    url <- "http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:4199/"
} else {
    url <- "http://localhost:4199/"
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

shinyUI(ui)

