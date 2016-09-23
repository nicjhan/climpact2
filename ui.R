
library(shinythemes)
library(markdown)

ui <- navbarPage("Climpact2", theme = shinytheme("readable"),
    tabPanel("Getting Started",
	    fluidPage(
		    includeMarkdown("getting_started.md")
	    )
    ),
    tabPanel("Load and Check Data", fluidPage(
        fluidRow(
       	    column(4,
                h4('1. Load Dataset'),
                wellPanel(
	            fileInput('dataFile', NULL,
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
   		        checkboxInput('header', 'Includes header', TRUE),
	            radioButtons('separator', 'Separator',
    	               c(Comma=',', Semicolon=';', Tab='\t', Space=' '), ','),
				radioButtons('quote', 'Quote',
                             c(None='', 'Double Quote'='"', 'Single Quote'="'"), '"')
            )),
            column(4,
                h4('2. Enter Dataset Infomation'),
                wellPanel(
                textInput("stationName", "Station name:"),
                numericInput("stationLat", "Latitude:", 0, min = -90, max = 90),
                numericInput("stationLon", "Longitude:", 0, min = 0, max = 360),
                numericInput("startYear", "Start year:", 1970),
                numericInput("endYear", "End year:", 1970)
                #dateRangeInput('dateRange', label = 'Base period:', startview="decade",
                #              start = as.Date("2010-01-01", "%Y-%m-%d"), end = as.Date("2014-01-01", "%Y-%m-%d"))
            )),
            fluidRow(
                column(4,
                    h4('3. Process Data and Quality Control'),
                    wellPanel(
                    actionButton("doQualityControl", "Process"),
                    textOutput("qualityControlError")
                )),
                column(4,
                    h4('4. Evaluate Quality Control output'),
                    conditionalPanel(
                        condition = "output.qualityControlError == ''",
                        wellPanel(
                            uiOutput("qcLink")
                        )
                    ),
                    conditionalPanel(
                        condition = "output.qualityControlError != ''",
                        wellPanel(
                            "Please complete step 3: ",
                            tags$b("Process Data and Quality Control")
                        )
                    )
                )
            )
        )
    )),
    tabPanel("Calculate Climate Indices", fluidPage(
        h4('1. Input User Parameters'),
        wellPanel(
        fluidRow(
            column(6,
                textInput("plotTitle", "Plotting title:")
            )
        ),
        hr(),
        fluidRow(
            column(12,
                "See ",
                a("Section 3.5", target="_blank", href="http://localhost:4199/user_guide/html/GUI.htm"),
                " of the ",
                a("ClimPact2 user guide", target="_blank", href="http://localhost:4199/user_guide/ClimPACT2_user_guide.htm"),
                " for help with the following fields.",
                br(),
                br()
            )
        ),
        fluidRow(
            column(4,
                numericInput("wsdin", "WSDIn Days:", 2, min = 0),
                numericInput("csdin", "CSDIn Days:", 2, min = 0),
                numericInput("rxnday", "RxnDay Days:", 3, min = 0),
                numericInput("txtn", "n for nTXnTN and nTXbnTNb:", 2, min = 0)
            ),
            column(4,
                numericInput("hdd", "Base temp for HDDHeat:", 18, min = 0),
                numericInput("cdd", "Base temp for CDDHeat:", 18, min = 0),
                numericInput("gdd", "Base temp for GDDgrow:", 10, min = 0),
                numericInput("rnnmm", "Number of days precip >= nn (Rnnmm):", 30, min = 0),
                numericInput("spei", "SPEI/SPI over months:", 24, min = 0)
            ),
            column(4,
                strong("Custom day count index (e.g. number of days where TX > 40, named TXgt40)"),
                br(),
                selectInput("custVariable", label="Variable:",
                    choices = list("TN", "TX", "TM", "PR", "DTR"),
                    selected = "TN"
                ),
                selectInput("custOperation", label="Operation:",
                    choices = list(">", ">=", "<", "<="),
                    selected = ">"
                ),
                numericInput("custThreshold", "Threshold:", 0)
            )
        )),
        fluidRow(
            column(6,
                h4('2. Calculate Indices'),
                conditionalPanel(
                    condition = "output.qualityControlError == ''",
                    wellPanel(
                        actionButton("calculateIndices", "Calculate Indices"),
                        textOutput("indiceCalculationError")
                    )
                ),
                conditionalPanel(
                    condition = "output.qualityControlError != ''",
                    wellPanel(
                        "Please complete ",
                        tags$b("Load and Check Data")
                    )
                )
            ),
            column(6,
                h4('3. View Indices'),
                conditionalPanel(
                    condition = "output.indiceCalculationError == ''",
                    wellPanel(
                        uiOutput("indicesLink")
                    )
                ),
                conditionalPanel(
                    condition = "output.indiceCalculationError != ''",
                    wellPanel(
                        "Please complete step 2: ",
                        tags$b("Calculate Indices.")
                    )
                )
            )
        )
    ))
)

shinyUI(ui)


