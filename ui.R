
library(shinythemes)
library(markdown)

if (Sys.info()["nodename"] == 'ip-172-31-0-164') {
    url <- "http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:4199/"
} else {
    url <- "http://localhost:4199/"
}

ui <- navbarPage(title="Climpact2", id="mainNavbar", theme = shinytheme("cerulean"),
    tabPanel(title="Getting Started",
	    fluidPage(
		    includeMarkdown("getting_started.md")
	    )
    ),
    tabPanel(title="Load and Check Data", fluidPage(
        fluidRow(
       	    column(4,
                h4('1. Load Dataset'),
                wellPanel(
	            fileInput('dataFile', NULL,
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                "The dataset ",
                strong("must"),
                " use the format described in ",
                a("Appendix B", target="_blank",
                  href=paste(url, "/user_guide/html/appendixB.htm", sep="")),
                " of the ",
                a("ClimPact2 user guide", target="_blank",
                  href=paste(url, "/user_guide/ClimPACT2_user_guide.htm", sep="")),
                tags$hr(),
                p('If you want a sample dataset,',
                  'first save this sample',
                  a("sydney_observatory_hill_1936-2015.txt", target="_blank",
                    href=paste(url, "sample_data/sydney_observatory_hill_1936-2015.txt", sep="")),
                  ' and then load.')
            )),
            column(4,
                h4('2. Enter Dataset Infomation'),
                wellPanel(
                textInput("stationName", "Station name:"),
                numericInput("stationLat", "Latitude:", 0, min = -90, max = 90),
                numericInput("stationLon", "Longitude:", 0, min = -180, max = 180),
                numericInput("startYear", "Base Period Start year:", 1970, min = 0),
                numericInput("endYear", "Base Period End year:", 2010, min = 0)
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
                ),
                column(4,
                    h4('5. Calculate Climate Indices'),
                    conditionalPanel(
                        condition = "output.qualityControlError == ''",
                        wellPanel(
                            actionLink("calculateIndicesTabLink",
                                       "Go to the Calculate Climate Indices tab")
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
    tabPanel(title="Calculate Climate Indices", value="calculateIndices", fluidPage(
        h4('6. Input User Parameters'),
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
                a("Section 3.5", target="_blank",
                  href=paste(url, "/user_guide/html/GUI.htm", sep="")),
                " of the ",
                a("ClimPact2 user guide", target="_blank",
                  href=paste(url, "/user_guide/ClimPACT2_user_guide.htm", sep="")),
                " for help with the following fields.",
                br(),
                br()
            )
        ),
        fluidRow(
            column(4,
                numericInput("wsdin", "WSDIn Days:", 1, min = 1, max = 10),
                numericInput("csdin", "CSDIn Days:", 1, min = 0),
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
                h4('7. Calculate Indices'),
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
                h4('8. View Indices'),
                conditionalPanel(
                    condition = "output.indiceCalculationError == ''",
                    wellPanel(
                        uiOutput("indicesLink")
                    )
                ),
                conditionalPanel(
                    condition = "output.indiceCalculationError != ''",
                    wellPanel(
                        "Please complete step 7: ",
                        tags$b("Calculate Indices.")
                    )
                )
            )
        )
    )),
    tabPanel(title="Sector Data Correlation", fluidPage(
        fluidRow(
       	    column(6,
                h4('1. Load Sector Data'),
                wellPanel(
	            fileInput('sectorDataFile', NULL,
                    accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                "The dataset ",
                strong("must"),
                " use the format described in ",
                a("Appendix B", target="_blank",
                  href=paste(url, "/user_guide/html/appendixB.htm", sep="")),
                " of the ",
                a("ClimPact2 user guide", target="_blank",
                  href=paste(url, "/user_guide/ClimPACT2_user_guide.htm", sep="")),
                tags$hr(),
                p('If you want a sample dataset,',
                  'first save this sample',
                  a("wheat_yield_nsw_1922-1999.csv", target="_blank",
                    href=paste(url, "sample_data/wheat_yield_nsw_1922-1999.csv", sep="")),
                  ' and then load.')
            )),
            column(6,
                h4('2. Sector Data Infomation'),
                wellPanel(
                textInput("sectorPlotName", "Plot name:"),
                checkboxInput("detrendCheck", "Detrend data", value = TRUE, width = NULL)
            ))
        ),
        fluidRow(
           column(12,
                h4('3. Make correlation plots'),
                wellPanel(
                actionButton("calculateSectorCorrelation", "Calculate Correlation"),
                textOutput("sectorCorrelationError")
                )
            )
        )
    ))
)

shinyUI(ui)

