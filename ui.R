
library(shinythemes)
library(markdown)

shinyUI(navbarPage("Climpact2", theme = shinytheme("readable"),
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
	            fileInput('file1', '',
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
                textInput("caption", "Station name:"),
                numericInput("lat", "Latitude:", 0, min = -90, max = 90),
                numericInput("lon", "Longitude:", 0, min = 0, max = 360),
                dateRangeInput('dateRange', label = 'Base period:',
                              start = Sys.Date() - 2, end = Sys.Date() + 2)
            )),
            fluidRow(
                column(4,
                    h4('3. Process Data and Quality Control'),
                    wellPanel(
                    actionButton("button", "Process"),
                    actionButton("button", "Cancel")
                )),
                column(4,
                    h4('4. Evaluate Quality Control output'),
                    wellPanel(
                    "Please ",
                    a("view QC output", target="_blank", href="http://climpact2-indice-plots.s3-website-us-west-2.amazonaws.com/"),
                    " and carefully evaluate before continuing. Refer to Appendix C in the ClimPACT2 user guide for help."
                ))
            )
        )
    )),
    tabPanel("Calculate Climate Indices", fluidPage(
        h4('1. Input User Parameters'),
        wellPanel(
        fluidRow(
            column(6,
                textInput("caption", "Plotting title:")
            )
        ),
        hr(),
        fluidRow(
            column(12,
                "See ",
                a("Section 3.5", target="_blank", href="https://github.com/ARCCSS-extremes/climpact2/blob/master/user_guide/ClimPACT2_user_guide.htm"),
                " of the ",
                a("ClimPact2 user guide", target="_blank", href="https://github.com/ARCCSS-extremes/climpact2/blob/master/user_guide/ClimPACT2_user_guide.htm"),
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
                numericInput("n", "n for nTXnTN and nTXbnTNb:", 2, min = 0)
            ),
            column(4,
                numericInput("hdd", "Base temp for HDDHeat:", 18, min = 0),
                numericInput("cdd", "Base temp for CDDHeat:", 18, min = 0),
                numericInput("gdd", "Base temp for GDDgrow:", 10, min = 0),
                numericInput("prec", "Number of days precip >= nn (Rnnmm):", 30, min = 0),
                numericInput("prec", "SPEI/SPI over months:", 24, min = 0)
            ),
            column(4,
                strong("Custom day count index (e.g. number of days where TX > 40, named TXgt40)"),
                br(),
                selectInput("variable", label="Variable:",
                    choices = list("TN" = 1, "TX" = 2, "TM" = 3, "PR" = 3, "DTR" = 4),
                    selected = 1
                ),
                selectInput("operation", label="Operation:",
                    choices = list(">" = 1, ">=" = 2, "<" = 3, "<=" = 4),
                    selected = 1
                ),
                numericInput("threshold", "Threshold:", 0)
            )
        )),
        fluidRow(
            column(6,
                h4('2. Calculate Indices'),
                wellPanel(
                    actionButton("calculateIndices", "Calculate Indices")
                )
            ),
            column(6,
                h4('3. View Indices'),
                conditionalPanel(
                    condition = "output.indicesCalculated",
                    wellPanel(
                            a("View", target="_blank", href="http://climpact2-indice-plots.s3-website-us-west-2.amazonaws.com/"),
                            " or ",
                            a("Download", target="_blank", href="http://climpact2-indice-plots.s3-website-us-west-2.amazonaws.com/"),
                            " plots. "
                    )
                ),
                conditionalPanel(
                    condition = "!output.indicesCalculated",
                    wellPanel(
                        "Please complete step 2. calculate indices."
                    )
                )
            )
        )
    ))
))

