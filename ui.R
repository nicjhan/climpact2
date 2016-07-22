
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
	            radioButtons('sep', 'Separator',
    	               c(Comma=',', Semicolon=';', Tab='\t'), ',')
            )),
            column(4,
                h4('2. Enter Dataset Infomation'),
                wellPanel(
                textInput("caption", "Station name:"),
                numericInput("lat", "Latitude:", 180, min = 0, max = 360),
                numericInput("lon", "Longitude:", 0, min = -90, max = 90),
                dateRangeInput('dateRange', label = 'Base period:',
                              start = Sys.Date() - 2, end = Sys.Date() + 2),
                actionButton("button", "Validate")
            )),
            column(4,
                h4('3. Process Data and Quality Control'),
                wellPanel(
                actionButton("button", "Process"),
                actionButton("button", "Cancel"),
                hr(),
                "Please ",
                actionLink("button", "View QC Output"),
                "and carefully evaluate before continuing. Refer to Appendix C in the ClimPACT2 user guide for help."
            ))
        )
    )),
    tabPanel("Calculate Climate Indices", fluidPage(
        h4('1. Input User Parameters'),
        wellPanel(
        fluidRow(
            column(4,
                textInput("caption", "Plotting title:"),
                hr(),
                "See section 3.5 of the ClimPACT2 user guide for help with the following fields",
                br(),
                numericInput("wsdin", "WSDIn Days:", 2, min = 0),
                numericInput("csdin", "CSDIn Days:", 2, min = 0),
                numericInput("rxnday", "RxnDay Days:", 3, min = 0),
                numericInput("n", "n for nTXnTN and nTXbnTNb:", 2, min = 0)
            ),
            column(4,
                numericInput("hdd", "base temp for HDDHeat:", 18, min = 0),
                numericInput("cdd", "base temp for CDDHeat:", 18, min = 0),
                numericInput("gdd", "base temp for GDDgrow:", 10, min = 0),
                numericInput("prec", "number of days precip >= nn (Rnnmm):", 30, min = 0),
                numericInput("prec", "SPEI/SPI over months:", 24, min = 0)
            ),
            column(4,
                "Custom day count index (e.g. number of days where TX > 40, named TXgt40)",
                numericInput("wsdin", "Variable:", 2, min = 0),
                numericInput("csdin", "Operation:", 2, min = 0),
                numericInput("rxnday", "Threshold:", 3, min = 0),
                hr(),
                actionButton("button", "Validate")
            )
        )),
        h4('2. Calculate and View Indices'),
        wellPanel(
        fluidRow(
            column(12,
                actionButton("button", "Calculate Indices"),
                "Then ",
                actionLink("button", "View"),
                " or ",
                actionLink("button", "Download"),
                " plots. "
            )
        ))
    ))
))

