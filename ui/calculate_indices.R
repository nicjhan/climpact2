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
              href=paste("output.fileServerUrl", "/user_guide/html/GUI.htm", sep="")),
            " of the ",
            a("ClimPact2 user guide", target="_blank",
              href=paste("output.fileServerUrl", "/user_guide/ClimPACT2_user_guide.htm", sep="")),
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
))

