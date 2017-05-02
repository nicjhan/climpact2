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
              href=paste(file_url, "/user_guide/html/appendixB.htm", sep="")),
            " of the ",
            a("ClimPact2 user guide", target="_blank",
              href=paste(file_url, "/user_guide/ClimPACT2_user_guide.htm", sep="")),
            tags$hr(),
            p('If you want a sample dataset,',
              'first save this sample',
              a("sydney_observatory_hill_1936-2015.txt", target="_blank",
                href=paste(file_url, "sample_data/sydney_observatory_hill_1936-2015.txt", sep="")),
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
))

