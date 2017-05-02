tabPanel(title="Sector Data Correlation", fluidPage(
    fluidRow(
        column(6,
            h4('9. Load Sector Data'),
            wellPanel(
          fileInput('sectorDataFile', NULL,
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
              a("wheat_yield_nsw_1922-1999.csv", target="_blank",
                href=paste(file_url, "sample_data/wheat_yield_nsw_1922-1999.csv", sep="")),
              ' and then load.')
        )),
        column(6,
            h4('10. Sector Data Infomation'),
            wellPanel(
            textInput("sectorPlotName", "Plot name:"),
            checkboxInput("detrendCheck", "Detrend data", value = TRUE, width = NULL)
        ))
    ),
  fluidRow(
    column(6,
            h4('11. Make correlation plots'),
            wellPanel(
            actionButton("calculateSectorCorrelation", "Calculate Correlation"),
            textOutput("sectorCorrelationError")
            )
        ),
       column(6,
              h4('12. View correlation'),
              conditionalPanel(
                condition = "output.sectorCorrelationError== ''",
                wellPanel(
                  uiOutput("sectorCorrelationLink")
                )
              ),
              conditionalPanel(
                condition = "output.sectorCorrelationError != ''",
                wellPanel(
                  "Please complete step 11: ",
                  tags$b("Calculate Correlation.")
                )
              )
         )
    )
)
