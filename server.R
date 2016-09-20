

server <- function(input, output, session) {

    output$indicesCalculated <- eventReactive(input$calculateIndices, {
        cat("Hello There")
        TRUE
    })

    # Validate the stationName field
    stationName <- reactive({
        validate(
            need(input$stationName != "", "Please enter a station name")
        )
        input$stationName
    })
    output$missingStationName <- eventReactive(input$doQualityControl, {
        stationName()
    })

    output$qualityControlDone <- eventReactive(input$doQualityControl, {
        cat("Doing quality control")

        station <- stationName()
        cat(station)
        dataFile <- input$dataFile

        withProgress(message = "Processing data", value = 0, {
            n <- 100
            for (i in 1:n) {
                # Increment the progress bar
                incProgress(1/n)
                Sys.sleep(0.1)
            }
        })

        TRUE
    })

	output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        read.csv(inFile$datapath, header=input$header, sep=input$separator,
                     quote=input$quote)
  })

  outputOptions(output, "indicesCalculated", suspendWhenHidden=FALSE)
  outputOptions(output, "qualityControlDone", suspendWhenHidden=FALSE)
}

shinyServer(server)
