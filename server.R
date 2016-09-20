
source("climpact2.R")
package.check()
source("ancillary/climpact2.etsci-functions.r")

server <- function(input, output, session) {

    # Set up globals
    global.vars()

    output$indicesCalculated <- eventReactive(input$calculateIndices, {
        cat("Hello There")
        TRUE
    })

    # Validate the stationName and dataFile fields. FIXME: do this properly
    stationNameMissing <- reactive({
        validate(
            need(input$stationName != "", message="Please enter a station name")
        )
        ""
    })
    output$stationNameMissing <- eventReactive(input$doQualityControl, {
        stationNameMissing()
    })
    dataFileMissing <- reactive({
        validate(
            need(!is.null(input$dataFile), message="Please load a dataset")
        )
        ""
    })
    output$dataFileMissing <- eventReactive(input$doQualityControl, {
        dataFileMissing()
    })

    output$qualityControlDone <- eventReactive(input$doQualityControl, {
        cat("Doing quality control")

        stationNameMissing()
        dataFileMissing()

        dataFile <- input$dataFile
        if (is.null(dataFile))
          return(NULL)

        latitude <- input$stationLat
        longitude <- input$stationLon
        base.year.start <- input$dateRange[0]
        base.year.end <- input$dateRange[1]
        ofilename <- tempfile()

        # input$dataFile will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        user.data <- read.user.file(dataFile$datapath)
        metadata <- create.metadata(latitude,longitude,base.year.start,base.year.end,user.data$dates,ofilename)
        QC.wrapper(metadata, user.data, user.file)

        withProgress(message = "Processing data", value = 0, {
            n <- 20
            for (i in 1:n) {
                # Increment the progress bar
                incProgress(1/n)
                Sys.sleep(0.1)
            }
        })

        TRUE
    })

    outputOptions(output, "indicesCalculated", suspendWhenHidden=FALSE)
    outputOptions(output, "qualityControlDone", suspendWhenHidden=FALSE)
}

shinyServer(server)
