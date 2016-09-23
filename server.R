
server <- function(input, output, session) {

    output$indicesCalculated <- eventReactive(input$calculateIndices, {
        TRUE
    })

    # Validate the stationName and dataFile fields. FIXME: do this properly
    stationNameMissing <- reactive({
        validate(
            need(input$stationName != "", message="Please enter a station name")
        )
        ""
    })
    output$qualityControlError <- eventReactive(input$doQualityControl, {
        stationNameMissing()
    })
    dataFileMissing <- reactive({
        validate(
            need(!is.null(input$dataFile), message="Please load a dataset")
        )
        ""
    })
    output$qualityControlError <- eventReactive(input$doQualityControl, {
        dataFileMissing()
    })

    output$qcLink <- renderText({
       #a("view QC output", target="_blank", href=
        qcDir <- get.qc.dir()
        print("QC dir")
        print(qcDir)
        HTML(paste("<a target=\"_blank\" href=\"http://localhost:4199/",qcDir,"/\">QC output</a>", sep=""))
    })

    output$qualityControlError <- eventReactive(input$doQualityControl, {
        cat("Doing quality control")
        source("climpact2.R")
        # Set up globals
        package.check()
        source("ancillary/climpact2.etsci-functions.r")
        global.vars()

        stationNameMissing()
        dataFileMissing()

        dataFile <- input$dataFile
        if (is.null(dataFile))
          return(NULL)

        latitude <- input$stationLat
        longitude <- input$stationLon
        stationName <- input$stationName
        base.year.start <- input$dateRange[1]
        base.year.end <- input$dateRange[2]
        base.year.start <- as.numeric(format(base.year.start, "%Y"));
		base.year.end <-as.numeric(format(base.year.end, "%Y"));

        outputDir <- 'output'
        dir.create(outputDir)

        # input$dataFile will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        error <- load.data.qc(dataFile$datapath, outputDir, latitude, longitude, stationName, base.year.start,base.year.end)
        if (error !=  "") {
            return(error)
        }

        withProgress(message = "Processing data", value = 0, {
            n <- 20
            for (i in 1:n) {
                # Increment the progress bar
                incProgress(1/n)
                Sys.sleep(0.1)
            }
        })

        return("")
    })

    outputOptions(output, "indicesCalculated", suspendWhenHidden=FALSE)
    outputOptions(output, "qualityControlError", suspendWhenHidden=FALSE)
}

shinyServer(server)
