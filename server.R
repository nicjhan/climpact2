
library(shiny)
library(servr)
servr::httw(port=4199, browser=FALSE, daemon=TRUE)

server <- function(input, output, session) {

    # Validate the stationName and dataFile fields.
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
    output$qualityControlError <- eventReactive(input$calculateIndices, {
        dataFileMissing()
    })
    # Validate the plot title.
    plotTitleMissing <- reactive({
        validate(
            need(input$plotTitle != "", message="Please enter a plotting title")
        )
        ""
    })
    output$indiceCalculationError <- eventReactive(input$calculateIndices, {
        plotTitleMissing()
    })

    datasetChanges <- reactive({
        input$doQualityControl
    })

    indiceChanges <- reactive({
        input$calculateIndices
    })

    output$qcLink <- renderText({
        datasetChanges()
        qcDir <- get.qc.dir()
        HTML(paste("Please view the <a target=\"_blank\" href=\"http://localhost:4199/",qcDir,"/\">QC output</a> and carefull evaluate before continuing. Refer to <a target=\"_blank\" href=\"http://localhost:4199/user_guide/html/AppendixC.htm\">Appendix C</a> of the <a target=\"_blank\" href=\"http://localhost:4199/user_guide/ClimPACT2_user_guide.htm\">ClimPACT2 user guide</a> for help.", sep=""))
    })

    output$indicesLink <- renderText({
        indiceChanges()
        HTML(paste("View <a target=\"_blank\" href=\"http://localhost:4199/",get.indices.dir(),"/\">indices</a>, <a target=\"_blank\" href=\"http://localhost:4199/",get.plots.dir(),"/\">plots</a>,  <a target=\"_blank\" href=\"http://localhost:4199/",get.trends.dir(),"/\">trends</a>, <a target=\"_blank\" href=\"http://localhost:4199/",get.thresh.dir(),"/\">trends</a> OR download all.", sep=""))
    })

    output$qualityControlError <- eventReactive(input$doQualityControl, {
        source("climpact2.R")
        # Set up globals
        package.check()
        source("ancillary/climpact2.etsci-functions.r")
        global.vars()

        stationNameMissing()
        dataFileMissing()

        dataFile <- input$dataFile
        if (is.null(dataFile)) {
          return(NULL)
        }

        latitude <- input$stationLat
        longitude <- input$stationLon
        stationName <- input$stationName

        base.year.start <- input$startYear
        base.year.end <- input$endYear

        #base.year.start <- input$dateRange[1]
        #base.year.end <- input$dateRange[2]
        #base.year.start <- as.numeric(format(base.year.start, "%Y"));
		#base.year.end <-as.numeric(format(base.year.end, "%Y"));

        #outputDir <- tempdir()
        outputDir <- './output'
        dir.create(outputDir)

        # input$dataFile will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message="Processing data", value=0)

        error <- load.data.qc(progress, dataFile$datapath, outputDir, latitude, longitude, stationName, base.year.start,base.year.end)
        if (error !=  "") {
            return(error)
        }

        return("")
    })

    output$indiceCalculationError <- eventReactive(input$calculateIndices, {

        plotTitleMissing()

        plot.title <- input$plotTitle
        wsdi_ud <- input$wsdin
        csdi_ud <- input$csdin
        rx_ui <- input$rxnday
        txtn_ud <- input$txtn
        Tb_HDD <- input$hdd
        Tb_CDD <- input$cdd
        Tb_GDD <- input$cdd
        rnnmm_ud <- input$rnnmm
        custom_SPEI <- input$spei
        var.choice <- input$custVariable
        op.choice <- input$custOperation
        constant.choice <- input$custThreshold

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message="Calculating indices", value=0)

        error <- draw.step2.interface(progress, plot.title, wsdi_ud, csdi_ud, rx_ui, txtn_ud, rnnmm_ud, Tb_HDD, Tb_CDD, Tb_GDD, custom_SPEI, var.choice, op.choice, constant.choice)

        return("")
    })

    outputOptions(output, "indiceCalculationError", suspendWhenHidden=FALSE)
    outputOptions(output, "qualityControlError", suspendWhenHidden=FALSE)
}

shinyServer(server)
