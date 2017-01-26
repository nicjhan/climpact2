
library(shiny)
library(servr)
source("climpact2.R")
package.check()
source("ancillary/climpact2.etsci-functions.r")

try(servr::httw(host='0.0.0.0', port=4199, browser=FALSE, daemon=TRUE))

# FIXME: can use session to get url.
if (Sys.info()["nodename"] == 'ip-172-31-0-164') {
    file_url <- "\"http://ec2-52-65-87-111.ap-southeast-2.compute.amazonaws.com:4199/"
} else {
    file_url <- "\"http://localhost:4199/"
}

server <- function(input, output, session) {

    output$qualityControlError <- eventReactive(input$doQualityControl, {
        stationName()
    })
    output$qualityControlError <- eventReactive(input$calculateIndices, {
        dataFile()
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
        HTML(paste("Please view the <a target=\"_blank\" href=",file_url,qcDir,"/\">QC output</a> and carefully evaluate before continuing. Refer to <a target=\"_blank\" href=",file_url,"user_guide/html/appendixC.htm\">Appendix C</a> of the <a target=\"_blank\" href=",file_url, "/user_guide/ClimPACT2_user_guide.htm\">ClimPACT2 user guide</a> for help.", sep=""))
    })

    output$indicesLink <- renderText({
        indiceChanges()
        HTML(paste("View <a target=\"_blank\" href=",file_url,get.indices.dir(),"/\">indices</a>, <a target=\"_blank\" href=",file_url,get.plots.dir(),"/\">plots</a>,  <a target=\"_blank\" href=",file_url,get.trends.dir(),"/\">trends</a>, <a target=\"_blank\" href=",file_url,get.thresh.dir(),"/\">thresholds</a> OR <a target=\"_blank\" href=",file_url,get.output.zipfile(),"\">download all</a>.", sep=""))
    })

    stationLat <- reactive({
        validate(
            need(input$stationLat >= -90 && input$stationLat <= 90,
                 'Latitude must be between -90 and 90.')
        )
        input$stationLat
    })

    stationLon <- reactive({
        validate(
            need(input$stationLon >= -180 && input$stationLon <= 180,
                 'Longitude must be between -180 and 180')
        )
        input$stationLon
    })

    stationName <- reactive({
        validate(
            need(input$stationName != "", message="Please enter a station name")
        )
        input$stationName
    })

    dataFile <- reactive({
        validate(
            need(!is.null(input$dataFile), message="Please load a dataset")
        )
        input$dataFile
    })

    sectorDataFile <- reactive({
        validate(
            need(!is.null(input$sectorDataFile), message="Please load a dataset")
        )
        input$sectorDataFile
    })

	observeEvent(input$calculateIndicesTabLink, {
    	updateTabsetPanel(session, "mainNavbar",
                           selected="calculateIndices")
  	})

    observeEvent(input$dataFile, {
        val <- strsplit(input$dataFile$name, "[_\\.]")[[1]][1]
        updateTextInput(session, "stationName", value=val)
        updateTextInput(session, "plotTitle", value=val)
    })

    output$qualityControlError <- eventReactive(input$doQualityControl, {

        # Set up globals in Climpact2
        global.vars()

        file <- dataFile()
        if (is.null(file)) {
            return("Bad data file")
        }

        latitude <- stationLat()
        longitude <- stationLon()
        station <- stationName()

        base.year.start <- input$startYear
        base.year.end <- input$endYear

        outputDir <- tempfile(tmpdir='./output')
        dir.create(outputDir)

        # input$dataFile will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message="Processing data", value=0)

        error <- load.data.qc(progress, file$datapath, outputDir, latitude,
                              longitude, station,
                              base.year.start, base.year.end)
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

        error <- draw.step2.interface(progress, plot.title, wsdi_ud, csdi_ud,
                                      rx_ui, txtn_ud, rnnmm_ud, Tb_HDD, Tb_CDD,
                                      Tb_GDD, custom_SPEI, var.choice, op.choice,
                                      constant.choice)
        return("")
    })

    outputOptions(output, "indiceCalculationError", suspendWhenHidden=FALSE)
    outputOptions(output, "qualityControlError", suspendWhenHidden=FALSE)
}

shinyServer(server)
