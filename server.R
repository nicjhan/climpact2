
library(shiny)
library(servr)
library(dplyr)
library(corrplot)
library(ggplot2)
source("climpact2.R")
package.check()
source("ancillary/climpact2.etsci-functions.r")
source("industry_correlation.R")

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
    
    # Validate sector plot title.
    sectorPlotTitleMissing <- reactive({
      validate(
        need(input$sectorPlotName != "", message="Please enter a plotting title")
      )
      ""
    })
    output$sectorCorrelationError <- eventReactive(input$calculateSectorCorrelation, {
      sectorPlotTitleMissing()
    })

    datasetChanges <- reactive({
        input$doQualityControl
    })

    indiceChanges <- reactive({
        input$calculateIndices
    })
    
    sectorCorrelationChanges <- reactive({
      input$calculateSectorCorrelation
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
    
    output$sectorCorrelationLink <- renderText({
      sectorCorrelationChanges()
      HTML(paste("View <a target=\"_blank\" href=",file_url,get.corr.dir(),"/\">plots</a>", sep=""))
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

    # switch to calculateIndices tab
	  observeEvent(input$calculateIndicesTabLink, {
    	updateTabsetPanel(session, "mainNavbar",
                           selected="calculateIndices")
  	})
	  
	  # switch to getting started tab
	  observeEvent(input$doGetStarted, {
	    updateTabsetPanel(session, "mainNavbar",
	                      selected="gettingStarted")
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
    
    ## Correlation functionality
    
    # React to upload
    observeEvent(input$sectorDataFile, {
      val <- strsplit(input$sectorDataFile$name, "[_\\.]")[[1]][1]
      updateTextInput(session, "sectorPlotName", value=val)
    })
    
    # Handle calculation of correlation between climate/industry data
    output$sectorCorrelationError <- eventReactive(input$calculateSectorCorrelation, {
      
      if(!exists("corrdir")){
        return("Correlation directory does not exist, please use Process button on Load & Check Data")
      }
      
      climate.data <- dataFile()
      if (is.null(climate.data)) {
        return("Bad data file")
      }
      
      sector.data <- sectorDataFile()
      if (is.null(sector.data)) {
        return("Bad sector data file")
      }
      
      plotTitleMissing()

      plot.title <- input$sectorPlotName
      detrendCheck <- input$detrendCheck

      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message="Calculating correlation", value=0)

      error <- draw.correlation(progress, climate.data$datapath, sector.data$datapath, stationName(), plot.title, detrendCheck)

      ifelse(error=="",return(""),return(error))
    })

    outputOptions(output, "indiceCalculationError", suspendWhenHidden=FALSE)
    outputOptions(output, "qualityControlError", suspendWhenHidden=FALSE)
    outputOptions(output, "sectorCorrelationError", suspendWhenHidden=FALSE)
    
    # toggle state of buttons depending on certain criteria
    observe(toggleState('doQualityControl', !is.null(input$dataFile)))
    observe(toggleState('calculateIndices', !is.null(input$dataFile)))
    observe(toggleState('calculateSectorCorrelation', !is.null(input$dataFile) & !is.null(input$sectorDataFile)))
}

shinyServer(server)
