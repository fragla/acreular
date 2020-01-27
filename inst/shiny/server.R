library(acreular)
library(mime)
library(readxl)
library(DT)

options(shiny.sanitize.errors = FALSE)

#addResourcePath('example-data', system.file("extdata", package="eq5d"))

shinyServer(function(input, output) {

  output$choose_calc_type <- renderUI({
    radioButtons("multi", "Multiple calculations:",
                 c("Single"="single", "Multiple"="multiple"),
                 selected="multiple",
                 inline=T)
  })

  output$choose_dataset <- renderUI({
    fileInput("data", "Choose data file",
              accept = c(
                mimemap["csv"],
                mimemap["xls"],
                mimemap["xlsx"])
    )
  })

  output$ra_table <- DT::renderDataTable({
    if(is.null(input$data))
      return()

    res <- getTableData()

    return(res)
  })

  output$export_table <- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download_table", 'Download Output File')
    }
  })

  output$download_table <- downloadHandler(
    filename = function() {
      paste(input$version, "_", input$country, "_", input$type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getTableData(), file, row.names = FALSE)
    }
  )

  getTableData <- reactive({
    dat <- dataset()

    names(dat) <- tolower(names(dat))
    dat.list = purrr::pmap(as.list(dat), list)

    classification <- lapply(dat.list, function(x) {
    obj <- acrEularRA(ljc=x$ljc, sjc=x$sjc, duration=x$duration, onset=as.Date(x$onset,"%d/%m/%Y"),
                      assessment=as.Date(x$assessment, "%d/%m/%Y"), apr=x$apr, crp=x$crp, esr=x$esr,
                      serology=x$serology, ccp=x$ccp, rf=x$rf) ##add in uln

      acrEularRAClassification(obj)
    })

    classification <- as.character(unlist(classification))

    dat$ACREULAR <- classification
    dat
  })

  rawdata <- reactive({
    if(input$data$type %in% c(mimemap["xls"], mimemap["xlsx"])) {
      dat <- read_excel(input$data$datapath, na=c("NA",""))
      dat <- as.data.frame(dat)
    }
    else {
      dat <- read.csv(file=input$data$datapath, header=TRUE, stringsAsFactors=FALSE, na.strings=c("NA",""))
    }
  })

  dataset <- reactive({
    dat <- rawdata()
    return(dat)
  })

  getColumnIndex <- function(dat) {
    col.names <- getColumnNames()
    idx <- match(tolower(col.names), tolower(colnames(dat)))
    names(idx) <- col.names
  }

  getColumnNames <- reactive({
    return(c("ljc", "sjc", "duration", "onset", "assessment", "apr", "crp",
             "esr", "serology", "ccp", "rf"))
  })

})
