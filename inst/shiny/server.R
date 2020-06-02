library(acreular)
library(DT)
library(mime)
library(readxl)
library(ggplot2)
library(ggiraph)
library(ggiraphExtra)
library(shinyWidgets)
library(FSA)
library(PMCMRplus)
library(parsedate)

options(shiny.sanitize.errors = FALSE)

addResourcePath('example-data', system.file("extdata", package="acreular"))

shinyServer(function(input, output) {

  defaults <- reactiveValues()
  defaults$acreular <- list(ljc="LJC", sjc="SJC", duration="Duration", onset="Onset", assessment="Assessment",
                            apr="APR", crp="CRP", esr="ESR", serology="Serology", ccp="CCP", rf="RF")

  defaults$eular <- list(das1="DAS_1", das2="DAS_2")
  defaults$acr <- list(sjc28="SJC28", tjc28="TJC28", ptGA="ptGA", ptPain="ptPain", phGA="phGA", haq="haq", apr="APR")#,
                       #sjc28="SJC28", tjc28="TJC28", ptGA="ptGA", ptPain="ptPain", phGA="phGA", haq="haq", apr="APR")

  output$choose_dataset <- renderUI({
    fileInput("data", "Choose data file",
              accept = c(
                mimemap["csv"],
                mimemap["xls"],
                mimemap["xlsx"])
    )
  })

  output$choose_calc_type <- renderUI({
    radioButtons("multi", "Multiple calculations:",
                 c("Single"="single", "Multiple"="multiple"),
                 selected="multiple",
                 inline=T)
  })

  output$choose_condition <- renderUI({
    selectInput("condition", "Condition:",
                c("Rheumatoid arthritis"="ra")
    )
  })

  output$choose_criteria <- renderUI({
    if(is.null(input$condition))
      return()

    if(input$condition=="ra") {
      calculations <- c("ACR/EULAR (2010) RA" = "acreular",
                        "EULAR response" = "eular")#,
                        #"ACR 20/50/70" = "acr")
    }

    checkboxGroupInput("calculations", "Calculations:", calculations)

  })

  output$include_raw_data <- renderUI({
    checkboxInput("raw", "Include all submitted data in table", TRUE)
  })

  getPaired <- reactive({
    data <- getTableDataByGroup()
    group.totals <- table(data[input$group])

    if(ncol(data) > 0) {
      id.columns <- lapply(data, function(x){
        id.totals <- table(x)
        length(unique(id.totals))==1 & length(unique(group.totals))==1 & sum(id.totals)==sum(group.totals)
      })
      return(names(id.columns)[which(unlist(id.columns))])
    }
  })

  output$show_paired <- renderUI({
    if(!is.null(input$group) && input$group != "None") {
      if(length(getPaired())==0)
        return()

      data <- getTableData()
      id.groups <- getPaired()
      if(!is.null(id.groups)) {
        tagList(
          checkboxInput("paired", "Data are paired", TRUE),
          selectInput("id", "ID column:",
                      choices=id.groups, selected=FALSE, selectize = FALSE)
        )
      }
    }
  })

  output$show_average <- renderUI({
    checkboxInput("average", "Show mean/median on plot", TRUE)
  })

  output$choose_average_method <- renderUI({
    radioButtons("average_method", "Show:",
                 c("Mean"="mean", "Median"="median"),
                 selected="mean",
                 inline=T)
  })

  output$acreular_table <- DT::renderDataTable({
    if(is.null(input$data))
      return()

    res <- getTableData()
    dt <- datatable(res, options=list(columnDefs = list(list(visible=FALSE, targets=which(!defaults$colnames %in% input$display_columns))), scrollX = TRUE))
    return(dt)
  })

  output$export_table <- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download_table", 'Download Output File')
    }
  })

  output$download_table <- downloadHandler(
    filename = function() {
      paste(input$condition, "_table_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(getTableData(), file, row.names = FALSE)
    }
  )

  output$export_plot <- renderUI({
    if(!is.null(input$data)) {
      downloadButton("download_plot", 'Download plot')
    }
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$condition, "_", input$plot_type, "_", format(Sys.time(), "%Y%m%d%H%M%S"), ".pdf", sep = "")
    },
    content = function(file) {
      ggsave(file, getPlot(),"pdf")
    }
  )

  rawdata <- reactive({
    if(input$data$type %in% c(mimemap["xls"], mimemap["xlsx"])) {
      dat <- read_excel(input$data$datapath, na=c("NA",""))
      dat <- as.data.frame(dat)
    }
    else {
      dat <- read.csv(file=input$data$datapath, header=TRUE, stringsAsFactors=FALSE, na.strings=c("NA",""))
    }

    defaults$colnames <- colnames(dat)
    return(dat)
  })

  dataset <- reactive({
    dat <- rawdata()
    return(dat)
  })

  getTableData <- reactive({
    dat <- dataset()

    dat.copy <- dat
    names(dat.copy) <- tolower(names(dat.copy))

    if("onset" %in% names(dat.copy))
      dat.copy$onset <- as.Date(parse_date(dat.copy$onset))

    if("assessment" %in% names(dat.copy))
      dat.copy$assessment <- as.Date(parse_date(dat.copy$assessment))

    dat.list = purrr::pmap(as.list(dat.copy), list)

    if(input$condition=="ra") {
      if("acreular" %in% input$calculations) {
        classification <- lapply(dat.list, function(x) {
          obj <- acrEularRA(ljc=x$ljc, sjc=x$sjc, duration=x$duration, onset=x$onset,
                            assessment=x$assessment, apr=x$apr, crp=x$crp, esr=x$esr,
                            serology=x$serology, ccp=x$ccp, rf=x$rf) ##add in uln

          acrEularRAClassification(obj)
        })
        classification <- as.character(unlist(classification))
        dat$ACREULAR <- classification
      }

      if("eular" %in% input$calculations) {
        dat$EULARResp <- eularResponse(dat.copy$das1, dat.copy$das2)
      }

      if("acr" %in% input$calculations) {
        acr.response <- lapply(dat.list, function(x) {
          acr1 <- acrRA(tjc=x$tjc28_1, sjc=x$sjc28_1, ptGA=x$ptga_1, ptPain=x$ptpain_1, phGA=x$phga_1, haq=x$haq_1, apr=x$apr_1)
          acr2 <- acrRA(tjc=x$tjc28_2, sjc=x$sjc28_2, ptGA=x$ptga_2, ptPain=x$ptpain_2, phGA=x$phga_2, haq=x$haq_2, apr=x$apr_2)
          acrResponse(acr1, acr2)
        })
        dat$ACRResponse <- as.character(unlist(acr.response))
      }
    }
    dat
  })

  getTableDataByGroup <- reactive({
    data <- getTableData()
    data <- data[!is.na(data[[input$plot_data]]),]

    if(!is.null(input$group) && input$group != "None") {
      data <- data[which(data[,input$group] %in% input$group_member),]
    }

    return(data)
  })

  getTableDisplayData <- reactive({
    data <- getTableData()
    data <- data[,colnames(data) %in% input$display_columns]
  })

  output$choose_display_columns <- renderUI({

    if(is.null(input$data))
      return()

    pickerInput(
      inputId = "display_columns",
      label = "Select/deselect display columns",
      choices = defaults$colnames,
      selected =defaults$colnames,
      options = list(
        `actions-box` = FALSE,
        `none-selected-text` = "Please select at least one."),
      multiple = TRUE
    )
  })

  ignoreIncomplete <- reactive({
    ignore.incomplete <- ifelse(is.null(input$ignore_incomplete), TRUE, input$ignore_incomplete)
    return(ignore.incomplete)
  })

  output$plot <- renderggiraph({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    if(input$group!="None" && is.null(input$group_member))
      return()

    code <- getPlot()

    output <- ggiraph(code = print(code), selection_type = "single")

    return(output)
  })

  getPlot <- reactive({
    if(is.null(input$group)) {
      return()
    }

    if(input$plot_type=="density") {
      print("Density")
      code <- density_plot()
    } else if(input$plot_type=="ecdf") {
      print("ECDF")
      code <- ecdf_plot()
    } else if(input$plot_type=="radar") {
      print("Radar")
      code <- radar_plot()
    }
    else {
      stop("Unable to identify plot type")
    }
    return(code)
  })

  density_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    data <- getTableDataByGroup()

    if(nrow(data) > 0) {
      ave.meth <- get_average_method()

      if(input$group=="None"){ # || !input$raw) {
        p <- ggplot(data, aes_string(x=input$plot_data)) +
             geom_density(color="darkblue", fill="lightblue", alpha=0.4)

        if(input$average) {
          p <- p + geom_vline_interactive(aes_string(xintercept=ave.meth(data[[input$plot_data]])),
              color="darkblue", linetype="dashed", tooltip = paste0(input$average_method, ": ", get_average_value()), data_id = "density_mean")
        }

      } else {
        colours <- getGroupColours()
        mu <- get_average_value()
        p <- ggplot(data, aes_string(x=input$plot_data, fill=input$group)) +
             geom_density(alpha=0.4) + scale_fill_manual(values=colours) + scale_color_manual(values=colours)

        if(input$average) {
          p <- p + geom_vline_interactive(data=mu, aes_string(xintercept="x", color="group"),
               linetype="dashed", show.legend=FALSE, tooltip = paste0(input$average_method, ": ", mu$x), data_id = paste0("density_", input$average_method, "_", mu$group))
        }
      }

      return(p)
    }
  })

  ecdf_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$group))
      return()

    data <- getTableDataByGroup()

    ave.meth <- get_average_method()

    if(input$group=="None") {# || !input$raw) {

      p <- ggplot(data, aes_string(input$plot_data)) + stat_ecdf(geom = "step", colour="darkblue")

      if(input$average) {
        p <- p + geom_vline_interactive(aes_string(xintercept=ave.meth(data[[input$plot_data]])),
            color="darkblue", linetype="dashed", tooltip = paste0(input$average_method, ": ", get_average_value()), data_id = "ecdf_mean")
      }

    } else {
      colours <- getGroupColours()
      p <- ggplot(data, aes_string(input$plot_data, colour = input$group)) +
        stat_ecdf(geom = "step") + scale_color_manual(values=colours)
      mu <- get_average_value()

      if(input$average) {
        p <- p + geom_vline_interactive(data=mu, aes_string(xintercept="x", color="group"),
             linetype="dashed", show.legend=FALSE, tooltip = paste0(input$average_method, ": ", mu$x), data_id = paste0("ecdf_", input$average_method, "_", mu$group))
      }
    }

    p <- p + labs(y='Cumulative probability')

    return(p)

  })

  radar_plot <- reactive({
    if(is.null(input$data) || is.null(input$plot_type) || is.null(input$radar_data) || is.null(input$group))
      return()

    data <- getTableDataByGroup()
    print(input$radar_data)
    if(input$group=="None") {

      print(input$radar_data)
      data <- data[,names(data) %in% input$radar_data]
      print(colnames(data))
      p <- ggRadar(data=data, rescale=FALSE, colour = "#F8766D", alpha = 0.4)
    } else {
      colours <- getGroupColours()
      data <- data[,names(data) %in% c(input$radar_data, input$group)]
      p <- ggRadar(data=data,aes_string(color=input$group), rescale=FALSE) +
        scale_fill_manual(values=colours) + scale_color_manual(values=colours) +
        theme(legend.position="right")
    }
    return(p)
  })

  output$choose_plot_data <- renderUI({
    if(is.null(dataset()))
      return()

    dat <- dataset()
    dat <- dat[sapply(dat, function(x) is.numeric(x))]
    selectInput("plot_data", "Plot data:",
                colnames(dat)
    )
  })

  output$choose_plot_type <- renderUI({
    selectInput("plot_type", "Plot type:",
        c("Density"="density", "ECDF"="ecdf", "Radar"="radar")
    )
  })

  output$choose_radar_data <- renderUI({
    print(is.null(input$data))
    print(input$plot_type)
    if(is.null(input$data) || input$plot_type!="radar") {
      return()
    }
    data <- getTableData()
    data <- data[sapply(data, function(x) is.numeric(x))]

    pickerInput(
      inputId = "radar_data",
      label = "Select/deselect radar data",
      choices = colnames(data),
      selected = colnames(data),
      options = list(
        `actions-box` = FALSE,
        `none-selected-text` = "Please select at least one."),
      multiple = TRUE
    )
  })

  output$choose_group_by <- renderUI({
    if(is.null(input$data)) {
      return()
    }
    data <- getTableData()
    data <- data[sapply(data, function(x) is.character(x) || is.logical(x) || is.factor(x))]
    data <- data[!sapply(data, function(x) is.Date(x))]
    groups <- "None"
    if(ncol(data) > 0) {
      include <- apply(data, 2, function(x) {length(unique(x))!=length(x)})
      groups <- c(groups, names(which(include)))
    }
    selectInput("group", "Group by:",
                groups
    )
  })

  output$choose_group_members <- renderUI({

    if(is.null(input$group) || input$group=="None") {
      return()
    }
    data <- getTableData()

    pickerInput(
      inputId = "group_member",
      label = "Select/deselect group members",
      choices = na.omit(unique(data[[input$group]])),
      selected =na.omit(unique(data[[input$group]])),
      options = list(
        `actions-box` = FALSE,
        `none-selected-text` = "Please select at least one."),
      multiple = TRUE
    )
  })

  getStatistics <- reactive({
    if(is.null(input$group) || input$group=="None") {
      return()
    }

    data <- getTableDataByGroup()
    stats <- NULL
    if(length(input$group_member)==2) {
      stats <- getWilcoxStats()
    } else if (length(input$group_member) > 2) {
      if(!is.null(getPaired()) & length(getPaired()) > 0 & !is.null(input$paired) && input$paired) {
        print("Friedman")
        stats <- getFriedmanStats()
      } else {
        print("Kruskal")
        stats <- getKruskalStats()
      }
    }
    return(stats)
  })

  output$statistics <- renderUI({

    stats <- getStatistics()
    if(is.null(stats))
      return("Select a group to perform statistical tests.")

    taglist <- tagList(
      h5(stats$method),
      p(paste("Data", stats$data.name)),
      p(paste0(names(stats$statistic), " = ", round(stats$statistic,1))),
      p("p.value = ", round(stats$p.value,5))
    )
    if(length(input$group_member) > 2 & stats$p.value < 0.05) {
      taglist[[length(taglist)+1]] <- actionButton("posthoc","View post hoc tests")
    }
    return(taglist)
  })

  output$posthocTable <- renderDataTable({
    stats <- getStatistics()
    table <- data.frame(lapply(stats$posthoc$res, function(y) if(is.numeric(y)) round(y, 5) else y))
    datatable(table)
  })

  observeEvent(input$posthoc,{
    stats <- getStatistics()
    showModal(
      modalDialog(
        h2("Post hoc tests"),
        p(stats$posthoc$method),
        DT::dataTableOutput('posthocTable'),
        uiOutput("export_posthoc"),
        size = "m"
      )
    )
  })

  getWilcoxStats <- reactive({
    data <- getTableDataByGroup()
    paired <- FALSE
    if(!is.null(getPaired()) & length(getPaired()) > 0 & !is.null(input$paired) && input$paired) {
      data <- data[order(data[input$id], data[input$group]),]
      paired <- TRUE
    }
    res <- wilcox.test(as.formula(paste(input$plot_data," ~ ", input$group)), data, paired=paired)
    return(res)
  })

  getKruskalStats <- reactive({
    data <- getTableDataByGroup()
    res <- kruskal.test(as.formula(paste(input$plot_data," ~ ", input$group)), data)

    if(res$p.value < 0.05) {
      res$posthoc <- dunnTest(as.formula(paste(input$plot_data," ~ ", input$group)), data)
      res$posthoc$method <- paste("Dunn's test with", res$posthoc$method, "correction")
    }
    return(res)
  })

  getFriedmanStats <- reactive({
    data <- getTableDataByGroup()
    res <- friedman.test(as.formula(paste(input$plot_data, " ~ ", input$group, " | ", input$id)),
                  data = data)
    if(res$p.value < 0.05) {
      nt <- frdAllPairsNemenyiTest(as.formula(paste(input$plot_data, " ~ ", input$group, " | ", input$id)), data)
      nt.stats <- na.omit(as.data.frame(as.table(nt$statistic)))
      nt.p.value <- na.omit(as.data.frame(as.table(nt$p.value)))
      nt$res <- data.frame(Comparison=paste(nt.stats$Var2, "-", nt.stats$Var1), "Mean rank diff"=nt.stats$Freq, P.adj=nt.p.value$Freq)
      nt$method <- paste("Nemenyi test with", nt$p.adjust.method, "correction")
      res$posthoc <- nt
    }
    return(res)
  })

  output$export_posthoc <- renderUI({
      downloadButton("download_posthoc", 'Download Post Hoc Data')
  })

  output$download_posthoc <- downloadHandler(
    filename = function() {
      paste(input$condition, "_post_hoc_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      stats <- getStatistics()
      write.csv(stats$posthoc$res, file, row.names = FALSE)
    }
  )

  output$ignore_incomplete <- renderUI({
    checkboxInput("ignore_incomplete", "Ignore data with incomplete/missing dimension scores", TRUE)
  })

  output$stats_tests <- renderTable({

      data.frame(Groups=c(2, 2, ">2", ">2"),
                 Paired=c("No", "Yes", "No", "Yes"),
                 Test=c("Wilcoxon rank sum test", "Wilcoxon signed rank test",
                        "Kruskal-Wallis rank sum test with Dunn's test for post hoc testing.", "Friedman's rank sum test with the Nemenyi test for post hoc testing."))

  })

  get_average_method <- reactive({
    if(input$average_method=="mean") {
      return(mean)
    } else {
      return(median)
    }
  })

  get_average_value <- reactive({

    data <- getTableDataByGroup()

    if(nrow(data)==0)
      return()

    ave.meth <- get_average_method()

    if(input$group == "None") {
      mu <- aggregate(as.formula(paste(input$plot_data, "~ 1")), data, function(x){round(ave.meth(x),3)})
    } else {
      mu <- aggregate(data[[input$plot_data]], list(group=data[[input$group]]), function(x){round(ave.meth(x),3)})
    }
    return(mu)
  })

  ggplotColours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }

  getGroupColours <- reactive({
    data <- getTableData()
    groups <- unique(data[[input$group]])
    colours <- ggplotColours(length(groups))
    names(colours) <- groups
    return(colours)
  })

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
})
