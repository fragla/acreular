library(ggiraph)
library(shinycssloaders)

shinyUI(
  navbarPage(title="acreular",
             header=singleton(tags$head(
               tags$style(type="text/css", "text {font-family: sans-serif}")
               )),

    tabPanel("Data", tags$style(HTML("
              .col-sm-4, .col-sm-8 {margin-top:5px;}")),
      sidebarPanel(
        uiOutput("choose_calc_type"),
        conditionalPanel(
          condition = "input.multi == 'multiple'",
          uiOutput("choose_dataset"),
          hr(),
          uiOutput("choose_condition"),
          uiOutput("choose_display_columns"),
          uiOutput("choose_criteria")
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.multi == 'multiple'",
          withSpinner(DT::dataTableOutput("acreular_table")),
          uiOutput("export_table")
        ),
        conditionalPanel(
          condition = "input.multi == 'single'",
          uiOutput("choose_dimensions"),
          textOutput("acreular_text")
        )
      )
    ),
    tabPanel("Analysis",
      sidebarPanel(
        uiOutput("choose_plot_data"),
        uiOutput("choose_plot_type"),
        conditionalPanel(
          condition = "input.plot_type == 'radar'",
          uiOutput("choose_radar_data")
        ),
        uiOutput("choose_group_by"),
        conditionalPanel(
          condition = "input.group != 'None'",
          uiOutput("choose_group_members")
        ),
        uiOutput("show_average"),
        uiOutput("choose_average_method")
      ),
      mainPanel(
        column(8,
            withSpinner(ggiraphOutput("plot")),
            uiOutput("export_plot")
        ),
        column(4,
            h4("Statistical analysis"),
            uiOutput("statistics")
        )
      )
    ),
    tabPanel("Settings",
             div(style="padding-left:20%;padding-right:20%;",
                 h3("Error handling"),
                 uiOutput("ignore_incomplete")
             )
    ),
    tabPanel("Help/FAQs",


    )
  )
)
