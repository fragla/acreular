library(shinycssloaders)

shinyUI(
  navbarPage(
    title="acreular",
    header=singleton(
      tags$head(tags$style(type="text/css", "text {font-family: sans-serif}"))
    ),
    tabPanel("Data", tags$style(HTML("
              .col-sm-4, .col-sm-8 {margin-top:5px;}")),
      sidebarPanel(
        uiOutput("choose_calc_type"),
        conditionalPanel(
          condition = "input.multi == 'multiple'",
          uiOutput("choose_dataset"),
          hr()
        )
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.multi == 'multiple'",
          withSpinner(DT::dataTableOutput("ra_table")),
          uiOutput("export_table")
        ),
        conditionalPanel(
          condition = "input.multi == 'single'",
          uiOutput("choose_dimensions"),
          textOutput("ra_text")
        )
      )
    ),
    tabPanel("Settings",
      h3("Rheumatoid arthritis")
    ),
    tabPanel("Help/FAQs",
             div(style="padding-left:20%;padding-right:20%;",
                 "Foo"
             )
    )
  )
)
