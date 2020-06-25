library(shiny)
library(shinythemes)
library(plotly)
# library(d3heatmap)
library(heatmaply)
# library(shinyHeatmaply)


# Define UI for random distribution app ----
# ui <- fluidPage(theme = shinytheme("slate"),
ui <- fluidPage(theme = shinytheme("cosmo"),

    # App title ----
    titlePanel("rCOMPASS"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
            helpText("Welcome to the COMPASS Shiny interface for VESPUCCI!"),

            # Input: Select the random distribution type ----
            selectInput("version", label = h4("Select Compendium version"),
                        choices = list("VESPUCCI - v 1.0 (legacy) vitis_vinifera legacy normalized" = 1,
                                       "VESPUCCI - v 2.0 (latest) vitis_vinifera tpm_sample normalized" = 2,
                                       "VESPUCCI - v 2.0 (latest) vitis_vinifera limma normalized" = 3),
                        selected = 1),

            # br() element to introduce extra vertical spacing ----


            # textInput:
            textAreaInput("tAI_bf", h4("Insert comma-separated gene ids"), "VIT_00s0246g00220,VIT_00s0332g00060,VIT_00s0332g00110,VIT_00s0332g00160,VIT_00s0396g00010,VIT_00s0505g00030,VIT_00s0505g00060,VIT_00s0873g00020,VIT_00s0904g00010"),
            hr(),
            submitButton()
            ),


        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        # tabPanel("Heatmap d3", d3heatmapOutput("d3heatmap")),
                        tabPanel("Heatmaply", plotlyOutput("heatmaply", height = "640px")),
                        tabPanel("Heatmap plotly", plotlyOutput("heatmap")),
                        # tabPanel("Summary", verbatimTextOutput("summary")),
                        # tabPanel("Network", plotOutput("network")),
                        tabPanel("Table", tableOutput("table")),
                        tabPanel("About",
                                 h5(p("Welcome to the COMPASS Shiny interface toVESPUCCI!")),
                                 h5(p("VESPUCCI is a database for exploring and analyzing a comprehensive Vitis vinifera specific cross-platform expression compendium. This compendium was carefully constructed by collecting, homogenizing and formally annotating publicly available microarray and RNA-seq experiments.")),
                                h5(p("COMPASS (COMpendia Programmatic Access Support Software) is a software layer that provides a GraphQL endpoint to query compendia built using COMMAND>_ technology.")),
                                h5(p("Shiny is an R framework for building web applications.")),
                                )

            )
        )
    )
)

server <- function(input, output) {

    # Reactive expression to generate the module
    my_module <- reactive({
    my_bf <- strsplit(input$tAI_bf, split=",")[[1]]
    mod <- create_module_bf(biofeaturesNames = my_bf, version = "legacy")
    })


    # output$d3heatmap <- renderD3heatmap({
    #     d3heatmap(my_module(), labRow = strsplit(input$tAI_bf, split=",")[[1]],
    #                          scale = "column")
    # })

    output$heatmaply <- renderPlotly({
        heatmaply(my_module(),
                  labRow = strsplit(input$tAI_bf, split=",")[[1]],
                  showticklabels = c(TRUE, TRUE),
                  fontsize_col = 7.5,
                  col = gplots::greenred(50),
                  plot_method = 'plotly')
    })

    output$heatmap <- renderPlotly({
        plot_ly(
            z = my_module(),
            colorscale = list(c(0,0.5,1),c("green", "white", "red")),
            type = "heatmap"
            )
    })

    # output$network <- renderPlot({
    # })

    # Generate an HTML table view of the data ----
    output$table <- renderTable({
       my_module()
    })

}

# Run the application
shinyApp(ui = ui, server = server)
