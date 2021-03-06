library(shiny)
library(shinythemes)
library(plotly)
library(heatmaply)
library(DT)

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
            selectInput("version",
                        label = h4("Select Compendium version"),
                        choices = list("VESPUCCI - v 1.0 (legacy) vitis_vinifera legacy normalized" = 1,
                                       "VESPUCCI - v 2.0 (latest) vitis_vinifera tpm_sample normalized" = 2,
                                       "VESPUCCI - v 2.0 (latest) vitis_vinifera limma normalized" = 3),
                        selected = 1),

            # br() element to introduce extra vertical spacing ----


            # textInput:
            textAreaInput("tAI_bf", h4("Insert comma-separated gene ids"), "VIT_00s0246g00220,VIT_00s0332g00060,VIT_00s0332g00110,VIT_00s0332g00160,VIT_00s0396g00010,VIT_00s0505g00030,VIT_00s0505g00060,VIT_00s0873g00020,VIT_00s0904g00010", height = 160),
            hr(),
            submitButton()
            ),


        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Heatmaply", plotlyOutput("heatmaply", height = "640px")),
                        tabPanel("pheatmap", plotOutput("pheatmap", height = "640px")),
                        # tabPanel("Heatmap plotly", plotlyOutput("heatmap")),
                        # tabPanel("Summary", verbatimTextOutput("summary")),
                        # tabPanel("Network", plotOutput("network")),
                        # tabPanel("Table", tableOutput("table")),
                        tabPanel("DT",DTOutput("tbl")),
                        tabPanel("About",
                                 htmlOutput("header"),
                                 htmlOutput("stats01"),
                                 tableOutput("table"),
                                 htmlOutput("stats02"))
            )
        )
    )
)

server <- function(input, output) {

    # Reactive expression to generate the module
    my_module <- reactive({
    my_bf <- strsplit(input$tAI_bf, split=",")[[1]]
    if(input$version == 1) version = "legacy"
    mod <- create_module_bf(biofeaturesNames = my_bf, version = version)
    #ids <- get_ids_from_alias(target = "biofeature", alias_names = my_bf)
    #mod <- create_module_bf(biofeaturesNames = ids, version = version, useIds = TRUE)
    })

    output$heatmaply <- renderPlotly({
        heatmaply(my_module(),
                  labRow = strsplit(input$tAI_bf, split=",")[[1]],
                  showticklabels = c(TRUE, TRUE),
                  fontsize_col = 7.5,
                  col = gplots::greenred(50),
                  plot_method = 'plotly')
    })


    output$pheatmap <- renderPlot({
        pheatmap::pheatmap(mat =  as.matrix(my_module()))
    })

    # output$heatmap <- renderPlotly({
    #     plot_ly(
    #         z = my_module(),
    #         colorscale = list(c(0,0.5,1),c("green", "white", "red")),
    #         type = "heatmap"
    #         )
    # })

    # output$network <- renderPlot({
    # })

    output$header <- renderText({
        HTML(paste("<h4>","Overview","</h4>",
        "VESPUCCI is a database for exploring and analyzing a comprehensive Vitis vinifera
        specific cross-platform expression compendium. This compendium was carefully
        constructed by collecting, homogenizing and formally annotating publicly available
        microarray and RNA-seq experiments. COMPASS (COMpendia Programmatic Access Support
        Software) is a software layer that provides a GraphQL endpoint to query compendia
        built using COMMAND>_ technology. We are using ", tags$a(href="https://onertipaday.github.io/rcompass/", "rCOMPASS")," version", utils::packageVersion("rcompass"), "as a R wrapper to COMPASS and shiny version", utils::packageVersion("shiny"), "for the web app interface."))
    })

    output$stats01 <- renderText({
        paste("<h4>","Compendium Stats","</h4>",
              "The Vitis gene expression compendium version", paste(get_available_compendia()$versions[[1]], collapse = " "),"normalized contains values for", totalCounts(version="1.0", aggregate_type="biofeatures"), " biological features, measured for", totalCounts(version="1.0", aggregate_type="sampleSets"), " sample sets. This corresponds to a total of", totalCounts(aggregate_type = "experiments"), " experiments and", totalCounts(aggregate_type = "samples"), " samples measured on", totalCounts(aggregate_type = "platforms")," different platforms:")
    })

    output$table <- renderTable({
        get_platform_information() %>% dplyr::count(source, type)
    })

    output$stats02 <- renderText({
        paste0("For annotations, VESPUCCI relies on the following Ontologies: ", paste(get_ontologies()$name, collapse = ", "),".")
    })

    output$tbl = renderDT(
        my_module(),
        extensions = c('Buttons','FixedColumns'),
        options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel'),
            dom = 't',
            scrollX = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE
        )
    )
}

# Run the application
shinyApp(ui = ui, server = server)
