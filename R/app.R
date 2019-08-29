library(shiny)

u <- shinyUI(pageWithSidebar(

  headerPanel("Quick search"),
  sidebarPanel(
  #   textInput('vec1', 'Enter a vector (comma delimited)', "B9S8R7,Q7M2G6,D7SZ93,B8XIJ8,Vv00s0125g00280,Vv00s0187g00140,Vv00s0246g00010,Vv00s0246g00080,Vv00s0438g00020,Vv00s0246g00200,VIT_00s0246g00220,VIT_00s0332g00060,VIT_00s0332g00110,VIT_00s0332g00160,VIT_00s0396g00010,VIT_00s0505g00030,VIT_00s0505g00060,VIT_00s0873g00020,VIT_00s0904g00010")
  # ),
  textInput('term', 'Enter a single term', 'GO:0006260')),

  mainPanel(
    verbatimTextOutput("oid1")
    # verbatimTextOutput("oid2")
  )
))

s <- shinyServer(function(input, output) {

  output$oid1 <- renderPrint({
    cat("Biofeatures:\n")
    # cat(input$term)
    tmp <- getBiofeatureByAnnotationTerm(term=input$term)
    cat(tmp)
  }
  )

  # output$oid2<-renderPrint({
  #   x <- as.numeric(unlist(strsplit(input$vec1,",")))
  #   cat("As atomic vector:\n")
  #   print(x)
  # }
  # )
}
)
shinyApp(ui = u, server = s)
