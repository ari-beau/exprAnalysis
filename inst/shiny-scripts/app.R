library(shiny)

# reading files from https://shiny.rstudio.com/gallery/file-upload.html

# tabs from https://shiny.rstudio.com/gallery/tabsets.html

# Define the UI
ui <- fluidPage(

  # App title ----
  titlePanel("expressionAnalysis"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("This is a Shiny App from expressionAnalysis in R.
              It loads gene expression and sample data from files, and can
              rank differentially expressed genes in cases and controls and
              produce boxplots to visualize this."),

      tags$hr(),


      tags$div("Below are example expression and sample data files. The datasets
      are subsets of data from an ovarian cancer gene expression profiling
      experiment ",
      tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/20040092/", "(Bowen N.J. et al., 2009)"),
      ". The data contains expression data
      for 10 genes in 24 samples (12 normal human ovaries and 12 ovarian cancer
      epithelial cells)."),

      br(),


      downloadButton("expression", "Download expression data"),
      downloadButton("sample", "Download sample data"),

      # Horizontal line ----
      tags$hr(),

      # Input: Select a file ----
      fileInput("file1", "Choose Expression CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Select a file ----
      fileInput("file2", "Choose Sample CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = "comma",
                               Tab = "tab",
                               Semicolon = "semicolon"),
                   selected = "comma"),

      # Horizontal line ----
      tags$hr(),

      tags$p("Please specify how the case and controls are identified in the
             sample data."),
      textInput("case", "Case name", value = "Ovarian cancer"),
      textInput("control", "Control name", value = "Normal")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           br(),
                           # Input: Select file to view----
                           splitLayout(radioButtons("viewing", "File to view",
                                                 choices = c(Expression = "expression",
                                                             Sample = "sample"),
                                                 selected = "expression",
                                                 inline = TRUE),
                                    radioButtons("disp", "Display",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "head",
                                                 inline = TRUE)
                           ),

                           tableOutput("contents")),

                  tabPanel("Differential Gene Expression",
                           br(),
                           radioButtons("method", "Method to calculate differential expression",
                                        choices = c("t-test" = "t",
                                                    "Wilcoxon rank sum test" = "wilcoxon"),
                                        selected = "t"),
                           tableOutput("deg")),

                  tabPanel("Plot",
                           br(),
                           plotOutput("plot")))


    )

  )
)

# Define the server
server <- function(input, output) {
  expressionData <- reactiveValues(df_data = NULL)
  sampleData <- reactiveValues(df_data = NULL)

  # for expression and sample data
  output$contents <- renderTable({
    # input$file1 and 2 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    req(input$file2)

    tryCatch(
      {
        df <- loadData(input$file1$datapath, input$file2$datapath, sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    expressionData$df_data <- df$expressionData
    sampleData$df_data <- df$sampleData

    if(input$viewing == "expression") {
      if(input$disp == "head") {
        return(head(df$expressionData))
      }
      else {
        return(df$expressionData)
      }

    } else {
      # sample
      if(input$disp == "head") {
        return(head(df$sampleData))
      }
      else {
        return(df$sampleData)
      }
    }

  }, rownames = TRUE)

  output$textCase <- renderText({input$case})

  output$textControl <- renderText({input$control})


  # for rankDEG (not working)
  # problem with case/control/method (not with dataframes)
  output$deg <- renderTable({

    #if (! (is.null(input$case) | is.null(input$control))){
      if (! (is.null(input$method))){
        df <- rankDEG(expressionData = expressionData$df_data,
                      sampleData = sampleData$df_data,
                      case = input$case,
                      control = input$control,
                      method = as.character(input$method))

        # return(df)
      }
#    }



    # req(input$case)
    # req(input$control)
    #
    # if (! (is.null(input$case) | is.null(input$control))){
    #   tryCatch(
    #     {
    #       df <- rankDEG(expressionData = expressionData$df_data,
    #                     sampleData = sampleData$df_data,
    #                     case = input$case,
    #                     control = input$control,
    #                     method = input$method)
    #     },
    #     error = function(e) {
    #       # return a safeError if a parsing error occurs
    #       stop(safeError(e))
    #     }
    #   )
    # }
    # return(df)

  }, rownames = TRUE)


  output$plot <- renderPlot({
    exprPlot(expressionData = expressionData$df_data,
    sampleData = sampleData$df_data)
  })

  output$expression <- downloadHandler(
    filename = "OVExpression.csv",
    content = function(file) {
      download.file("https://raw.githubusercontent.com/ari-beau/expressionAnalysis/master/inst/extdata/OVExpression.csv", file)
    }
  )

  output$sample <- downloadHandler(
    filename = "OVSample.csv",
    content = function(file) {
      download.file("https://raw.githubusercontent.com/ari-beau/expressionAnalysis/master/inst/extdata/OVSample.csv", file)
    }
  )

}

# Create Shiny app ----
shiny::shinyApp(ui, server)



# [END]
