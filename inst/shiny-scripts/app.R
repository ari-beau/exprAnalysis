library(shiny)

# Define the UI
ui <- fluidPage(

  # App title
  titlePanel("expressionAnalysis"),

  # Sidebar
  sidebarLayout(

    # Sidebar panel for file input
    sidebarPanel(

      # Description
      tags$p("This is a Shiny App for expressionAnalysis in R.
              It loads gene expression and sample data from files, shows the
              pairwise correlation between genes, ranks differentially expressed
              genes in cases and controls and produces boxplots to visualize
             this."),

      tags$hr(),

      # Example files description
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

      tags$hr(),

      tags$p("The expression file should be a matrix with the first row as sample names,
      the first column as gene names, and numeric expression values in the rest of the matrix.
      The sample file should have the first column as sample names, and the second column as their type.
      Please refer to the example datasets for the format."),

      # Input for expression file
      fileInput("file1", "Choose Expression CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input for sample file
      fileInput("file2", "Choose Sample CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input for file separator
      radioButtons("sep", "Separator",
                   choices = c(Comma = "comma",
                               Tab = "tab",
                               Semicolon = "semicolon"),
                   selected = "comma"),


      tags$hr(),

      # Input to specifying case and control identifiers
      tags$p("Please specify how the case and controls are identified in the
             sample data."),
      textInput("case", "Case name", value = "Ovarian cancer"),
      textInput("control", "Control name", value = "Normal")

    ),

    # Main panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  # Data
                  tabPanel("Data",
                           br(),
                           # Input: Select file to view----
                           splitLayout(radioButtons("viewing", "File to view",
                                                 choices = c(Expression = "expression",
                                                             Sample = "sample"),
                                                 selected = "expression"),
                                       radioButtons("norm", "Apply normalization ",
                                                    choices = c("Log2 transformation" = "log",
                                                                "Total count normalization" = "total",
                                                                "Z-score standardization" = "standard",
                                                                "None" = "none"),
                                                    selected = "none"),
                                        radioButtons("disp", "Display",
                                                     choices = c(Head = "head",
                                                                 All = "all"),
                                                     selected = "head")),
                           br(),

                           tableOutput("contents")),

                  # Correlation
                  tabPanel("Pairwise Correlation of Genes",
                           br(),
                           plotOutput("corr")),

                  # DEG
                  tabPanel("Differential Gene Expression",
                           br(),
                           radioButtons("method", "Method to calculate differential expression",
                                        choices = c("t-test" = "t",
                                                    "Wilcoxon rank sum test" = "wilcoxon"),
                                        selected = "t"),
                           tableOutput("deg")),

                  # DEG plot
                  tabPanel("Expression Plot",
                           br(),
                           tags$p("Type in names of genes to include into the plot.
                                  Each gene should be separated by a comma. If
                                  textbox is empty or if none of the genes given are in
                                  the dataset, all genes are included in the plot."),
                           textInput("include", "Genes", value = ""),
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

    if (input$norm == "none") {
      ; # no normalization
    } else {
      df$expressionData <- exprNormalization(df$expressionData, method = input$norm)
    }

    # saving data to variables
    expressionData$df_data <- df$expressionData
    sampleData$df_data <- df$sampleData

    if(input$viewing == "expression") {
      # expression
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


  output$deg <- renderTable({
      if (! (is.null(input$method))){
        df <- rankDEG(expressionData = expressionData$df_data,
                      sampleData = sampleData$df_data,
                      case = input$case,
                      control = input$control,
                      method = as.character(input$method))

      }

  }, rownames = TRUE)


  output$plot <- renderPlot({
    if (gsub(" ", "", input$include) == "") {
      genesIncluded <- NULL
    } else {
      genesIncluded <- gsub(" ", "", unlist(strsplit(input$include,",")))
    }

    exprPlot(expressionData = expressionData$df_data,
    sampleData = sampleData$df_data,
    genes = genesIncluded)
  })

  output$expression <- downloadHandler(
    # downloading example expression file
    filename = "OVExpression.csv",
    content = function(file) {
      download.file("https://raw.githubusercontent.com/ari-beau/expressionAnalysis/master/inst/extdata/OVExpression.csv", file)
    }
  )

  output$sample <- downloadHandler(
    # downloading example sample file
    filename = "OVSample.csv",
    content = function(file) {
      download.file("https://raw.githubusercontent.com/ari-beau/expressionAnalysis/master/inst/extdata/OVSample.csv", file)
    }
  )

  output$corr <- renderPlot({
    correlationPlot(expressionData = expressionData$df_data)
  })

}

# Create Shiny app ----
shiny::shinyApp(ui, server)



# [END]
