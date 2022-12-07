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

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = "comma",
                               Tab = "tab",
                               Semicolon = "semicolon"),
                   selected = "comma"),

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data",
                           # Input: Select file to view----
                           radioButtons("viewing", "File to view",
                                        choices = c(Expression = "expression",
                                                    Sample = "sample"),
                                        selected = "expression"),

                           tableOutput("contents")),

                  tabPanel("Differential Gene Expression",
                           textInput("case", "Case name"),
                           textInput("control", "Control name"),
                           radioButtons("method", "Method to calculate differential expression",
                                        choices = c("t-test" = "t",
                                                    "Wilcoxon rank sum test" = "wilcoxon"),
                                        selected = "t"),
                           tableOutput("deg")),

                  tabPanel("Plot",
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

}

# Create Shiny app ----
shiny::shinyApp(ui, server)



# [END]
