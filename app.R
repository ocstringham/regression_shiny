library(shiny)
library(shinyjs)
library(shinythemes)
library(sortable)
library(ggplot2)

example_text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

# Define UI for application
ui <- fluidPage( 
  
  theme = shinytheme("simplex"),
  
  # Application title
  titlePanel("Linear Regression"),

  # File Input
  fluidRow(
        useShinyjs(),
        column(12, align="left", p(example_text)),
        column(12, align="left", h3("1. Upload data file"), p(example_text)),
        column(12, align="center",
          fileInput(inputId = "file1", label = "Select a .csv file", 
                    accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
                   )
        )),
    
  # Table
  fluidRow(column(12, align = "left", h3(uiOutput("tableTitle")))),
  fluidRow(column(12, align = "left", p(uiOutput("tableText")))),
  fluidRow(column(10, align='center', offset = 1, DT::dataTableOutput("table"))),

  # Select response variable
  fluidRow(column(12, align = "left", h3(uiOutput("response_var_title")))),
  fluidRow(column(12, align = "left", p(uiOutput("response_var_text")))),
  fluidRow(column(12, align = "left", uiOutput("response_var"))),
  
  
  # Viz data
  fluidRow(column(12, align = "left", h3(uiOutput("viz_title")))),
  fluidRow(column(12, align = "left", p(uiOutput("viz_text")))),
  fluidRow(column(12, align = "center", uiOutput("plot_button"))),
  fluidRow(column(12, align = "left", plotOutput("viz_plots"))),
  
    
  # # Buckets
  # fluidRow(column(12, align = "center", uiOutput("buckets")))
    
)

#----------------------------------------------------------------------------- #
 
# Define server logic required 
server <- function(input, output) {
  
  # input csv file
  input_file <- reactive({
    if (is.null(input$file1)) {
      return("")
    }
    
    # actually read the file
    read.csv(file = input$file1$datapath)
  })
  
  
  
  # output table
  
  ## text
  output$tableTitle = renderText({
    req(input_file())
    "2. View data"})
  
  output$tableText = renderText({
    req(input_file())
    example_text})
  
  ## button to hide/show table
  
  # ## only show when table is loaded
  # output$show_button = renderUI({
  #   req(input$file1)
  #   actionButton(inputId = "button", label = "show / hide table")
  # })
  # 
  # 
  # ## observe the button being pressed
  # observeEvent(input$button, {
  #   shinyjs::toggle("table")
  # })
  
  
  ## table
  output$table <- DT::renderDataTable({
    
    # render only if there is data available
    req(input_file())
    
    # reactives are only callable inside an reactive context like render
    data <- input_file()
    data
  })
  
  
  
  # Select response variable, only show once user clicks I'm ready
  output$response_var_title = renderText({
    req(input$file1)
    "3. Select independent variable"})
  
  output$response_var_text = renderText({
    req(input$file1)
    example_text})
  
  output$response_var = renderUI({
    req(input$file1)
    cols = colnames(input_file())
    radioButtons(inputId = "response_var", label = "Select an independent variable.",
             choices = cols, selected = character(0))
  }
  )
  
  
  # Viz data
  
  ## text
  output$viz_title = renderText({
    req(input$file1)
    "4. Vizualize data"})
  
  output$viz_text = renderText({
    req(input$file1)
    example_text})

  
  ### get response var
  getRV <- reactive({ input$response_var})
  
  # ### observe the button being pressed
  # 
  # output$plot_button = renderUI({
  #   req(input$file1)
  #   actionButton(inputId = "plotButton", label = "Plot Data")
  # })
  # 
  # 
  # observeEvent(input$plot_button, {
  #   shinyjs::toggle("viz")
  # })
  
  # output$viz = renderPrint(get)
  
  output$viz_plots = renderPlot({

    ## get data
    data <- input_file()

    ## get response variable
    response = getRV()

    ## plot random column
    ggplot(data, aes(y = response, x = names(data)[2])) +
      geom_point()

  })
  
  
  # Drag and Drop Col names
  output$buckets = renderUI( 
      {
        # create list of colnames
        req(input$file1)
        data = input_file()
        cols = colnames(data)
        
        
        # create bucket list
        bucket_list(
          header = "Drag the items in any desired bucket",
          group_name = "bucket_list_group",
          orientation = "horizontal",


          add_rank_list(
            text = "Drag from here",
            labels = as.list(cols),
            input_id = "rank_list_1",
            css_id = "list1", 
            options = sortable_options(
              group = list(
                pull = "clone",
                put = FALSE))
            ),

          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = "rank_list_2",
            css_id = "list2")
      )

    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
