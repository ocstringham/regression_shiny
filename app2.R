library(shiny)
library(shinyjs)
library(sortable)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("App"),
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fileInput(inputId = "file1", label = "Select a .csv file", 
                accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
      ),
      uiOutput("show_button")
    ),
    
    mainPanel(
      DT::dataTableOutput("table")
    )
  ),
  
  fluidRow(uiOutput("buckets"))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # input csv file
  input_file <- reactive({
    if (is.null(input$file1)) {
      return("")
    }
    
    # actually read the file
    read.csv(file = input$file1$datapath)
  })
  
  
  
  # button to hide/show table
  
  ## only show when table is loaded
  output$show_button = renderUI({
    req(input$file1)
    actionButton(inputId = "button", label = "show / hide table")
  })
  
  ## observe the button being pressed
  observeEvent(input$button, {
    shinyjs::toggle("table")
  })
  
  
  
  # output table
  output$table <- DT::renderDataTable({
    
    # render only if there is data available
    req(input_file())
    
    # reactives are only callable inside an reactive context like render
    data <- input_file()
    data
    
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
              name = "list_group1",
              put = FALSE))
        ),
        
        add_rank_list(
          text = "to here",
          labels = NULL,
          input_id = "rank_list_2",
          css_id = "list2", 
          options = sortable_options(group = list(name = "list_group1")))
      )
      
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
