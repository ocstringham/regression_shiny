library(shiny)
library(shinyjs)
library(shinythemes)
library(sortable)
library(ggplot2)
library(egg)

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
  # fluidRow(column(12, align = "center", uiOutput("plot_button"))),
  fluidRow(column(12, align = "center", uiOutput("expl_var"))),
  fluidRow(column(12, align = "left", plotOutput("viz_plots"))),
  
  
  # Build models
  fluidRow(column(12, align = "left", h3(uiOutput("model_title")))),
  fluidRow(column(12, align = "left", p(uiOutput("model_text")))),
  fluidRow(column(12, align = "left", uiOutput("buckets1"))),
  fluidRow(column(12, align = "left", uiOutput("buckets2"))),
  # fluidRow(column(4, align = "center", uiOutput("buckets3")))
  
  
  # View Output
  fluidRow(column(12, align = "left", h3(uiOutput("output_title")))),
  fluidRow(column(12, align = "left", p(uiOutput("output_text")))),
  fluidRow(column(12, align = "left", p(uiOutput("model_output"))))
  
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
  

  
  ## table
  output$table <- DT::renderDataTable({
    
    # render only if there is data available
    req(input_file())
    
    # reactives are only callable inside an reactive context like render
    data <- input_file()
    data
  })
  
  
  
  # Select response variable
  output$response_var_title = renderText({
    req(input_file())
    "3. Select independent variable"})
  
  output$response_var_text = renderText({
    req(input_file())
    example_text})
  
  output$response_var = renderUI({
    req(input_file())
    cols = colnames(input_file())
    radioButtons(inputId = "response_var", label = "Select an independent variable.",
             choices = cols, selected = character(0))
  }
  )
  
  
  # Viz data
  
  ## text
  output$viz_title = renderText({
    req(input_file())
    "4. Vizualize data"})
  
  output$viz_text = renderText({
    req(input_file())
    example_text})
  
  ## radio for expl var
  output$expl_var = renderUI({
    req(input_file())
    req(getRV())
    response = getRV()
    data <- input_file()
    expl = names(data)[! names(data) %in% response]
    radioButtons(inputId = "expl_var", label = "Select an dependent variable to plot.",
                 choices = expl, selected = character(0),  inline=T)
  }
  )

  
  ### get response var
  getRV <- reactive({ input$response_var})
  getEV <- reactive({input$expl_var})
  
  
  ## create plot
  output$viz_plots = renderPlot({
    
    ## reqs
    req(input_file())
    req(getRV())
    
    ## get data
    data <- input_file()

    ## get response/expl variables
    response = getRV()
    expl = getEV()
    

    ## plot
    
    ### if response is numeric
    if(is.numeric(data[, response])){
      
      if(is.numeric(data[, expl])){
        ggplot(data, aes_string(y = response, x = expl)) +
          geom_point() +
          theme_presentation()
      }else if(is.character(data[, expl])){
        ggplot(data, aes_string(y = response, x = expl)) +
          geom_boxplot() + 
          theme_presentation()
      }
      
    # if response is character
    }else if(is.character(data[, response])){
    
      if(is.numeric(data[, expl])){
        ggplot(data, aes_string(y = response, x = expl)) +
          geom_boxplot() + 
          theme_presentation()
      }else if(is.character(data[, expl])){
        ggplot(data, aes_string(group = response, fill = response, x = expl)) +
          geom_bar(position = position_dodge2(preserve = "single")) + 
          scale_fill_brewer(palette = 1, type = 'qual') +
          theme_presentation()
      }
    }

  })
  
  
  # Build and run models
  
  ## Text
  output$model_title = renderText({
    req(input_file())
    "5. Build models"})
  
  output$model_text = renderText({
    req(input_file())
    example_text})
    

  
  
  ## Drag and Drop Col names
  output$buckets1 = renderUI( 
      {
        # create list of colnames
        req(input_file())
        req(getRV())
        
        response = getRV()
        data <- input_file()
        cols = names(data)[! names(data) %in% response]
        
        # create bucket list
        h4("Model 1")
        bucket_list(
          header = "Drag the items in any desired bucket",
          group_name = "bucket_list1",
          orientation = "horizontal",


          add_rank_list(
            text = "Drag from here",
            labels = as.list(cols),
            input_id = "b1_rank_list_1",
            css_id = "b1_list1"),

          add_rank_list(
            text = "to here",
            labels = NULL,
            input_id = "b1_rank_list_2",
            css_id = "b1_list2")
      )

    })
  
  
  output$buckets2 = renderUI( 
    {
      # create list of colnames
      req(input_file())
      req(getRV())
      
      response = getRV()
      data <- input_file()
      cols = names(data)[! names(data) %in% response]
      
      
      # create bucket list
      bucket_list(
        header = "Drag the items in any desired bucket",
        group_name = "bucket_list2",
        orientation = "horizontal",
        
        add_rank_list(
          text = "Drag from here",
          labels = as.list(cols),
          input_id = "b2_rank_list_1",
          css_id = "b2_list1"),
        
        add_rank_list(
          text = "to here",
          labels = NULL,
          input_id = "b2_rank_list_2",
          css_id = "b2_list2")
      )
      
    })
  
  
  
  # Model output
  
  ## text
  output$viz_title = renderText({
    req(input_file())
    "6. Model Output"})
  
  output$viz_text = renderText({
    req(input_file())
    example_text})
  
  ## output
  
  ### get Vars used in models
  getModel1 <- reactive({ input$b1_rank_list_2})
  getModel2 <- reactive({ input$b2_rank_list_2})
  
  output$model_output = renderText( {
    req(input_file())
    req(getModel1())
    model1 = getModel1()
    model2 = getModel2()
    paste(model1, model2)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)










# # create list of each plot
# plot_list = list()
# for(i in 1:length(expl)){
#   
#   # get data type: either numeric or character
#   if(is.numeric(data$expl[i])){
#     plot_list[[i]] =
#       renderPlot({
#       ggplot(data, aes_string(y = response, x = expl[i])) +
#       geom_point()
#       })
#     
#   }else if(is.character(data$expl[i])){
#     plot_list[[i]] =
#       renderPlot({
#       ggplot(data, aes_string(y = response, x = expl[i])) +
#       geom_boxplot()
#       })
#   }
# }

# print(plot_list[[1]])
# wrap_plots(plot_list, ncol = 3)
# do.call(tagList, plot_list)

