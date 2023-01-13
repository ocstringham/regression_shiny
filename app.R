library(shiny)
library(shinyjs)
library(shinythemes)
library(sortable)
library(glue)
library(tibble)
library(dplyr)
library(ggplot2)
library(egg)

example_text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."



# Define UI for application
ui <- fluidPage( 
  
  theme = shinytheme("simplex"),
  
#   # css
#   tags$head(
#     tags$style(HTML("
#   
#   .colBord {
#       border-color: #8d8d8d;
#       border-width: 1px;
#       border-style: groove;
#       border-radius: 5px;
#       border-spacing: 5px;
# }
# "))),
  
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
  fluidRow(column(12, align = "center", uiOutput("expl_var"))),
  fluidRow(column(12, align = "left", plotOutput("viz_plots"))),
  
  
  # Build models
  fluidRow(column(12, align = "left", h3(uiOutput("model_title")))),
  fluidRow(column(12, align = "left", p(uiOutput("model_text")))),
  fluidRow(column(3, offset = 1, align = "left", uiOutput("box_model1")),
           column(3, offset = 1, align = "left", uiOutput("box_model2")),
           column(3, offset = 1, align = "left", uiOutput("box_model3")),
           column(3, offset = 1, align = "left", uiOutput("box_model4")),
           column(3, offset = 1, align = "left", uiOutput("box_model5")),
           column(3, offset = 1, align = "left", uiOutput("box_model6"))
  ), 
  
  
  # View Output
  fluidRow(column(12, align = "left", h3(uiOutput("output_title")))),
  fluidRow(column(12, align = "left", p(uiOutput("output_text")))),
  fluidRow(column(12, align = "center", uiOutput("model_output")))
  
)

#----------------------------------------------------------------------------- #
 
# Define server logic required 
server <- function(input, output, session) {
  
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
  getRV <- reactive({ input$response_var })
  getEV <- reactive({ input$expl_var })
  getAllEV <- reactive({
    req(input_file())
    req(getRV())
    data <- input_file()
    response = getRV()
    expl = names(data)[! names(data) %in% response]
  })
  
  
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
    

  ## Number of Candidate models to explore

  ## Check box for model selection -> create module
  output$box_model1 = renderUI({
    req(getRV())
    expl = getAllEV()
    checkboxGroupInput(inputId = "model1",
                       label = "Model 1",
                      choices = expl)
  })
  
  output$box_model2 = renderUI({
    req(getRV())
    expl = getAllEV()
    checkboxGroupInput(inputId = "model2",
                       label = "Model 2",
                       choices = expl)
  })
  
  
  output$box_model3 = renderUI({
    req(getRV())
    expl = getAllEV()
    checkboxGroupInput(inputId = "model3",
                       label = "Model 3",
                       choices = expl)
  })
  
  output$box_model4 = renderUI({
    req(getRV())
    expl = getAllEV()
    checkboxGroupInput(inputId = "model4",
                       label = "Model 4",
                       choices = expl)
  })
  
  output$box_model5 = renderUI({
    req(getRV())
    expl = getAllEV()
    checkboxGroupInput(inputId = "model5",
                       label = "Model 5",
                       choices = expl)
  })
  
  output$box_model6 = renderUI({
    req(getRV())
    expl = getAllEV()
    checkboxGroupInput(inputId = "model6",
                       label = "Model 6",
                       choices = expl)
  })


  
  
  # Model output
  
  ## text
  output$output_title = renderText({
    req(input_file())
    "6. Model Output"})
  
  output$output_text = renderText({
    req(input_file())
    example_text})
  
  ## output
  
  ### get Vars used in models
  getModel1 <- reactive({ input$model1 })
  getModel2 <- reactive({ input$model2 })
  getModel3 <- reactive({ input$model3 })
  getModel4 <- reactive({ input$model4 })
  getModel5 <- reactive({ input$model5 })
  getModel6 <- reactive({ input$model6 })
  
  output$model_output = renderTable( {
    
    req(input_file())
    req(getModel1())
    
    # get each model
    model1 = getModel1()
    model2 = getModel2()
    model3 = getModel3()
    model4 = getModel4()
    model5 = getModel5()
    model6 = getModel6()
    
    # run lm
    
    ## get data
    data = input_file()
    
    ## Run models
    
    ### fun to run and process results
    run_mod = function(data, rv, ev){
      lm(data=data, formula = paste(rv, "~", paste(ev, collapse="+"), sep = ""))
    }
    
    proc_mod = function(no, ev, mod){
      tribble(
        ~Model, ~`Independent Variable`, ~`Dependent Variables`, ~R2, ~AIC,
        glue("Model {no}"), getRV(), paste(ev, collapse=", "), round(summary(mod)$r.squared,3), round(AIC(mod),2)
      )
    }
    
    ### Run 1st model
    mod1 = run_mod(data, getRV(), model1)

    #### Get Results
    mod_results = proc_mod(1, model1, mod1)
    
    ### Run other models
    if(!is.null(model2)){
      mod2 = run_mod(data, getRV(), model2)
      mod_results = mod_results %>% bind_rows(proc_mod(2, model2,mod2))
    }
    
    if(!is.null(model3)){
      mod3 = run_mod(data, getRV(), model3)
      mod_results = mod_results %>% bind_rows(proc_mod(3, model3,mod3))
    }
    
    if(!is.null(model4)){
      mod4 = run_mod(data, getRV(), model4)
      mod_results = mod_results %>% bind_rows(proc_mod(4, model4,mod4))
    }
    
    if(!is.null(model5)){
      mod5 = run_mod(data, getRV(), model5)
      mod_results = mod_results %>% bind_rows(proc_mod(5, model5,mod5))
    }
    
    if(!is.null(model6)){
      mod6 = run_mod(data, getRV(), model6)
      mod_results = mod_results %>% bind_rows(proc_mod(6, model6,mod6))
    }
    
    mod_results

    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)










