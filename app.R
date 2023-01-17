library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
# library(sortable)
library(reactable)
library(glue)
library(tibble)
library(dplyr)
library(ggplot2)
library(egg)

example_text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

# Set def col width
dcw = 8
# def offset
dos = 2


# Define Texts

siteDesc = "This web application allows the user to input an data sheet, specify indpendent and dependent variables, view variable relationships, create multi candiate models for linear regression, and obtain summary statistics of each model's output."
uploadText = "Choose a file from your computer to upload. This file must be in .csv format."
viewDataText = "Take a look at your data in the table below. Make sure everything looks as it should. This table shows 10 rows at a time and your can view subsequent rows by clicking Next in the bottom right corner of the table. If something is wrong, go back to the original data sheet and revise as needed. Then, re-save the file as a .csv and upload it again."
selectRespVarText = "Select your dependent variable, also known as the response variable. Your dependent variable is the variable you are seeking to explain (or predict) using indepdent variables, also known a explanatory variables or covariates."
vizDataText = "A first step before analysis is to vizually explore your data. Specifically, using bivariate (2 variables) plots to see the relationship between your dependent and all independent variables is useful to obtain an intuitive understanding of your data. Click an independent variable below to view a bivariate plot with the dependent variable. If the indepedent variable is numeric, the plot will be a scatterplot. If the indepedent is categorical, (i.e., yes or no) then the plot will be a box plot. The box plots are also overlayed with a 'jitter' plot showing where the distribution of data points are."

# UI ----
ui <- fluidPage( 
  
  ## set theme
  theme = shinytheme("spacelab"),
  
  
  ## Application title
  # titlePanel("Linear Regression"),
  fluidRow(column(dcw, offset = dos, align="left", 
                  h1("Ecological Detective Lab"))),
  fluidRow(column(dcw, offset = dos, align="left", 
                  h4("Multi-Model Inferences and Observational Data"))),
  fluidRow(column(dcw, offset = dos, align="left", 
                  h5("Rutgers | Ecology 300"))),

  # -------------------------------------------------------------------------- #
  
  ## File Input ----
  fluidRow(
        useShinyjs(),
        column(dcw, offset = dos, align="left", p(siteDesc)),
        column(dcw, offset = dos, align="left", h3("1. Upload data file"), p(uploadText)),
        column(dcw, offset = dos, align="center",
          fileInput(inputId = "file1", label = "Select a .csv file", 
                    accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")
                   )
        )),
    
  # -------------------------------------------------------------------------- #
  
  ## Table ----
  fluidRow(column(dcw, offset = dos, align = "left", h3(uiOutput("tableTitle")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("tableText")))),
  fluidRow(column(dcw, offset = dos, align='center', reactableOutput("table"))),

  # -------------------------------------------------------------------------- #
  
  ## Select response variable ----
  fluidRow(column(dcw, offset = dos, align = "left", h3(uiOutput("response_var_title")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("response_var_text")))),
  fluidRow(column(dcw, offset = dos, align = "center", uiOutput("response_var"))),
  
  # -------------------------------------------------------------------------- #
  
  ## Viz data ----
  fluidRow(column(dcw, offset = dos, align = "left", h3(uiOutput("viz_title")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("viz_text")))),
  fluidRow(column(dcw, offset = dos, align = "center", uiOutput("expl_var"))),
  fluidRow(column(dcw-2, offset = dos+1, align = "center", plotOutput("viz_plots"))),
  
  # -------------------------------------------------------------------------- #
  
  ## Build models ----
  fluidRow(column(dcw, offset = dos, align = "left", h3(uiOutput("model_title")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("model_text")))),
  fluidRow(
    column(dcw, offset = dos, align = "left",
           column(4, align = "left", uiOutput("box_model1")),
           column(4, align = "left", uiOutput("box_model2")),
           column(4, align = "left", uiOutput("box_model3")))
  ),
  fluidRow(
    column(dcw, offset = dos, align = "left",
           column(4, align = "left", uiOutput("box_model4")),
           column(4, align = "left", uiOutput("box_model5")),
           column(4, align = "left", uiOutput("box_model6"))
    )
  ), 
  
  # -------------------------------------------------------------------------- #
  
  ## View Output ----
  fluidRow(column(dcw, offset = dos, align = "left", h3(uiOutput("output_title")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("output_text")))),
  fluidRow(column(dcw, offset = dos, align='center', reactableOutput("model_output"))),
  
  
  # -------------------------------------------------------------------------- #
  
  ## View Coeffs Output ----
  fluidRow(column(dcw, offset = dos, align = "left", h4(uiOutput("coeffsTitle")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("coeffsText")))),
  fluidRow(column(dcw, offset = dos, align = "center", uiOutput("coeffsChoice"))),
  fluidRow(column(dcw, offset = dos, align='center', reactableOutput("coeffsTable"))),
  
  # -------------------------------------------------------------------------- #
  
  ## Interpret results ----
  fluidRow(column(dcw, offset = dos, align = "left", h3(uiOutput("int_title")))),
  fluidRow(column(dcw, offset = dos, align = "left", p(uiOutput("int_text"))))

)

#----------------------------------------------------------------------------- #
 
# Server ----
server <- function(input, output, session) {
  
  # -------------------------------------------------------------------------- #
  
  # input csv file ----
  input_file <- reactive({
    if (is.null(input$file1)) {
      return("")
    }
    
    # actually read the file
    read.csv(file = input$file1$datapath)
  })
  
  # -------------------------------------------------------------------------- #
  
  # output table ----
  
  ## text
  output$tableTitle = renderText({
    req(input_file())
    "2. View data"})
  
  output$tableText = renderText({
    req(input_file())
    viewDataText})
  

  
  ## table
  output$table <- renderReactable({
    
    # render only if there is data available
    req(input_file())
    
    # reactives are only callable inside an reactive context like render
    data <- input_file()
    reactable(data)
  })
  
  # -------------------------------------------------------------------------- #
  
  # Select response variable ----
  output$response_var_title = renderText({
    req(input_file())
    "3. Select dependent variable"})
  
  output$response_var_text = renderText({
    req(input_file())
    selectRespVarText})
  
  output$response_var = renderUI({
    req(input_file())
    cols = colnames(input_file())
    # radioButtons(inputId = "response_var", label = "Select an independent variable.",
    #          choices = cols, selected = character(0))
    radioGroupButtons(
      inputId = "response_var", label = "Select an dependent variable.", 
      choices = cols, selected = character(0),
      justified = TRUE, status = "primary", direction = "vertical",  individual = TRUE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  }
  )
  
  # -------------------------------------------------------------------------- #
  
  # Viz data ----
  
  ## text
  output$viz_title = renderText({
    req(input_file())
    "4. Vizualize data"})
  
  output$viz_text = renderText({
    req(input_file())
    vizDataText})
  
  ## radio for expl var
  output$expl_var = renderUI({
    req(input_file())
    req(getRV())
    response = getRV()
    data <- input_file()
    expl = names(data)[! names(data) %in% response]
    # radioButtons(inputId = "expl_var", label = "Select an dependent variable to plot.",
    #              choices = expl, selected = character(0),  inline=T)
    radioGroupButtons(
      inputId = "expl_var", label = "Select an independent variable to plot.", 
      choices = expl, selected = character(0),
      justified = TRUE, status = "primary", direction = "vertical",  individual = TRUE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
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
          geom_point(alpha = 0.7, color = "#19538C") +
          theme_presentation()
      }else if(is.character(data[, expl])){
        ggplot(data, aes_string(y = response, x = expl)) +
          geom_boxplot(color = "#19538C") + 
          geom_jitter(color = "#19538C", alpha = 0.4) +
          theme_presentation()
      }
      
    # if response is character
    }else if(is.character(data[, response])){
    
      if(is.numeric(data[, expl])){
        ggplot(data, aes_string(y = response, x = expl)) +
          geom_boxplot(color = "#19538C") + 
          geom_jitter(color = "#19538C", alpha = 0.4, height = 0) +
          theme_presentation()
      }else if(is.character(data[, expl])){
        ggplot(data, aes_string(group = response, fill = response, x = expl)) +
          geom_bar(position = position_dodge2(preserve = "single")) + 
          scale_fill_brewer(palette = 1, type = 'qual') +
          theme_presentation()
      }
    }

  })
  
  # -------------------------------------------------------------------------- #
  
  # Build and run models ----
  
  ## Text
  output$model_title = renderText({
    req(input_file())
    "5. Build models"})
  
  output$model_text = renderText({
    req(input_file())
    example_text})
    

  ## Number of Candidate models to explore

  ## Check box for model selection -> create module
  
  create_cb = function(no, expl){
    awesomeCheckboxGroup(
      inputId = glue("model{no}"),
      label = glue("Candidate Model {no}"),
      choices = expl,
      status = "primary"
    )
  }
  
  output$box_model1 = renderUI({
    req(getRV())
    expl = getAllEV()
    create_cb(1, expl)
  })
  
  output$box_model2 = renderUI({
    req(getRV())
    expl = getAllEV()
    create_cb(2, expl)
  })
  
  
  output$box_model3 = renderUI({
    req(getRV())
    expl = getAllEV()
    create_cb(3, expl)
  })
  
  output$box_model4 = renderUI({
    req(getRV())
    expl = getAllEV()
    create_cb(4, expl)
  })
  
  output$box_model5 = renderUI({
    req(getRV())
    expl = getAllEV()
    create_cb(5, expl)
  })
  
  output$box_model6 = renderUI({
    req(getRV())
    expl = getAllEV()
    create_cb(6, expl)
  })


  # -------------------------------------------------------------------------- #
  
  # Model output ----
  
  ## text
  output$output_title = renderText({
    req(input_file())
    "6. Model output"})
  
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
  
  output$model_output = renderReactable( {
    
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
      smod = summary(mod)
      tribble(
        ~Model, ~`Indep. Variable`, ~`Depen. Variables`, ~RSS, ~R2, ~AIC,
        glue("Model {no}"), getRV(), paste(ev, collapse=" + "), 
        round(sum(smod$residuals ^ 2),0), round(smod$r.squared,3), round(AIC(mod),1)
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
    
    reactable(mod_results)

    
  })
  
  
  # Table of model coeffs ----
  
  ## Select model ----
  output$coeffsTitle = renderText({
    req(input_file())
    "View model coefficient estimates"})
  
  output$coeffsText = renderText({
    req(input_file())
    example_text})
  
  output$coeffsChoice = renderUI({
    
    req(input_file())
    req(getModel1())
    
    ## get list of avail models 
    # avail_mods = c("Model 1", "Model 2", "Model 3",
    #              "Model 4", "Model 5", "Model 6")
    
    avail_mods = c("Model 1")
    if(!is.null(getModel2())) avail_mods = c(avail_mods, "Model 2")
    if(!is.null(getModel3())) avail_mods = c(avail_mods, "Model 3")
    if(!is.null(getModel4())) avail_mods = c(avail_mods, "Model 4")
    if(!is.null(getModel5())) avail_mods = c(avail_mods, "Model 5")
    if(!is.null(getModel6())) avail_mods = c(avail_mods, "Model 6")
    
    
    ## radio
    radioGroupButtons(
      inputId = "coeffsChoice", label = "Select a model.", 
      choices = avail_mods, selected = character(0),
      justified = TRUE, status = "primary", direction = "vertical",  individual = TRUE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  }
  )
  
  
  
  ## Create coeff table ----
  
  ### get chosen model
  getCoeffMod <- reactive({ input$coeffsChoice })
  
  output$coeffsTable = renderReactable( { 
    
    # has to be a way to share data btwn these funs but no time to figure out
    req(input_file())
    req(getModel1())
    req(getCoeffMod())
    
    
    # get model
    modText = getCoeffMod()
    if(modText == "Model 1"){
      mod = getModel1()
    }else if(modText == "Model 2"){
      mod = getModel2()
    }else if(modText == "Model 3"){
      mod = getModel3()
    }else if(modText == "Model 4"){
      mod = getModel4()
    }else if(modText == "Model 5"){
      mod = getModel5()
    }else if(modText == "Model 6"){
      mod = getModel6()
    }
    
    
    # run lm
    
    ## get data
    data = input_file()
    
    ## Run models
    
    ### fun to run and process results
    run_mod = function(data, rv, ev){
      lm(data=data, formula = paste(rv, "~", paste(ev, collapse="+"), sep = ""))
    }
    
    ### fun to convert p value to stars
    make_stars <- function(pval) {
      stars = ""
      if(pval <= 0.001)
        stars = "***"
      if(pval > 0.001 & pval <= 0.01)
        stars = "**"
      if(pval > 0.01 & pval <= 0.05)
        stars = "*"
      if(pval > 0.05 & pval <= 0.1)
        stars = "."
      stars
    }
    
    # coeff table
    mod_results = broom::tidy(run_mod(data, getRV(), mod)) %>% 
      mutate(estimate = round(estimate, 2), std.error = round(std.error, 2), 
             statistic = round(statistic, 2)) %>% 
      mutate(signif = sapply(p.value, function(x) make_stars(x))) %>% 
      select(-p.value, -statistic) %>% 
      rename(p.value = signif)
    
    
    reactable(mod_results)
    
    })
  
  
  # -------------------------------------------------------------------------- #
  
  # interpret results ----
  
  ## text
  output$int_title = renderText({
    req(input_file())
    "7. Interpret model results"})
  
  output$int_text = renderText({
    req(input_file())
    example_text})
  
}

# Run the application 
shinyApp(ui = ui, server = server)










