fluidRow(column(12, align = "left", uiOutput("buckets1"))),
fluidRow(column(12, align = "left", uiOutput("buckets2"))),
# fluidRow(column(4, align = "center", uiOutput("buckets3")))


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


# modular functions

## check box
model_selection_box = function(id, number){
  moduleServer(
    id,
    function(input, output, session){
      
      req(reactive({input_file()}))
      req(reactive({getRV()}))
      
      response =  reactive({getRV()})
      data <-  reactive({input_file()})
      expl = names(data)[! names(data) %in% response]
      
      return(checkboxGroupInput(inputId = glue("model{number}"),
                                label = glue("Model {number}"),
                                choices = expl))
    }
  )
}


# output$box_model1 = renderUI({
#   
#   req(input_file())
#   req(getRV())
#   response = getRV()
#   data <- input_file()
#   expl = names(data)[! names(data) %in% response]
#   
#   checkboxGroupInput(inputId = "model1", 
#                      label = "Model 1",
#                     choices = expl)
# })
# 


# getAllEV <- reactive({
#   req(input_file())
#   req(getRV())
#   data <- input_file()
#   expl = names(data)[! names(data) %in% response]
# })
