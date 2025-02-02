library("shiny")
library("DiagrammeR")

server <- function(input, output) {
  # json_data will be reactive
  json_data <- reactiveVal(NULL)  
  
  # if there's input, set the json_data with given json file.
  observe({
    if (!is.null(input$file1)) {
      json_path <- input$file1$datapath
      json <- readLines(json_path)
      json_data(json)
    }
  })
  
  # if user pushes file_del, it will remove json_data values
  observeEvent(input$file_del, {
    json_data(NULL)
  })
  
  # if user pushes exit, it will close the window and return json
  observeEvent(input$close, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    #if (input$close > 0) stopApp()  
  })
  
  # ui1 is for select input for "FunctionList", "Data Server", "FaaS Server", and "General Configuration"
  output$ui1 <- renderUI({
    
    # bring the json_data if it's not empty
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    # create a uuid for Invocation ID
    exampleid <- uuid::UUIDgenerate()
    if (is.null(json$InvocationID)){
      json$InvocationID <- exampleid
    }
    
    # set the default value for Log folder name: FaaSrLog
    if (is.null(json$FaaSrLog)){
      json$FaaSrLog <- "FaaSrLog"
    }
    
    # no selection will return nothing
    if (is.null(input$select1))
      return()
    
    switch (input$select1,
            # if select1 is "FunctionList", give a text input for func_name
            # and it calls ui5
            "Functions" = list(
              textInput("func_name", "Action Name:", placeholder= "Action_1"),
              uiOutput("ui5")
            ),
            # if select1 is "Data Server", give a text input for data_name
            # and it calls ui3
            "Data Server" = list(
              textInput("data_name", "Data Server Name:", placeholder = "Initial_data_server_name"),
              uiOutput("ui3")
            ),
            # if select1 is "FaaS Server", give a text input for faas_name
            # and it calls ui4
            "FaaS Server" = list(
              textInput("faas_name", "FaaS Server Name:", placeholder = "Initial_faas_server_name"),
              uiOutput("ui4")
            ),
            # if select1 is "General Configuration", give text inputs and select inputs.
            "General Configuration" = list(
              selectInput("function_invoke", "First Function to be executed:", names(json$FunctionList), selected=json$FunctionInvoke),
              selectInput("logging_data_server", "Logging Data Server to leave logs:", c("None",names(json$DataStores)), selected=json$LoggingDataStore),
              selectInput("default_data_server", "Default Data Server to be used in put/get/arrow/log:", names(json$DataStores), selected=json$DefaultDataStore),
              textInput("faasr_log", "Log file name(Optional, default=FaaSrLog):", value = json$FaaSrLog, placeholder = "FaaSrLog"),
              textInput("invocation_id", "InvocationID(Optional, must follow UUID format):", value = json$InvocationID, placeholder = exampleid),
              fluidRow(
                column(6,
                       actionButton("gen_apply", label = "Apply")),
                column(6,
                       div(style = "position:absolute;right:1em;", 
                           actionButton("gen_delete", label = "Delete")))
              )
            )
    )
  })
  
  # ui5 is for action name
  output$ui5 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    return(
      list(
        # it shows a text input for func_act
        # it also calls ui2
        textInput("func_act", "Function Name:", value= json$FunctionList[[input$func_name]]$FunctionName, placeholder = "function_1"),
        uiOutput("ui2")
      )
    )
  })
  
  # ui2 is for FunctionList
  # Read the json data with given func_act, separating ui is required.
  output$ui2 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    return(
      list(
        selectInput("func_faas", "Function FaaS Server:", names(json$ComputeServers), selected = json$FunctionList[[input$func_name]]$FaaSServer),
        textAreaInput("func_args", "Function Arguments:", value = unretrieve(json$FunctionList[[input$func_name]]$Arguments), placeholder = "arg1=input1.csv,\narg2=input2.csv", height = "100px", resize = "vertical"),
        textInput("func_next", "Next Actions to Invoke:", value = unretrieve(json$FunctionList[[input$func_name]]$InvokeNext), placeholder = "Action_2, Action_3"),
        textInput("func_container", "Function's Action Container(Optional):", value= json$ActionContainers[[input$func_name]], placeholder = "faasr/github-actions-tidyverse"),
        textInput("func_gh_repo", "Repository/Path, where the function is stored:", value = unretrieve(json$FunctionGitRepo[[input$func_act]]), placeholder = "username/reponame, https://url.git"),
        textInput("func_gh_package", "Dependencies - Github Package for the function:", value = unretrieve(json$FunctionGitHubPackage[[input$func_act]]), placeholder = "username/package_reponame"),
        textInput("func_cran_repo", "Dependencies - Repository/Path for the function:", value = unretrieve(json$FunctionCRANPackage[[input$func_act]]), placeholder = "CRAN_package_name, dplyr"),
        fluidRow(
          column(6,
                 actionButton("func_apply", label = "Apply")),
          column(6,
                 div(style = "position:absolute;right:1em;", 
                     actionButton("func_delete", label = "Delete")))
        )
      )
    )
  })
  
  # ui3 is for data servers.
  output$ui3 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    return(
      list(
        textInput("data_endpoint", "Data Server Endpoint(optional for s3):", value = json$DataStores[[input$data_name]]$Endpoint, placeholder = "https://play.min.io"),
        textInput("data_bucket", "Data Server Bucket:", value = json$DataStores[[input$data_name]]$Bucket, placeholder = "my_bucket"),
        textInput("data_region", "Data Server Region (optional for minio):", value = json$DataStores[[input$data_name]]$Region, placeholder = "us-east-1"),
        textInput("data_writable", "Data Server Writable permission:", value = json$DataStores[[input$data_name]]$Writable, placeholder = "TRUE"),
        fluidRow(
          column(6,
                 actionButton("data_apply", label = "Apply")),
          column(6,
                 div(style = "position:absolute;right:1em;", 
                     actionButton("data_delete", label = "Delete")))
        )
      )
    )
  })
  
  # ui4 is for faas servers
  output$ui4 <- renderUI({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    return(
      list(
        selectInput("faas_type", "Select type:", c("GitHubActions", "OpenWhisk", "Lambda"), selected = json$ComputeServers[[input$faas_name]]$FaaSType),
        conditionalPanel(
          condition = "input.faas_type == 'GitHubActions'",
          textInput("faas_gh_user", "GitHubActions User name:", value = json$ComputeServers[[input$faas_name]]$UserName, placeholder = "user_name"),
          textInput("faas_gh_repo", "GitHubActions Action Repository name:", value = json$ComputeServers[[input$faas_name]]$ActionRepoName, placeholder = "repo_name"),
          textInput("faas_gh_ref", "GitHubActions Branch:", value = json$ComputeServers[[input$faas_name]]$Branch, placeholder = "main")
        ),
        conditionalPanel(
          condition = "input.faas_type == 'Lambda'",
          textInput("faas_ld_region", "Lambda Region:", value = json$ComputeServers[[input$faas_name]]$Region, placeholder = "us-east-1")
        ),
        conditionalPanel(
          condition = "input.faas_type == 'OpenWhisk'",
          textInput("faas_ow_end", "OpenWhisk Endpoint(Optional, default=IBMcloud):", value = json$ComputeServers[[input$faas_name]]$Endpoint, placeholder = "https://00.00.00.00"),
          textInput("faas_ow_name", "OpenWhisk Namespace:", value = json$ComputeServers[[input$faas_name]]$Namespace, placeholder = "namespace_name"),
          textInput("faas_ow_region", "OpenWhisk Region:", value = json$ComputeServers[[input$faas_name]]$Region, placeholder = "us-west")
        ),
        fluidRow(
          column(6,
                 actionButton("faas_apply", label = "Apply")),
          column(6,
                 div(style = "position:absolute;right:1em;", 
                     actionButton("faas_delete", label = "Delete")))
        )
      )
    )
  })
  
  # if user pushes func_delete button, it will remove that function.
  observeEvent(input$func_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    func_name <- input$func_name
    if (is.null(func_name)){
      return()
    }
    func_act <- json$FunctionList[[func_name]]$FunctionName
    json$FunctionList[[func_name]] <- NULL
    json$ActionContainers[[func_name]] <- NULL
    func_set <- NULL
    for (func in names(json$FunctionList)){
      if (func != func_name){
        func_set <- unique(c(func_set, json$FunctionList[[func]]$FunctionName))
        if (func_name %in% json$FunctionList[[func]]$InvokeNext){
          json$FunctionList[[func]]$InvokeNext <- json$FunctionList[[func]]$InvokeNext[json$FunctionList[[func]]$InvokeNext != func_name]
        }
      }
    }
    if (!func_act %in% func_set){
      json$FunctionCRANPackage[[func_act]] <- NULL
      json$FunctionGitHubPackage[[func_act]] <-NULL
      json$FunctionGitRepo[[func_act]] <- NULL
    }
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  # if user pushes faas_delete button, it will remove that faas server.
  observeEvent(input$faas_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    faas_name <- input$faas_name
    if (is.null(faas_name)){
      return()
    }
    json$ComputeServers[[faas_name]] <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  # if user pushes data_delete button, it will remove that data server.
  observeEvent(input$data_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else {
      json <- list()
    }
    data_name <- input$data_name
    if (is.null(data_name)){
      return()
    }
    json$DataStores[[data_name]] <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  # if user pushes gen_delete button, it will remove that general configurations.
  observeEvent(input$gen_delete, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json$FunctionInvoke <- NULL
    json$InvocationID <- NULL
    json$FaaSrLog <- NULL
    json$LoggingDataStore <- NULL
    json$DefaultDataStore <- NULL
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
    
  })
  
  # if user pushes func_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$func_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    func_name <- input$func_name
    if (is.null(func_name)){
      return()
    }
    json$FunctionList[[func_name]]$FunctionName <- input$func_act
    json$FunctionList[[func_name]]$FaaSServer <- input$func_faas
    json$FunctionList[[func_name]]$Arguments <- retrieve(input$func_args)
    json$FunctionList[[func_name]]$InvokeNext <- retrieve(input$func_next)
    json$ActionContainers[[func_name]] <- input$func_container
    json$FunctionCRANPackage[[input$func_act]] <- retrieve(input$func_cran_repo)
    json$FunctionGitHubPackage[[input$func_act]] <-retrieve(input$func_gh_package)
    json$FunctionGitRepo[[input$func_act]] <- retrieve(input$func_gh_repo)
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
  })
  
  # if user pushes faas_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$faas_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    faas_name <- input$faas_name
    if (is.null(faas_name)){
      return()
    }
    json$ComputeServers[[faas_name]]$FaaSType <- input$faas_type
    switch(json$ComputeServers[[faas_name]]$FaaSType,
           "GitHubActions"={
             json$ComputeServers[[faas_name]]$UserName <- input$faas_gh_user
             json$ComputeServers[[faas_name]]$ActionRepoName <- input$faas_gh_repo
             json$ComputeServers[[faas_name]]$Branch <- input$faas_gh_ref
           },
           "Lambda"={
             json$ComputeServers[[faas_name]]$Region <- input$faas_ld_region
           },
           "OpenWhisk"={
             json$ComputeServers[[faas_name]]$Endpoint <- input$faas_ow_end
             json$ComputeServers[[faas_name]]$Namespace <- input$faas_ow_name
             json$ComputeServers[[faas_name]]$Region <- input$faas_ow_region
           }
    )
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    
    json_data(json_pretty)
  })
  
  # if user pushes data_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$data_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    data_name <- input$data_name
    if (is.null(data_name)){
      return()
    }
    json$DataStores[[data_name]]$Endpoint <- input$data_endpoint
    json$DataStores[[data_name]]$Bucket <- input$data_bucket
    json$DataStores[[data_name]]$Region <- input$data_region
    json$DataStores[[data_name]]$Writable <- input$data_writable
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    
    json_data(json_pretty)
  })
  
  # if user pushes gen_apply button, it will save the selected and text inputs into the json.
  observeEvent(input$gen_apply, {
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    json$FunctionInvoke <- input$function_invoke
    json$InvocationID <- input$invocation_id
    json$FaaSrLog <- input$faasr_log
    if (input$logging_data_server == "None"){
      json$LoggingDataStore <- NULL
    } else {
      json$LoggingDataStore <- input$logging_data_server
    }
    json$DefaultDataStore <- input$default_data_server
    json_source <- jsonlite::toJSON(json, auto_unbox=TRUE)
    json_pretty <- jsonlite::prettify(json_source)
    json_data(json_pretty)
    
  })
  
  # this function is to convert the list into "a=b, c=d" form
  unretrieve <- function(val){
    if (!is.null(val)){
      if (!is.list(val)){
        val_merged_char <- NULL
        for (val_key in val){
          if (val_key == val[length(val)]){
            val_merged_char <- paste0(val_merged_char, val_key)
          } else {
            val_merged_char <- paste0(val_merged_char, val_key,", ")
          }
        }
        return(val_merged_char)
      } else {
        val_merged <- NULL
        for (val_key in names(val)){
          if (val_key == names(val[length(val)])){
            val_merged <- paste0(val_merged, val_key, "=",val[[val_key]])
          } else{
            val_merged <- paste0(val_merged, val_key, "=",val[[val_key]], ",\n")  
          }
        }
        return(val_merged)
      }
    } else {
      return(val)
    }
  }
  
  # this function is to convert the "a=b, c=d" form into the list
  retrieve <- function(val) {
    if (!is.null(val)) {
      val_list <- unlist(strsplit(strsplit(val, '\n')[[1]], ','))
      text_list <- list()
      for (text in val_list){
        parts <- strsplit(text, '=')
        if (length(parts[[1]])<2){
          text_trim <- trimws(text)
          text_list <- unlist(c(text_list, text_trim))
        } else {
          re_input_key <- trimws(gsub('[\'"]', '', parts[[1]][1]))
          re_input_val <- trimws(gsub('[\'"]', '', parts[[1]][2]))
          text_list[[re_input_key]] <- re_input_val
        }
      }
      return(text_list)
    } else {
      return(val)
    }
  }
  
  # this is a download button implementation. 
  # if user pushes download button, it will create a json file with current json_data
  output$downjson <- downloadHandler(
    filename = function() {
      "payload.json"
    },
    content = function(file) {
      json_result <- json_data()
      if (!is.null(json_result)) {
        writeLines(json_result, file)
      }
    }
  )
  
  # fsm_func is a Finite State Machine for functions
  output$fsm_func <- renderGrViz({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    fsm_func_edge <- NULL
    fsm_func_name <- NULL
    fsm_first_func <- NULL
    
    for (funcname in names(json$FunctionList)) {
      if (!is.null(json$FunctionInvoke) && funcname == json$FunctionInvoke){
        fsm_first_func <- paste0(funcname, " [label=<",funcname," <br/> <font color='red'><font point-size='25'> ",json$FunctionList[[funcname]]$FunctionName,"</font></font>>];")
      } else{
        fsm_func_name <- paste0(fsm_func_name,funcname, " [label=<",funcname," <br/> <font color='red'><font point-size='25'> ",json$FunctionList[[funcname]]$FunctionName,"</font></font>>];")
      }
      for (next_func in json$FunctionList[[funcname]]$InvokeNext){
        if (next_func %in% names(json$FunctionList)){
          fsm_func_edge <- paste0(fsm_func_edge,funcname,"->",next_func,";")
        }
      }
    }
    
    
    grViz(paste0("digraph{
      graph[rankdir = LR, fontname=Helvetica];
      subgraph cluster_0 {
        graph[shape = rectangle];
        style = rounded;
        bgcolor = LemonChiffon;
        color = LemonChiffon;
        label = 'Functions';
        node[width=3, fixedsize=false, fontsize=32, shape = doublecircle, style=filled, fontname=Helvetica, fillcolor=white, color=gray32];
        ",fsm_first_func,"
        node[width=3, fixedsize=false, fontsize=32, shape = circle, style=filled, fontname=Helvetica, fillcolor=white, color=white];
        ",fsm_func_name,"
      };
      edge[color=black, arrowsize=1];
      ",fsm_func_edge,"
    }"))
    
  })
  
  # fsm_data is a drawing for data servers.
  output$fsm_data <- renderGrViz({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    fsm_data_name <- NULL
    fsm_logging <- NULL
    
    for (dataname in names(json$DataStores)) {
      if (!is.null(json$DefaultDataStore) && dataname == json$DefaultDataStore){
        fsm_logging <- paste0(dataname," [label=",dataname,"];")
      } else {
        fsm_data_name <- paste0(fsm_data_name,dataname," [label=",dataname,"];")
      }
    }
    
    grViz(paste0("digraph{
      graph[rankdir = LR, fontname=Helvetica];
      subgraph cluster_1 {
        graph[shape = rectangle];
        style = rounded;
        bgcolor = darkolivegreen3;
        color = darkolivegreen3;
        label = 'Data Servers';
        node[width=3, fixedsize=shape, fontsize=16, shape = folder, style=filled, fontname=Helvetica, fillcolor=white, color=gray32];
        ",fsm_logging,"
        node[width=3, fixedsize=shape, fontsize=16, shape = folder, style=filled, fontname=Helvetica, fillcolor=white, color=white];
        ",fsm_data_name,"
      };
    }"))
    
  })
  
  # fsm_faas is a drawing for faas servers.
  output$fsm_faas <- renderGrViz({
    if (!is.null(json_data())) {
      json <- jsonlite::fromJSON(json_data())
    } else{
      json <- list()
    }
    
    fsm_faas_name <- NULL
    
    
    for (faasname in names(json$ComputeServers)) {
      fsm_faas_name <- paste0(fsm_faas_name,faasname," [label=",faasname,"];")
    }
    
    grViz(paste0("digraph{
      graph[rankdir = LR, fontname=Helvetica];
      subgraph cluster_1 {
        graph[shape = rectangle];
        style = rounded;
        bgcolor = Lightskyblue;
        label = 'FaaS Servers';
        color=Lightskyblue;
        node[width=3, fixedsize=shape, fontsize=16, shape = tab, style=filled, fontname=Helvetica, fillcolor=white, color=white];
        ",fsm_faas_name,"
      };
    }"))
    
  })
  
  # This is to implement user friendly functions.
  # If user clicks the drawings, it will show the data on the left.
  observe({
    req(input$fsm_func_click)
    new_func <- input$fsm_func_click$nodeValues[[1]]
    updateTextInput(inputId="func_name", value=trimws(new_func, "right"))
    updateSelectInput(inputId="select1", selected="Functions")
  })
  observe({
    req(input$fsm_faas_click)
    new_faas <- input$fsm_faas_click$nodeValues[[1]]
    updateTextInput(inputId="faas_name", value=new_faas)
    updateSelectInput(inputId="select1", selected="FaaS Server")
  })
  observe({
    req(input$fsm_data_click)
    new_data <- input$fsm_data_click$nodeValues[[1]]
    updateTextInput(inputId="data_name", value=new_data)
    updateSelectInput(inputId="select1", selected="Data Server")
  })
}
