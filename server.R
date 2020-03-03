server <- function(input, output, session) {
  
  ###################################Reactive Values###############################
  #Value for file
  file <- reactiveValues()
  #Value for summary
  statistics <- reactiveValues()
  #Value for DT
  vals<-reactiveValues()
  #Value for model DTF
  model <- reactiveValues()
  #Value for plot to download
  plot <- reactive(hist(rnorm(100)))
  #Value for saved model
  savedModel <- reactiveValues()
  #List with DT
  listDT <- reactiveValues()
  #Bootstrap model variable
  bootstrapModel <- reactiveValues()
  #Static model variable
  staticModel <- reactiveValues()
  #Download model DT
  downloadRV <- reactiveValues(df = 0)
  downloadData <- reactive(downloadRV$df)
  ####OK#############################Functions#####################################
  #View file
  contentFile <- reactive({
    inFile <- input$file_name1
    #If file_name1 is epty return null//otherwise read file
    if (is.null(inFile))
      return(NULL)
    read_excel(inFile$datapath, input$sheet1)
  })
  
  #Delete string variable
  delString <- function() {
    #Change all columns to be numeric columns
    x<-contentFile()
    x <- lapply(x, unlist)
    x <- lapply(x, as.numeric)
    
    x <- as.data.frame(x)
    #Delete all columns which contain NA, becouse it means that they cannot be numeric values
    x<- x[ , -which(names(x) %in% colnames(x)[colSums(is.na(x)) > 0])]
    return(x)
  }
  
  #button side-by-side
  buttonRow<-function (inputId, label) 
  {
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,class="input-small"))
    actionButton(inputId=inputId, label=label,style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width: 10%; margin-left:15px;")
  }
  
  #Hellwig method
  hellwig <- function( y, x, method="pearson")
  {
    requireNamespace("utils")
    x <- as.data.frame(x)
    cm <- stats::cor(x, method=method) # correlation matrix among indeps
    cd <- stats::cor(x, y, method=method) # correlations with dependent
    # list of combination vectors
    k <- sapply( seq(1, length(x)), function(i)
      utils::combn(length(x), i, simplify=FALSE) )
    k <- do.call("c", k)
    # function calculating individual capacities
    hfun <- function(v)
    {
      sapply(v, function(i) cd[i]^2 / sum(abs(cm[v,i])) )
    }
    h <- sapply(k, hfun)
    #Data frame of all results
    data.frame( k = sapply( k, paste, collapse="-"),
                h = sapply(h, sum),
                stringsAsFactors=FALSE)
  }
  
  #First letter to upper case
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    return(x)
  }
  
  #Bootstrap coefficients vector function
  boot_sample_func <- function(data_boot, N, j) {
    
    #Prepare N length vector
    parameter_coefVector <- rep(NA, N)
    #linear model of data
    model_boot <- lm(data_boot)
    #Extract N times coeffs from model prepared each time by directed frequent re-sampling from sample from 1 to nrow of data,
    #and making model only with rows equals randomly drawn numbers.
    for(i in 1:N){
      boot_sample <- sample(1:nrow(data_boot), 
                            nrow(data_boot), 
                            replace = T)
      
      model_boot <- lm(data_boot, 
                       data = data_boot[boot_sample, ])
      parameter_coefVector[i] <- coef(model_boot)[j]
    }
    return(parameter_coefVector)
  }
  
  #Comparision - enumeration confidence intervals of bootstrap method
  comparision_boot <- function(data_boot, N, lower_limit, upper_limit) {
    
    #Prepare lists
    parameters_vectors <- list()
    parameters_boot <- list()
    
    
    for(j in 1:ncol(data_boot)) {
      
      parameter_coefVector <- boot_sample_func(data_boot, N, j)
      parameters_vectors[[j]] <- parameter_coefVector
      parameters_boot[[j]]  <- quantile(parameters_vectors[[j]], c(lower_limit,upper_limit)) %>% round(4)
      
    }
    
    parameters = do.call(rbind, parameters_boot)
    parameters <- t(parameters)
    colnames(parameters) <- c("(Intercept)", colnames(data_boot[-1]))
    return(parameters)
    
  }
  
  #Comparision - enumeration confidence intervals of classic method
  comparision_classic <- function(data, lower_limit, upper_limit) {
    
    #Classic method model
    model_classic <- lm(data)
    #Coeffs of liner model
    coefs_model_classic <- summary(model_classic)$coefficients
    
    #Classic method confidence intervals
    classic_coefs <- apply(coefs_model_classic, 1, function(x){
      x[1] + qnorm(c(lower_limit, upper_limit))*x[2]}) %>% round(4)
    rownames(classic_coefs) <- c(percent(lower_limit), percent(upper_limit))
    
    return(classic_coefs)
  }
  
  #Comparision plots of confidence intervals
  comparision_plot <- function(data_boot, N, data, j) {
    
    #Vector of bootstrap method confidence intervals
    parameter_coefVector <- boot_sample_func(data_boot, N, j)
    model_classic <- lm(data)
    coefs_model_classic <- summary(model_classic)$coefficients
    
    comp_plot <- tibble(bootstrap = parameter_coefVector,
                        classic = 
                          coefs_model_classic[j,1] + 
                          rnorm(100) * 
                          coefs_model_classic[j,2]) %>%
      gather(type, parameter)
    
    return(comp_plot)
  }
  
  #val_input validation, if isn't numeric or is smaller than lower make it equal lower if is higher than bigger make it equal bigger
  #otherwise make it equal other
  validate_func <- function(lower,bigger,other, val_input) {
    if(!is.numeric(val_input)) {
      lower
    } else if(!(is.null(val_input) || is.na(val_input))){
      
      if(val_input < lower) {lower}
      else if(val_input > bigger) {bigger}
      else  {isolate(val_input)}
      
    } else {other}
  }
  ####OK#########################Uploaded data info##############################
  #Add uploaded file to list of dt's
  observe({
    listDT$Data[["Uploaded"]] <- delString()
  })
  
  #Valueboxes (nrow, ncol)
  observeEvent(input$file_name1, {  
    #Number of rows
    output[["numbrows"]] <- renderUI ({
      if(is.null(file$file_name1))return()
      file <- contentFile()
      valueBox(
        formatC(nrow(file), format="d", big.mark=',')
        ,'Number of rows'
        ,icon = icon("bars")
        ,color = "purple")
      
    })
    #Number of variables
    output[["numbcols"]] <- renderUI ({
      if(is.null(file$file_name1))return()
      file <- contentFile()
      valueBox(
        formatC(ncol(file), format="d", big.mark=',')
        ,'Number of variables'
        ,icon = icon("columns")
        ,color = "green")  
    })
    
  })
  
  #Basic statics button
  output[["button1"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    
    actionButton(inputId="basicStats", label="Basic Statistics",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width: 10%")
  })
  
  #Basic statics
  observeEvent(input$basicStats, {
    #If someone click on statics button view summary div
    toggle('summary_div')
    output$summary <- function() {
      summary(contentFile()) %>% 
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
    }
    
  })
  
  ##########################Render DT########################
  
  observeEvent(input$file_name1, {
    #Save file in variable
    vals$Data<-data.table(contentFile())
  })
  output$MainBody<-renderUI({
    if(is.null(file$file_name1))return()
    
    fluidPage(
      box(width=12,
          h3(strong("Uploaded file"),align="center"),
          hr(),
          column(6,offset = 6,
                 HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                 actionButton(inputId = "Add_row_head",label = "Add a new row"),
                 actionButton(inputId = "Del_row_head",label = "Delete selected rows"),
                 HTML('</div>')
          ),
          
          column(12,div(style = "height:500px; overflow-y: scroll;overflow-x: scroll;", dataTableOutput('Main_table'))),
          tags$script(HTML('$(document).on("click", "input", function () {
                           var checkboxes = document.getElementsByName("row_selected");
                           var checkboxesChecked = [];
                           for (var i=0; i<checkboxes.length; i++) {
                           if (checkboxes[i].checked) {
                           checkboxesChecked.push(checkboxes[i].value);
                           }
                           }
                           Shiny.onInputChange("checked_rows",checkboxesChecked);
  })')),
      tags$script("$(document).on('click', '#Main_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
});")

      )
      )
    })
  
  output$Main_table<-renderDataTable({
    DT=vals$Data
    DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$Data),'"><br>')
    
    DT[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Delete</button>
             <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(vals$Data),'>Modify</button>
             </div>
             
             ')
    datatable(DT,
              escape=F)}
      )
  
  observeEvent(input$Add_row_head,{
    vals$Data<-rbind(vals$Data,vals$Data[nrow(vals$Data),])
  })
  
  
  observeEvent(input$Del_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$checked_rows))
    
    vals$Data=vals$Data[-row_to_del]}
  )
  
  
  ##Managing in row deletion
  modal_modify<-modalDialog(
    fluidPage(
      h3(strong("Row modification"),align="center"),
      hr(),
      div(style = "overflow-x: scroll;", dataTableOutput('row_modif')),
      actionButton("save_changes","Save changes"),
      
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       Shiny.onInputChange('newValue', list_value)
                       });"))
    ),
    size="l"
      )
  
  
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete")
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   vals$Data=vals$Data[-row_to_del]
                 }
                 else if (input$lastClickId%like%"modify")
                 {
                   showModal(modal_modify)
                 }
               }
  )
  
  output$row_modif<-renderDataTable({
    selected_row=as.numeric(gsub("modify_","",input$lastClickId))
    old_row=vals$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row))
    {
      if (is.numeric(vals$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" type="number" id=new_',i,'><br>')
      }
      else
        row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i,'><br>')
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=rbind(old_row,row_change)
    rownames(DT)<-c("Current values","New values")
    DT
    
  },escape=F,options=list(dom='t',ordering=F),selection="none"
  )
  
  
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 colnames(DF)=colnames(vals$Data)
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickId))]<-DF
                 
               }
  )
  
  #################################Charts#############################
  
  ###Histograms##
  #Update model select
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "hist_model", choices = "Pending Upload")
    else
      updateSelectInput(session, "hist_model", choices = names(listDT$Data))
    
  })
  #Update variable select
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "histSelect", choices = "Pending Upload")
    else
      updateSelectInput(session, "histSelect", choices = names(listDT$Data[[input$hist_model]]))
  })
  
  output$hist <- reactivePlot(function() {
    
    x<-listDT$Data[[input$hist_model]][,input$histSelect]
    x <- as.numeric(unlist(x))
    #Check if x is continous variable
    if (!is.numeric(x) | is.na(x)) {
      createAlert(session, "alarm", alertId = "uwaga", 
                  title = "Uwaga: ",
                  content = "Histogram przyjmuje tylko wartosci ciagle!", 
                  style = "danger", dismiss = TRUE, append = TRUE)
    }
    if (is.numeric(x)) {
      closeAlert(session, "niebezpieczenstwo")
    }
    
    req(is.numeric(x))
    hist(x, breaks = input$breaks, col = '#6495ED', border = "white", xlab = input$histSelect, ylab= "count", main = paste("Histogram of",input$histSelect))
  })
  
  output[["boxHist"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    box(
      title = "Histogram", width = 7, status = "primary",collapsible = T, solidHeader = T,
      bsAlert("alarm"),
      plotOutput(outputId = "hist"),
      div(style="float:right;",
          uiOutput("downloadHist_butt")
      )
      
    )
  })
  
  
  ###Correlation matrix##
  
  #Update model select
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "matrix_model", choices = "Pending Upload")
    else
      updateSelectInput(session, "matrix_model", choices = names(listDT$Data))
    
  })
  #Update variable select
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "matrixSelect", choices = "Pending Upload")
    else
      updateSelectInput(session, "matrixSelect", choices = names(listDT$Data[[input$matrix_model]]))
  })
  
  output$matrix <- reactivePlot(function() {
    
    y<-listDT$Data[[input$matrix_model]][,input$matrixSelect]
    y <- as.numeric(unlist(y))
    
    x<-listDT$Data[[input$matrix_model]]
    x <- x[ , -which(names(x) %in% input$matrixSelect)]
    
    xy <- data.frame(y,x)
    if(input$matrix=="independent")
    {
      M<-cor(x)
      col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
      plot <- corrplot(M, method="color", col=col(200),  
                       type="upper", order="hclust", 
                       addCoef.col = "black", # Add coefficient of correlation
                       tl.col="black", tl.srt=45, #Text label color and rotation
                       # Combine with significance
                       sig.level = 0.01, insig = "blank", 
                       # hide correlation coefficient on the principal diagonal
                       diag=FALSE)
      
    } else {
      M<-cor(xy[,1:length(xy)])[1,2:length(xy), drop=FALSE]
      rownames(M) <- input$matrixSelect
      col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
      plot <- corrplot(M, cl.pos='n',method="color", col=col(200),
                       type="upper",addCoef.col = "black",sig.level = 0.01, insig = "blank",
                       tl.col="black", tl.srt=45)
    }
  })
  
  output[["boxMatrix"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    box(
      title = "Correlation matrix", width = 7, status = "primary",collapsible = T, solidHeader = T,
      plotOutput(outputId = "matrix", width = "100%"),
      div(style="float:right;",
          uiOutput("downloadCorrMatrix_butt")
      )
    )
    
  })
  
  ##Correlation plots##
  #Update model select
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "corrplot_model", choices = "Pending Upload")
    else
      updateSelectInput(session, "corrplot_model", choices = names(listDT$Data))
    
  })
  #Update variable select
  observe({
    if(is.null(file$file_name1))
    {
      updateSelectInput(session, "yAxis", choices = "Pending upload")
      updateSelectInput(session, "xAxis", choices = "Pending upload")
      updateSelectInput(session, "colorSelect", choices = c("Default" = "black"))
      updateSelectInput(session, "shapeSelect", choices = c("Default" = 20))
    }
    else
    {
      updateSelectInput(session, "yAxis", choices = names(listDT$Data[[input$corrplot_model]]))
      updateSelectInput(session, "xAxis", choices = names(listDT$Data[[input$corrplot_model]]))
      updateSelectInput(session, "colorSelect", choices = c("Default" = "black",names(contentFile())))
      updateSelectInput(session, "shapeSelect", choices = c("Default" = 20, names(contentFile())))
    }
    
  })
  
  output$corrPlot <- reactivePlot(function() {
    x<-listDT$Data[[input$corrplot_model]]
    
    #SE
    if(input$se == 'yes')
      se = T
    else
      se =F
    
    #Shape
    if(input$shapeSelect== 20)
      shape = 20
    else
      shape = x[,input$shapeSelect]
    
    #Color
    if(input$colorSelect=="black")
    {
      plot <- ggplot(x, aes(x = x[,input$xAxis], y = x[,input$yAxis], shape = factor(shape))) +
        geom_point() +
        labs(x = input$xAxis,
             y = input$yAxis)  +
        scale_shape_discrete(name  = input$shapeSelect)
    }
    else
    {
      plot <- ggplot(x, aes(x = x[,input$xAxis], y = x[,input$yAxis], col = factor(x[,input$colorSelect]), shape = factor(shape))) +
        geom_point() +
        labs(x = input$xAxis,
             y = input$yAxis) +
        scale_colour_discrete(name  = input$colorSelect) +
        scale_shape_discrete(name  = input$shapeSelect)
    }
    
    
    #Smooth
    if(input$smooth == "yes")
      plot <- plot + stat_smooth(method = input$method, se = se)
    
    if(input$whiteNoise == "yes")
      plot <- plot + geom_jitter()
    
    return(plot)
  })
  
  output[["boxcorrPlot"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    box(
      title = "Correlation plot", width =8, status = "primary", collapsible = T, solidHeader = T,
      plotOutput(outputId = "corrPlot", width = "100%"),
      div(style="float:right;",
          uiOutput("downloadCorrPlot_butt")
      )
    )
    
  })
  
  ##Others##
  #Update model select
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "others_model", choices = "Pending Upload")
    else
      updateSelectInput(session, "others_model", choices = names(listDT$Data))
    
  })
  #Update variable select
  observe({
    
    if(is.null(file$file_name1))
    {
      updateSelectInput(session, "yAxisOther", choices = "Pending upload")
      updateSelectInput(session, "xAxisOther", choices = "Pending upload")
    }
    else
    {
      updateSelectInput(session, "yAxisOther", choices = names(listDT$Data[[input$others_model]]))
      updateSelectInput(session, "xAxisOther", choices = names(listDT$Data[[input$others_model]]))
    }
    
  })
  
  output$otherCharts <- reactivePlot(function() {
    
    x<-listDT$Data[[input$others_model]]
    
    #SE
    if(input$chartSelect == 'boxplot')
    {
      plot <- ggplot(x, aes(x=x[,input$xAxisOther], y=x[,input$yAxisOther], fill=factor(x[,input$xAxisOther])))+
        geom_boxplot(alpha=0.3) +
        theme(legend.position="none") + labs(x = input$xAxisOther,
                                             y = input$yAxisOther)
    } else if (input$chartSelect == 'density') {
      plot <- ggplot(x, aes(x=x[,input$xAxisOther], group=x[,input$yAxisOther], fill=factor(x[,input$yAxisOther]))) +
        geom_density(adjust=1.5 , alpha=0.3) + labs(x = input$xAxisOther,
                                                    y = input$yAxisOther)
    } else if (input$chartSelect == 'densityRid') {
      plot <- ggplot(x, aes(x=x[,input$xAxisOther], y=x[,input$yAxisOther], fill=factor(x[,input$yAxisOther]))) +
        geom_density_ridges() +
        theme_ridges() +
        theme(legend.position = "none") + labs(x = input$xAxisOther,
                                               y = input$yAxisOther)
    } else {
      plot <- ggplot(x, aes(x=x[,input$xAxisOther], y=x[,input$yAxisOther], fill=factor(x[,input$xAxisOther]))) +
        geom_violin(alpha=0.6) +
        theme(legend.position="none") + labs(x = input$xAxisOther,
                                             y = input$yAxisOther)
    }
    return(plot)
    
  })
  
  output[["boxotherCharts"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    box(
      title = firstup(paste(input$chartSelect)), width = 8, status = "primary",collapsible = T, solidHeader = T,
      plotOutput(outputId = "otherCharts", width = "100%"),
      div(style="float:right;",
          uiOutput("downloadOtherChart_butt")
      )
    )
    
  })
  
  ####OK##################Read file#########################
  #Read file
  observeEvent(input$file_name1, {
    file$file_name1=input$file_name1
  })
  
  ####OK##################Delete file################
  #Delete file with button
  observeEvent(input$deleteFile1, {
    reset('file_name1')
    file$file_name1=NULL
    hide('summary_div')
  })
  
  ######################Download plot#################
  output$downloadHist <- downloadHandler(
    filename = 'hist.png',
    content = function(file) {
      png(file, width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = NA)
      x<-contentFile()[, input$histSelect]
      x <- as.numeric(unlist(x))
      hist(x, breaks = input$breaks, col = '#6495ED', border = "white", xlab = input$histSelect, ylab= "count", main = paste("Histogram of",input$histSelect))
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  output$downloadCorrPlot <- downloadHandler(
    filename = 'hist.png',
    content = function(file) {
      png(file, width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = NA)
      corrik
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  output$downloadCorrMatrix <- downloadHandler(
    filename = 'hist.png',
    content = function(file) {
      png(file, width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = NA)
      x<-contentFile()[, input$histSelect]
      x <- as.numeric(unlist(x))
      hist(x, breaks = input$breaks, col = '#6495ED', border = "white", xlab = input$histSelect, ylab= "count", main = paste("Histogram of",input$histSelect))
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  output$downloadOtherChart <- downloadHandler(
    filename = 'hist.png',
    content = function(file) {
      png(file, width = 1000, height = 1000, units = "px", pointsize = 12, bg = "white", res = NA)
      x<-contentFile()[, input$histSelect]
      x <- as.numeric(unlist(x))
      hist(x, breaks = input$breaks, col = '#6495ED', border = "white", xlab = input$histSelect, ylab= "count", main = paste("Histogram of",input$histSelect))
      
      dev.off()
    },
    contentType = 'image/png'
  )
  #######################Charts Download buttons################
  output[["downloadHist_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    
    downloadButton("downloadHist", "Download",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;")
  })
  
  output[["downloadCorrPlot_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    
    downloadButton("downloadCorrPlot", "Download",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width: 100%")
  })
  
  output[["downloadCorrMatrix_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    
    downloadButton("downloadCorrMatrix", "Download",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width: 100%")
  })
  
  output[["downloadOtherChart_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    
    downloadButton("downloadOtherPlot", "Download",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width: 100%")
  })
  ######################Model#####################
  #Check if someone deleted file
  observe({
    x <- contentFile()
    if(is.null(file$file_name1))
    {
      updatePickerInput(session,"modelIndVar",choices="Pending Upload")
      updateSelectInput(session, "modelDepVar", choices = "Pending Upload")
    }
    else
    {
      updatePickerInput(session,"modelIndVar",choices=names(x))
      updateSelectInput(session, "modelDepVar", choices = names(x))
    }
  })
  
  #Save chosen model button
  output[["downloadSaveModel_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "button", style = "float:left;width:40%",
        actionButton(inputId="saveModel", label="Save model",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear result button
  output[["clearModel_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "button", style = "float:left; padding-left:5px;width:25%",
        actionButton(inputId="clearModel", label="Clear",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear model data table
  observeEvent(input$clearModel, {
    output[["modelDTX"]] <- NULL
  })
  
  #Download model button
  observeEvent(input$saveModel, {
    output[["downloadModelDT_butt"]] <- renderUI({
      
      if(is.null(file$file_name1))return()
      downloadButton("downloadModelDT", "Download",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;")
    })
  })
  
  #Download model DT
  output$downloadModelDT <- downloadHandler(
    filename = function() {
      paste("model.xlsx")
    },
    content = function(file) {
      
      write.xlsx(downloadData(), file)
      
    }
  )
  
  #Save file in variable
  observeEvent(input$file_name1, {
    model$Data<-data.table(contentFile())
  })
  
  #Render DT on click saveModel button 
  observeEvent(input$saveModel, {
    
    if(input$modelName %in% names(listDT$Data))
    {
      showNotification(paste("Model wasn't saved. Name already exist"), duration = 5, type = "warning")
      
    } else if(!is.null(input$modelIndVar) & !(input$modelDepVar %in% input$modelIndVar)) {
      
      #Prepare Independent and Dependent variable
      y<-contentFile()[, input$modelDepVar]
      y <- as.numeric(unlist(y))
      y <- as.data.frame(y)
      colnames(y) <- input$modelDepVar
      
      x<-contentFile()
      x <- x[ , input$modelIndVar[1:length(input$modelIndVar)]]
      x <- lapply(x, unlist)
      x <- lapply(x, as.numeric)
      
      x <- as.data.frame(x)
      
      yx <- data.frame(y,x)
      listDT$Data[[input$modelName]] <- yx
      savedModel$Data<-yx
      downloadRV$df <- yx
      
      output$modelDT <- renderDataTable({
        datatable(yx,options = list(scrollX = TRUE))
        
      })
      
      #Render saved model DT in UI
      output[["modelDTX"]] <- renderUI({
        
        if(is.null(file$file_name1))return()
        box(title = "Chosen model", width = 9,status = "primary",collapsible = T, solidHeader = T,
            dataTableOutput('modelDT'),
            div(style="float:right;",
                uiOutput("downloadModelDT_butt")
            )
        )
        
        
        
      })
    }
  })
  
  
  ########################Static Model########################
  #Check if someone deleted file
  observe({
    if(is.null(file$file_name1))
    {
      updateSelectInput(session, "statisticMethod_model", choices = "Pending Upload")
      updateSelectInput(session, "statisticMethod", choices = "Pending Upload")
    }
    else
    {
      updateSelectInput(session, "statisticMethod_model", choices = names(listDT$Data))
      updateSelectInput(session, "statisticMethod", choices = c("Hellwig" = "hellwig",
                                                                "Graph method" = "graphMethod"))
    }
  })
  
  #Submit button
  output[["statisticMethod_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "static_button", style = "float:left;width:25%",
        actionButton(inputId="staticMethod", label="Submit",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width: 100%")
    )
  })
  
  #Save static model button
  output[["saveStaticModel_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "static_button", style = "float:left; padding-left:5px;width:25%",
        actionButton(inputId="saveStaticModel", label="Save",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear static model button
  output[["clearStaticModel_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "static_button", style = "float:left; padding-left:5px;width:25%",
        actionButton(inputId="clearStaticModel", label="Clear",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear statistic method results
  observeEvent(input$clearStaticModel, {
    output[["hellwig_resultsDT"]] <- NULL
    output[["model_Hellwig"]] <- NULL
  })
  
  observeEvent(input$staticMethod, {
    varIn <- listDT$Data[[input$statisticMethod_model]][,-1]
    varDep <- listDT$Data[[input$statisticMethod_model]][,1]
    if(input$statisticMethod=="graphMethod")
    {
      
      output$graphMatrix <- reactivePlot(function(){
        wspKor <- cor(varDep)
        
        t_alfa <- 2.7764
        warKryt <- sqrt(t_alfa^2/(ncol(varDep) - 2 + t_alfa^2))
        
        
        for(i in 1:ncol(varDep))
        {
          for(j in 1:ncol(varDep))
          {
            if(i!=j)
              if(abs(wspKor[i,j])<=warKryt)
                wspKor[i,j] = 0
          }
        }
        
        col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
        plot <- corrplot(wspKor, method="color", col=col(200),  
                         type="upper", order="hclust", 
                         addCoef.col = "black", # Add coefficient of correlation
                         tl.col="black", tl.srt=45, #Text label color and rotation
                         # Combine with significance
                         sig.level = 0.01, insig = "blank", 
                         # hide correlation coefficient on the principal diagonal
                         diag=FALSE)
        
      })
      
    } else if(input$statisticMethod=="hellwig") {
      
      #Hellwig method, sort and get max
      hellwig <- hellwig(varDep,varIn)
      sortedHellwig <- arrange(hellwig,desc(h))
      maxHellwig <- sortedHellwig[which.max(sortedHellwig$h),]
      maxHellwig_unlisted <- unlist(maxHellwig[1])
      
      #separate variables from hellwig mehtod
      colMaxHellwig <- read.table(text = maxHellwig_unlisted, sep = "-", colClasses = "character")
      
      #Hellwig method data.frame
      hellwigOutput <- data.frame(varDep, varIn[,as.numeric(colMaxHellwig)])
      colMaxHellwig <- as.numeric(colMaxHellwig) + 1
      colnames(hellwigOutput)<- colnames(listDT$Data[[input$statisticMethod_model]])[c(1,as.numeric(colMaxHellwig))]
      staticModel$Data <- hellwigOutput
      
      output$hellwigResults <- renderDataTable({datatable(sortedHellwig,options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 20, 50,100)
      ))
      })
      
      output$hellwigDT <- renderDataTable({data.table(hellwigOutput)})
      
      #Hellwig method results DT
      output[["hellwig_resultsDT"]] <- renderUI({
        box(
          title = "Hellwig results", width = 12, status = "primary",collapsible = T, solidHeader = T,
          dataTableOutput("hellwigResults")
        )
      })
      
      #Chosen model DT
      output[["model_Hellwig"]] <- renderUI({
        box(
          title = "Hellwig Data Table", width = 8, status = "primary",collapsible = T, solidHeader = T,
          dataTableOutput("hellwigDT")
        )
      })
      
    }
  })
  
  #Save static model
  observeEvent(input$saveStaticModel, {
    if(input$statisticMethod == "hellwig")
      listDT$Data[[paste(input$statisticMethod_model, "hellwig", sep = "_")]] <- staticModel$Data
    else
      listDT$Data[[paste(input$statisticMethod_model, "graphMethod", sep = "_")]] <- staticModel$Data
  })
  
  #TODO
  output[["graphMatrix_output"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    box(
      title = "Histogram", width = 7, status = "primary",collapsible = T, solidHeader = T,
      bsAlert("alarm"),
      plotOutput(outputId = "graphMatrix"),
      div(style="float:right;",
          uiOutput("downloadHist_butt")
      )
      
    )
  })
  
  ########################Bootstrap######################
  #Check if file exist
  observe({
    if(is.null(file$file_name1))
      updateSelectInput(session, "boostrapMethod_model", choices = "Pending Upload")
    else
      updateSelectInput(session, "boostrapMethod_model", choices = names(listDT$Data))
  })
  
  #Alpha validation, if isn't numeric than make it equal 0.5 if is smaller than 0 alpha = 0.001
  #if bigger than 0.999 alpha = 0.999 otherwise alpha = 0.7
  observeEvent(input$alfa,{
    updateNumericInput(session, "alfa", value = ({
      if(!is.numeric(input$alfa)) {
        0.5
      } else if(!(is.null(input$alfa) || is.na(input$alfa))){
        
        if(input$alfa < 0) {0.001}
        else if(input$alfa > 0.999) {0.999}
        else  {isolate(input$alfa)}
        
      } else {0.7}
    })
    )
  })
  
  #Bootstrap trails validation, if isn't numeric or is smaller than 100 make it equal 100,
  #if is higher than 10000 make it equal 10000 otherwise make it equal 100
  observeEvent(input$bootstrapTrials,{
    updateNumericInput(session, "bootstrapTrials", value = validate_func(100,10000,100,input$bootstrapTrials)
    )
  })
  
  #Submit button - button to use bootstrap method
  output[["bootstrapMethod_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "bootstrap_button", style = "float:left;width:25%",
        actionButton(inputId="bootstrapMethod", label="Submit",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Save bootstrap model button
  output[["saveBootstrapModel_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "bootstrap_button", style = "float:left; padding-left:5px;width:25%",
        actionButton(inputId="saveBootstrapModel", label="Save",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear result button
  output[["clearBootstrapModel_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "bootstrap_button", style = "float:left; padding-left:5px;width:25%",
        actionButton(inputId="clearBootstrapModel", label="Clear",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear bootstrap result
  observeEvent(input$clearBootstrapModel, {
    output$summary2 <- NULL
  })
  
  #Bootstrap method - render summary
  observeEvent(input$bootstrapMethod, {
    alpha <- 0.035
    #Number of bootstrap trails
    N <- 100
    #Chosen model by user
    bootstrap_dataset <- listDT$Data[[input$boostrapMethod_model]]
    
    #Repeat bootstrap method until there is no variable with confidential interval which include 0
    repeat{
      bootstrap_coefs <- sapply(rep(NA, N), function(x){
        boot_sample <- sample(1:nrow(bootstrap_dataset), 
                              nrow(bootstrap_dataset), 
                              replace = T)
        model <- lm(data = bootstrap_dataset[boot_sample, ])
        coef(model)
      })
      conf_interval <- apply(bootstrap_coefs, 1, function(x){quantile(x, c(alpha/2, 1-alpha/2))})
      
      #Check if any of variable confidential interval include 0
      zero_coefs <- apply(conf_interval, 2, function(x){ifelse(x[1]*x[2] <= 0, TRUE, FALSE)})[-1]
      #Sum all those variables
      sum <- sum(zero_coefs)
      
      #If there is no such a variable end method
      if(sum==0) break
      #If there is only one variable with 0 in confidential interval variable
      else if(sum==1) bootstrap_dataset <- bootstrap_dataset[-(which(zero_coefs, arr.ind = TRUE)+1)]
      #Otherwise continue
      else{
        delta <- 4*alpha
        alpha_t <- alpha + delta
        repeat{
          conf_interval <- apply(bootstrap_coefs, 1, function(x){quantile(x, c(alpha_t/2, 1-alpha_t/2))})
          zero_coefs <- apply(conf_interval, 2, function(x){ifelse(x[1]*x[2] <= 0, TRUE, FALSE)})[-1]
          sum <- sum(zero_coefs)
          
          if(sum==1){
            bootstrap_dataset <- bootstrap_dataset[-(which(zero_coefs, arr.ind = TRUE)+1)]
            break
          }
          else if(sum==0){
            delta <- delta/2
            alpha_t <- alpha_t - delta
          }
          else{
            alpha_t <- alpha_t + delta
          }
        }
      }
    }
    
    colnames(bootstrap_dataset[-1])
    
    
    #### Created model
    bootstrapModel$Data <- bootstrap_dataset
    bootstrap_model <- lm(data = bootstrap_dataset)
    bootstrap_summary <- summary(bootstrap_model)
    output$summary2 <- renderPrint({bootstrap_summary})
    
    
  })
  
  #Save bootstrap model as current model name with "_bootstrap"
  observeEvent(input$saveBootstrapModel, {
    listDT$Data[[paste(input$boostrapMethod_model, "bootstrap", sep = "_")]] <- bootstrapModel$Data
  })
  ####OK##################Compare###############
  #If file exist update select inputs with names of data tables
  observe({
    if(is.null(file$file_name1))
    {
      updateSelectInput(session, "compare_model1", choices = "Pending Upload")
      updateSelectInput(session, "compare_model2", choices = "Pending Upload")
    }
    else
    {
      updateSelectInput(session, "compare_model1", choices = names(listDT$Data))
      updateSelectInput(session, "compare_model2", choices = names(listDT$Data))
    }
  })
  
  #Density size validation, if isn't numeric or is smaller than 1 make it equal 1 if is higher than 5 make it equal 5
  #otherwise make it equal 2
  observeEvent(input$densitySize,{
    updateNumericInput(session, "densitySize", value = validate_func(1,5,2,input$densitySize)
    )
  })
  
  #Bootstrap trails validation, if isn't numeric or is smaller than 100 make it equal 100,
  #if is higher than 10000 make it equal 10000 otherwise make it equal 100
  observeEvent(input$compareTrials,{
    updateNumericInput(session, "compareTrials", value = validate_func(100,10000,100,input$compareTrials)
    )
  })
  
  #Submit button - to compare which method is better
  output[["compare_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "compare_button", style = "float:left;width:25%",
        actionButton(inputId="compare", label="Submit",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear button - to clear the result
  output[["clearCompare_butt"]] <- renderUI({
    
    if(is.null(file$file_name1))return()
    div(id = "compare_button", style = "float:left; padding-left:5px;width:25%",
        actionButton(inputId="clearCompare", label="Clear",style="color:#fff; background-color: #337ab7; border-color: #2e6da4;width:100%")
    )
  })
  
  #Clear data tables and plots
  observeEvent(input$clearCompare,{
    output[["coef_DT"]] <- NULL
    output[["coef_plot"]] <- NULL
  })
  
  #Comparisions of confidence intervals DT
  observeEvent(input$compare,{
    output[["coef_DT"]] <- renderUI({
      
      #Names of tab panels
      confidenceInterval <- c("90% confidence interval", "95% confidence interval", "99% confidence interval")
      #Rownames
      lowerLimits <- c(0.05, 0.025, 0.005)
      upperLimits <- c(0.95,0.975,0.995)
      
      #Do three tab panels includes data tables with comparision of two methods for diffrent confidence interval
      do.call(tabsetPanel, c(id='t',lapply(1:3, function(i) {
        
        #bootstrap method confidence interval data table
        boot_limits <- comparision_boot(listDT$Data[[input$compare_model1]], input$compareTrials, lowerLimits[i], upperLimits[i])
        #classic method confidence interval data table
        classic_limits <- comparision_classic(listDT$Data[[input$compare_model2]], lowerLimits[i], upperLimits[i])
        
        tabPanel(
          title=confidenceInterval[i],
          h3("Bootstrap model limits"),
          #Render bootstrap method data table
          renderDataTable(datatable(boot_limits,options = list(dom = 't',searching = FALSE))),
          h3("Classic model limits"),
          #Render classic method data table
          renderDataTable(datatable(classic_limits,options = list(dom = 't',searching = FALSE)))
          
        )
      })
      ))
    })
    
    #Confidence interval plots
    output[["coef_plot"]] <- renderUI({
      
      variable_names <- names(listDT$Data[[input$compare_model1]])
      variable_names[1] <- "(Intercept)"
      
      #Make as many tab panels as is variables + 1
      do.call(tabsetPanel, c(id='t',lapply(1:ncol(listDT$Data[[input$compare_model1]]), function(i) {
        
        tabPanel(
          title=variable_names[i],
          h3("Comparision"),
          #Render from tibble (comparision_plot function) plot to i-tabpanel
          renderPlot({
            comp_plot <- comparision_plot(listDT$Data[[input$compare_model1]], input$compareTrials, listDT$Data[[input$compare_model2]], i)
            comp_plot %>% ggplot(aes(x = parameter, col = type), main="Porownanie wykresow gestosci-waga") + geom_density(size = input$densitySize)
          })
        )
      })
      ))
    })
  })
  
  }
