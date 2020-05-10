MonthPayment = 30
YearPayment = 100


notPaid <- function(vals, data, time_period = "month") {
  
  cols = ifelse(time_period == "month", -1, 1)
  
  if(length(vals) == 1)
  {
    notPaid = data %>% filter_at(vars(data[,str_detect(names(data),"wplata")] %>% colnames() %>% .[cols]), any_vars(. == vals[1]))
    
    if(cols == -1)
      notPaid_dates = apply(notPaid[,str_detect(names(notPaid),"wplata")] %>% .[,cols], 1, function(x){names(x[which(x == vals[1])])}) %>% as.list()
    
  } else {
    notPaid = data %>% filter_at(vars(data[,str_detect(names(data),"wplata")] %>% colnames() %>% .[cols]), any_vars(. > vals[1] & . < vals[2]))   
    
    if(cols == -1)
      notPaid_dates = apply(notPaid[,str_detect(names(notPaid),"wplata")] %>% .[,cols], 1, function(x){names(x[which((x > vals[1]) & (x < vals[2]))])}) %>% as.list()
  }
  
  if(cols == -1){
    notPaid_dates = lapply(notPaid_dates, function(x, y = notPaid){paste(gsub("wplata_","",x),collapse = ", ") }) %>% list.rbind()
    
    notPaid_dates = gsub("_","-", notPaid_dates)
  }
  
  notPaid_df = data.frame(Imie = notPaid$imie, Nazwisko = notPaid$nazwisko,"Imie opiekuna" = notPaid$imie_opiekuna,
                          "Nazwisko opiekuna" = notPaid$nazwisko_opiekuna, "Numer opiekuna" = notPaid$telefon_opiekuna,
                          "Email opiekuna" = notPaid$email_opiekuna)
  return(notPaid_df)
}

#Add to bs4dash possibility to use offset in column function
column <- function(width, ..., offset = 0) {
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0) 
    colClass <- paste0(colClass, " offset-sm-", offset)
  shiny::div(class = colClass, ...)
}

#Users passwords
credentials <- data.frame(
  user = c("basia"), # mandatory
  password = c("123"), # mandatory
  email = "barjal@poczta.inet.pl",
  name = "Barbara",
  surname = "Jałocha",
  sex = "Kobieta",
  clients_number = 52,
  groups = 3,
  stringsAsFactors = FALSE
)
USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

server = function(input, output, session ){
  
  ############################Reactive Values############################
  filesData = reactiveValues(data = data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB")))
  templatesData = reactiveValues(data =  dbReadTable(databaseConnection, "maile"))
  clients = reactiveValues(data = dbReadTable(databaseConnection, "uczestnicy"))
  excelData <- reactiveValues(data = NULL)
  csvData <- reactiveValues(data = NULL)
  
  ###################################Calnedar###############################
  #Calendar schedules
  calendar_schedule = reactiveValues(schedules = data.frame(calendarId = NULL,title = NULL,body = NULL, start = NULL,
                                                            end = NULL, category = NULL))
  
  #Creating calendar
  output$calendar <- renderCalendar({
    calendar(defaultView = "month", taskView = TRUE, scheduleView = c("time", "allday")) %>% 
      set_calendars_props(id = "courses", name = "Courses", color = "#FFF", bgColor = "#E41A1C") %>% 
      set_calendars_props(id = "hobbies", name = "Hobbies", color = "#FFF", bgColor = "#377EB8") %>% 
      set_calendars_props(id = "social", name = "Social", color = "#FFF", bgColor = "#4DAF4A") %>% 
      set_calendars_props(id = "birthday", name = "Birthday", color = "#FFF", bgColor = "#EEE8AA")
  })
  
  #Setting view of calendar
  observeEvent(input$monthButton,
               cal_proxy_view("calendar", "month"),
               ignoreInit = TRUE
  )  
  observeEvent(input$weekButton,
               cal_proxy_view("calendar", "week"),
               ignoreInit = TRUE
  )     
  observeEvent(input$dayButton,
               cal_proxy_view("calendar", "day"),
               ignoreInit = TRUE
               
  )
  
  #Moving on the calendar
  observeEvent(input$calendarToday,
               cal_proxy_today("calendar"),
               ignoreInit = TRUE
  )  
  observeEvent(input$calendarPrevious,
               cal_proxy_prev("calendar"),
               ignoreInit = TRUE
  )     
  observeEvent(input$calendarNext,
               cal_proxy_next("calendar"),
               ignoreInit = TRUE
               
  )
  
  #Adding event to calendar
  observeEvent(input$createEvent,
               cal_proxy_create("calendar", id = "social", start = sprintf("%s %s",as.character(input$eventDate[1]),format(input$startTime, "%H:%M")),
                                end = sprintf("%s %s",as.character(input$eventDate[2]),format(input$endTime, "%H:%M")), title = input$eventName,body = input$eventBody,category = "time"),
               ignoreInit = TRUE
               
  )
  
  ###################################Files###############################
  #Upload file to dropbox
  observeEvent(input$uploadFile,{
    #File name correction
    file = file.rename(input$file$datapath, paste0(input$file$name))
    file = input$file$name
    #Upload file to dropbox
    drop_upload(file)
  })
  
  #Display files uploaded to Dropbox
  output$files_dropbox = renderDataTable({
    filesData$data
    
  })
  
  #Update the files in pickerInput after loading a new one
  observeEvent(input$uploadFile,{
    updatePickerInput(session = session, inputId = "filesNames",
                      choices =drop_dir()$name)
  })
  
  #Update the files shown after loading a new one
  observeEvent(input$uploadFile,{
    filesData$data <- data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB"))
  })
  
  #Render filesPicker with file names as choices
  output[["filesPicker"]] <- renderUI ({
    pickerInput(
      inputId = "filesNames", 
      label = "Wybierz pliki do pobrania", 
      choices = drop_dir()$name, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  #Donload file from Dropbox
  observeEvent(input$downloadFile,{
    for(file in input$filesNames)
      drop_download(file, overwrite = TRUE)
  })
  
  ###################################Mails###############################
  #Save template to database
  observeEvent(input$saveTemplate,{
    dbWriteTable(databaseConnection, "maile", data.frame(Name = input$templateNameCreate, Body = input$templateBodyCreate), append = TRUE)
    templatesData$data = dbReadTable(databaseConnection, "maile")
  })
  
  #Release the "templates picker / edit" with the names of the created templates  
  output[["templateNameEdit"]] <- renderUI ({
    pickerInput(
      inputId = "templateNameEdit", 
      label = "Wybierz szablon do edycji", 
      choices = templatesData$data$Name, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = FALSE
    )
  })
  
  #Release the "templates body / edit" with the names of the created templates  
  output[["templateBodyEdit"]] <- renderUI ({
    textAreaInput("templateBodyEdit", "Szablon maila", templatesData$data$Body[which(templatesData$data$Name == input$templateNameEdit)], resize = "both") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  })
  
  #Edit template body  
  observeEvent(input$editTemplate,{
    templatesData$data$Body[which(templatesData$data$Name == input$templateNameEdit)] <- input$templateBodyEdit
    dbWriteTable(databaseConnection, "maile", data.frame(Name = templatesData$data$Name, Body = templatesData$data$Body), append = TRUE)
  })
  
  #Render list of clients mails
  output[["mailList"]] <- renderUI ({
    pickerInput(
      inputId = "mailList", 
      label = "Lista maili", 
      choices = clients$data$email_opiekuna, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  #Render list of templates / send
  output[["templateList"]] <- renderUI ({
    pickerInput(
      inputId = "templateList", 
      label = "Wybierz szablon maila", 
      choices = templatesData$data$Name, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = FALSE
    )
  })
  
  #Render mail title as a name of chosen template
  output[["mailTitle"]] <- renderUI ({
    textInput("mailTitle", "Tytuł maila", input$templateList)
  })
  
  #Render content of the selected template
  output[["mailBody"]] <- renderUI ({
    textAreaInput("mailBody", "Mail",  templatesData$data$Body[which(templatesData$data$Name == input$templateList)], resize = "both") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  })
  
  #Send mail via gmail
  observeEvent(input$sendMail,{
    # Sending mail
    send.mail(from = "mateusz.jalocha2107@gmail.com",
              to = input$mailList,
              subject = input$mailTitle,
              body = input$mailBody,
              encoding = 'utf-8',
              smtp = list(host.name = "smtp.gmail.com", port = 465,
                          user.name = "mateusz.jalocha2107@gmail.com",
                          passwd = "cazhdesodtvaqrft", ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
  })
  
  ###################################Data###############################
  #Upload excel file
  observeEvent(input$uploadExcelData,{
    #Save data in reactive value
    excelData$data <- read_xlsx(input$excelFile$datapath)
    #Write and read data
    dbWriteTable(databaseConnection, "uczestnicy", unique(rbind(clients$data, excelData$data)), overwrite = TRUE)
    clients$data = data.table(dbReadTable(databaseConnection, "uczestnicy"))
  })
  
  #Upload csv file
  observeEvent(input$uploadCSVData,{
    #Save data in reactive value
    csvData$data <- read.csv(input$csvFile$datapath)
    #Write and read data
    dbWriteTable(databaseConnection, "uczestnicy", unique(rbind(clients$data, csvData$data)), overwrite = TRUE)
    clients$data = data.table(dbReadTable(databaseConnection, "uczestnicy"))
  })
  
  #Display clients data
  output$datasetTable = renderDataTable({
    data.table(clients$data,options=list(search=list(encoding = "UTF-8"),regex = TRUE))
    
  })
  ###################################Desktop###############################
  #Upload excel file
  observe({
    output$paymentsSum_month = renderText({
      sum_of_payments = sum(clients$data[paste0("wplata_",format(Sys.Date(),"%m"),"_",format(Sys.Date(),"%Y"))])
      total_payments = nrow(clients$data) * MonthPayment
      paste0(sum_of_payments," zl/",total_payments, " zl")
    })
    
  })
  
  observe({
    output$paymentsSum_year = renderText({
      sum_of_payments = sum(clients$data["wplata_roczna"])
      total_payments = nrow(clients$data) * YearPayment
      paste0(sum_of_payments," zl/",total_payments, " zl")
    })
    
  })
  
  observe({
    output$notpaidMonth = renderText({
      payments_columns = clients$data[,str_detect(names(clients$data),"wplata")]
      length(payments_columns[payments_columns == 0])
    })
    
  })
  
  observe({
    output$barPlot_payments = renderPlotly({
      monthPayments_columns = clients$data[,str_detect(names(clients$data),"wplata")] %>% .[,-1]
      yearPayment = clients$data[,str_detect(names(clients$data),"wplata")] %>% .[,1]
      notPaid_year = length(yearPayment[yearPayment<YearPayment])
      notPaid = apply(monthPayments_columns, 2, function(x){length(x[x<MonthPayment])})
      paid = nrow(monthPayments_columns) - notPaid
      date = gsub("_", "-", colnames(monthPayments_columns))
      date = gsub("wplata-", "", date)
      data = data.frame(Paid = c(paid,nrow(monthPayments_columns) - notPaid_year), Not_paid = c(notPaid,notPaid_year), Date = c(date,"wpłata roczna"))
      
      fig <- plot_ly(data, x = ~Date, y = ~Paid, type = 'bar', name = 'wplaciły')
      fig <- fig %>% add_trace(y = ~Not_paid, name = 'nie wplaciły')
      fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group')
    })
    
  })
  
  observe({
    output$notPaidMonth_list = renderDataTable({
      notPaid(0, clients$data, "month")
    })
    
  })
  
  observe({
    output$notPaidYear_list = renderDataTable({
      notPaid(0, clients$data, "year")
    })
    
  })
  
  observe({
    output$partPaidYear_list = renderDataTable({
      notPaid(c(0, YearPayment), clients$data,"year")
    })
    
  })
  
  observe({
    output$partPaidMonth_list = renderDataTable({
      notPaid(c(0,MonthPayment), clients$data,"month")
    })
    
  })
  ###################################Login############################
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  ###################################User panel########################################
  observeEvent(input$userPanelLink, {
    updatebs4TabItems(
      session,
      inputId = "sidebarMenu",
      selected = 7
    )
  })
  
  output$userName <- renderText({
    paste(reactiveValuesToList(res_auth)$name, reactiveValuesToList(res_auth)$surname, collapse = " ")
  })
  
  output$userPanelName <- renderText({
    paste("Użytkownik:", reactiveValuesToList(res_auth)$name, reactiveValuesToList(res_auth)$surname, collapse = " ")
  })
  
  output$userInformations <- renderText({
    paste(paste0("Imie: ", reactiveValuesToList(res_auth)$name), paste0("Nazwisko: ", reactiveValuesToList(res_auth)$surname),
        paste0("Email: ", reactiveValuesToList(res_auth)$email), paste0("Płeć: ", reactiveValuesToList(res_auth)$sex), collapse = "\n")
  })
  output$userInformations <- renderUI({
    HTML(paste(paste0("<b>Imie: </b> ", reactiveValuesToList(res_auth)$name), paste0("<b>Nazwisko: </b>", reactiveValuesToList(res_auth)$surname),
               paste0("<b>Email: </b> ", reactiveValuesToList(res_auth)$email), paste0("<b>Płeć: </b>", reactiveValuesToList(res_auth)$sex),
               paste0("<b>Login: </b> ", reactiveValuesToList(res_auth)$user),sep = "<br/>"))
  })
  
  output$clientsNumber <- renderText({
    paste(reactiveValuesToList(res_auth)$clients_number)
  })
  
  
  output$groupsNumber <- renderText({
    paste(reactiveValuesToList(res_auth)$groups)
  })
  
  observeEvent(input$editData, {
    showModal(modalDialog(
      title = "Esti jest kupom"
    ))
  })
  observeEvent(input$changePassword, {
    showModal(modalDialog(
      title = "Esti jest kupom2"
    ))
  })
  
  shinyjs::onclick("my_img",  showModal(modalDialog(
    title = "Esti jest kupom2"
  )))
  ###################################Expenses##############################
  expensesDataset = reactiveValues(data = data.frame(nazwa = NULL, kategoria = NULL, kwota = NULL, data = NULL))
  observeEvent(input$saveExpense,{
    expensesDataset$data <- rbind(expensesDataset$data,data.frame(nazwa = isolate(input$addExpenseName), kategoria = isolate(input$addExpenseCategory), kwota = isolate(input$addExpenseAmount), data = isolate(as.character(input$addExpenseDate))))
    output$result_add  <- DT::renderDataTable(expensesDataset$data)
    output$result  <- DT::renderDataTable(expensesDataset$data)
    shinyjs::reset("input-panel")
  })
  
  ##See expenses##
  observeEvent(input$subsetDate,{
    s <- subset(expensesDataset$data,as.Date(expensesDataset$data$data)>=as.Date(input$dates[1]) & as.Date(expensesDataset$data$data)<=as.Date(input$dates[2]))
    output$result  <- DT::renderDataTable(s)
  })
  observeEvent(input$clear,{
    output$result  <- DT::renderDataTable(expensesDataset$data)
  })
  
  
  output$piePlot <- renderPlotly({
    plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste('$', X1960, ' billions'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE)%>% 
      layout(title = 'Zestawienie miesięcznych wydatków',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  output$hist <- renderPlotly({
    p2 <- data %>%
      count(Categorie) %>%
      plot_ly(x = ~Categorie, y = ~data$X1960) %>% 
      add_bars()
    
    subplot(p2) %>% hide_legend()
  })
}
