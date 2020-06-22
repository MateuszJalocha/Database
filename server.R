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
  
  if(time_period == "month"){
    notPaid_df = data.frame(Imie = notPaid$imie, Nazwisko = notPaid$nazwisko,"Imie opiekuna" = notPaid$imie_opiekuna,
                            "Nazwisko opiekuna" = notPaid$nazwisko_opiekuna, "Numer opiekuna" = notPaid$telefon_opiekuna,
                            "Email opiekuna" = notPaid$email_opiekuna,"Daty bez wpłat" = notPaid_dates,
                            "Ile zaległych wpłat" = lengths(strsplit(notPaid_dates, ",")))
    notPaid_df = notPaid_df[order(notPaid_df$Ile.zaległych.wpłat, decreasing = TRUE),]
  }
  else{
    notPaid_df = data.frame(Imie = notPaid$imie, Nazwisko = notPaid$nazwisko,"Imie opiekuna" = notPaid$imie_opiekuna,
                            "Nazwisko opiekuna" = notPaid$nazwisko_opiekuna, "Numer opiekuna" = notPaid$telefon_opiekuna,
                            "Email opiekuna" = notPaid$email_opiekuna)
  }
    
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

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

confirmAlert <- function(session,button_labels, inputId, title, type = NULL) {
  confirmSweetAlert(
    session = session,
    inputId = inputId,
    title = title,
    type = type,
    btn_labels = button_labels
  )
}  

informationAlert <- function(session,title, type = "success") {
  sendSweetAlert(
    session = session,
    title = title,
    type = type
  )
}

deleteDownloadButtons <- function(df, id, ...) {
  # function to create one action button as string
  deleteButton <- function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(id, i, sep="_"),
        label = NULL,
        icon = icon('trash'),
        onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'))
  }
  
  downloadButton <- function(i) {
    as.character(
      actionButton(
        # The id prefix with index
        paste(id, i, sep="_"),
        label = NULL,
        icon = icon('download'),
        onclick = 'Shiny.setInputValue(\"downloadPressed\", this.id, {priority: "event"})'))
  }
  deleteCol <- unlist(lapply(seq_len(nrow(df)), deleteButton))
  downloadCol <- unlist(lapply(seq_len(nrow(df)), downloadButton))
  
  # Return a data table
  DT::datatable(cbind(df, "Usuń" = deleteCol, "Pobierz" = downloadCol),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(
                    list(targets = 1, sortable = FALSE))
                ))
}

parseDeleteDownloadEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}  

findMatch <- function(df,df2) {
  theSame = c()
  for(i in 1:nrow(df2)){
    for(j in 1:nrow(df)){
      print(all_equal(df[j,],df2[i,]))
      if(all_equal(df[j,],df2[i,]) == TRUE)
        theSame = c(theSame, j)
    }
  }
  return(theSame)
}

addPayments_picker <- function(inputId, label, type, data,multiple = TRUE){
  pickerInput(
    inputId = inputId, 
    label = label, 
    choices = addPayments_choices(type, data), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = multiple
  )
}

addPayments_choices <- function(type, data){
  months = c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień",
             "Październik", "Listopad", "Grudzień")
  choices_all= colnames(data[colnames(data) %in% grep(paste0("wplata", collapse = "|"), colnames(data), value = TRUE)])
  if(type == "month"){
    choices = choices_all[!choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("_", " ", gsub("wplata_", "", choices))
    choices = lapply(strsplit(choices, " "), function(x){ paste(months[as.numeric(x[1])], x[2])}) %>% unlist()
  } else {
    choices = choices_all[choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("wplata_roczna_", "", choices)
  }
  return(choices)
}

additionalDates_picker <- function(inputId, label, type, data,multiple = TRUE){
  pickerInput(
    inputId = inputId, 
    label = label, 
    choices = additionalDates_choices(type,data), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = multiple
  )
}

additionalDates_choices <- function(type,data){
  months = c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień",
             "Październik", "Listopad", "Grudzień")
  current_date = Sys.Date()
  years = c((year(current_date) - 1 ): (year(current_date) + 1))
  dates = expand.grid(months, years)
  dates = paste(dates$Var1, dates$Var2)
  
  choices_all= colnames(data[colnames(data) %in% grep(paste0("wplata", collapse = "|"), colnames(data), value = TRUE)])
  if(type == "month"){
    choices = choices_all[!choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("_", " ", gsub("wplata_", "", choices))
    choices = lapply(strsplit(choices, " "), function(x){ paste(months[as.numeric(x[1])], x[2])}) %>% unlist()
    choices = dates[!dates %in% choices]
  } else {
    choices = choices_all[choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("wplata_roczna_", "", choices)
    choices = years[!years %in% choices]
  }
  return(choices)
}

convertDates <- function(date){
  months = list("Styczeń" = "01", "Luty" = "02", "Marzec" = "03", "Kwiecień" = "04", "Maj" = "05", "Czerwiec" = "06",
                "Lipiec" = "07", "Sierpień" = "08", "Wrzesień" = "09", "Październik" = "10", "Listopad" = "11",
                "Grudzień" = "12")
  converted_dates = lapply(as.list(date), function(x) {
                                              month = strsplit(x, " ")[[1]][1]
                                              gsub(paste0(month, " "), paste0("wplata_",months[[month]],"_"), x)
                                              })
  
  return(converted_dates)
}

convertYear <- function(date){
  converted_year = paste0("wplata_roczna_", date)
  
  return(converted_year)
}

clients_picker <- function(inputId, label, data){
  pickerInput(
    inputId = inputId, 
    label = label, 
    choices = paste0(data$imie, " ",data$nazwisko, ", ", data$pesel), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = TRUE
  )
}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
        ignore.case=TRUE)
}

sort_columns <- function(data){
  current_date = Sys.Date()
  important_dates = c(paste0(paste0("wplata_0",month(current_date) - 1), "_",year(current_date)),
                      paste0(paste0("wplata_0",month(current_date)), "_",year(current_date)),
                      paste0("wplata_roczna_",year(current_date)))
  
  choices_all= colnames(data[colnames(data) %in% grep(paste0("wplata", collapse = "|"), colnames(data), value = TRUE)])
  data_infos = data[, -which(names(data) %in%choices_all)]
  data_dates = data[, which(names(data) %in%choices_all)]
  
  choices_months = choices_all[!choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
  choices_months = gsub("_", "-", gsub("wplata_", "", choices_months)) %>% data.frame() %>% separate(".", c("Month", "Year"))
  choices_months = choices_months[with(choices_months, order(Year, Month)),]
  choices_months = paste0("wplata_",choices_months$Month, "_",choices_months$Year)
  
  choices_years = choices_all[choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
  choices_years = gsub("wplata_roczna_", "", choices_years) %>% data.frame()
  colnames(choices_years) = "Year"
  choices_years = choices_years[order(choices_years$Year),]
  choices_years = paste0("wplata_roczna_",choices_years)
  
  choices = c(choices_months, choices_years)
  data = cbind(imie = data_infos$imie, nazwisko = data_infos$nazwisko, data[,which(names(data) %in%important_dates)],
               data_infos[,-which(names(data_infos) %in%c("imie", "nazwisko"))],
               data_dates[,choices[!choices %in% important_dates]])
  return(data)
}

selectedClients_rows = function(data, input_values) {
  data = paste0(data$imie, " ",data$nazwisko, ", ", data$pesel)
  return(which(input_values %in% data))
}

server = function(input, output, session ){
  
  ############################Reactive Values############################
  filesData = reactiveValues(data = data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB")))
  templatesData = reactiveValues(data = NULL)
  clients = reactiveValues(data = Dane)
  excelData <- reactiveValues(data = NULL)
  csvData <- reactiveValues(data = NULL)
  sendMailConfirmation <- reactiveValues()
  deleteFile <- reactiveValues(deleteRow = NULL)
  credentials <- reactiveValues(data = data.frame(
    user = c("basia"), # mandatory
    password = c("123"), # mandatory
    email = "barjal@poczta.inet.pl",
    name = "Barbara",
    surname = "Jałocha",
    image = "anon_image.jpg",
    clients_number = 52,
    groups = 3,
    stringsAsFactors = FALSE
  ))
  
  #OKKOMENTARZE##################################Calnedar###############################
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
  
  #OKKOMENTARZE##################################Files###############################
  #Upload file to dropbox
  observeEvent(input$uploadFile,{
    #File name correction
    file = file.rename(input$file$datapath, paste0(input$file$name))
    file = input$file$name
    #Upload file to dropbox
    drop_upload(file)
    
    informationAlert(session,"Plik został wgrany.","success")
  })
  
  #Display files uploaded to Dropbox
  output$files_dropbox = renderDataTable({
    deleteDownloadButtons(filesData$data, 'button')
    
  })
  
  #Confirm if u want to delete the Dropbox file with the button in the Data table
  observeEvent(input$deletePressed, {
    #In which row the button was pressed
    deleteFile$deleteRow <- parseDeleteDownloadEvent(input$deletePressed)
    confirmAlert(session,c("Nie", "Tak"), "deleteFileConfirmation", "Czy chcesz usunąć plik?")
   
  })
  
  #Delete the Dropbox file with the button in the Data table
  observeEvent(input$deleteFileConfirmation, {
    #Assign user decision to reactive variable
    deleteFile$delete <- input$deleteFileConfirmation
    #Delete the Dropbox file and refresh files list
    if(deleteFile$delete)
    {
      drop_delete(filesData$data$Nazwa[deleteFile$deleteRow])
      filesData$data <- data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB"))
      informationAlert(session,"Plik został usunięty.","success")
      
    } else {
      informationAlert(session,"Plik nie został usunięty.","error")
    }
    #Clear confirmation
    deleteFile$delete <- NULL
    
  })
  
  #Confirm if u want to download the Dropbox file with the button in the Data table
  observeEvent(input$downloadPressed, {
    #In which row the button was pressed
    rowNum <- parseDeleteDownloadEvent(input$downloadPressed)
    
    #Download file from Dropbox
    drop_download(filesData$data$Nazwa[rowNum],overwrite = TRUE)
    confirmAlert(session,c("Wyjdź", "Potwierdź"), "fileDownloaded", "Plik został ściągnięty.","success")
  })
  
  #Update the files in pickerInput after uploading a new one
  observeEvent(input$uploadFile,{
    updatePickerInput(session = session, inputId = "filesNames",
                      choices =drop_dir()$name)
  })
  
  #Update the files shown after uploading a new one
  observeEvent(input$uploadFile,{
    filesData$data <- data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB"))
  })
  
  #Render filesPicker to download multiple files with file names as choices
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
  
  #Render filesPicker to delete multiple files with file names as choices
  output[["filesPickerDelete"]] <- renderUI ({
    pickerInput(
      inputId = "filesNamesDelete", 
      label = "Wybierz pliki do usunięcia", 
      choices = drop_dir()$name, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  #Donload multiple files from Dropbox
  observeEvent(input$downloadFiles,{
    successDownloading = c()
    for(file in input$filesNames)
      successDownloading = c(successDownloading,drop_download(file, overwrite = TRUE))
    if(sum(successDownloading) == 0)
      informationAlert(session,"Żaden z plików nie został pobrany.","error")
    else if(sum(successDownloading) != length(successDownloading))
      informationAlert(session,"Któryś z plików nie został pobrany.","error")
    else
      informationAlert(session,"Pliki zostały pobrane.","success")
  })
  
  #Delete multiple files at Dropbox
  observeEvent(input$deleteFiles,{
    for(file in input$filesNamesDelete)
      drop_delete(file)
    informationAlert(session,"Pliki zostały usunięte","success")
    filesData$data <- data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB"))
  })
  
  #OKKOMENTARZE##################################Mails###############################
  #Save template to database
  observeEvent(input$saveTemplate,{
    templateWritten = dbWriteTable(databaseConnection, "maile", data.frame(Name = input$templateNameCreate, Body = input$templateBodyCreate), append = TRUE)
    #Updating the displayed templates
    templatesData$data = dbReadTable(databaseConnection, "maile")
    
    #Check if template was written
    if(templateWritten)
      informationAlert(session,"Szablon maila został zapisany.","success")
    else
      informationAlert(session,"Ups! Coś poszło nie tak.","error")
  })
  
  #Release the "templates picker / edit section" with the names of the created templates  
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
  
  #Release the "templates body / edit section" with the names of the created templates  
  output[["templateBodyEdit"]] <- renderUI ({
    textAreaInput("templateBodyEdit", "Szablon maila", templatesData$data$Body[which(templatesData$data$Name == input$templateNameEdit)], resize = "both") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  })
  
  #Edit template body  
  observeEvent(input$editTemplate,{
    templatesData$data$Body[which(templatesData$data$Name == input$templateNameEdit)] <- input$templateBodyEdit
    #Updating the displayed templates
    templateEdited = dbWriteTable(databaseConnection, "maile", templatesData$data, overwrite = TRUE)
    
    #Check if template was edited
    if(templateEdited)
      informationAlert(session, "Szablon maila został zedytowany.","success")
    else
      informationAlert(session,"Ups! Coś poszło nie tak.","error")
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
  
  #Render list of templates / send mail section
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
  
  #Confirmation of willingness to send an e-mail
  observeEvent(input$sendMail,{
    confirmAlert(session,c("Nie", "Tak"), "sendMailConfirmation", "Czy chcesz wysłać maila?")
  })

  #Sending e-mail via gmail  
  observeEvent(input$sendMailConfirmation, {
    #Assign user decision to reactive variable
    sendMailConfirmation$confirm <- input$sendMailConfirmation
    #Check if user want to send e-mail
    if(sendMailConfirmation$confirm)
    {
      #If user have not selected any recipient show alert and clearconfirmation
      if(length(input$mailList) == 0)
      {
        informationAlert(session,"Musisz wybrać chociaż jednego odbiorce maila.","error")
        sendMailConfirmation$confirm <- NULL
      }
      #Else send e-mails via prepared account
      else
      {
        #Sending mail
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
        informationAlert(session,"Mail został wysłany.","success")
        sendMailConfirmation$confirm <- NULL
      }
      
    } else {
      informationAlert(session,"Mail nie został wysłany.","error")
      sendMailConfirmation$confirm <- NULL
    }
    #Clear confirmation
    sendMailConfirmation$confirm <- NULL
    
  })
  
  #OKKOMENTARZE##################################Data###############################
  #Upload excel file and join it with already uploaded data
  observeEvent(input$uploadExcelData,{
    #Save data in reactive value
    excelData$data <- read_xlsx(input$excelFile$datapath)
    #Write and read data
    dbWriteTable(databaseConnection, "uczestnicy", unique(rbind(clients$data, excelData$data)), overwrite = TRUE)
    clients$data = data.table(dbReadTable(databaseConnection, "uczestnicy"))
  })
  
  #Upload csv file and join it with already uploaded data
  observeEvent(input$uploadCSVData,{
    #Save data in reactive value
    csvData$data <- read.csv(input$csvFile$datapath)
    #Write and read data
    dbWriteTable(databaseConnection, "uczestnicy", unique(rbind(clients$data, csvData$data)), overwrite = TRUE)
    clients$data = data.table(dbReadTable(databaseConnection, "uczestnicy"))
  })
  
  #Display clients data /include responsive design and (action, delete) buttons
  output$datasetTable = renderDataTable({
    datatable(clients$data, extensions = c ("Responsive", "Buttons"),editable = TRUE, options = list(dom = 'Bfrtip',
              buttons = list(list(extend = "collection", text = 'Dodaj',
                  action = DT::JS("function ( e, dt, node, config ) {
                                  Shiny.setInputValue('addObservation', true, {priority: 'event'});}")),
                  list(extend = "collection", text = 'Usuń',
                       action = DT::JS("function ( e, dt, node, config ) {
                                       Shiny.setInputValue('deleteObservation', true, {priority: 'event'});}")))))
  
    
  })
  
  #Edit cells at data table by click
  observeEvent(input$datasetTable_cell_edit, {
    clients$data[input$datasetTable_cell_edit$row,input$datasetTable_cell_edit$col] <<- input$datasetTable_cell_edit$value
    dbWriteTable(databaseConnection, "uczestnicy",  clients$data, overwrite = TRUE)  
    })
  
  #Add observation button
  observeEvent(input$addObservation, {
    showModal(modalDialog(
      title = "Dodaj uczestnika",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("addClient", "Potwierdź") 
      ),
      #Form
      column(
        width = 12,
        offset = 2,
        textInput("addName", "Imie"),
        textInput("addSurname", "Nazwisko"),
        numericInput("addPesel",value = 0,"Pesel"),
        dateInput("addBirthDate", value = "2000-01-01","Data urodzenia"),
        textInput("addBirthPlace", "Miejsce urodzenia"),
        textInput("addAddress", "Adres"),
        numericInput("addPhone", value = 0,"Telefon"),
        textInput("addMail", "E-mail"),
        textInput("addSchool", "Szkoła"),
        textInput("addGuardianName", "Imie opiekuna"),
        textInput("addGuardianSurname", "Nazwisko opiekuna"),
        numericInput("addGuardianPhone", value = 0,"Telefon opiekuna"),
        textInput("addGuardianMail", "E-mail opiekuna")
      )
    ))
  })

  #Add client to database based on filled form
  observeEvent(input$addClient, {
    #Temporary dataframe with informations about client
    clientData = data.frame(input$addName,input$addSurname,input$addPesel,input$addBirthDate,input$addBirthPlace,
                            input$addAddress,input$addPhone,input$addMail,input$addSchool,input$addGuardianName,
                            input$addGuardianSurname,input$addGuardianPhone,input$addGuardianMail)
    clientsData_colnames =  colnames(clients$data)
    colnames(clientData) = clientsData_colnames[!clientsData_colnames %in% grep(paste0("wplata", collapse = "|"), clientsData_colnames, value = T)]
    
    #Temporary dataframe with columns about payments that are already in database, but filled with "-"
    payments_colnames = clientsData_colnames[clientsData_colnames %in% grep(paste0("wplata", collapse = "|"), clientsData_colnames, value = T)]
    payments_info = data.frame(t(rep("-",length(payments_colnames))))
    colnames(payments_info) = payments_colnames
    
    #If exist at least one payment column, bind by columns temporary dataframes with informations about client and payments columns
    if(nrow(payments_info) != 0)
      clientData = cbind(clientData, payments_info)
    clients$data = rbind(clientData, clients$data)
    
    informationAlert(session,"Osoba została wprowadzona.","success")
    removeModal()
  })
  
  #Confirmation if user wants to delete selected rows on data table, after "Delete" button was pushed
  observeEvent(input$deleteObservation, {
      if (!is.null(input$datasetTable_rows_selected)) {
        confirmAlert(session,c("Nie", "Tak"), "confirmDeleteRows", "Czy chcesz usunąć wybrane osoby?")
      }
  })
  
  #Delete selected rows on data table
  observeEvent(input$confirmDeleteRows, { 
    clients$data <- clients$data[-as.numeric(input$datasetTable_rows_selected),]
    informationAlert(session,"Usuwanie zakończone pomyślnie.","success")
    })

  #Create date picker for "add payments" button
  output[["datePicker"]] <- renderUI({
    if(input$addPayments_type == "Miesięczna")
      addPayments_picker("datesList", "Wybierz date", "month", clients$data)
    else
      addPayments_picker("datesList", "Wybierz date", "year", clients$data)
  })
  
  #Show modal with form after "add payment" button was pushed / includes clients picker
  observeEvent(input$addPayments, {
    showModal(modalDialog(
      title = "Dodaj wpłatę",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("addPayments_confirm", "Potwierdź") 
      ),
      column(
        width = 12,
        offset = 2,
        #Select payment type
        prettyRadioButtons(inputId = "addPayments_type",
                            label = "Wybierz rodzaj wpłaty",
                            choices = c("Miesięczna", "Roczna"),
                            inline = TRUE),
        uiOutput("datePicker"),
        clients_picker("clientsList", "Wybierz osoby", clients$data),
        textInput("addPayments_amount", "Wprowadź kwote")
      )
    ))
  })
  
  #If you confirm the addition of a deposit, add based on the pesels of the amount / includes clients picker
  observeEvent(input$addPayments_confirm, {
    #Select PESEL of the selected customers
    pesel = map(strsplit(input$clientsList, ", "),2)
    
    #Distinguish between monthly and annual due to the different ways of converting dates
    if(input$addPayments_type == "Miesięczna")
      dates = unlist(convertDates(input$datesList))
    else
      dates = unlist(convertYear(input$datesList))

    #Add payments
    lapply(dates, function(x, data = clients$data){
      clients$data[clients$data$pesel %in% pesel,x] = as.numeric(input$addPayments_amount)
    })
    
    informationAlert(session, "Dodano wpłate.", type = "success")
  })
  
  #Show modal with form after "Add payment of your choice" button was pushed / without clients picker
  observeEvent(input$addPaymentsPicker, {
    showModal(modalDialog(
      title = "Dodaj wpłatę",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("addPaymentsPicker_confirm", "Potwierdź") 
      ),
      column(
        width = 12,
        offset = 2,
        #Select payment type
        prettyRadioButtons(inputId = "addPayments_type",
                           label = "Wybierz rodzaj wpłaty",
                           choices = c("Miesięczna", "Roczna"),
                           inline = TRUE),
        uiOutput("datePicker"),
        textInput("addPaymentsPicker_amount", "Wprowadź kwote")
      )
    ))
  })
  
  #If you confirm the addition of a deposit, the payment will be added to the marked lines in data table / without clients picker
  observeEvent(input$addPaymentsPicker_confirm, {
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$datasetTable_rows_selected)
  
    #Distinguish between monthly and annual due to the different ways of converting dates
    if(input$addPayments_type == "Miesięczna")
      dates = unlist(convertDates(input$datesList))
    else
      dates = unlist(convertYear(input$datesList))
    
    #Add payments
    lapply(dates, function(x, data = clients$data){
      clients$data[selected_rows,x] = as.numeric(input$addPaymentsPicker_amount)
    })
    
    informationAlert(session, "Dodano wpłate.", type = "success")
  })
  
  #Create date picker for "add column" /Includes dates +/- a year from the current one, excluding those in the data set columns
  output[["addColumns_datePicker"]] <- renderUI({
    if(input$addColumns_type == "Miesięczna")
      additionalDates_picker("addColumns_datesList", "Wybierz date", "month", clients$data)
    else
      additionalDates_picker("addColumns_datesList", "Wybierz date", "year", clients$data)
  })
  
  #Create date picker for "edit column" /Contains only the dates contained in the columns
  output[["editColumns_datePicker"]] <- renderUI({
    if(input$editColumns_type == "Miesięczna")
      addPayments_picker("editColumns_datesList", "Wybierz date", "month", clients$data,FALSE)
    else
      addPayments_picker("editColumns_datesList", "Wybierz date", "year", clients$data,FALSE)
  })
  
  #Create additional date picker for "edit column /Includes dates +/- a year from the current one, excluding those in the data set columns
  output[["editColumnsAdditional_datePicker"]] <- renderUI({
    if(input$editColumns_type == "Miesięczna")
      additionalDates_picker("editColumnsAdditional_datesList", "Wybierz date", "month", clients$data,FALSE)
    else
      additionalDates_picker("editColumnsAdditional_datesList", "Wybierz date", "year", clients$data,FALSE)
  })
  
  #Create date picker for "delete column" /Contains only the dates contained in the columns
  output[["deleteColumns_datePicker"]] <- renderUI({
    if(input$deleteColumns_type == "Miesięczna")
      addPayments_picker("deleteColumns_datesList", "Wybierz date", "month", clients$data)
    else
      addPayments_picker("deleteColumns_datesList", "Wybierz date", "year", clients$data)
  })

  #Show modal with adding, editing and deleting columns 
  observeEvent(input$managePaymentsColumn, {
    showModal(modalDialog(
      title = "Zarządzaj kolumnami wpłat",
      footer =  tagList(
        modalButton("Wyjdź")
      ),
      bs4TabSetPanel(
        id = "manageColumns_panel",
        side = "left",
        bs4TabPanel(
          tabName = "Dodaj kolumne",
          active = TRUE,
          column(
            width = 12,
            offset = 2,
            prettyRadioButtons(inputId = "addColumns_type",
                               label = "Wybierz typ kolumn",
                               choices = c("Miesięczna", "Roczna"),
                               inline = TRUE, selected = "Miesięczna"),
            uiOutput("addColumns_datePicker"),
            actionButton("addPaymentColumn", "Dodaj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Edytuj Kolumne",
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            prettyRadioButtons(inputId = "editColumns_type",
                               label = "Wybierz typ kolumn",
                               choices = c("Miesięczna", "Roczna"),
                               inline = TRUE),
            uiOutput("editColumns_datePicker"),
            uiOutput("editColumnsAdditional_datePicker"),
            actionButton("editPaymentColumn", "Edytuj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Usuń kolumne",
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            prettyRadioButtons(inputId = "deleteColumns_type",
                               label = "Wybierz typ kolumn",
                               choices = c("Miesięczna", "Roczna"),
                               inline = TRUE),
            uiOutput("deleteColumns_datePicker"),
            actionButton("deletePaymentColumn", "Usuń", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
        
      )
    ))
  })
  
  #If you confirm the addition of a column, add a column with the corresponding date filled with zeros
  observeEvent(input$addPaymentColumn, {
    if(input$addColumns_type == "Miesięczna")
      dates = unlist(convertDates(input$addColumns_datesList))
    else
      dates = unlist(convertYear(input$addColumns_datesList))

    #Add columns to dataframe
    lapply(dates, function(x, data = clients$data){
      clients$data[[paste(x)]] = 0
    })
    
    #Sort columns in dataframe
    clients$data = sort_columns(clients$data)
    informationAlert(session, "Dodano kolumne.", type = "success")
  })
  
  #If the user agrees to edit the column, change its date to the selected
  observeEvent(input$editPaymentColumn, {
    if(input$editColumns_type == "Miesięczna") {
      date = unlist(convertDates(input$editColumns_datesList))
      new_date = unlist(convertDates(input$editColumnsAdditional_datesList))
    } else
    {
      dates = unlist(convertYear(input$editColumns_datesList))
      new_date = unlist(convertYear(input$editColumnsAdditional_datesList))
    }
    
    #Edit column name
    colnames(clients$data)[colnames(clients$data) %in% date] = new_date
    
    #Sort columns in dataframe
    clients$data = sort_columns(clients$data)
    informationAlert(session, "Edytowano kolumne.", type = "success")
  })
  
  #If the user approves the deletion, delete the selected columns
  observeEvent(input$deletePaymentColumn, {
    if(input$deleteColumns_type == "Miesięczna")
      dates = unlist(convertDates(input$deleteColumns_datesList))
    else
      dates = unlist(convertYear(input$deleteColumns_datesList))
    
    #Delete selected columns
    clients$data = clients$data [,-which(names(clients$data)%in%dates)]
    
    #Sort columns in dataframe
    clients$data = sort_columns(clients$data)
    informationAlert(session, "Usunięto kolumne.", type = "success")
  })
  
  #OKKOMENTARZE##################################School register###############################
 
  output$schoolRegister_dataTable = renderDataTable({
    schoolRegister_data = data.frame(Uczestnik = paste(clients$data$imie, clients$data$nazwisko),
                                     Opiekun = paste(clients$data$imie_opiekuna, clients$data$nazwisko_opiekuna),
                                     Telefon_opiekuna = clients$data$telefon_opiekuna)
    datatable(schoolRegister_data, extensions = c ("Responsive"),editable = TRUE)
  })
  
  #Create date picker for "add payments" button
  output[["datePicker_presences"]] <- renderUI({
      addPayments_picker("datesList_presences", "Wybierz date", "month", clients$data)
  })
  
  #Show modal with form after "add payment" button was pushed / includes clients picker
  observeEvent(input$addPresences, {
    showModal(modalDialog(
      title = "Dodaj obecności",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("addPresences_confirm", "Potwierdź") 
      ),
      column(
        width = 12,
        offset = 2,
        #Select payment type
        prettyRadioButtons(inputId = "addPresences_type",
                           label = "Wybierz rodzaj",
                           choices = c("Obecność", "Nieobceność"),
                           inline = TRUE),
        uiOutput("datePicker_presences"),
        clients_picker("clientsList_presences", "Wybierz osoby", clients$data)
      )
    ))
  })
  
  
  #If you confirm the addition of a deposit, add based on the pesels of the amount / includes clients picker
  observeEvent(input$addPresences_confirm, {
    #Select PESEL of the selected customers
    print(input$clientsList_presences)
    
    #Convert dates to numeric format
    dates = unlist(convertDates(input$datesList_presences))
    
    #Add payments
    lapply(dates, function(x, data = clients$data){
      clients$data[selectedClients_rows(clients$data, input$clientsList_presences),x] = as.numeric(input$addPayments_amount)
    })
    
    informationAlert(session, "Dodano obecność.", type = "success")
  })

  
  #Show modal with form after "Add payment of your choice" button was pushed / without clients picker
  observeEvent(input$addPresencesPicker, {
    showModal(modalDialog(
      title = "Dodaj obecności",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("addPresencesPicker_confirm", "Potwierdź") 
      ),
      column(
        width = 12,
        offset = 2,
        #Select payment type
        prettyRadioButtons(inputId = "addPresences_type",
                           label = "Wybierz rodzaj",
                           choices = c("Obecność", "Nieobecność"),
                           inline = TRUE),
        uiOutput("datePicker_presences")
      )
    ))
  })
  
  #If you confirm the addition of a deposit, the payment will be added to the marked lines in data table / without clients picker
  observeEvent(input$addPresencesPicker_confirm, {
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$addPresencesPicker_rows_selected)
    
    #Convert dates to numeric format
    dates = unlist(convertDates(input$datesList_presences))
    
    #Add payments
    lapply(dates, function(x, data = clients$data){
      clients$data[selected_rows,x] = as.numeric(input$addPaymentsPicker_amount)
    })
    
    informationAlert(session, "Dodano obecność.", type = "success")
  })
  
  ###################################Dashboard###############################
  #Upload excel file
  observe({
    output$paymentsSum_month = renderText({
      sum_of_payments = sum(clients$data[paste0("wplata_",format(Sys.Date(),"%m"),"_",format(Sys.Date(),"%Y"))])
      total_payments = nrow(clients$data) * MonthPayment
      paste0(sum_of_payments," zł/",total_payments, " zł")
    })
    
  })
  
  observe({
    output$paymentsSum_year = renderText({
      sum_of_payments = sum(clients$data["wplata_roczna"])
      total_payments = nrow(clients$data) * YearPayment
      paste0(sum_of_payments," zł/",total_payments, " zł")
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
      
      fig <- plot_ly(data, x = ~Date, y = ~Paid, type = 'bar', name = 'wpłaciły')
      fig <- fig %>% add_trace(y = ~Not_paid, name = 'nie wpłaciły')
      fig <- fig %>% layout(xaxis = list(title = 'Data'),yaxis = list(title = 'Liczba osób'), barmode = 'group')
    })
    
  })
  
  #List the tables of users who have not paid their monthly contributions
  observe({
    output$notPaidMonth_list = renderDataTable({
      df = notPaid(0, clients$data, "month")
    })
    
  })
  
  #List the tables of users who have not paid their annual contribution
  observe({
    output$notPaidYear_list = renderDataTable({
      df = notPaid(0, clients$data, "year")

    })
    
  })
  
  #List the tables of users who have not paid their share of the annual payment
  observe({
    output$partPaidYear_list = renderDataTable({
      df = notPaid(c(0, YearPayment), clients$data,"year")

    })
    
  })
  
  #List the tables of users who have not paid a portion of their monthly payments
  observe({
    output$partPaidMonth_list = renderDataTable({
      df = notPaid(c(0,MonthPayment), clients$data,"month")
    })
    
  })
  #OKKOMENTARZE##################################Login############################
  #User authentication
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials$data)
  )
  #OKKOMENTARZE##################################User panel########################################
  #Redirect to the user panel by pressing the name and surname
  observeEvent(input$userPanelLink, {
    #Selected 9 becouse it is position of user panel at sidebar menu
    updatebs4TabItems(
      session,
      inputId = "sidebarMenu",
      selected = 9
    )
  })
  
  #Render Sidebar user panel / Image, name and surname 
  output[["sidebar_userPanel"]] <- renderUI({
    bs4SidebarUserPanel(
      img = credentials$data$image,
      text =actionLink(inputId = "userPanelLink", label = textOutput("userName"))
    )
  })
  
  #Render User card at user panel tab
  output[["userPanel"]] <- renderUI({
    bs4UserCard(
      src = credentials$data$image,
      status = "info",
      title = textOutput("userPanelName"),
      elevation = 4,
      htmlOutput("userInformations"),
      br(),
      fluidRow(
        column(
          width = 5,
          offset = 7,
          actionButton("editData", "Edytuj dane", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          actionButton("changePassword", "Zmień hasło", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      )
    )
  })
  
  #Render name and surname based on credentials for sidebar panel
  output$userName <- renderText({
    paste(credentials$data$name, credentials$data$surname, collapse = " ")
  })
  
  #Render name and surname based on credentials for user card
  output$userPanelName <- renderText({
    paste("Użytkownik:", credentials$data$name, credentials$data$surname, collapse = " ")
  })
  
  #Render information shown in the user card
  output$userInformations <- renderUI({
    HTML(paste(paste0("<b>Imie: </b> ", credentials$data$name), paste0("<b>Nazwisko: </b>", credentials$data$surname),
               paste0("<b>Email: </b> ", credentials$data$email),paste0("<b>Login: </b> ", credentials$data$user),
               sep = "<br/>"))
  })
  
  #If the user clicks on the data edition, show a modal with the form
  observeEvent(input$editData, {
    showModal(modalDialog(
      title = "Edytuj Dane",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("editUserData", "Potwierdź") 
      ),
      column(
            width = 12,
            offset = 2,
            textInput("changeName", "Imie", value = paste(reactiveValuesToList(res_auth)$name)),
            textInput("changeSurname", "Nazwisko", value = paste(reactiveValuesToList(res_auth)$surname)),
            textInput("changeLogin", "Login", value = paste(reactiveValuesToList(res_auth)$user)),
            fileInput("changeImg", "Nowe zdjęcie")
      )
    ))
  })
  
  #Confirmation whether you want to change the data
  observeEvent(input$editUserData, {
    confirmAlert(session,c("Nie", "Tak"), "confirmChangedData", "Potwierdzić wprowadzone zmiany?")
  })
  
  #Change the data
  observeEvent(input$confirmChangedData, {
    credentials$data$name = input$changeName
    credentials$data$surname = input$changeSurname
    credentials$data$user = input$changeLogin
    #If the user uploaded a picture, set it in the application 
    if(length(input$changeImg) != 0)
    {
      #File name correction
      changeImg = file.rename(input$changeImg$datapath, input$changeImg$name)
      changeImg = input$changeImg$name
      
      #Upload file to dropbox
      drop_upload(changeImg, path = "Zdjęcia", mode = "overwrite")
      #Download file to www directory
      drop_download(paste0("Zdjęcia/",input$changeImg$name), local_path = "www",overwrite = TRUE)
      
      #Rename the photo to include the user name
      renameFile(paste0("www/",input$changeImg$name), paste0("www/",credentials$data$user,"_img.jpg"), overwrite = TRUE)
      credentials$data$image <- paste0(credentials$data$user,"_img.jpg")
    }
    informationAlert(session,"Dane konta zostały zmienione.","success")
    removeModal()
  })
  
  #If the user clicks on the password edition, show a modal with the form             
  observeEvent(input$changePassword, {
    showModal(modalDialog(
      title = "Zmień hasło",
      footer =  tagList(
        modalButton("Wyjdź"),
        actionButton("confirmPassword", "Potwierdź") 
      ),
      column(
        width = 12,
        offset = 2,
        passwordInput("actualPassword", "Aktualne hasło"),
        passwordInput("newPassword", "Nowe hasło"),
        passwordInput("confirmNewPassword", "Potwierdź hasło")
      )
    ))
  })
  
  #Confirmation whether you want to change the password
  observeEvent(input$confirmPassword, {
    confirmAlert(session,c("Nie", "Tak"), "confirmChangedPassword", "Chcesz zmienić hasło?")
  })

  #Change the password
  observeEvent(input$confirmChangedPassword, {
    #Verification that the user has entered the correct current password 
    if(input$actualPassword == credentials$data$password)
    {
      #Verification that the user has entered the same new and current password
      if(input$newPassword == input$confirmNewPassword) {
        credentials$data$password <- input$newPassword
        if(input$actualPassword == input$newPassword)
        {
          informationAlert(session,"Hasło zostało zmienione.", "success")
          removeModal()
        }
        else
          informationAlert(session,"Nowe hasło i stare nie różnią się.", "error")
      }
      else {
        informationAlert(session,"Nowe hasło i jego potwierdzenie różnią się.", "error")
      }
    }
    else {
      informationAlert(session,"Aktualne hasło nie zgadza się.", "error")
    }
  })

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
