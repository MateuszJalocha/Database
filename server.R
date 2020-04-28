
server = function(input, output, session ){
  
  ############################Reactive Values############################
  filesData = reactiveValues(data = data.table(Nazwa = drop_dir()$name,Rozmiar = paste(round(drop_dir()$size *10^-6,3), "MB")))
  templatesData = reactiveValues(data =  dbReadTable(databaseConnection, "maile"))
  clients = reactiveValues(data = dbReadTable(databaseConnection, "uczestnicy"))
  
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
    #Correct file name
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
  
  
}
