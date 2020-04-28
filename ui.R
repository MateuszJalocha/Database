#########################################
########Navbar, sidebar, footer##########
#########################################

#Navbar options
navbar = bs4DashNavbar(
  skin = "light",
  status = "white",
  border = TRUE,
  sidebarIcon = "bars",
  controlbarIcon = "th",
  fixed = FALSE
)

#Controlbar options
controlbar = bs4DashControlbar(
  skin = "light",
  title = "My right sidebar",
  width = 250,
  sliderInput(
    inputId = "obs", 
    label = "Number of observations:",
    min = 0, 
    max = 1000, 
    value = 500
  ),
  column(
    width = 12,
    align = "center",
    radioButtons(
      inputId = "dist", 
      label = "Distribution type:",
      c("Normal" = "norm",
        "Uniform" = "unif",
        "Log-normal" = "lnorm",
        "Exponential" = "exp")
    )
  )
)

#Footer options
footer = bs4DashFooter()


#####################################
##########Sidebar content############
#####################################

sidebar = bs4DashSidebar(
  skin = "dark",
  status = "primary",
  title = "Baza danych",
  brandColor = "primary",
  url = "https://www.google.fr",
  elevation = 3,
  opacity = 0.8,
  bs4SidebarUserPanel(
    img = "https://image.flaticon.com/icons/svg/1149/1149168.svg", 
    text = "Imie Nazwisko"
  ),
  bs4SidebarMenu(
    bs4SidebarMenuItem(
      "Dashboard",
      tabName = "dashboardMenu",
      icon = "dashboard"
    ),
    bs4SidebarMenuItem(
      "Dane",
      tabName = "data",
      icon = "database"
    ),
    bs4SidebarMenuItem(
      "Wydatki",
      tabName = "expenses",
      icon = "credit-card"
    ),
    bs4SidebarMenuItem(
      "Kalendarz",
      tabName = "calendarMenu",
      icon = "calendar"
    ),
    bs4SidebarMenuItem(
      "Mail",
      tabName = "mailMenu",
      icon = 'envelope'
    ),
    bs4SidebarMenuItem(
      "Dokumenty",
      tabName = "filesMenu",
      icon = "file"
    )
  )
)




#####################################
##########Body content###############
#####################################

############Dashboard tab###############

#Informations on persons who have not made payments and total payments in the months concerned
payments = fluidRow(
  column(
    width = 6,
    bs4Card(
      title = "Suma wplat",
      maximizable = TRUE,
      closable = TRUE,
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE
    )
  ),
  column(
    width = 6,
    bs4Card(
      title = "Osoby, ktore nie wplacily",
      closable = TRUE,
      width = 12,
      status = "danger",
      solidHeader = FALSE,
      collapsible = TRUE
    )
  )
)

#To do list and calendar
additional_boxes = fluidRow(
  column(
    width = 6,
    bs4Card(
      title = "Rzeczy do zrobienia",
      closable = TRUE,
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE
    )
  ),
  column(
    width = 6,
    bs4Card(
      title = "Kalendarz",
      closable = TRUE,
      width = 12,
      solidHeader = FALSE,
      gradientColor = "success",
      collapsible = TRUE
    )
  )
)

#Information boxes about total payments in the current month, number of people who have not paid this month and 
#total number of persons who have not paid
info_valueBoxes = fluidRow(
  bs4ValueBox(
    width = 3,
    value = 1500,
    subtitle = "Suma wplat w tym miesiącu",
    status = "primary",
    icon = "shopping-cart",
    href = "#"
  ),
  bs4ValueBox(
    width = 3,
    value = 11,
    subtitle = "Liczba osob, ktore nie wpłaciły w tym miesiącu",
    status = "danger",
    icon = "cogs"
  ),
  bs4ValueBox(
    width= 3,
    value = 12,
    subtitle = "Łączna liczba osób, które nie wpłaciły",
    status = "warning",
    icon = "sliders"
  )
)

##########Calendar tab############
calendar = fluidRow(
  
  #Create event
  column(
    width = 4,
    bs4Card(
      closable = FALSE,
      title = "Stwórz wydarzenie",
      width = 12,
      solidHeader = FALSE,
      tags$h4("Wybierz kolor"),
      actionButton("eventRed", "",
                   style="color: #fff; background-color: #E41A1C; border-color: #2e6da4; height: 30px; width: 30px"),
      actionButton("eventBlue", "", 
                   style="color: #fff; background-color: #377EB8; border-color: #2e6da4; height: 30px; width: 30px"),
      actionButton("eventGreen", "", 
                   style="color: #fff; background-color: #4DAF4A; border-color: #2e6da4; height: 30px; width: 30px"),
      actionButton("eventYellow", "", 
                   style="color: #fff; background-color: #EEE8AA; border-color: #2e6da4; height: 30px; width: 30px"),
      textInput("eventName", "Nazwa wydarzenia"),
      textInput("eventBody", "Treść wydarzenia"),
      dateRangeInput('eventDate',
                     label = 'Data wydarzenia',
                     start = Sys.Date(), end = Sys.Date()
      ),
      fluidRow(
        splitLayout(cellWidths = c("35%", "35%"),
                    timeInput("startTime", "Godzina rozpoczęcia", seconds = FALSE, value = strptime("13:00:00", "%T")),
                    
                    timeInput("endTime", "Godzina zakończenia", seconds = FALSE, value = strptime("14:00:00", "%T"))
        )
      ),
      actionButton("createEvent", "Stwórz wydarzenie", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  
  #Calendar
  column(
    width = 7,
    bs4Card(
      title = "April",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      actionGroupButtons(
        inputIds = c("calendarToday", "calendarPrevious", "calendarNext"),
        labels = list("Dzisiaj", "<",">"),
        status = "primary"
      ),
      br(),
      calendarOutput("calendar"),
      br(),
      actionGroupButtons(
        inputIds = c("monthButton", "weekButton", "dayButton"),
        labels = list("Mesiąc", "Tydzień","Dzień"),
        status = "primary"
      )
    )
  )
  
  
)


#########Files tab##########
files = fluidRow(
  
  #Upload file
  column(
    width = 4,
    bs4Card(
      closable = FALSE,
      title = "Wgraj plik",
      width = 12,
      solidHeader = FALSE,
      fileInput("file", "Proszę wybrać plik"),
      actionButton("uploadFile", "Zapisz plik", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  
  #Uploaded files
  column(
    width = 7,
    bs4Card(
      title = "Wgrane pliki",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      dataTableOutput("files_dropbox"),
      br(),
      uiOutput("filesPicker"),
      actionButton("downloadFile", "Pobierz plik", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )
)


###########Mail tab############
#Mail tab
mail = fluidRow(
  
  #Create and edit mail template
  column(
    width = 5,
    
    #Create mail template
    bs4Card(
      closable = FALSE,
      collapsed = TRUE,
      title = "Utwórz template maila",
      width = 12,
      solidHeader = FALSE,
      textInput("templateNameCreate", "Nazwa szablonu"),
      textAreaInput("templateBodyCreate", "Szablon maila", "Dzień dobry,", resize = "both") %>%
        shiny::tagAppendAttributes(style = 'width: 100%;'),
      actionButton("saveTemplate", "Zapisz szablon", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    #Edit mail template
    bs4Card(
      closable = FALSE,
      collapsed = TRUE,
      title = "Edytuj template",
      width = 12,
      solidHeader = FALSE,
      uiOutput("templateNameEdit"),
      uiOutput("templateBodyEdit"),
      actionButton("editTemplate", "Edytuj szblon", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  
  #Send a group mail
  column(
    width = 7,
    bs4Card(
      title = "Wyślij grupowego maila",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      fluidRow(
        column(width = 4,
               uiOutput("mailList")
        ),
        column(width = 4,
               uiOutput("templateList")
        )
      ),
      uiOutput("mailTitle"),
      uiOutput("mailBody"),
      actionButton("sendMail", "Wyślij maila", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )
)

#############Tab items#################

calendarMenu =  bs4TabItem(tabName = "calendarMenu",
                           calendar)

dashboardMenu = bs4TabItem(tabName = "dashboardMenu",
                           info_valueBoxes, payments, additional_boxes)
filesMenu = bs4TabItem(tabName = "filesMenu",
                       files)
mailMenu = bs4TabItem(tabName = "mailMenu",
                      mail)

#Combine all body components
body = bs4DashBody(
  bs4TabItems(dashboardMenu,calendarMenu, filesMenu,mailMenu)
)



#####################################
######Combine all components#########
#####################################


ui = bs4DashPage(
  enable_preloader = TRUE,
  controlbar_collapsed = TRUE,
  title = "Database App",
  navbar = navbar,
  sidebar = sidebar,
  controlbar = controlbar,
  footer = footer,
  body = body
)
