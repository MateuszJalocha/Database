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
    text =actionLink(inputId = "userPanelLink", label = textOutput("userName"))
  ),
  bs4SidebarMenu(
    id = "sidebarMenu",
    bs4SidebarMenuItem(
      "Dashboard",
      tabName = "dashboardMenu",
      icon = "dashboard"
    ),
    bs4SidebarMenuItem(
      "Dane",
      tabName = "dataMenu",
      icon = "database"
    ),
    bs4SidebarMenuItem(
      "Wydatki",
      icon = "credit-card",
      bs4SidebarMenuSubItem(
        text = "Twoje wydatki",
        tabName = "seeExpensesMenu",
        icon = "circle-thin"
      ),
      bs4SidebarMenuSubItem(
        text = "Zarządzaj wydatkami",
        tabName = "addExpensesMenu",
        icon = "circle-thin"
      )
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
    ),
    bs4SidebarMenuItem(
      "Panel użytkownika",
      tabName = "userPanelMenu",
      icon = "dashboard"
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
      collapsible = TRUE,
      plotlyOutput("barPlot_payments")
    )
  ),
  column(
    width = 6,
    bs4TabCard(
      id = "notPaidTab",
      title = "Kto nie wpłacił",
      closable = FALSE,
      width = 12,
      bs4TabPanel(
        tabName = "Nie wpłacili miesięcznej",
        active = TRUE,
        dataTableOutput("notPaidMonth_list")
      ),
      bs4TabPanel(
        tabName = "Nie wpłacili rocznej",
        active = FALSE,
        dataTableOutput("notPaidYear_list")
      ),
      bs4TabPanel(
        tabName = "Wpłacili część miesięcznej",
        active = FALSE,
        dataTableOutput("partPaidMonth_list")
      ),
      bs4TabPanel(
        tabName = "Wpłacili część rocznej",
        active = FALSE,
        dataTableOutput("partPaidYear_list")
      )
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
    value = textOutput("paymentsSum_month"),
    subtitle = "Suma wplat w tym miesiącu",
    status = "primary",
    icon = "shopping-cart",
    href = "#"
  ),
  bs4ValueBox(
    width = 3,
    value = textOutput("paymentsSum_year"),
    subtitle = "Liczba osob, ktore nie wpłaciły w tym miesiącu",
    status = "danger",
    icon = "cogs"
  ),
  bs4ValueBox(
    width= 3,
    value = textOutput("notpaidMonth"),
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

#########Data tab##########
data = fluidRow(
  bs4Card(
    title = "Wgrane dane",
    width = 12,
    closable = FALSE,
    solidHeader = FALSE,
    dataTableOutput("datasetTable")
  ),
  bs4TabCard(
    id = "dataTabPanels",
    title = "Wgraj dane",
    closable = FALSE,
    width = 4,
    bs4TabPanel(
      tabName = "Excel",
      active = TRUE,
      bs4TabPanel(tabName = "",fileInput("excelFile", "Proszę wybrać plik xls")),
      actionButton("uploadExcelData", "Wgraj dane", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    bs4TabPanel(
      tabName = "CSV",
      active = FALSE,
      bs4TabPanel(tabName = "",fileInput("csvFile", "Proszę wybrać plik csv")),
      actionButton("uploadCSVData", "Wgraj dane", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )
)


############User panel tab###############

#Informations on persons who have not made payments and total payments in the months concerned
userPanel = fluidRow(
  column(
    width = 9,
    offset = 4,
    bs4UserCard(
      useShinyjs(),
      src =  img(id="my_img",src="https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",style="cursor:pointer;"),
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
  )
)

userPanelInformations = fluidRow(
  bs4ValueBox(
    width = 3,
    value = textOutput("clientsNumber"),
    subtitle = "Liczba klientów",
    status = "primary",
    icon = "shopping-cart",
    href = "#"
  ),
  bs4ValueBox(
    width = 3,
    value = textOutput("groupsNumber"),
    subtitle = "Liczba grup",
    status = "danger",
    icon = "cogs"
  )
)


#########Add expenses tab##########

addExpenses = fluidRow(
  column(
    width = 5,
    bs4Card(
      closable = FALSE,
      title = "Dodaj wydatek",
      width = 12,
      solidHeader = FALSE,
      div(shinyjs::useShinyjs(), id = "input-panel",
          textInput("addExpenseName", "Nazwa wydatku"),
          textInput("addExpenseCategory", "Kategoria wydatku"),
          textInput("addExpenseAmount", "Kwota wydatku"),
          dateInput("addExpenseDate", "Data", format = "yyyy-mm-dd"),
          actionButton("saveExpense", "Zapisz wydatek", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ))),
  column(
    width = 7,
    wellPanel(h3(strong("Twoje wydatki")), br(),
              DT::dataTableOutput("result_add"),
              hr(),
              actionButton("deleteExpense", "Usuń pozycję", class = "btn-primary",
                           icon=icon("trash", class = NULL, lib = "font-awesome"))
    ))
)

#########See expenses tab##########

seeExpenses = fluidRow(
  bs4ValueBox(
    width = 2,
    value = 123,
    subtitle = "Suma wydatków w bieżącym miesiącu",
    status = "primary",
    icon = "shopping-cart"
  ),
  bs4ValueBox(
    width = 2,
    value = 11,
    subtitle = "Dom",
    status = "danger",
    icon = "cogs"
  ),
  bs4ValueBox(
    width= 2,
    value = 12,
    subtitle = "Przyjemności",
    status = "warning",
    icon = "sliders"
  ),
  bs4ValueBox(
    width= 2,
    value = 12,
    subtitle = "Dzieci",
    status = "warning",
    icon = "sliders"
  ),
  column(width = 3,
         dateRangeInput("dates", h3("Przedział czasowy")),
         actionButton("subsetDate", "Zatwierdź", class = "btn-primary"),
         actionButton("clear","Wyczyść")),
  
  column(
    width = 7,
    wellPanel(h3(strong("Twoje wydatki")), br(),
              DT::dataTableOutput("result")
    )),    
  column(
    width = 6,
    plotlyOutput("piePlot")
  ),
  column(
    width = 6,
    plotlyOutput("hist")
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
dataMenu = bs4TabItem(tabName = "dataMenu",
                      data)
userPanelMenu = bs4TabItem(tabName = "userPanelMenu",
                           userPanelInformations,userPanel)
addExpensesMenu = bs4TabItem(tabName = "addExpensesMenu",
                             addExpenses)
seeExpensesMenu = bs4TabItem(tabName = "seeExpensesMenu",
                             seeExpenses)
#Combine all body components
body = bs4DashBody(
  bs4TabItems(dashboardMenu,calendarMenu, filesMenu,mailMenu,dataMenu, userPanelMenu,addExpensesMenu,seeExpensesMenu)
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

# Wrap UI with secure_app
ui <- secure_app(ui)