#Add to bs4dash possibility to use offset in column function
column <- function(width, ..., offset = 0) {
  if (!is.numeric(width) || (width < 1) || (width > 12)) 
    stop("column width must be between 1 and 12")
  colClass <- paste0("col-sm-", width)
  if (offset > 0) 
    colClass <- paste0(colClass, " offset-sm-", offset)
  shiny::div(class = colClass, ...)
}

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
  uiOutput("monthPayment_input"),
  uiOutput("yearPayment_input"),
  uiOutput("monthPayment_input_max"),
  uiOutput("yearPayment_input_max"),
  uiOutput("number_of_classes"),
  div(style="text-align: center;",actionButton("globalChanges", "Zapisz zmiany", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
  uiOutput("selectedGroup"),
  br(),
  skin = "light",
  title = "Ustawienia limitów",
  width = 250,
  controlbar_overlay = TRUE,
  inputId = "controlBar"
)

#Footer options
footer = bs4DashFooter()


#####################################
##########Sidebar content############
#####################################

sidebar = bs4DashSidebar(
  skin = "dark",
  status = "primary",
  title = tags$h4("Baza danych"),
  brandColor = "primary",
  src = "https://img.icons8.com/plasticine/100/000000/database-clocks.png",
  elevation = 3,
  opacity = 0.8,
  uiOutput("sidebar_userPanel"),
  bs4SidebarMenu(
    id = "sidebarMenu",
    bs4SidebarMenuItem(
      "Dashboard",
      tabName = "dashboardMenu",
      icon = "dashboard"
    ),
    bs4SidebarMenuItem(
      "Dane",
      icon = "database",
      bs4SidebarMenuSubItem(
        text = "Wpłaty",
        tabName = "paymentsDataMenu",
        icon = "money-bill"
      ),
      bs4SidebarMenuSubItem(
        text = "Dane osobowe",
        tabName = "dataMenu",
        icon = "table"
      )
    ),
    bs4SidebarMenuItem(
      "Dziennik",
      tabName = "classregisterMenu",
      icon = "book"
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
      icon = "address-card"
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
      title = "Wpłaty",
      maximizable = TRUE,
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE,
      plotlyOutput("barPlot_payments")
    )
  ),
  column(
    width = 6,
    bs4Card(
      title = "Kalendarz",
      maximizable = TRUE,
      closable = TRUE,
      width = 12,
      status = "primary",
      solidHeader = FALSE,
      collapsible = TRUE,
      calendarOutput("calendar")
    )
  )
)

#Information on who has not paid in or has paid only part of the amount
notPaid_informations = fluidRow(
  bs4TabCard(
    id = "notPaidTab",
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

#Information boxes about total payments in the current month, number of people who have not paid this month and 
#total number of persons who have not paid
info_valueBoxes = fluidRow(
  column(
    width = 3,
    bs4ValueBox(
      width = 12,
      value = tags$h5(textOutput("paymentsSum_month")),
      subtitle = textOutput("paymentsSum_month_subtitle"),
      status = "success",
      icon = "shopping-cart"
    )
  ),
  column(
    width = 3,
    bs4ValueBox(
      width = 12,
      value = tags$h5(textOutput("paymentsSum_year")),
      subtitle = textOutput("paymentsSum_year_subtitle"),
      status = "danger",
      icon = "cogs"
    )
  ),
  column(
    width = 3,
    bs4ValueBox(
      width= 12,
      value = tags$h5(textOutput("notpaid_all")),
      subtitle = textOutput("notpaid_all_subtitle"),
      status = "warning",
      icon = "sliders"
    )
  ),
  column(
    width = 3,
    bs4ValueBox(
      width = 12,
      value = tags$h5(textOutput("number_of_clients")),
      subtitle = textOutput("number_of_clients_subtitle"),
      status = "info",
      icon = "users"
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
      uiOutput("fileInput"),
      actionButton("uploadFile", "Zapisz plik", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  ),
  
  #Uploaded files
  column(
    width = 8,
    bs4Card(
      title = "Wgrane pliki",
      width = 12,
      closable = FALSE,
      solidHeader = FALSE,
      dataTableOutput("files_dropbox"),
      br(),
      fluidRow(
        column(
          width = 6,
          uiOutput("filesPicker"),
          actionButton("downloadFiles", "Pobierz plik", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        column(
          width = 6,
          uiOutput("filesPickerDelete"),
          actionButton("deleteFiles", "Usuń pliki", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      )
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
      title = "Utwórz szablon maila",
      width = 12,
      solidHeader = FALSE,
      textInput("templateNameCreate", "Nazwa szablonu"),
      textAreaInput("templateBodyCreate", "Szablon maila", "Dzień dobry,", resize = "vertical") %>%
        shiny::tagAppendAttributes(style = 'width: 100%;'),
      actionButton("saveTemplate", "Zapisz szablon", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    #Edit mail template
    bs4Card(
      closable = FALSE,
      collapsed = TRUE,
      title = "Edytuj szablon",
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
      fileInput("filesMail", label = "Wgraj pliki", multiple = T),
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
    dataTableOutput("datasetTable"),
    br(),
    actionButton("addToGroups", "Dodaj grupę", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("addToGroupsPicker", "Dodaj grupę wybranym", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("manageGroups", "Zarządzaj grupami", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
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


#########Payments tab#####################
paymentsData = fluidRow(
  bs4Card(
    title = "Wpłaty",
    width = 12,
    closable = FALSE,
    solidHeader = FALSE,
    dataTableOutput("paymentsTable"),
    br(),
    actionButton("addPayments", "Dodaj wpłatę", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("addPaymentsPicker", "Dodaj wpłatę wybranym", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("managePaymentsColumn", "Zarządzaj kolumnami wpłat", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
)
#########School register tab##########
classRegister = fluidRow(
  bs4Card(
    title = "Lista uczestników",
    width = 12,
    closable = FALSE,
    solidHeader = FALSE,
    dataTableOutput("schoolRegister_dataTable"),
    actionButton("addPresences", "Dodaj obecności", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("addPresencesPicker", "Dodaj obecności wybranym", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    actionButton("managePresencesColumn", "Zarządzaj kolumnami obecności", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    
  )
)
############User panel tab###############

#Informations on persons who have not made payments and total payments in the months concerned
userPanel = fluidRow(
  column(
    width = 9,
    offset = 4,
    uiOutput("userPanel")
  )
)
 
#############Tab items#################

dashboardMenu = bs4TabItem(tabName = "dashboardMenu",
                           info_valueBoxes, payments, notPaid_informations)
filesMenu = bs4TabItem(tabName = "filesMenu",
                       files)
mailMenu = bs4TabItem(tabName = "mailMenu",
                      mail)
dataMenu = bs4TabItem(tabName = "dataMenu",
                      data)
paymentsDataMenu = bs4TabItem(tabName= "paymentsDataMenu",
                              paymentsData)
userPanelMenu = bs4TabItem(tabName = "userPanelMenu",
                           userPanel)
classregisterMenu = bs4TabItem(tabName = "classregisterMenu",
                               classRegister)
#Combine all body components
body = bs4DashBody(
  useShinyjs(), bs4TabItems(dashboardMenu, filesMenu,mailMenu,dataMenu,paymentsDataMenu,classregisterMenu, userPanelMenu)
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
ui <- shinymanager::secure_app(ui,
                               tags_bottom = tags$div(
                                 tags$p(
                                   "Trial account (login: admin, hasło: 123)"
                                 )
                               ),
                               background = "linear-gradient(rgb(66,139,202),rgb(45,84,94));",
                               tags$style(HTML('#auth_ui {background-color: #BF00FF;}'))
                               )
