header <- dashboardHeader(titleWidth = 300,title = "Econometric model",
                          dropdownMenu(
                            type = "messages",
                            messageItem(
                              from = "Mateusz",
                              message = "You can view 1st project",
                              href = "https://spotthestation.nasa.gov/sightings/"
                            ),
                            messageItem(
                              from = "Mateusz",
                              message = "You can view 2nd project",
                              href = "https://spotthestation.nasa.gov/sightings/"
                            )
                          )
)


sidebar <- dashboardSidebar(width = 300,
                            useShinyjs(),
                            sidebarMenu(
                              id = "tabs",
                              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                              menuItem("Upload Data", tabName = "uploaded", icon = icon("upload", lib = "glyphicon"),
                                       
                                       menuSubItem(icon = NULL,
                                                   fileInput("file_name1", "Select Data")),
                                       menuSubItem(icon = NULL,
                                                   actionButton("deleteFile1", "Delete file")),
                                       menuSubItem(icon = NULL,
                                                   sliderInput("sheet1", "Sheet:", min=1, max=20, value=1,
                                                               width = '95%'))
                              ),
                              
                              menuItem("Charts", tabName = "charts", icon = icon("stats", lib = "glyphicon"),
                                       menuItem("Correlation plots", tabName = "corrPlots", icon = icon("menu-right", lib = "glyphicon")),
                                       menuItem("Histograms", tabName = "hist", icon = icon("menu-right", lib = "glyphicon")),
                                       menuItem("Correlations matrix", tabName = "corrMatrix", icon = icon("menu-right", lib = "glyphicon")),
                                       menuItem("Others", tabName = "otherCharts", icon = icon("menu-right", lib = "glyphicon"))
                                       
                              ),
                              menuItem("Make model", tabName = "model", icon = icon("indent-right", lib = "glyphicon")),
                              menuItem("Statistic method", tabName = "statisticMethod", icon = icon("signal", lib = "glyphicon")),
                              menuItem("Boostrap method", tabName = "bootstrapMethod", icon = icon("bitcoin", lib = "glyphicon")),
                              menuItem("Model diagnostics", tabName = "modelDiagnostics", icon = icon("scale", lib = "glyphicon")),
                              menuItem("Model analysis", tabName = "modelAnalysis", icon = icon("tasks", lib = "glyphicon")),
                              menuItem("Compare models", tabName = "compareModels", icon = icon("resize-small", lib = "glyphicon")),
                              tags$script(HTML("$('body').addClass('sidebar-mini');"))
                            )
)

#Dashboard fluid rows
frow1 <- fluidRow(
  div(style="padding-left:15px;",
      uiOutput("numbrows"),
      uiOutput("numbcols")
  )
)
frow2 <- fluidRow( 
  uiOutput("MainBody")
  
)
frow3 <- fluidRow(
  div(style="padding-left:30px;",
      uiOutput("button1")
  ),
  hidden(
    div(id='summary_div',
        tableOutput("summary")
    )
  )
)

#Tab items

dashboard <- tabItem(tabName = "dashboard",
                     frow1,frow2,frow3)


corrPlots <-  tabItem(tabName = "corrPlots", class = "active",
                      
                      h2("Correlation plots"),
                      fluidRow(
                        column(3,
                               fluidRow(
                                 box(
                                   title = "Variables", width = 12, status = "primary", collapsible = T, solidHeader = T,
                                   selectInput("corrplot_model", "Select model", choices = "Pending Upload"),
                                   selectInput("yAxis", "Select y-axis variable", choices = "Pending Upload"),
                                   selectInput("xAxis", "Select x-axis variable", choices = "Pending Upload")
                                   
                                 )
                               ),
                               
                               fluidRow(
                                 box(
                                   title = "Parameters", width = 12, status = "primary", collapsible = T, solidHeader = T, collapsed = T,
                                   radioButtons("smooth", "Smooth:",
                                                c("Yes" = "yes",
                                                  "No" = "no")),
                                   radioButtons("se", "Se:",
                                                c("Yes" = "yes",
                                                  "No" = "no")),
                                   radioButtons("method", "Method:",
                                                c("lm" = "lm",
                                                  "loess" = "loess",
                                                  "gam" = "gam")),
                                   selectInput("colorSelect", "Color by?", choices = "Pending Upload"),
                                   selectInput("shapeSelect", "Shape by?", choices = "Pending Upload"),
                                   radioButtons("whiteNoise", "Add white noise:",
                                                c("Yes" = "yes",
                                                  "No" = "no"), selected = "no")
                                 )
                               )
                        ),
                        uiOutput("boxcorrPlot")
                      )
                      
                      
)


hist <-  tabItem(tabName = "hist",
                 h2("Histograms"),
                 box(
                   title = "Parameters", width = 4, status = "primary", collapsible = T, solidHeader = T,
                   selectInput("hist_model", "Select model", choices = "Pending Upload"),
                   sliderInput("breaks", "Breaks:", min=1, max=200, value=20),
                   selectInput("histSelect", "Select x-axis variable", choices = "Pending Upload")
                   
                 ),
                 
                 uiOutput("boxHist")
                 
)

corrMatrix <- tabItem(tabName = "corrMatrix",
                      h2("Correlation matrix"),
                      box(
                        title = "Parameters", width = 4, status = "primary",collapsible = T, solidHeader = T,
                        selectInput("matrix_model", "Select model", choices = "Pending Upload"),
                        radioButtons("matrix", "Select matrix type:",
                                     c("Independent" = "independent",
                                       "Dependent" = "dependent")),
                        selectInput("matrixSelect", "Select dependent variable", choices = "Pending Upload")
                      ),
                      uiOutput("boxMatrix")
)

otherCharts <- tabItem(tabName = "otherCharts", class = "active",
                       h2("Other charts"),
                       fluidRow(
                         box(
                           title = "Parameters", width = 3, status = "primary",collapsible = T, solidHeader = T,
                           selectInput("others_model", "Select model", choices = "Pending Upload"),
                           selectInput("chartSelect", "Select plot type", choices = c("Boxplot" = "boxplot",
                                                                                      "Density" = "density",
                                                                                      "Density Ridges"= "densityRid",
                                                                                      "Violin" = "violin")),
                           
                           selectInput("yAxisOther", "Select y-axis variable", choices = "Pending Upload"),
                           
                           selectInput("xAxisOther", "Select x-axis variable", choices = "Pending Upload")
                         ),
                         uiOutput("boxotherCharts")
                       )
)

model <- tabItem(tabName = "model",
                 h2("Choose variables to your model"),
                 box(
                   title = "Parameters", width = 3, status = "primary",collapsible = T, solidHeader = T,
                   selectInput("modelDepVar", "Select dependent variable", choices = "Pending Upload"),
                   pickerInput("modelIndVar","Select independent variable", choices="Pending upload", options = list(`actions-box` = T),multiple = T),
                   textInput("modelName", "Choose your model name", "Model 1"),
                   uiOutput("downloadSaveModel_butt"),
                   uiOutput("clearModel_butt")
                 ),
                 
                 uiOutput("modelDTX")
                 
)

statisticMethod <- tabItem(tabName = "statisticMethod",
                           h2("Choose statistic method"),
                           fluidRow(
                             column(3,
                                    box(
                                      title = "Parameters", width = 12, status = "primary",collapsible = T, solidHeader = T,
                                      selectInput("statisticMethod_model", "Select model", choices = "Pending Upload"),
                                      selectInput("statisticMethod","Select statistic method", choices="Pending upload"),
                                      
                                      uiOutput("statisticMethod_butt"),
                                      uiOutput("saveStaticModel_butt"),
                                      uiOutput("clearStaticModel_butt")
                                    ),
                                    uiOutput("hellwig_resultsDT")
                             ),
                             uiOutput("model_Hellwig")
                           ),
                           uiOutput("graphMatrix_output")
)

bootstrapMethod <- tabItem(tabName = "bootstrapMethod",
                           box(
                             title = "Parameters", width = 3, status = "primary",collapsible = T, solidHeader = T,
                             selectInput("boostrapMethod_model", "Select model", choices = "Pending Upload"),
                             numericInput("alfa", "Select alfa:",value = 0.5, min = 0.001, max = 0.999, step = 0.01),
                             numericInput("bootstrapTrials", "Select number of trials:",value = 100, min = 100, max = 10000, step = 100),
                             
                             uiOutput("bootstrapMethod_butt"),
                             uiOutput("saveBootstrapModel_butt"),
                             uiOutput("clearBootstrapModel_butt")
                             
                           ),
                           
                           verbatimTextOutput("summary2")
)

compareModels <- tabItem(tabName = "compareModels",
                         fluidRow(
                           column(3,
                                  box(
                                    title = "Parameters", width = 12, status = "primary",collapsible = T, solidHeader = T,
                                    selectInput("compare_model1", "Select model for bootstrap method", choices = "Pending Upload"),
                                    selectInput("compare_model2", "Select model for classic method", choices = "Pending Upload"),
                                    numericInput("compareTrials", "Select number of trials:",value = 100, min = 100, max = 10000, step = 100),
                                    numericInput("densitySize", "Select size of density:",value = 1, min = 1, max = 5, step = 1),
                                    
                                    uiOutput("compare_butt"),
                                    uiOutput("clearCompare_butt")
                                  )
                           ),
                           column(7,
                                  uiOutput("coef_DT")
                           )
                         ),
                         uiOutput("coef_plot")
                         
)
# combine fluid rows to make the body
body <- dashboardBody(tabItems(dashboard,hist,corrMatrix, corrPlots, otherCharts, uiOutput("basicStats"),model, statisticMethod, bootstrapMethod, compareModels))

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header,sidebar,body)