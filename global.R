 #Shiny
library(bs4Dash)
library(shiny)
library(shinyWidgets)
library(shinyTime)
library(shinyjs)
#Connecting with Dropbox
library(rdrop2)
library(httpuv)
#Sending mails
library(rJava)
library(mailR)
#MySQL
library(RMySQL)
#Data table
library(data.table)
library(DT)
#XLSX
library(xlsx)
library(readxl)
#Calendar
library(tuicalendr)
#Generally useful
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(plyr)
library(stringr)
library(rlist)
library(shinyalert)
library(R.utils)
#Login
library(shinymanager)
#Map
library(purrr)
#String contains
library(sjmisc)
#Converting dates
library(lubridate)
#Split vector into columns
library(reshape2)
#Database connection
library(odbc)
library(DBI)
#Connection args
library(config)
#File exists
library(RCurl)


#Dropbox
token <- readRDS("droptoken.rds")
drop_acc(dtoken = token)
drop_auth(rdstoken = "droptoken.rds") #auth for drop_delete

#Database connection
conn_args <- config::get("dataconnection")

databaseConnection <- dbConnect(odbc::odbc(),
                                Driver = conn_args$driver, 
                                Server = conn_args$server, 
                                UID = conn_args$uid, 
                                PWD = conn_args$pwd, 
                                Port = conn_args$port, 
                                Database = conn_args$database,
                                TDS_Version = conn_args$version)

#Change labels at login panel
set_labels(
  language = "en",
  "Please authenticate" = "Zaloguj się",
  "Username:" = "Login:",
  "Password:" = "Hasło:",
  "Login" = "Zaloguj"
)
