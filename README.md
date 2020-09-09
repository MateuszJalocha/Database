# Database

The application was developed for the management of associations clients.  The main goal is to support the registration of attendance and payments, however, it also provides a number of additional functionalities, which are listed below. Currently, at https://matjalocha.shinyapps.io/Database you can use the "beta" version using a trial account (**login:** admin, **password:** 123). The following screenshots show the application layout.

<p align="center">
<img align = "center" src ="Images/database_Dashboard.png" />
</p>

<p align="center">
<img align = "center" src ="Images/database_Payments.png" />
</p>

## Functionalities

- **Dashobard** - basic information on the number of overdue payments and the number of customers. List of overdue payments on the chart and in the table.
- **Wpłaty** - registration and management of monthly and annual payments
- **Dane osobowe** - personal data of customers. Possibility of adding new clients using xlsx and csv files and a built-in form. Moreover, it is possible to add clients to specific groups.
- **Dziennik** - registration of customer attendance
- **Mail** - creating email templates together with editing them and sending group emails by registered user email
- **Dokumenty** - uploading files to the Dropbox with the ability to delete and download them
- **Panel użytkownika** - user account information and the possibility to change it together with a profile photo


## Files

- **www directory** - you can find there profile image
- **Images directory** - you can find there images with application layout
- **global.R** - libraries and database connection
- **server.R** - back-end
- **ui.R** - front-end

## Main libraries

- **bs4Dash**, **shiny**, **shinyWidgets**, **shinymanager** - Layout and login

- **odbc**, **DBI** - Database connection

- **rdrop2**, **rJava**, **mailR** - Dropbox and mail connection

- **ggplot2**, **plotly** - Visualization

- **tidyr**, **dplyr**, **plyr**, **stringr**, **rlist** - Generally useful libraries

## Contributors

- **Mateusz Jałocha** (mat.jalocha@gmail.com, https://www.linkedin.com/in/mateusz-jałocha-8942701b6/)
- **Estera Saidło** (https://github.com/EsteraSaidlo, https://www.linkedin.com/in/estera-saidło-b6882a1b5/)
