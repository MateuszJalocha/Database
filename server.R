#Global variables
ActualMonth = month(Sys.Date())
ActualYear = year(Sys.Date())
Credentials = dbReadTable(databaseConnection, "wlasciciel_konta")
Clients = dbReadTable(databaseConnection, "uczestnicy")
Groups = dbReadTable(databaseConnection, "grupy")
Amounts = dbReadTable(databaseConnection, "kwota_wplaty")
Group = "wszyscy"
UserPanelPosition = 3
ProfileDir = "Baza Danych/Profilowe/"

#Creating a data frame with the data of persons who have not paid or have paid part of the payments
notPaid <- function(vals, data, time_period) {
  
  #Verification of the length of the given comparative values. If the length is 1, it checks who has not paid and in what months.
  #If the length is 2, it verifies who has made part of the contributions and in what months.
  if(length(vals) == 1)
  {
    #Checking whether annual or monthly payments are to be checked
    if(time_period == "month") {
      #Filtering people who have not paid in one of the months 
      notPaid = data %>% filter_at(vars(.[,str_detect(names(.),"wplata"), drop = FALSE] %>% colnames() %>% as.data.frame() %>%
                                          filter(!str_detect(., "roczna")) %>% unlist() %>% as.vector()), any_vars(. == vals[1]))
      #Writing down dates on which individuals have not paid
      notPaid_dates = apply(notPaid[,str_detect(names(notPaid),"wplata"), drop = FALSE]  %>% select(-contains("roczna")), 1, function(x){names(x[which(x == vals[1])])})
      
      #Checking that the dates have been assigned correctly
      if(nrow(notPaid) != length(notPaid_dates))
        notPaid_dates = notPaid_dates %>% as.data.frame() %>% as.list()
      
    } else {
      #Filtering people who have not paid in one of the years
      notPaid = data %>% filter_at(vars(data[,str_detect(names(data),"wplata"), drop = FALSE] %>% colnames() %>% as.data.frame() %>%
                                          filter(str_detect(., "roczna")) %>% unlist() %>% as.vector()), any_vars(. == vals[1]))
    }
    
  } else {
    
    #Checking whether annual or monthly payments are to be checked
    if(time_period == "month")
    {
      #Filtering people who have paid part of the deposit in one of the months
      notPaid = data %>% filter_at(vars(data[,str_detect(names(data),"wplata"), drop = FALSE] %>% colnames() %>% as.data.frame() %>%
                                          filter(!str_detect(., "roczna")) %>% unlist() %>% as.vector()), any_vars(. > vals[1] & . < vals[2]))
      
      #Writing down dates on which individuals have not paid
      notPaid_dates = apply(notPaid[,str_detect(names(notPaid),"wplata"), drop = FALSE]  %>% select(-contains("roczna")), 1, function(x){names(x[which((x > vals[1]) & (x < vals[2]))])})
      
      #Checking that the dates have been assigned correctly
      if(nrow(notPaid) != length(notPaid_dates))
        notPaid_dates = notPaid_dates %>% as.data.frame() %>% as.list()
      
    } else {
      #Filtering people who have paid part of the deposit in one of the years
      notPaid = data %>% filter_at(vars(data[,str_detect(names(data),"wplata"), drop = FALSE] %>% colnames() %>% as.data.frame() %>%
                                          filter(str_detect(., "roczna")) %>% unlist() %>% as.vector()), any_vars(. > vals[1] & . < vals[2]))    
    }
  }
  
  #Checking whether annual or monthly payments are to be checked
  if(time_period == "month"){
    
    #Recording dates with outstanding payments in one table and correcting their row names
    notPaid_dates = lapply(notPaid_dates, function(x, y = notPaid){paste(gsub("wplata_","",x),collapse = ", ") }) %>% list.rbind()
    notPaid_dates = gsub("_","-", notPaid_dates)
    rownames(notPaid_dates) = seq(1, nrow(notPaid_dates))
    
    #Combining tables with personal data and information on dates with overdue payments
    #and sorting them according to the number of overdue payments (degressive)
    notPaid_df = data.frame(Imie = notPaid$imie, Nazwisko = notPaid$nazwisko,"Imie opiekuna" = notPaid$imie_opiekuna,
                            "Nazwisko opiekuna" = notPaid$nazwisko_opiekuna, "Numer opiekuna" = notPaid$telefon_opiekuna,
                            "Email opiekuna" = notPaid$email_opiekuna,"Daty bez wpłat" = notPaid_dates,
                            "Ile zaległych wpłat" = lengths(strsplit(notPaid_dates, ",")))
    notPaid_df = notPaid_df[order(notPaid_df$Ile.zaległych.wpłat, decreasing = TRUE),]
  } else{
    #In the case of overdue annual payments, only personal data shall be written off
    notPaid_df = data.frame(Imie = notPaid$imie, Nazwisko = notPaid$nazwisko,"Imie opiekuna" = notPaid$imie_opiekuna,
                            "Nazwisko opiekuna" = notPaid$nazwisko_opiekuna, "Numer opiekuna" = notPaid$telefon_opiekuna,
                            "Email opiekuna" = notPaid$email_opiekuna)
  }
  return(notPaid_df)
}

#Verification whether tables with overdue payments exist
tables_exists_verification <- function(vals, data, time_period, kind) {
  
  data <- tryCatch(
    {
      #Use the function to create a table with overdue payments and add spaces in the names of their columns
      addWhitespaces(notPaid(vals, data, time_period), kind = kind)
    },
    error=function(cond) {
      #Checking whether monthly or annual payments are taken into account and creating appropriate blank tables for the selected option
      if(time_period == "month"){
        col_names = c("Imie","Nazwisko","Imie opiekuna","Nazwisko opiekuna", "Numer opiekuna",
                      "Email opiekuna","Daty bez wpłat","Ile zaległych wpłat")
        setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)),col_names)
      } else{
        col_names = c("Imie","Nazwisko","Imie opiekuna","Nazwisko opiekuna", "Numer opiekuna","Email opiekuna")
        setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)),col_names)
      }
      
    }
  )
}

#Checking if there are any files on the dropbox
files_exists_verification <- function(path){
  
  #Checking for errors
  files <- tryCatch(
    {
      #Create data tables containing information about the files
      data.table(Nazwa = drop_dir(path, dtoken = token)$name,Rozmiar = paste(round(drop_dir(path, dtoken = token)$size *10^-6,3), "MB"))
    },
    #In case of errors, create empty data tables
    error=function(cond) {
      
      col_names = c("Nazwa", "Rozmiar")
      setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)),col_names)
    },
    warning=function(cond) {
      
      col_names = c("Nazwa", "Rozmiar")
      setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)),col_names)
    }
  )
  
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

#Checking if there are any dates without any deposits to display
paymentsPlot_cols_exists_vericifaction <- function(data, year, currentMonth, currentYear, prevDates = 2, nextDates = 2){
  
  #Checking for errors
  data <- tryCatch(
    {
      #Distinction into columns for annual and monthly contributions
      if(year){
        #Select rows for annual contributions from the current year and those in which not all contributions have been made
        data = data[grep("-", data$Date, invert = year),]
        data$Date = data$Date %>% as.character() %>% as.numeric()
        toDisplay = data[data$Date == currentYear,]
        verifyDisplay = data[!(data$Date == currentYear),] %>% .[!(.$Not_paid == 0 & .$Part_paid == 0),]
      } else {
        #Select rows for monthly deposits from the current month +/- "nextDates"/"prevDates" and those in which not all deposits have been made
        data = data[grep("-", data$Date, invert = year),] %>% separate(Date,c("Month","Year") ,"-", remove = FALSE)
        data$Month = as.numeric(data$Month)
        toDisplay = data[data$Month >= currentMonth - prevDates & data$Month <= currentMonth + nextDates & data$Year == currentYear,]
        verifyDisplay = data[!(data$Month >= currentMonth - prevDates & data$Month <= currentMonth + nextDates & data$Year == currentYear),] %>% .[!(.$Not_paid == 0 & .$Part_paid == 0),]
      }
      
      #If there are missing deposits in the "remote" dates, add them to the data table that will be displayed
      if(nrow(verifyDisplay) != 0)
        toDisplay = rbind(verifyDisplay, toDisplay)
      #Delete added columns 'Month' and 'Year'
      toDisplay[,!(colnames(toDisplay) %in% c("Month", "Year"))]
    },
    #In case of errors, create empty data table
    error=function(cond) {
      col_names = c("Paid", "Not_paid", "Part_paid", "Date")
      setNames(data.frame(matrix(ncol = length(col_names), nrow = 0)),col_names)
      
    }
  )
  return(data)
}

#Selecting the columns is the display on the graph (Dashboard)
select_paymentsPlot_cols <- function(data,prevDates =2, nextDates = 2, currentMonth,currentYear) {
  
  toDisplay_year = paymentsPlot_cols_exists_vericifaction(data = data, year = TRUE, currentMonth = currentMonth, currentYear = currentYear, prevDates = prevDates, nextDates = nextDates)
  toDisplay_month = paymentsPlot_cols_exists_vericifaction(data = data, year = FALSE, currentMonth = currentMonth, currentYear = currentYear, prevDates = prevDates, nextDates = nextDates)
  
  return(rbind(toDisplay_year, toDisplay_month))
}

#Creating a confirmation alert
confirmAlert <- function(session,button_labels, inputId, title, type = NULL) {
  confirmSweetAlert(
    session = session,
    inputId = inputId,
    title = title,
    type = type,
    btn_labels = button_labels
  )
}  

#Creating a information alert
informationAlert <- function(session,title, type = "success") {
  sendSweetAlert(
    session = session,
    title = title,
    type = type
  )
}

#Creating a delete buttons for Files tab
deleteDownloadButtons <- function(df, id, ...) {
  #Function to create one action button as string
  deleteButton <- function(i) {
    as.character(
      actionButton(
        #The id prefix with index
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
                extensions = c ("Responsive"),
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(
                    list(targets = 1, sortable = FALSE))
                ))
}

#Return at which row the delete or download button was pressed
parseDeleteDownloadEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}  

#Creating a picker with the column names contained in the data
addPayments_picker <- function(inputId, label, type, data,multiple = TRUE, kind){
  pickerInput(
    inputId = inputId, 
    label = label, 
    choices = addPayments_choices(type, data, kind), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = multiple
  )
}

#Replacement of the included payment and presences columns with their verbal representation
addPayments_choices <- function(type, data, kind){
  
  #Create a vector with the verbal equivalents of months and choose the names of columns concerning payments from the data
  months = c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień",
             "Październik", "Listopad", "Grudzień")
  choices_all= sort_columns(data, FALSE,kind=kind)
  
  #Checking whether monthly or annual payments are taken into account and
  #replacing the names of the columns with affordable representations to be used as elections
  if(type == "month"){
    choices = choices_all[!choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("_", " ",  gsub(paste0(kind,"_"), "", choices))
    choices = lapply(strsplit(choices, " "), function(x){ paste(months[as.numeric(x[1])], x[2])}) %>% unlist()
  } else {
    choices = choices_all[choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("wplata_roczna_", "", choices)
  }
  
  return(choices)
}

#Creating a picker with the groups contained in the data
groups_picker <- function(inputId, label, data,multiple = TRUE,selected = NULL){
  if(is.null(ncol(data))){
    choices = data
  } else{
    choices = data$grupa
  }
  pickerInput(
    inputId = inputId, 
    label = label, 
    choices = unique(unlist(choices)), 
    selected = selected,
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = multiple
  )
}

#Creating a picker with additional dates
additionalDates_picker <- function(inputId, label, type, data,multiple = TRUE, kind){
  pickerInput(
    inputId = inputId, 
    label = label, 
    choices = additionalDates_choices(type,data,kind), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = multiple
  )
}

#Creation of additional elections containing all months of the current, previous or next year excluding those already included in the data
additionalDates_choices <- function(type,data, kind){
  #Create vector with all months
  months = c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień",
             "Październik", "Listopad", "Grudzień")
  
  #Create a date frame containing the names of months and years 5 months from the current date
  displayDates = add_with_rollback(Sys.Date(), months(-5:5)) %>% format(., "%m %Y") %>% as.data.frame() %>% separate(.,.,c("month", "year"), " ")
  displayDates$month = displayDates$month %>% as.numeric() %>% months[.]
  dates = paste(displayDates$month, displayDates$year)
  
  #Create a date frame containing years from the current date
  years = c((year(current_date) - 1 ): (year(current_date) + 1))
  
  #Selecting the names of columns containing "kind", which take the values "deposit" or "presence"
  choices_all= colnames(data[colnames(data) %in% grep(paste0(kind, collapse = "|"), colnames(data), value = TRUE)])
  
  #Checking whether annual or monthly payments or presences are to be checked and
  #selecting only those dates that are not included in the data
  if(type == "month"){
    choices = choices_all[!choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("_", " ", gsub(paste0(kind,"_"), "", choices))
    choices = lapply(strsplit(choices, " "), function(x){ paste(months[as.numeric(x[1])], x[2])}) %>% unlist()
    choices = dates[!dates %in% choices]
  } else {
    choices = choices_all[choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
    choices = gsub("wplata_roczna_", "", choices)
    choices = years[!years %in% choices]
  }
  return(choices)
}

#Converting selected month to proper format
convertDates <- function(date, kind){
  months = list("Styczeń" = "01", "Luty" = "02", "Marzec" = "03", "Kwiecień" = "04", "Maj" = "05", "Czerwiec" = "06",
                "Lipiec" = "07", "Sierpień" = "08", "Wrzesień" = "09", "Październik" = "10", "Listopad" = "11",
                "Grudzień" = "12")
  converted_dates = lapply(as.list(date), function(x) {
    month = strsplit(x, " ")[[1]][1]
    gsub(paste0(month, " "), paste0(paste0(kind,"_"),months[[month]],"_"), x)
  })
  
  return(converted_dates)
}

#Converting selected year to "wplata_roczna_selectedYear" format
convertYear <- function(date){
  converted_year = paste0("wplata_roczna_", date)
  
  return(converted_year)
}

#Creating a picker with the clients contained in the data
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

#Sort columns to display in tables
sort_columns <- function(data, sorting_for_dataFrame = TRUE, kind){
  current_date = Sys.Date()
  #Select the names of columns with payments or presences
  choices_all= colnames(data[colnames(data) %in% grep(paste0(kind, collapse = "|"), colnames(data), value = TRUE)])
  
  #Create tables with information about the participant and payments
  data_infos = data.frame(Uczestnik = paste(data$imie, data$nazwisko),
                          Opiekun = paste(data$imie_opiekuna, data$nazwisko_opiekuna),
                          Telefon_opiekuna = data$telefon_opiekuna)
  data_dates = data[, which(names(data) %in%choices_all), drop = FALSE]
  
  #Assign columns with monthly and annual contributions to the variables
  choices_months = choices_all[!choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
  choices_years = choices_all[choices_all %in% grep(paste0("roczna", collapse = "|"), choices_all, value = TRUE)]
  
  #Check if there are any monthly payments, if so, select months and years and then sort in ascending order
  if(length(choices_months) != 0) {
    choices_months = gsub("_", "-", gsub(paste0(kind,"_"), "", choices_months)) %>% data.frame() %>% separate(".", c("Month", "Year"))
    choices_months = choices_months[with(choices_months, order(Year, Month)),]
    choices_months = paste0(kind,"_",choices_months$Month, "_",choices_months$Year)
  }
  #Check if there are any annual payments if you choose the years, then sort them in ascending order
  if(length(choices_years) != 0) {
    choices_years = gsub("wplata_roczna_", "", choices_years) %>% data.frame()
    colnames(choices_years) = "Year"
    choices_years = choices_years[order(choices_years$Year),]
    choices_years = paste0("wplata_roczna_",choices_years)
  }
  #Combine the sorted dates of monthly and annual payments
  choices = c(choices_months, choices_years)
  
  #Check if the sorting for the data frame has been selected - if not return only the sorted names
  if(sorting_for_dataFrame){
    
    #Assign to the variable valid for checking dates: current and previous month and current year
    important_dates = c(paste0(paste0(kind,"_0",month(current_date) - 1), "_",year(current_date)),
                        paste0(paste0(kind,"_0",month(current_date)), "_",year(current_date)),
                        paste0("wplata_roczna_",year(current_date)))
    
    #Checking for errors
    data <- tryCatch(
      {
        #Check if there is at least one important date in the data
        if(length(data[,which(names(data) %in% important_dates)]) != 0) {
          data = cbind(data_infos, data[,match(important_dates,colnames(data), nomatch = 0), drop = FALSE], data_dates[,choices[!choices %in% important_dates], drop = FALSE])
        } else {
          data = cbind(data_infos,data_dates[,choices[!choices %in% important_dates], drop = FALSE])
        }
      },
      error=function(cond) {
        #If an error has occurred, return participants personal data
        data = data_infos
        return(data)
      }
    )
  } else{
    data =  choices
  }
  return(data)
}

#Return indexes of customers who have been marked
selectedClients_rows = function(data, input_values) {
  data = paste0(data$imie, " ",data$nazwisko, ", ", data$pesel)
  return(which( data%in% input_values))
}

#Verification whether the payment column for the current month or year has been added
currentDate_exists_verification <- function(data, colname, type,text,payment) {
  
  #Checking for errors
  data <- tryCatch(
    {
      #Summary of payments made in the current month or year and all that should be made
      sum_of_payments = sum(data[colname][data[colname] >= 0])
      total_payments = nrow(data[data[colname] >= 0,,drop = FALSE]) * payment
      list(paste0(sum_of_payments," zł/",total_payments, " zł"),text)
    },
    #In case of an error, show a message that the payment column for the current month or year has not been added
    error=function(cond) {
      if(type == "month")
        return(list("-","Nie dodano kolumny wpłat bieżącego miesiąca"))
      else
        return(list("-","Nie dodano kolumny wpłaty rocznej"))
    }
  )
  return(data)
}

#Verification whether any column concerning deposits has been added to the data
anyDateCols_exists_verification <- function(data, value,text) {
  
  #If there is no column for deposits in the data, return a message about it
  if(length(data[,str_detect(names(data),"wplata")])){
    payments_columns = data[,str_detect(names(data),"wplata")]
    return(list(length(payments_columns[payments_columns == value]),text))
  } else{
    return(list("-", "Nie dodano żadnej kolumny wpłat"))
  }
}

#Verification whether the data contain any customer
clients_exists_verification <- function(data, text){
  
  #Checking for errors
  data <- tryCatch(
    {
      list(nrow(data),text)
    },
    #In case of an error, show the message that no client has been added
    error=function(cond) {
      return(list("-","Nie dodano żadnego klienta"))
    }
  )
  return(data)
}

#Replace "_" and "." with spaces in the column names
addWhitespaces <- function(data,kind){
  #Renaming of contribution columns
  new_colnames = data[,str_detect(names(data),paste(kind)), drop = FALSE] %>% colnames() %>%
    str_replace_all("_","-") %>%
    str_replace_all(paste0(kind,"-"),paste0(kind," "))
  colnames(data)[str_detect(names(data),paste(kind))] = new_colnames
  
  #Replacing the other columns "_" and "." with spaces
  colnames(data) = gsub("_", " ", colnames(data))
  colnames(data) = gsub("\\.", " ", colnames(data))
  
  #Change "-1" to "-" in displaying data tables
  for(i in new_colnames){
    data[[i]][data[[i]] == -1] = "-"
  }
  return(data)
}

#Preparing data to create a chart with people who have paid, failed to pay and paid a part of it
notPaid_partPaid_paid_plot <- function(data, current_year,MonthPayment, YearPayment, current_month){
  #Assigning to the variables columns with monthly and annual payments
  monthPayments_columns = data[,str_detect(names(data),"wplata"), drop = FALSE] %>% select(-contains("roczna"))
  yearPayment = data[,str_detect(names(data),"wplata"), drop = FALSE] %>% select(contains("roczna"))
  
  #Assigning to the variable number of persons who made payments, failed to make payments and paid part of the payments
  notPaid_year = apply(yearPayment, 2, function(x){length(x[x==0])})
  partPaid_year = apply(yearPayment, 2, function(x){length(x[x<YearPayment & x>0])})
  paid_year =apply(yearPayment, 2, function(x){length(x[x>=YearPayment])})
  
  notPaid = apply(monthPayments_columns, 2, function(x){length(x[x==0])})
  partPaid = apply(monthPayments_columns, 2, function(x){length(x[x<MonthPayment & x>0])})
  paid = apply(monthPayments_columns, 2, function(x){length(x[x>=MonthPayment])})
  
  #Change the names of deposit dates to nicer and more readable
  dateMonths = gsub("_", "-", colnames(monthPayments_columns))
  dateMonths = gsub("wplata-", "", dateMonths)
  dateYears = gsub("wplata_roczna_", "", colnames(data[,str_detect(names(data),"roczna"), drop = F])) %>% as.numeric()
  
  #Select only the current, previous and next year for display
  select_years = which(dateYears >= current_year - 1)
  
  data = data.frame(Paid = c(paid,paid_year[select_years]), Not_paid = c(notPaid,notPaid_year[select_years]),
                    Part_paid = c(partPaid,partPaid_year[select_years]),Date = c(dateMonths,dateYears[select_years]))
  
  return(data)
}

#Verification whether the file can be loaded
file_exists_verification <- function(path, file_type){

  #Checking for errors
  file <- tryCatch(
    {
      if(file_type == "xlsx")
        read.xlsx(path, 1, encoding = "UTF-8") %>% .[rowSums( is.na(.) ) < ncol(.), ]
      else
        read.csv(path,sep = ";") %>% .[rowSums( is.na(.) ) < ncol(.), ]
    },
    error=function(cond) {
      NULL
    }
  )
}

#Verifying the correctness of the file 
file_compliance <- function(session,clients_ALL, file, data, group, kind,user_id) {
  #Assignment to variables: personal data, payments and presences columns
  data_infos = data %>% .[,-grep(paste0(kind,"|id_|grupa"), colnames(.))]
  payments_columns = data %>% .[,grep(kind, colnames(.)), drop = FALSE]
  
  data_colnames = data %>% .[,-grep(paste0(kind,"|id_|grupa"), colnames(.))] %>% colnames()
  
  #Checking that the file format is correct, the numbers of columns match and the names of the columns match
  if(is.numeric(file)){
    informationAlert(session,"Zły format pliku", type = "warning")
  } else if(ncol(file) != length(data_colnames)){
    informationAlert(session,"Liczby kolumn się nie zgadzają", type = "warning")
  } else if(length(setdiff(colnames(file),data_colnames))){
    informationAlert(session,"Nazwy kolumn się nie zgadzają", type = "warning")
  } else{
    if(nrow(data)!=0){
      #Create columns with the appropriate "id_participant" number and columns with payments and presences filled in "-1"
      id_columns = data %>% .[,grep("id_", colnames(.))]
      id_columns[1:nrow(file),1] = seq(id_columns$id_uczestnika[length(id_columns$id_uczestnika)] + 1, id_columns$id_uczestnika[length(id_columns$id_uczestnika)] + nrow(file))
      id_columns = id_columns[1:nrow(file),]
      payments_columns[1:nrow(file),] = -1
      payments_columns = payments_columns[1:nrow(file),]
    } else{
      id_columns = data.frame("id_uczestnika" = seq(1, nrow(file)), "id_uzytkownika" = rep(user_id, nrow(file)))
    }
    #Checking for errors
    data <- tryCatch(
      {
        #Convert date from file to "Date" format
        file$data_urodzenia = as.Date(file$data_urodzenia) %>% as.character()
        
        if(nrow(payments_columns) != 0){
          #Create a data frame with the data contained in the uploaded file
          new_data = data.frame(id_columns,imie = file$imie, nazwisko = file$nazwisko, grupa = rep(group, nrow(file)),file[,-which(colnames(file) %in% c("imie", "nazwisko"))],payments_columns)
        } else{
          new_data = data.frame(id_columns,imie = file$imie, nazwisko = file$nazwisko, grupa = rep(group, nrow(file)),file[,-which(colnames(file) %in% c("imie", "nazwisko"))])
        }
        rownames(new_data) = seq(nrow(clients_ALL) + 1, nrow(clients_ALL) + nrow(file))
        new_data
      },
      #If an error has occurred, inform the user that the order of the columns does not match
      error=function(cond) {
        informationAlert(session,"Kolejności kolumn się nie zgadzają", type = "warning")
      }
    )
  }
}

#Adding data from the file to the database
append_rows_files <- function(session, clients_ALL, payments_ALL, schoolRegister_ALL, fileData, clientsData, clientsData_presences, group, connection, clientsTable_name,paymentsTable_name, presencesTable_name,user_id){
  #Verifying the correctness of the file and entering it into the database
  data = file_compliance(session,clients_ALL, fileData, clientsData, group, kind = "wplata",user_id = user_id)
  data_presences = file_compliance(session,clients_ALL, fileData, clientsData_presences, group, kind = "obecnosc", user_id = user_id)
  #Check if the data set is empty - if so, do nothing
  if(!is.null(data)) {
    #Separating data into two tables - (clients and payments & clients and presences)
    append_data = clients_and_payments_separating(data, kind = "wplata")
    append_data_presences = clients_and_payments_separating(data, kind = "obecnosc")
    append_data$clients$data_urodzenia = append_data$clients$data_urodzenia %>% as.Date()
    
    #Checking if the clients from the file are no longer in the database - If there are no customers in the file that are no longer in the database, inform the user
    if(nrow(match_df(clients_ALL[clients_ALL$id_uzytkownika == user_id,-grep("id_|grupa", colnames(clients_ALL))],append_data$clients[,-grep("id_|grupa", colnames(append_data$clients))])) < nrow(append_data$clients)) {
      #Assigning to a variable number of rows in which there are already existing customers in the database
      identical_rows = as.numeric(rownames(match_df(append_data$clients[,-grep("id_", colnames(append_data$clients))],clients_ALL[,-grep("id_", colnames(clients_ALL))])))
      
      #If there are recurring customers, do not add them to the database
      if(length(identical_rows) != 0) {
        #Selecting only non-repeating participants and giving them subsequent row names and id's
        new_rownames = rownames(append_data$clients)
        new_userID = append_data$clients$id_uczestnika
        append_data$clients = append_data$clients[!rownames(append_data$clients) %in% identical_rows,]
        rownames(append_data$clients) = new_rownames[1:nrow(append_data$clients)]
        append_data$clients$id_uczestnika = new_userID[1:nrow(append_data$clients)]
      }

      #Select only distinct observations from the file
      unique_rows = append_data$clients[,!grepl("grupa|id_",colnames(append_data$clients))] %>% distinct() %>% rownames()
      append_data$clients = append_data$clients[-as.numeric(unique_rows),]
      append_data$payments = append_data$payments[which(append_data$payments$id_uczestnika == as.numeric(unique_rows)),]
      append_data_presences$payments = append_data_presences$payments[which(append_data_presences$payments$id_uczestnika == append_data_presences$clients$id_uczestnika[as.numeric(unique_rows)]),]

      #Correct row names and save tables with customer data and payments
      if(nrow(append_data$payments) != 0)
        rownames(append_data$payments) = seq(nrow(payments_ALL) + 1, nrow(payments_ALL) + nrow(append_data$payments))
      if(nrow(append_data_presences$payments) != 0)
        rownames(append_data_presences$payments) = seq(nrow(payments_ALL) + 1, nrow(payments_ALL) + nrow(append_data_presences$payments))
      if(nrow(append_data$clients) != 0)
        rownames(append_data$clients) = seq(nrow(clients_ALL) + 1, nrow(clients_ALL) + nrow(append_data$clients))
      
      return(list(clients = append_data$clients, payments = append_data$payments, presences = append_data_presences$payments))
    } else {
      #If the dataset is empty, inform the user that the clients from the file are already in the database
      informationAlert(session,"Wszyscy z dodawanych klientów znajdują już się w bazie", type = "warning")
    }
  }
}

#Combining the table with customers and payments into one
clients_and_payments_merging <- function(clients, payments) {
  
  #Replacement of the payment table with information on payment dates in rows by dates in columns
  payments = payments %>% spread(data, wartosc) %>% .[,-which(names(.) %in% c("id_uzytkownika","grupa"))]
  
  #Checking for errors, if the payment table is not missing, return the customer data table
  data <- tryCatch(
    {
      base::merge(clients,payments, by = "id_uczestnika")
    },
    error=function(cond) {
      clients
    }
  )
  return(data)
}

#Separating data into two tables - (clients and payments)
clients_and_payments_separating <- function(data,kind) {
  payments = data[,grep(paste0(kind,"|id_|grupa"),colnames(data))] %>% gather(key = "data", value = "wartosc", -c(id_uczestnika, id_uzytkownika,grupa))
  clients = data[,grep(paste0(kind),colnames(data), invert = TRUE)]
  return(list(clients = clients,payments = payments))
}

#Adding deposit columns to the deposit table
clients_and_payments_writeTable_addPaymentsCols <- function(clients_ALL, payments_ALL, data,group, kind) {
  #If the user has not opened the side panel and a group has not been assigned to the variable, enter the group "all"
  if(is.null(group))
    group = "wszyscy"
  
  #Separating data into two tables - (clients and payments)
  data_ALL = clients_and_payments_separating(data, kind = kind)
  
  #Selecting rows with newly added payments and changing the column concerning the group in payments made to the group selected in the general settings
  eddited_payments = data.frame(data_ALL$payments[,grep("id_", names(data_ALL$payments))], data_ALL$payments[,-grep("id_", names(data_ALL$payments))])
  eddited_payments$grupa = group
  eddited_payments = eddited_payments[!(rownames(eddited_payments) %in% rownames(match_df(eddited_payments,payments_ALL))),]
  
  #Update the row names
  rownames(eddited_payments) = seq(nrow(payments_ALL) + 1, nrow(payments_ALL) + nrow(eddited_payments))
  
  #Replace participant data in the personal data table by columns: "id_user" and "id_participant". and adding rows with new payments to the table with all payments
  clients_ALL[which(clients_ALL$id_uczestnika %in% data_ALL$clients$id_uczestnika & clients_ALL$id_uzytkownika %in% data_ALL$clients$id_uzytkownika),] = data_ALL$clients
  payments_ALL = rbind(payments_ALL,eddited_payments)
  
  return(list(clients = clients_ALL, payments = payments_ALL))
}

#Edit table with customer and payment data
clients_and_payments_writeTable_edit <- function(clients_ALL, payments_ALL, data,group,kind,addGroups = FALSE) {
  #Separating data into two tables - (clients and payments)
  data_ALL = clients_and_payments_separating(data, kind)
  
  #Select the payment table and organize its columns and change the column concerning the group in payments made to the group selected in the general settings
  eddited_payments = data.frame(data_ALL$payments[,grep("id_", names(data_ALL$payments))], data_ALL$payments[,-grep("id_", names(data_ALL$payments))])
  eddited_payments$grupa = group
  
  #Replace participant data in the personal data table by columns: "id_user" and "id_participant"
  clients_ALL[which(clients_ALL$id_uczestnika %in% data_ALL$clients$id_uczestnika & clients_ALL$id_uzytkownika %in% data_ALL$clients$id_uzytkownika),] = data_ALL$clients
  
  #If a group is not added, update the payments after columns: "id_user", "id_participant", "date" and "group"
  if(!addGroups){
    payments_ALL[which(payments_ALL$id_uczestnika %in% eddited_payments$id_uczestnika & payments_ALL$id_uzytkownika %in% eddited_payments$id_uzytkownika &
                         payments_ALL$data %in% eddited_payments$data & payments_ALL$grupa %in% group),] = eddited_payments
  }
  
  #Sort table records with payments after: "id_user" and "date"
  payments_ALL = payments_ALL[order(payments_ALL$id_uzytkownika, payments_ALL$data),]
  
  return(list(clients = clients_ALL, payments = payments_ALL))
}

#Delete rows in the payment table
clients_and_payments_writeTable_delete <- function(clients_ALL, payments_ALL, data, user_id, group, kind, delete_groups, participant_id = NULL) {
  #Separating data into two tables - (clients and payments)
  data_ALL = clients_and_payments_separating(data, kind = kind)
  
  #Checking whether a group or a client is to be removed
  if(delete_groups){
    #Replace participant data in the personal data table by columns: "id_user" and "id_participant"
    clients_ALL[which(clients_ALL$id_uczestnika %in% data_ALL$clients$id_uczestnika & clients_ALL$id_uzytkownika %in% data_ALL$clients$id_uzytkownika),] = data_ALL$clients  
    #Delete rows in payments based on columns: "id_user" and "group"
    payments_ALL = payments_ALL[!(payments_ALL$id_uzytkownika == user_id & payments_ALL$grupa == group),]
    
  } else {
    #Remove rows containing "id_participant" and "id_user" from the payments and customer data table
    clients_ALL = filter(clients_ALL, !(id_uczestnika %in% participant_id & id_uzytkownika %in% user_id))
    payments_ALL =  filter(payments_ALL, !(id_uczestnika %in% participant_id & id_uzytkownika %in% user_id))
  }
  
  return(list(clients = clients_ALL, payments = payments_ALL))
}

#Returns data tables with customer information
data_infos_dataTable <- function(data, kind, disable_toEdit_cols) {
  datatable(addWhitespaces(data[,!grepl("wplata|id_",colnames(data))], kind = kind), extensions = c ("Responsive", "Buttons"),
            editable = list(target = 'cell', disable = list(columns = disable_toEdit_cols)), options = list(dom = 'Bfrtip',
                                                                                                            buttons = list(list(extend = "collection", text = 'Dodaj',
                                                                                                                                action = DT::JS("function ( e, dt, node, config ) {
                                                                                                                                                Shiny.setInputValue('addObservation', true, {priority: 'event'});}")),
                                                                                                                           list(extend = "collection", text = 'Usuń',
                                                                                                                                action = DT::JS("function ( e, dt, node, config ) {
                                                                                                                                                Shiny.setInputValue('deleteObservation', true, {priority: 'event'});}")))))
  }

#Returns data tables with payment or presence information
data_payments_dataTable <- function(data, kind, disable_toEdit_cols) {
  datatable(addWhitespaces(sort_columns(data,kind = kind),kind = kind), extensions = c ("Responsive"),
            editable = list(target = 'cell', disable = list(columns = disable_toEdit_cols)))
}

#Selects the rows that contain the selected string in a given column
id_filter <- function(data, column, id){
  return(data %>% filter(.[[column]] == id))
}

#Check that the deposit amount entered is a positive number
global_payments_validation <- function(session,year,maxYear,month,maxMonth, number_of_classes) {
  
  #If any of the values is "-" then assign "-1" to it
  month = ifelse(month == "-", -1, month)
  year = ifelse(year == "-", -1, year)
  maxMonth = ifelse(maxMonth == "-", -1, maxMonth)
  maxYear = ifelse(maxMonth == "-", -1, maxYear)
  number_of_classes = ifelse(number_of_classes == "-", -1, number_of_classes)
  
  #If any of the values is less than 0, inform the user that no changes have been made
  if(as.numeric(year) > 0 & as.numeric(month) > 0 & as.numeric(maxMonth) > 0 & as.numeric(maxYear) > 0 & as.numeric(number_of_classes) > 0)
    return(0)
  else
    informationAlert(session, "Zmienne dotyczące wysokości wpłat i liczby zajęć muszą być liczbami dodatnimi. Zmiany nie zostały wprowadzone", type = "warning")
}

#Upload a file to Dropbox - used if the uploaded file has the same name as the one already uploaded
replaceFile <- function(folderPath, file, session) {
  #Upload to Dropbox
  drop_upload(file, folderPath, dtoken = token)
  
  informationAlert(session,"Plik został wgrany","success")
}

#Convert column encoding to UTF16
convertToUTF16 <- function(column){
  lapply(column, function(x) unlist(iconv(x,from="UTF-8",to="UTF-16LE",toRaw=TRUE)))
}

#Apply encoding converting to all columns
Tonvarchar <- function(data,columns_names){
  for(i in 1:length(columns_names)){
    data[[columns_names[i]]]<- convertToUTF16(data[[columns_names[i]]])
  }
  return(data)
}

#Update picker input for users groups
updatePickerInputs <- function(session,rows_selected, data_group,editInputId, deleteInputId) {
  
  #Assign the selected rows to the variable
  selected_rows = as.numeric(rows_selected)
  
  #Select only groups that repeat in all selected participants
  picker_options = strsplit(data_group[selected_rows], ", ") %>%
    unlist() %>%
    table() %>%
    .[. == length(selected_rows)] %>%
    names()
  
  #Update
  updatePickerInput(session = session,inputId = editInputId, choices = picker_options)
  updatePickerInput(session = session,inputId = deleteInputId, choices = picker_options)
}

#Update files picker input 
updateFilePicker <- function(session, inputId, choices){
  updatePickerInput(session = session, inputId = inputId,
                    choices =choices)
}

#Remove NA's and "-" from client emails list
clientsMails_validation <- function(mails) {
  return(mails%>% .[!is.na(.)] %>% .[!(. %in% "-")] %>% unique())
}

#Add personal data and payments and presence information to data tables with all infos.
addClient_function <- function(data, clientData, kind, clients_ALL, payments_ALL){
  
  #Set client colnames to variable
  clientsData_colnames =  colnames(data)
  
  #Temporary dataframe with columns about payments or presences that are already in database, but filled with "-"
  payments_colnames = clientsData_colnames[clientsData_colnames %in% grep(paste0(kind, collapse = "|"), clientsData_colnames, value = T)]
  payments_info = data.frame(t(rep(-1,length(payments_colnames))))
  colnames(payments_info) = payments_colnames
  
  #If exist at least one payment column, bind by columns temporary dataframes with informations about client and payments columns
  if(nrow(payments_info) != 0)
    clientData = cbind(clientData, payments_info, stringsAsFactors = FALSE)
  
  #Bind by rows all clients table with the new one
  clientData_separated = clients_and_payments_separating(clientData, kind)
  clients_ALL = rbind(clients_ALL, clientData_separated$clients)
  clients_ALL[order(clients_ALL$id_uzytkownika, clients_ALL$id_uczestnika),]
  rownames(clients_ALL) = seq(1,nrow(clients_ALL))
  
  #Bind by rows all payments table  with informations aoubt just added client
  payments_ALL[order(payments_ALL$id_uzytkownika, payments_ALL$id_uczestnika),]
  payments_ALL = rbind(payments_ALL, clientData_separated$payments)
  rownames(payments_ALL) = seq(1,nrow(payments_ALL))
  
  return(list(clients = clients_ALL, payments = payments_ALL))
}

#Verify if the client is already in the database
client_already_exist <- function(session,clients_ALL, append_data, information,user_id){
  append_data$data_urodzenia = append_data$data_urodzenia %>% as.Date()
  #Checking if the clients from the file are no longer in the database - If there are no customers in the file that are no longer in the database, inform the user
  if(nrow(match_df(clients_ALL[clients_ALL$id_uzytkownika == user_id,-grep("id_|grupa", colnames(clients_ALL))],append_data[,-grep("id_|grupa", colnames(append_data))])) != nrow(append_data)) {
    #Assigning to a variable number of rows in which there are already existing customers in the database
    identical_rows = as.numeric(rownames(match_df(append_data[,-grep("id_|grupa", colnames(append_data))],clients_ALL[,-grep("id_|grupa", colnames(clients_ALL))])))
    
    #If there are recurring customers, do not add them to the database
    if(length(identical_rows) != 0) {
      #Selecting only non-repeating participants and giving them subsequent row names and id's
      new_rownames = rownames(append_data)
      new_userID = append_data$id_uczestnika
      append_data = append_data[!rownames(append_data) %in% identical_rows,]
      rownames(append_data) = new_rownames[1:nrow(append_data)]
      append_data$id_uczestnika = new_userID[1:nrow(append_data)]
      
      return(append_data)
    } else{
      return(append_data)
    }
  } else {
    #If the data set is empty, inform the user that the clients from the file are already in the database
    informationAlert(session,information, type = "warning")
  }
}

server = function(input, output, session ){
  
  ############################Reactive Values############################
  filesData = reactiveValues(data = NULL)
  deleteFile <- reactiveValues(deleteRow = NULL)
  folderPath <- reactiveVal()
  templatesData = reactiveValues(data = NULL)
  templatesData_ALL = reactiveValues(data = dbReadTable(databaseConnection, "maile"))
  clients = reactiveValues(data = NULL)
  clients_ALL = reactiveValues(data = dbReadTable(databaseConnection, "uczestnicy"))
  payments_ALL = reactiveValues(data = dbReadTable(databaseConnection, "wplaty"))
  depositAmount= reactiveValues(data = NULL)
  groups= reactiveValues(data = NULL)
  groups_ALL= reactiveValues(data = dbReadTable(databaseConnection, "grupy"))
  clients_presence = reactiveValues(data = NULL)
  schoolRegister_ALL = reactiveValues(data = dbReadTable(databaseConnection,"obecnosci"))
  MonthPayment = reactiveVal()
  MaxMonthPayment = reactiveVal()
  YearPayment = reactiveVal()
  MaxYearPayment = reactiveVal()
  NumberOfClasses = reactiveVal()
  excelData <- reactiveValues(data = NULL)
  csvData <- reactiveValues(data = NULL)
  Group_current <- reactiveValues(data = "wszyscy")
  sendMailConfirmation <- reactiveValues()
  credentials <- reactiveValues(data = data.frame(
    id = c(Credentials$id_uzytkownika),
    user =c(Credentials$uzytkownik), # mandatory
    password = c(Credentials$haslo), # mandatory
    email = c(Credentials$mail),
    name = c(Credentials$imie),
    surname = c(Credentials$nazwisko),
    image =  c(Credentials$image),
    img_url = c(Credentials$img_url),
    gmail_passwd = c(Credentials$gmail_passwd),
    to_email = c(Credentials$drugi_mail),
    clients_number = lapply(Credentials$id_uzytkownika, function(x, data = Clients){data %>% filter(.[["id_uzytkownika"]] == x) %>% nrow()}) %>% unlist() %>% c(),
    groups = lapply(Credentials$id_uzytkownika, function(x, data = Groups){data %>% filter(.[["id_uzytkownika"]] == x) %>% nrow()}) %>% unlist() %>% c()
  ))
  paymentsSum_month_reactive = reactiveValues(data = NULL)
  paymentsSum_year_reactive = reactiveValues(data = NULL)
  notpaid_all_reactive = reactiveValues(data = NULL)
  number_of_clients_reactive = reactiveValues(data = NULL)
  
  observe({
    if(!is.null(res_auth$id)){
      clients_selected = id_filter(clients_ALL$data, "id_uzytkownika", res_auth$id)
      payments = id_filter(payments_ALL$data, "id_uzytkownika", res_auth$id)
      schoolRegister = id_filter(schoolRegister_ALL$data, "id_uzytkownika", res_auth$id)
      
      if(!is.null(input$selectGroup_sidebar)){
        clients_selected = clients_selected[clients_selected$grupa %like% input$selectGroup_sidebar,]
        payments = payments[payments$grupa %like% input$selectGroup_sidebar,]
        schoolRegister = schoolRegister[schoolRegister$grupa %like% input$selectGroup_sidebar,]
        Group_current$data = input$selectGroup_sidebar
      } else{
        clients_selected = clients_selected[clients_selected$grupa %like% Group_current$data,]
        payments = payments[payments$grupa %like% Group_current$data,]
        schoolRegister = schoolRegister[schoolRegister$grupa %like% Group_current$data,]
      }
      clients$data <<- clients_and_payments_merging(clients_selected,payments)
      clients_presence$data <<- clients_and_payments_merging(clients_selected,schoolRegister)
      
      templatesData$data = id_filter(templatesData_ALL$data, "id_uzytkownika", res_auth$id)
      groups$data = id_filter(groups_ALL$data, "id_uzytkownika", res_auth$id)
      depositAmount$data = id_filter(groups$data,"grupa", Group_current$data)
      
      MonthPayment(depositAmount$data  %>% select(wplata_miesieczna) %>% unlist())
      MaxMonthPayment(depositAmount$data  %>% select(maksymalna_wplata_miesieczna) %>% unlist())
      YearPayment(depositAmount$data %>% select(wplata_roczna) %>% unlist())
      MaxYearPayment(depositAmount$data %>% select(maksymalna_wplata_roczna) %>% unlist())
      NumberOfClasses(depositAmount$data %>% select(liczba_zajec) %>% unlist())
      folderPath(paste0("Baza Danych/", res_auth$email,"/"))
      filesData$data = files_exists_verification(folderPath())
    }
  })
  
  observeEvent(input$globalChanges,{
    if(!is.null(global_payments_validation(session,input$YearPayment, input$YearPayment_max,input$MonthPayment, input$MonthPayment_max, input$NumberOfClasses))){
      depositAmount$data$wplata_roczna = input$YearPayment
      depositAmount$data$maksymalna_wplata_roczna = input$YearPayment_max
      depositAmount$data$wplata_miesieczna = input$MonthPayment
      depositAmount$data$maksymalna_wplata_miesieczna = input$MonthPayment_max
      depositAmount$data$liczba_zajec = input$NumberOfClasses
      informationAlert(session, "Zmiany zostały wprowadzone", type = "success")
    } else{
      updateNumericInput(session, "MonthPayment", value = MonthPayment() %>% as.vector())
      updateNumericInput(session, "YearPayment", value = YearPayment()%>% as.vector())
      updateNumericInput(session, "YearPayment_max", value = MaxYearPayment()%>% as.vector())
      updateNumericInput(session, "MonthPayment_max", value = MaxMonthPayment()%>% as.vector())
      updateNumericInput(session, "NumberOfClasses", value = NumberOfClasses()%>% as.vector())
    }
    groups_ALL$data[groups_ALL$data$id_uzytkownika == depositAmount$data$id_uzytkownika & groups_ALL$data$grupa == depositAmount$data$grupa,] = depositAmount$data
    
    dbWriteTable(databaseConnection,"grupy",Tonvarchar(groups_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
  })
  
  #OKKOMENTARZE##################################Files###############################
  #File input
  output[["fileInput"]] <- renderUI({
    input$uploadFile
    fileInput("file", "Proszę wybrać plik")
  })
  
  #Upload file to dropbox
  observeEvent(input$uploadFile,{
    #File name correction
    file = file.rename(input$file$datapath, paste0(input$file$name))
    file = input$file$name
    
    #Upload file to dropbox
    if(file %in% filesData$data$Nazwa){
      confirmAlert(session,c("Nie", "Tak"), "replaceFileConfirmation", "Plik o tej nazwie już istnieje, czy chcesz go zastąpić?")
    } else {
      replaceFile(folderPath(), file, session)
      filesData$data = files_exists_verification(folderPath())
    }
  })
  
  #Replace file at Dropbox which has the same name
  observeEvent(input$replaceFileConfirmation, {
    #Replace file
    if(input$replaceFileConfirmation)
    {
      #File name correction
      file = file.rename(input$file$datapath, paste0(input$file$name))
      file = input$file$name
      
      #Upload file to dropbox
      replaceFile(folderPath(), file, session)
      filesData$data = files_exists_verification(folderPath())
      
    }
    
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
      print(token)
      drop_delete(paste0(folderPath(),filesData$data$Nazwa[deleteFile$deleteRow]),dtoken = token)
      filesData$data = files_exists_verification(folderPath())
      
      updateFilePicker(session, "filesNames", drop_dir(folderPath(), dtoken = token)$name)
      updateFilePicker(session, "filesNamesDelete", drop_dir(folderPath(), dtoken = token)$name)
      
    } else {
      informationAlert(session,"Plik nie został usunięty","error")
    }
    #Clear confirmation
    deleteFile$delete <- NULL
    
  })
  
  #Confirm if u want to download the Dropbox file with the button in the Data table
  observeEvent(input$downloadPressed, {
    #In which row the button was pressed
    rowNum <- parseDeleteDownloadEvent(input$downloadPressed)
  
    #Download file from Dropbox
    drop_download(paste0(folderPath(),filesData$data$Nazwa[rowNum], dtoken = token),local_path = paste0("Downloads/",filesData$data$Nazwa[rowNum]),overwrite = TRUE)
    informationAlert(session,"Plik został pobrany","success")
  })
  
  #Update the files in pickerInput after uploading a new one
  observeEvent(input$uploadFile,{
    updateFilePicker(session, "filesNames", drop_dir(folderPath(), dtoken = token)$name)
    updateFilePicker(session, "filesNamesDelete", drop_dir(folderPath(), dtoken = token)$name)
  })
  
  #Render filesPicker to download multiple files with file names as choices
  output[["filesPicker"]] <- renderUI ({
    pickerInput(
      inputId = "filesNames", 
      label = "Wybierz pliki do pobrania", 
      choices = drop_dir(folderPath(), dtoken = token)$name, 
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
      choices = drop_dir(folderPath(), dtoken = token)$name, 
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
    
    #Download files and verify how much you have successfully downloaded
    for(file in input$filesNames)
      successDownloading = c(successDownloading,drop_download(paste0(folderPath(),file), overwrite = TRUE, dtoken = token))
    
    #Return information about downloading process
    if(sum(successDownloading) == 0)
      informationAlert(session,"Żaden z plików nie został pobrany","error")
    else if(sum(successDownloading) != length(successDownloading))
      informationAlert(session,"Któryś z plików nie został pobrany","error")
    else
      informationAlert(session,"Pliki zostały pobrane","success")
  })
  
  #Delete multiple files at Dropbox
  observeEvent(input$deleteFiles,{
    for(file in input$filesNamesDelete)
      drop_delete(paste0(folderPath(),file), dtoken = token)
    
    #Upadte files informations
    updateFilePicker(session, "filesNames", drop_dir(folderPath(), dtoken = token)$name)
    updateFilePicker(session, "filesNamesDelete", drop_dir(folderPath(), dtoken = token)$name)
    
    informationAlert(session,"Pliki zostały usunięte","success")
    filesData$data = files_exists_verification(folderPath())
  })
  
  #OKKOMENTARZE##################################Mails###############################
  #Save template to database
  observeEvent(input$saveTemplate,{
    if(input$templateNameCreate %in% templatesData$data$Name) {
      informationAlert(session, "Podana nazwa szablonu już istnieje", type = "warning")
    } else if(nchar(input$templateNameCreate) < 2){
      informationAlert(session, "Nazwa szablonu musi mieć co najmniej 3 znaki", type = "warning")
    }else {
      #Add template to dataframe, write it to database and assign result of writing
      templatesData_ALL$data = rbind(templatesData_ALL$data, data.frame(id_uzytkownika = res_auth$id,Name = input$templateNameCreate, Body = input$templateBodyCreate)) 
      templatesData_ALL$data = templatesData_ALL$data[order(templatesData_ALL$data$id_uzytkownika),]
      templateWritten = dbWriteTable(databaseConnection,"maile",Tonvarchar(templatesData_ALL$data,columns_names=c("Name","Body")), overwrite=T,field.types = c(Name = "nvarchar(500)", Body = "nvarchar(2000)"),row.names = FALSE)
      
      
      #Verify if template was written
      if(templateWritten)
        informationAlert(session,"Szablon maila został zapisany","success")
      else
        informationAlert(session,"Ups! Coś poszło nie tak","error")
      
    }
    
  })
  
  #Render the "templates picker / edit section" with the names of the created templates  
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
  
  #Render the "templates body / edit section" with the names of the created templates  
  output[["templateBodyEdit"]] <- renderUI ({
    textAreaInput("templateBodyEdit", "Szablon maila", templatesData$data$Body[which(templatesData$data$Name == input$templateNameEdit)], resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  })
  
  #Edit template body  
  observeEvent(input$editTemplate,{
    templatesData_ALL$data$Body[which(templatesData_ALL$data$id_uzytkownika == res_auth$id & templatesData_ALL$data$Name == input$templateNameEdit)] = input$templateBodyEdit
    
    #Write eddited templates to database and assign result of writing
    templateEdited = dbWriteTable(databaseConnection,"maile",Tonvarchar(templatesData_ALL$data,columns_names=c("Name","Body")), overwrite=T,field.types = c(Name = "nvarchar(500)", Body = "nvarchar(2000)"))
    
    #Check if template was edited
    if(templateEdited)
      informationAlert(session, "Szablon maila został zedytowany","success")
    else
      informationAlert(session,"Ups! Coś poszło nie tak","error")
  })
  
  #Render list of clients mails
  output[["mailList"]] <- renderUI ({
    pickerInput(
      inputId = "mailList", 
      label = "Lista maili", 
      choices = clientsMails_validation(clients$data$email_opiekuna), 
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
    textAreaInput("mailBody", "Mail",  templatesData$data$Body[which(templatesData$data$Name == input$templateList)], resize = "vertical") %>%
      shiny::tagAppendAttributes(style = 'width: 100%;')
  })
  
  #Confirmation of willingness to send an e-mail
  observeEvent(input$sendMail,{
    confirmAlert(session,c("Nie", "Tak"), "sendMailConfirmation", "Czy chcesz wysłać maila?")
  })
  
  #Sending e-mail via gmail  
  observeEvent(input$sendMailConfirmation, {
    
    #Id 3 is assign to trial account
    if(res_auth$id == 3){
      sendMailConfirmation$confirm <- FALSE
    } else {
      #Assign user decision to reactive variable
      sendMailConfirmation$confirm <- input$sendMailConfirmation
    }
    
    if(sendMailConfirmation$confirm)
    {
      #If user have not selected any recipient show alert and clear confirmation
      if(length(input$mailList) == 0)
      {
        informationAlert(session,"Musisz wybrać chociaż jednego odbiorcę maila","error")
        sendMailConfirmation$confirm <- NULL
      } else
      {
        #Sending mail
        send.mail(from = res_auth$email %>% as.character(),
                  to = res_auth$to_email %>% as.character(),
                  bcc = input$mailList,
                  subject = input$mailTitle,
                  body = input$mailBody,
                  encoding = 'utf-8',
                  smtp = list(host.name = "smtp.gmail.com", port = 465,
                              user.name = res_auth$email %>% as.character(),
                              passwd = res_auth$gmail_passwd %>% as.character(), ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE)
        informationAlert(session,"Mail został wysłany","success")
        sendMailConfirmation$confirm <- NULL
      }
      
    } else {
      if(res_auth$id == 3){
        informationAlert(session,"Z konta próbnego nie można wysyłać wiadomości","error")
      } else {
        informationAlert(session,"Mail nie został wysłany","error")
      }
      sendMailConfirmation$confirm <- NULL
    }
    
    #Clear confirmation
    sendMailConfirmation$confirm <- NULL
    
  })
  
  #OKKOMENTARZE##################################Data##############################
  
  #Upload excel file and join it with already uploaded data
  observeEvent(input$uploadExcelData,{
    #Save data in reactive value
    excelData$data <- file_exists_verification(input$excelFile$datapath, "xlsx")
    
    if(!is.null(excelData$data)){
      #Join existing dataframes about clients, presences and payments with informations included in excel file / check that the data is correct and that the observations are not repeated.
      append_rows = append_rows_files(session, clients_ALL$data, payments_ALL$data, schoolRegister_ALL$data, excelData$data, clients$data,clients_presence$data,
                                      Group,databaseConnection, "uczestnicy","wplaty", "obecnosci",res_auth$id)
      if(!is.null(append_rows)){
        
        #If there are no observations about payments or presences number of columns is equal 3 else it is 5 
        if(ncol(append_rows$payments) == 5){
          payments_ALL$data = rbind(payments_ALL$data, append_rows$payments) %>% .[order(.$id_uzytkownika, .$data),]
          dbWriteTable(databaseConnection,"wplaty",Tonvarchar(payments_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
        }
        
        if(ncol(append_rows$presences) == 5){
          schoolRegister_ALL$data = rbind(schoolRegister_ALL$data, append_rows$presences) %>% .[order(.$id_uzytkownika, .$data),]
          dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(schoolRegister_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
        }
        clients_ALL$data = rbind(clients_ALL$data, append_rows$clients) %>% .[order(.$id_uzytkownika, .$id_uczestnika),]
        dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(clients_ALL$data,columns_names=c("imie","nazwisko","miejsce_urodzenia","adres","szkola","imie_opiekuna","nazwisko_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)"))
        
        informationAlert(session,"Dodawanie zakończone pomyślnie", type = "success")
      }
      
      
    }
    
  })
  
  #Upload csv file and join it with already uploaded data
  observeEvent(input$uploadCSVData,{
    #Save data in reactive value
    csvData$data <- file_exists_verification(input$csvFile$datapath, "csv")
    
    if(!is.null(csvData$data)){
      #Join existing dataframes about clients, presences and payments with informations included in csv file / check that the data is correct and that the observations are not repeated.
      append_rows = append_rows_files(session, clients_ALL$data, payments_ALL$data, schoolRegister_ALL$data, csvData$data, clients$data,clients_presence$data,
                                      Group,databaseConnection, "uczestnicy","wplaty", "obecnosci",res_auth$id)
      if(!is.null(append_rows)){
        
        #If there are no observations about payments or presences number of columns is equal 3 else it is 5
        if(ncol(append_rows$payments) == 5){
          payments_ALL$data = rbind(payments_ALL$data, append_rows$payments) %>% .[order(.$id_uzytkownika, .$data),]
          dbWriteTable(databaseConnection,"wplaty",Tonvarchar(payments_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
        }
        
        if(ncol(append_rows$presences) == 5){
          schoolRegister_ALL$data = rbind(schoolRegister_ALL$data, append_rows$presences) %>% .[order(.$id_uzytkownika, .$data),]
          dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(schoolRegister_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
        }
        clients_ALL$data = rbind(clients_ALL$data, append_rows$clients) %>% .[order(.$id_uzytkownika, .$id_uczestnika),]
        dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(clients_ALL$data,columns_names=c("imie","nazwisko","miejsce_urodzenia","adres","szkola","imie_opiekuna","nazwisko_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)"))
        
        informationAlert(session,"Dodawanie zakończone pomyślnie", type = "success")
      }
      
      
    }
    
  })
  
  #Display clients data /include responsive design and (action, delete) buttons
  output$datasetTable = renderDataTable({
    data_infos_dataTable(clients$data,kind="wplata", grep("grupa", colnames(clients$data)[!grepl("wplata|id_", colnames(clients$data))]))
  })
  
  #Edit cells at data table by click
  observeEvent(input$datasetTable_cell_edit, {
    
    #Verify which columns have been edited and if they do not apply to payments or id's
    edited_column = colnames(clients$data[,!grepl("wplata|id_",colnames(clients$data))][,input$datasetTable_cell_edit$col, drop = FALSE])
    
    clients$data[[edited_column]][input$datasetTable_cell_edit$row] <<- input$datasetTable_cell_edit$value
    
    data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata")
    dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
    dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
    
    clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
    payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
    
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
    if(input$addName != "" & input$addSurname != "" & input$addGuardianName != "" & input$addGuardianSurname != "") {
      #Temporary dataframe with informations about client / if blanks are left fill them with "-"
      clientData = data.frame(clients$data$id_uczestnika[nrow(clients$data)] + 1, res_auth$id,input$addName,input$addSurname,Group,input$addPesel,as.character(input$addBirthDate),
                              input$addBirthPlace,input$addAddress,input$addPhone,input$addMail,input$addSchool,input$addGuardianName,
                              input$addGuardianSurname,input$addGuardianPhone,input$addGuardianMail, stringsAsFactors = FALSE)
      clientData[clientData == ""] <- "-"
      clientsData_colnames =  colnames(clients$data)
      colnames(clientData) = clientsData_colnames[!clientsData_colnames %in% grep(paste0(kind, collapse = "|"), clientsData_colnames, value = T)]
      
      if(!is.null(client_already_exist(session,clients_ALL$data, clientData, "Klient znajduje się już bazie",res_auth$id))){
        
        #Add new client to an existing datatables 
        newClient_payments = addClient_function(clients$data, clientData, "wplata", clients_ALL$data, payments_ALL$data)
        newClient_presences = addClient_function(clients_presence$data, clientData, "obecnosc", clients_ALL$data, schoolRegister_ALL$data)
        
        dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(newClient_payments$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
        
        clients_ALL$data <- dbReadTable(databaseConnection, "uczestnicy")
        
        #If there are no observations about payments or presences number of columns is equal 3 else it is 5
        if(ncol(newClient_payments$payments) == 5){
          dbWriteTable(databaseConnection,"wplaty",Tonvarchar(newClient_payments$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
          payments_ALL$data <- dbReadTable(databaseConnection, "wplaty")
        }
        
        if(ncol(newClient_presences$payments) == 5){
          dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(newClient_presences$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
          schoolRegister_ALL$data <- dbReadTable(databaseConnection, "obecnosci")
        }
        informationAlert(session,"Osoba została wprowadzona","success")
        removeModal() 
      }
      
    } else{
      informationAlert(session,"Proszę uzupełnić imię i nazwisko uczestnika oraz opiekuna","warning")
    }
  })
  
  #Confirmation if user wants to delete selected rows on data table, after "Delete" button was pushed
  observeEvent(input$deleteObservation, {
    if (!is.null(input$datasetTable_rows_selected)) {
      confirmAlert(session,c("Nie", "Tak"), "confirmDeleteRows", "Czy chcesz usunąć wybrane osoby?")
    }
  })
  
  #Delete selected rows on data table
  observeEvent(input$confirmDeleteRows, { 
    if(input$confirmDeleteRows){
      #Remove clients by id
      del_ind <- clients$data$id_uczestnika[as.numeric(input$datasetTable_rows_selected)]
      clients$data = clients$data[!(clients$data$id_uczestnika %in% del_ind),]
      
      #Remove payments and presences informations
      data_ALL = clients_and_payments_writeTable_delete(clients_ALL$data, payments_ALL$data,clients$data, res_auth$id, Group_current$data, "wplata", FALSE, del_ind)
      data_ALL_presences = clients_and_payments_writeTable_delete(clients_ALL$data, schoolRegister_ALL$data,clients$data, res_auth$id, Group_current$data, "obecnosc", FALSE, del_ind)
      
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(data_ALL_presences$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      schoolRegister_ALL$data = dbReadTable(databaseConnection, "obecnosci")
      informationAlert(session,"Usuwanie zakończone pomyślnie","success")
    }
  })
  
  #Create date picker for "add groups" button
  output[["groupPicker"]] <- renderUI({
    groups_picker("groupsList", "Wybierz grupę", groups$data)
  })
  
  #Create date picker for "edit groups" button / which do you want to edit (includes picker)
  output[["editUsersGroupsPicker"]] <- renderUI({
    groups_picker("editUsersGroups", "Wybierz grupę, która zostanie zamieniona", NULL, multiple = FALSE)
  })
  
  #Create date picker for "edit groups" button / to which do you want to edit
  output[["editUsersGroupsPicker_2"]] <- renderUI({
    groups_picker("editUsersGroups_2", "Wybierz grupę na jaką zostanie dokonana zmiana", groups$data, multiple = FALSE)
  })
  
  #Create date picker for "delete groups" button / includes picker
  output[["deleteUsersGroupPicker"]] <- renderUI({
    groups_picker("deleteUsersGroup", "Wybierz grupę", NULL)
  })
  
  #Update picker input for editing groups for clients selected in pickers input / includes clients picker
  observeEvent(input$clientsListEditing, {
    if(!is.null(input$clientsListEditing)){
      picker_options = strsplit(clients$data$grupa[selectedClients_rows(clients$data, input$clientsListEditing)], ", ") %>%
        unlist() %>%
        table() %>%
        .[. == length(input$clientsListEditing)] %>%
        names()
      updatePickerInput(session = session,inputId = "editUsersGroups", choices = picker_options)
    } else {
      updatePickerInput(session = session,inputId = "editUsersGroups",choices = "")
    }
  }, ignoreNULL = FALSE)
  
  #Update picker input for deleting groups for clients selected in pickers input / includes clients picker
  observeEvent(input$clientsListDeleting, {
    if(!is.null(input$clientsListDeleting)){
      picker_options = strsplit(clients$data$grupa[selectedClients_rows(clients$data, input$clientsListDeleting)], ", ") %>%
        unlist() %>%
        table() %>%
        .[. == length(input$clientsListDeleting)] %>%
        names()
      
      updatePickerInput(session = session,inputId = "deleteUsersGroup", choices = picker_options)
    } else {
      updatePickerInput(session = session,inputId = "deleteUsersGroup", choices = "")
    }
  }, ignoreNULL = FALSE)
  
  #Show modal with form after "add group" button was pushed / includes clients picker
  observeEvent(input$addToGroups, {
    showModal(modalDialog(
      title = "Zarządzaj grupami użytkowników",
      footer =  tagList(
        modalButton("Wyjdź")
      ),
      bs4TabSetPanel(
        id = "manageUsersGroups_panel",
        side = "left",
        bs4TabPanel(
          tabName = "Dodaj grupę", #Add group
          active = TRUE,
          column(
            width = 12,
            offset = 2,
            clients_picker("clientsList", "Wybierz osoby", clients$data),
            uiOutput("groupPicker"),
            actionButton("addToGroups_confirm", "Dodaj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Edytuj grupę", #Edit group
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            clients_picker("clientsListEditing", "Wybierz osoby", clients$data),
            uiOutput("editUsersGroupsPicker"),
            uiOutput("editUsersGroupsPicker_2"),
            actionButton("addToGroups_edit", "Edytuj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Usuń grupę", #Delete group
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            clients_picker("clientsListDeleting", "Wybierz osoby", clients$data),
            uiOutput("deleteUsersGroupPicker"),
            actionButton("addToGroups_delete", "Usuń", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
        
      )
    ))
  })
  
  #Add groups based on selected rows / includes clients picker
  observeEvent(input$addToGroups_confirm, {
    
    if(is.null(input$clientsList))
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$groupsList)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else{
      
      #Add groups
      new_groups = lapply(strsplit(clients$data$grupa[selectedClients_rows(clients$data, input$clientsList)], ", "), function(x, val = input$groupsList){
        if(length(val[!(val %in% x)]) != 0){
          paste(paste(x, collapse =", "), val[!(val %in% x)], sep = ", ")
        } else {
          paste(x,collapse = ", ")
        }}) %>% unlist()
      
      clients$data$grupa[selectedClients_rows(clients$data, input$clientsList)] = new_groups
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata",addGroups =TRUE)
      
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      informationAlert(session, "Dodano grupy", type = "success")
    }
    
  })
  
  #Edit groups based on selected rows / includes clients picker
  observeEvent(input$addToGroups_edit, {
    if(is.null(input$clientsListEditing))
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$editUsersGroups) | is.null(input$editUsersGroups_2)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else if((input$editUsersGroups == "wszyscy") | (input$editUsersGroups_2 == "wszyscy")){
      informationAlert(session, "Nie można edytować grupy 'wszyscy'", type = "warning")
    } else{
      
      #Edit groups
      clients$data$grupa[selectedClients_rows(clients$data, input$clientsListEditing)] = str_replace(clients$data$grupa[selectedClients_rows(clients$data, input$clientsListEditing)], input$editUsersGroups,input$editUsersGroups_2)
      
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data,kind = "wplata",addGroups = FALSE)
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Edytowano grupę", type = "success")
    }
    
  })
  
  #Delete groups based on selected rows / includes clients picker
  observeEvent(input$addToGroups_delete, {
    
    if(is.null(input$clientsListDeleting))
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$deleteUsersGroup)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else if(input$deleteUsersGroup == "wszyscy"){
      informationAlert(session, "Nie można usunąć grupy 'wszyscy'", type = "warning")
    } else{
      
      #Delete groups
      clients$data$grupa[selectedClients_rows(clients$data, input$clientsListDeleting)] = str_replace(clients$data$grupa[selectedClients_rows(clients$data, input$clientsListDeleting)], paste(",", input$deleteUsersGroup), "")
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data,kind = "wplata",addGroups = FALSE)
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Usunięto grupę", type = "success")
    }
    
  })
  
  #Show modal with form after "add group" button was pushed with priority 1 / user needs to mark the clients by click
  observeEvent(input$addToGroupsPicker, priority = 1,{
    
    #Assign the selected rows to the variable
    selected_rows = as.numeric(input$datasetTable_rows_selected)
    
    #Select only groups that repeat in all selected participants
    options = strsplit(clients$data$grupa[selected_rows], ", ") %>%
      unlist() %>%
      table() %>%
      .[. == length(selected_rows)] %>%
      names()
  
    #Create date picker for "edit groups" button / which do you want to edit (without picker)
    output[["editUsersGroups_noPicker"]] <- renderUI({
      groups_picker("editUsersGroups_noPicker", "Wybierz grupę, która zostanie zamieniona", options, multiple = FALSE)
    })
    
    #Create date picker for "delete groups" button / without picker
    output[["deleteUsersGroup_noPicker"]] <- renderUI({
      groups_picker("deleteUsersGroup_noPicker", "Wybierz grupę", options)
    })
  })
  
  #Show modal with form after "add group" button was pushed with priority 0 / user needs to mark the clients by click
  observeEvent(input$addToGroupsPicker, priority = 0,{
    observe({
      showModal(modalDialog(
        title = "Zarządzaj grupami użytkowników",
        footer =  tagList(
          modalButton("Wyjdź")
        ),
        bs4TabSetPanel(
          id = "manageUsersGroupsPicker_panel",
          side = "left",
          bs4TabPanel(
            tabName = "Dodaj grupę", #Add group
            active = TRUE,
            column(
              width = 12,
              offset = 2,
              uiOutput("groupPicker"),
              actionButton("addToGroupsPicker_confirm", "Dodaj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
          ),
          bs4TabPanel(
            tabName = "Edytuj grupę", #Edit group
            active = FALSE,
            column(
              width = 12,
              offset = 2,
              uiOutput("editUsersGroups_noPicker"),
              uiOutput("editUsersGroupsPicker_2"),
              actionButton("addToGroupsPicker_edit", "Edytuj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
          ),
          bs4TabPanel(
            tabName = "Usuń grupę", #Delete group
            active = FALSE,
            column(
              width = 12,
              offset = 2,
              uiOutput("deleteUsersGroup_noPicker"),
              actionButton("addToGroupsPicker_delete", "Usuń", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            )
          )
          
        )
      ))
    })
    
    
  })
  
  #Add groups, that will be added to the marked lines in data table / without clients picker
  observeEvent(input$addToGroupsPicker_confirm, {
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$datasetTable_rows_selected)
    
    if(length(selected_rows) == 0)
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$groupsList)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else{
      
      
      #Add groups
      new_groups = lapply(strsplit(clients$data$grupa[selected_rows], ", "), function(x, val = input$groupsList){
        #Verify which of the added groups have not already been assigned to clients
        if(length(val[!(val %in% x)]) != 0){
          paste(paste(x, collapse =", "), val[!(val %in% x)], sep = ", ")
        } else {
          paste(x,collapse = ", ")
        }}) %>% unlist()
      clients$data$grupa[selected_rows] = new_groups
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata",addGroups =TRUE)
      
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      informationAlert(session, "Dodano grupy", type = "success")
    }
  })
  
  #Edit groups / without clients picker
  observeEvent(input$addToGroupsPicker_edit, {
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$datasetTable_rows_selected)
    
    if(length(selected_rows) == 0)
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$editUsersGroups) | is.null(input$editUsersGroups_2)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else if((input$editUsersGroups == "wszyscy") | (input$editUsersGroups_2 == "wszyscy")){
      informationAlert(session, "Nie można edytować grupy 'wszyscy'", type = "warning")
    } else{
      
      #Edit groups
      clients$data$grupa[selected_rows] = str_replace(clients$data$grupa[selected_rows], input$editUsersGroups,input$editUsersGroups_2)
      
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data,kind = "wplata",addGroups = FALSE)
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Edytowano grupę", type = "success")
    }
    
  })
  
  #Delete groups / without clients picker
  observeEvent(input$addToGroupsPicker_delete, {
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$datasetTable_rows_selected)
    
    if(length(selected_rows) == 0)
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$deleteUsersGroup)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else if(input$deleteUsersGroup == "wszyscy"){
      informationAlert(session, "Nie można usunąć grupy 'wszyscy'", type = "warning")
    } else{
      
      #Delete groups
      clients$data$grupa[selected_rows] = str_replace(clients$data$grupa[selected_rows], paste(",", input$deleteUsersGroup), "")
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data,kind = "wplata",addGroups = FALSE)
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Usunięto grupę", type = "success")
    }
  })
  
  #Create date picker for "delete group" (manage groups) button / generally not for specific clients
  output[["deleteGroupPicker"]] <- renderUI({
    groups_picker("deleteGroupsList", "Wybierz grupę", groups$data)
  })
  
  #Create date picker for "edit group" (manage groups) button / generally not for specific clients
  output[["editGroupPicker"]] <- renderUI({
    groups_picker("groupsList", "Wybierz grupę", groups$data,FALSE)
  })
  
  #Show modal with adding, editing and deleting columns 
  observeEvent(input$manageGroups, {
    showModal(modalDialog(
      title = "Zarządzaj kolumną grup",
      footer =  tagList(
        modalButton("Wyjdź")
      ),
      bs4TabSetPanel(
        id = "manageGroups_panel",
        side = "left",
        bs4TabPanel(
          tabName = "Dodaj grupę", #Add group
          active = TRUE,
          column(
            width = 12,
            offset = 2,
            textInput("groupName", "Nazwa grupy", ""),
            actionButton("addGroup", "Dodaj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Edytuj grupę", #Edit group
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            uiOutput("editGroupPicker"),
            textInput("editGroupName","Nowa nazwa grupy", ""),
            actionButton("editGroup", "Edytuj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Usuń grupę", #Delete group
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            uiOutput("deleteGroupPicker"),
            actionButton("deleteGroup", "Usuń", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
        
      )
    ))
  })
  
  #Add group to groups list / generally not for specific clients
  observeEvent(input$addGroup,{
    if(str_length(input$groupName)>15 | str_length(input$groupName) < 3) {
      informationAlert(session, "Nazwa grupy musi mieć pomiędzy 3 a 15 znaków", type = "warning")
      updateTextInput(session, "groupName", value = "")
    } else if(input$groupName %in% unique(groups$data$grupa)) {
      informationAlert(session, "Podana nazwa grupy już istnieje", type = "warning")
      updateTextInput(session, "groupName", value = "")
    } else {
      
      #Add group to dataframe
      new_group = data.frame(id_uzytkownika = res_auth$id, grupa = input$groupName, wplata_miesieczna = depositAmount$data$wplata_miesieczna,
                             wplata_roczna = depositAmount$data$wplata_roczna, maksymalna_wplata_miesieczna = depositAmount$data$maksymalna_wplata_miesieczna,
                             maksymalna_wplata_roczna = depositAmount$data$maksymalna_wplata_roczna, liczba_zajec = depositAmount$data$liczba_zajec)
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"grupy",Tonvarchar(new_group,columns_names=c("grupa")), append=T,field.types = c(grupa = "nvarchar(500)"))
      groups_ALL$data = dbReadTable(databaseConnection, "grupy")
      informationAlert(session, "Dodano grupę", type = "success")
    }
    
  })
  
  #Edit group / both generally and for specific clients
  observeEvent(input$editGroup, {
    if(str_length(input$editGroupName)>15 | str_length(input$editGroupName) < 3) {
      informationAlert(session, "Nazwa grupy musi mieć pomiędzy 3 a 15 znaków", type = "warning")
      updateTextInput(session, "editGroupName", value = "")
    } else if(input$editGroupName %in% unique(groups$data$grupa)) {
      informationAlert(session, "Podana nazwa grupy już istnieje", type = "warning")
      updateTextInput(session, "groupName", value = "")
    } else if(is.null(input$groupsList)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else if(input$groupsList == "wszyscy"){
      informationAlert(session, "Nie można edytować grupy 'wszyscy'", type = "warning")
    }else {
      
      #Edit generally
      groups_ALL$data$grupa[groups_ALL$data$grupa == input$groupsList & groups_ALL$data$id_uzytkownika == res_auth$id] = input$editGroupName
      
      #Edit for clients that are assigned to this group
      clients$data$grupa = str_replace(clients$data$grupa, input$groupsList, input$editGroupName)
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data,kind = "wplata",addGroups = FALSE)
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"grupy",Tonvarchar(groups_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Edytowano grupę", type = "success")
    }
  })
  
  #Delete group / both generally and for specific clients
  observeEvent(input$deleteGroup, {
    if(is.null(input$deleteGroupsList)){
      informationAlert(session, "Nie wybrano grup", type = "warning")
    } else if("wszyscy" %in% input$deleteGroupsList){
      informationAlert(session, "Nie można usunąć grupy 'wszyscy'", type = "warning")
    }else {
      groups_ALL$data = groups_ALL$data[!(groups_ALL$data$grupa == input$deleteGroupsList & groups_ALL$data$id_uzytkownika == res_auth$id),,drop = FALSE]
      new_groups = lapply(strsplit(clients$data$grupa, ", "), function(x, val = input$deleteGroupsList){
        #Delete group for clients that are assigned to this group
        if(length(x[!(x %in% val)]) != 1){
          paste(x[!(x %in% val)], collapse =", ")
        } else {
          x[!(x %in% val)]
        }}) %>% unlist()
      clients$data$grupa = new_groups
      data_ALL = clients_and_payments_writeTable_delete(clients_ALL$data, payments_ALL$data,clients$data,res_auth$id,input$deleteGroupsList,"wplata", TRUE)
      
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"grupy",Tonvarchar(groups_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Usunięto grupę", type = "success")
    }
    
  })
  
  #OKKOMENTARZE##################################Payments###############################
  
  #Display clients data /include responsive design and (action, delete) buttons
  output$paymentsTable = renderDataTable({
    data_payments_dataTable(clients$data,kind="wplata", grep("wplata",colnames(sort_columns(clients$data,kind="wplata")),fixed= TRUE,invert = TRUE))
  })
  
  #Edit cells at data table by click
  observeEvent(input$paymentsTable_cell_edit, {
    edited_column = colnames(sort_columns(clients$data,kind="wplata")[,input$paymentsTable_cell_edit$col, drop = FALSE])
    
    if(grepl("roczna", edited_column, fixed = TRUE))
      restricion = MaxYearPayment()
    else
      restricion = MaxMonthPayment()
    
    cell_edit_value = str_replace(input$paymentsTable_cell_edit$value, ",", ".")
    
    #Add payments
    if(!is.na(as.numeric(cell_edit_value))) {
      if((as.numeric(cell_edit_value) >= 0) & (as.numeric(cell_edit_value) <= restricion)) {
        clients$data[[edited_column]][input$paymentsTable_cell_edit$row] <<- round(as.numeric(cell_edit_value),2)
      } else {
        output$paymentsTable = renderDataTable({
          data_payments_dataTable(clients$data,kind="wplata",grep("wplata",colnames(sort_columns(clients$data,kind="wplata")),fixed= TRUE,invert = TRUE))
        })
        
        informationAlert(session, "Podana wartość przekracza maksymalną możliwą do wpisania lub jest ujemna", type = "warning")
      }
    } else if(cell_edit_value == "-"){
      clients$data[[edited_column]][input$paymentsTable_cell_edit$row] <<- -1
    }else{
      output$paymentsTable = renderDataTable({
        data_payments_dataTable(clients$data,kind="wplata",grep("wplata",colnames(sort_columns(clients$data,kind="wplata")),fixed= TRUE,invert = TRUE))
      })
      informationAlert(session, "Podana wartość musi być liczbą", type = "warning")
    }
    
    data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata")
    dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
    dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
    
    clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
    payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
    
  })
  
  #Create date picker for "add payments" button
  output[["datePicker"]] <- renderUI({
    if(input$addPayments_type == "Miesięczna")
      addPayments_picker("datesList", "Wybierz datę", "month", clients$data,kind = "wplata")
    else
      addPayments_picker("datesList", "Wybierz datę", "year", clients$data,kind = "wplata")
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
        textInput("addPayments_amount", "Wprowadź kwotę")
      )
    ))
  })
  
  #If you confirm the addition of a deposit, add based on the selected rows the amount / includes clients picker
  observeEvent(input$addPayments_confirm, {
    
    if(is.null(input$clientsList))
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$datesList)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else if(input$addPayments_amount == ""){
      informationAlert(session, "Nie wybrano kwoty", type = "warning")
    } else{
      
      #Distinguish between monthly and annual due to the different ways of converting dates
      if(input$addPayments_type == "Miesięczna"){
        dates = unlist(convertDates(input$datesList,kind="wplata"))
        restriction = MaxMonthPayment()
      } else {
        dates = unlist(convertYear(input$datesList))
        restriction = MaxYearPayment()
      }
      
      payment_amount = str_replace(input$addPayments_amount, ",", ".")
      if(!is.na(as.numeric(payment_amount)) | payment_amount == "-") {
        #Add payments
        if(((as.numeric(payment_amount) >= 0) & (as.numeric(payment_amount) <= restriction)) | payment_amount == "-"){
          if(payment_amount == "-")
            payment_amount = -1
          
          lapply(dates, function(x, data){
            clients$data[selectedClients_rows(clients$data, input$clientsList),x] = round(as.numeric(payment_amount),2)
          })
          
          data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata")
          dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
          dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
          
          clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
          payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
          informationAlert(session, "Dodano wpłatę", type = "success")
          
        } else{
          informationAlert(session, "Podana wartość przekracza maksymalną możliwą do wpisania lub jest ujemna", type = "warning")
        }
        
      } else{
        informationAlert(session, "Podaba wartość musi być liczbą", type = "warning")
      }
      
    }
    
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
        textInput("addPaymentsPicker_amount", "Wprowadź kwotę")
      )
    ))
  })
  
  #If you confirm the addition of a deposit, the payment will be added to the marked lines in data table / without clients picker
  observeEvent(input$addPaymentsPicker_confirm, {
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$paymentsTable_rows_selected)
    
    if(length(selected_rows) == 0)
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$datesList)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else if(input$addPaymentsPicker_amount == ""){
      informationAlert(session, "Nie wybrano kwoty", type = "warning")
    } else{
      
      #Distinguish between monthly and annual due to the different ways of converting dates
      if(input$addPayments_type == "Miesięczna") {
        dates = unlist(convertDates(input$datesList,kind="wplata"))
        restriction = MaxMonthPayment()
      } else{
        dates = unlist(convertYear(input$datesList))
        restriction = MaxYearPayment()
      }
      
      
      payment_amount = str_replace(input$addPaymentsPicker_amount, ",", ".")
      if(!is.na(as.numeric(payment_amount)) | payment_amount == "-") {
        #Add payments
        if(((as.numeric(payment_amount) >= 0) & (as.numeric(payment_amount) <= restriction)) | payment_amount == "-"){
          if(payment_amount == "-")
            payment_amount = -1
          
          lapply(dates, function(x, data){
            clients$data[selected_rows,x] = round(as.numeric(payment_amount),2)
          })
          
          data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata")
          dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
          dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
          
          clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
          payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
          informationAlert(session, "Dodano wpłatę", type = "success")
          
        } else{
          informationAlert(session, "Podana wartość przekracza maksymalną możliwą do wpisania lub jest ujemna", type = "warning")
        }
        
      } else{
        informationAlert(session, "Podaba wartość musi być liczbą", type = "warning")
      }
    }
  })
  
  #Create date picker for "add column" /Includes dates +/- a year from the current one, excluding those in the data set columns
  output[["addColumns_datePicker"]] <- renderUI({
    if(input$addColumns_type == "Miesięczna")
      additionalDates_picker("addColumns_datesList", "Wybierz datę", "month", clients$data,kind="wplata")
    else
      additionalDates_picker("addColumns_datesList", "Wybierz datę", "year", clients$data,kind="wplata")
  })
  
  #Create date picker for "edit column" /Contains only the dates contained in the columns
  output[["editColumns_datePicker"]] <- renderUI({
    if(input$editColumns_type == "Miesięczna")
      addPayments_picker("editColumns_datesList", "Wybierz datę do zmiany", "month", clients$data,FALSE,kind = "wplata")
    else
      addPayments_picker("editColumns_datesList", "Wybierz datę do zmiany", "year", clients$data,FALSE,kind = "wplata")
  })
  
  #Create additional date picker for "edit column /Includes dates +/- a year from the current one, excluding those in the data set columns
  output[["editColumnsAdditional_datePicker"]] <- renderUI({
    if(input$editColumns_type == "Miesięczna")
      additionalDates_picker("editColumnsAdditional_datesList", "Wybierz datę, na którą chcesz zmienić", "month", clients$data,FALSE,kind="wplata")
    else
      additionalDates_picker("editColumnsAdditional_datesList", "Wybierz datę, na którą chcesz zmienić", "year", clients$data,FALSE,kind="wplata")
  })
  
  #Create date picker for "delete column" /Contains only the dates contained in the columns
  output[["deleteColumns_datePicker"]] <- renderUI({
    if(input$deleteColumns_type == "Miesięczna")
      addPayments_picker("deleteColumns_datesList", "Wybierz datę", "month", clients$data,kind = "wplata")
    else
      addPayments_picker("deleteColumns_datesList", "Wybierz datę", "year", clients$data,kind = "wplata")
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
          tabName = "Dodaj kolumnę",
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
          tabName = "Edytuj Kolumnę",
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
          tabName = "Usuń kolumnę",
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
    if(is.null(input$addColumns_datesList)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else {
      if(input$addColumns_type == "Miesięczna")
        dates = unlist(convertDates(input$addColumns_datesList,kind="wplata"))
      else
        dates = unlist(convertYear(input$addColumns_datesList))
      
      #Add columns to dataframe
      lapply(dates, function(x){
        clients$data[[paste(x)]] = 0
      })
      #Save the data to the database and inform the user about the successful operation
      data_ALL = clients_and_payments_writeTable_addPaymentsCols(clients_ALL$data, payments_ALL$data,clients$data,Group_current$data, kind = "wplata")
      
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      payments_ALL$data = dbReadTable(databaseConnection, "wplaty")
      informationAlert(session, "Dodano kolumnę", type = "success")
    }
    
  })
  
  #If the user agrees to edit the column, change its date to the selected
  observeEvent(input$editPaymentColumn, {
    if(is.null(input$editColumns_datesList) | is.null(input$editColumnsAdditional_datesList)){
      informationAlert(session, "Należy wybrać dwie daty", type = "warning")
    } else {
      if(input$editColumns_type == "Miesięczna") {
        date = unlist(convertDates(input$editColumns_datesList,kind="wplata"))
        new_date = unlist(convertDates(input$editColumnsAdditional_datesList,kind="wplata"))
      } else
      {
        date = unlist(convertYear(input$editColumns_datesList))
        new_date = unlist(convertYear(input$editColumnsAdditional_datesList))
      }
      
      #Edit column name
      payments_ALL$data$data[payments_ALL$data$data == date & payments_ALL$data$id_uzytkownika == res_auth$id & payments_ALL$data$grupa == Group_current$data] = new_date
      payments_ALL$data = payments_ALL$data[order(payments_ALL$data$id_uzytkownika,payments_ALL$data$data),]
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(payments_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      informationAlert(session, "Edytowano kolumnę", type = "success")
    }
    
  })
  
  #If the user approves the deletion, delete the selected columns
  observeEvent(input$deletePaymentColumn, {
    
    if(is.null(input$deleteColumns_datesList)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else {
      if(input$deleteColumns_type == "Miesięczna")
        dates = unlist(convertDates(input$deleteColumns_datesList,kind="wplata"))
      else
        dates = unlist(convertYear(input$deleteColumns_datesList))
      
      #Delete selected columns
      payments_ALL$data= payments_ALL$data[-which(payments_ALL$data$id_uzytkownika == res_auth$id & payments_ALL$data$data %in% dates & payments_ALL$data$grupa == Group_current$data),]
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"wplaty",Tonvarchar(payments_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      informationAlert(session, "Usunięto kolumnę", type = "success")
    }
    
  })
  
  #OKKOMENTARZE##################################School register###############################
  #Create datatable with sorted presences columns
  output$schoolRegister_dataTable = renderDataTable({
    data_payments_dataTable(clients_presence$data,kind="obecnosc",
                            grep("obecnosc",colnames(sort_columns(clients_presence$data,kind="obecnosc")),fixed= TRUE,invert = TRUE))
  })
  
  #Create date picker for "add presences" button
  output[["datePicker_presences"]] <- renderUI({
    addPayments_picker(inputId = "datesList_presences", label = "Wybierz datę", type="month",data=clients_presence$data,kind="obecnosc")
  })
  
  #Create date picker for "add column" /Includes dates +/- a year from the current one, excluding those in the data set columns
  output[["addPresenceColumns_datePicker"]] <- renderUI({
    additionalDates_picker(inputId = "addColumns_datesList_presences", label="Wybierz datę", type="month", data=clients_presence$data,kind="obecnosc")
    
  })
  
  #Create date picker for "edit column" /Contains only the dates contained in the columns
  output[["editPresenceColumns_datePicker"]] <- renderUI({
    addPayments_picker(inputId = "editColumns_datesList_presences", label = "Wybierz datę do zmianyWybierz datę do zmiany",type="month",data= clients_presence$data,FALSE,kind="obecnosc")
  })
  
  #Create additional date picker for "edit column /Includes dates +/- a year from the current one, excluding those in the data set columns
  output[["editPresenceColumnsAdditional_datePicker"]] <- renderUI({
    additionalDates_picker(inputId = "editColumnsAdditional_datesList_presences", label="Wybierz datę, na którą chcesz zmienić",type="month", data=clients_presence$data,FALSE, kind="obecnosc")
    
  })
  
  #Create date picker for "delete column" /Contains only the dates contained in the columns
  output[["deletePresenceColumns_datePicker"]] <- renderUI({
    addPayments_picker(inputId = "deleteColumns_datesList_presences", label = "Wybierz datę",type="month",data= clients_presence$data,kind="obecnosc")
  })
  
  #Show modal with form after "add presence" button was pushed / includes clients picker
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
        uiOutput("datePicker_presences"),
        clients_picker("clientsList_presences", "Wybierz osoby", clients_presence$data)
      )
    ))
  })
  
  #If you confirm the addition of a presence, add based on the selected rows/ includes clients picker
  observeEvent(input$addPresences_confirm, {
    
    if(is.null(input$clientsList_presences))
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$datesList_presences)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else{
      
      #Convert dates to numeric format
      dates = unlist(convertDates(input$datesList_presences, "obecnosc"))
      
      max_presences_verification = 0
      restriction = NumberOfClasses()
      
      #Add presences
      lapply(dates, function(x){
        selected_rows = selectedClients_rows(clients_presence$data, input$clientsList_presences)
        
        #Maximum possible number of attendance
        max_presences_verification <<- selected_rows[which(clients_presence$data[selected_rows,x] == restriction)]
        
        #Select only rows which do not have maximum number of attendance
        if(length(max_presences_verification) != 0) {
          selected_rows = selected_rows[!(selected_rows %in% max_presences_verification)]
        }
        clients_presence$data[selected_rows,x] = clients_presence$data[selected_rows,x] + 1
      })
      
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, schoolRegister_ALL$data,clients_presence$data,Group_current$data, kind = "obecnosc")
      dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      schoolRegister_ALL$data = dbReadTable(databaseConnection, "obecnosci")
      
      if(length(max_presences_verification) != 0){
        informationAlert(session, "Dodano obecności, ale pominięte zostały osoby z ich maksymalną liczbą", type = "success")
      } else{
        informationAlert(session, "Dodano obecności", type = "success")
      }
    }
    
  })
  
  #Show modal with adding, editing and deleting columns 
  observeEvent(input$managePresencesColumn, {
    showModal(modalDialog(
      title = "Zarządzaj kolumnami obecności",
      footer =  tagList(
        modalButton("Wyjdź")
      ),
      bs4TabSetPanel(
        id = "managePresenceColumns_panel",
        side = "left",
        bs4TabPanel(
          tabName = "Dodaj kolumnę",
          active = TRUE,
          column(
            width = 12,
            offset = 2,
            uiOutput("addPresenceColumns_datePicker"),
            actionButton("addPresenceColumn", "Dodaj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Edytuj Kolumnę",
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            uiOutput("editPresenceColumns_datePicker"),
            uiOutput("editPresenceColumnsAdditional_datePicker"),
            actionButton("editPresenceColumn", "Edytuj", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        ),
        bs4TabPanel(
          tabName = "Usuń kolumnę",
          active = FALSE,
          column(
            width = 12,
            offset = 2,
            uiOutput("deletePresenceColumns_datePicker"),
            actionButton("deletePresenceColumn", "Usuń", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          )
        )
      )
    ))
  })
  
  #If you confirm the addition of a column, add a column with the corresponding date filled with zeros
  observeEvent(input$addPresenceColumn, {
    if(is.null(input$addColumns_datesList_presences)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else {
      dates = unlist(convertDates(input$addColumns_datesList_presences, "obecnosc"))
      
      #Add columns to dataframe
      lapply(dates, function(x){
        clients_presence$data[[paste(x)]] = 0
      })
      
      #Save the data to the database and inform the user about the successful operation
      data_ALL = clients_and_payments_writeTable_addPaymentsCols(clients_ALL$data, schoolRegister_ALL$data,clients_presence$data,Group_current$data, kind = "obecnosc")
      dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      schoolRegister_ALL$data = dbReadTable(databaseConnection, "obecnosci")
      informationAlert(session, "Dodano kolumnę", type = "success")
    }
  })
  
  #If the user agrees to edit the column, change its date to the selected
  observeEvent(input$editPresenceColumn, {
    if(is.null(input$editColumns_datesList_presences) | is.null(input$editColumnsAdditional_datesList_presences)){
      informationAlert(session, "Należy wybrać dwie daty", type = "warning")
    } else{
      date = unlist(convertDates(input$editColumns_datesList_presences, "obecnosc"))
      new_date = unlist(convertDates(input$editColumnsAdditional_datesList_presences, "obecnosc"))
      
      #Edit column name
      schoolRegister_ALL$data$data[schoolRegister_ALL$data$data == date & schoolRegister_ALL$data$id_uzytkownika == res_auth$id & schoolRegister_ALL$data$grupa == Group_current$data] = new_date
      schoolRegister_ALL$data = schoolRegister_ALL$data[order(schoolRegister_ALL$data$id_uzytkownika,schoolRegister_ALL$data$data),]
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(schoolRegister_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      informationAlert(session, "Edytowano kolumnę", type = "success")
    }
    
  })
  
  #If the user approves the deletion, delete the selected columns
  observeEvent(input$deletePresenceColumn, {
    
    if(is.null(input$deleteColumns_datesList_presences)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else {
      
      dates = unlist(convertDates(input$deleteColumns_datesList_presences, "obecnosc"))
      
      #Delete selected columns
      schoolRegister_ALL$data= schoolRegister_ALL$data[-which(schoolRegister_ALL$data$id_uzytkownika == res_auth$id & schoolRegister_ALL$data$data %in% dates & schoolRegister_ALL$data$grupa == Group_current$data),]
      
      #Save the data to the database and inform the user about the successful operation
      dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(schoolRegister_ALL$data,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      
      informationAlert(session, "Usunięto kolumnę", type = "success")
    }
    
  })
  
  #Show modal with form after "Add presence of your choice" button was pushed / without clients picker
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
        uiOutput("datePicker_presences")
      )
    ))
  })
  
  #If you confirm the addition of a deposit, the payment will be added to the marked lines in data table / without clients picker
  observeEvent(input$addPresencesPicker_confirm, {
    
    #Assign indexes of the marked lines
    selected_rows = as.numeric(input$schoolRegister_dataTable_rows_selected)
    
    if(length(selected_rows) == 0)
    {
      informationAlert(session, "Nie wybrano osób", type = "warning")
    } else if(is.null(input$datesList_presences)){
      informationAlert(session, "Nie wybrano daty", type = "warning")
    } else{
      
      #Convert dates to numeric format
      dates = unlist(convertDates(input$datesList_presences, "obecnosc"))
      max_presences_verification = 0
      restriction = NumberOfClasses()
      
      #Add presences
      lapply(dates, function(x){
        
        #Maximum possible number of attendance
        max_presences_verification <<- selected_rows[which(clients_presence$data[selected_rows,x] == restriction)]
        
        if(length(max_presences_verification) != 0) {
          #Select only rows which do not have maximum number of attendance
          selected_rows = selected_rows[!(selected_rows %in% max_presences_verification)]
        }
        clients_presence$data[selected_rows,x] =  clients_presence$data[selected_rows,x]+1
      })
      
      data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, schoolRegister_ALL$data,clients_presence$data,Group_current$data, kind = "obecnosc")
      dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
      dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
      
      clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
      schoolRegister_ALL$data = dbReadTable(databaseConnection, "obecnosci")
      
      if(length(max_presences_verification) != 0){
        informationAlert(session, "Dodano obecności, ale pominięte zostały osoby z ich maksymalną liczbą", type = "success")
      } else{
        informationAlert(session, "Dodano obecności", type = "success")
      }
    }
    
  })
  
  #Edit cells at data table by click
  observeEvent(input$schoolRegister_dataTable_cell_edit, {
    
    edited_column = colnames(sort_columns(clients_presence$data,kind="obecnosc")[,input$schoolRegister_dataTable_cell_edit$col, drop = FALSE])
    restriction = NumberOfClasses()
    
    #Checking for payments columns that the value entered is a number and is greater or equal 0 if it does not assign -1
    if(!is.na(as.numeric(input$schoolRegister_dataTable_cell_edit$value))) {
      if((as.numeric(input$schoolRegister_dataTable_cell_edit$value) >= 0) & (as.numeric(input$schoolRegister_dataTable_cell_edit$value) <= restriction)) {
        clients_presence$data[[edited_column]][input$schoolRegister_dataTable_cell_edit$row] <<- round(as.numeric(input$schoolRegister_dataTable_cell_edit$value),0)
      } else {
        output$schoolRegister_dataTable = renderDataTable({
          data_payments_dataTable(clients_presence$data,kind="obecnosc",grep("obecnosc",colnames(sort_columns(clients_presence$data,kind="obecnosc")),fixed= TRUE,invert = TRUE))
        })
        informationAlert(session, "Podana wartość przekracza maksymalną możliwą do wpisania lub jest ujemna", type = "warning")
      }
    } else {
      output$schoolRegister_dataTable = renderDataTable({
        data_payments_dataTable(clients_presence$data,kind="obecnosc",grep("obecnosc",colnames(sort_columns(clients$data,kind="obecnosc")),fixed= TRUE,invert = TRUE))
      })
      informationAlert(session, "Podana wartość musi być liczbą", type = "warning")
    }
    
    data_ALL = clients_and_payments_writeTable_edit(clients_ALL$data, schoolRegister_ALL$data,clients_presence$data,Group_current$data, kind = "obecnosc")
    dbWriteTable(databaseConnection,"obecnosci",Tonvarchar(data_ALL$payments,columns_names=c("grupa")), overwrite=T,field.types = c(grupa = "nvarchar(500)"))
    dbWriteTable(databaseConnection,"uczestnicy",Tonvarchar(data_ALL$clients,columns_names=c("imie","nazwisko","grupa","miejsce_urodzenia","adres","email","szkola","imie_opiekuna","nazwisko_opiekuna","email_opiekuna")), overwrite=T,field.types = c(imie = "nvarchar(500)",nazwisko = "nvarchar(500)",grupa = "nvarchar(500)",data_urodzenia="date",miejsce_urodzenia = "nvarchar(500)",adres = "nvarchar(500)",email = "nvarchar(500)",szkola = "nvarchar(500)",imie_opiekuna = "nvarchar(500)",nazwisko_opiekuna = "nvarchar(500)", email_opiekuna ="nvarchar(500)"))
    
    clients_ALL$data = dbReadTable(databaseConnection, "uczestnicy")
    schoolRegister_ALL$data = dbReadTable(databaseConnection, "obecnosci")
    
  })
  
  #OKKOMENTARZE##################################Dashboard###############################
  
  #Render current month payments to valueBox
  observe({
    output$paymentsSum_month = renderText({
      paymentsSum_month_reactive$data <- currentDate_exists_verification(clients$data, paste0("wplata_",format(Sys.Date(),"%m"),"_",format(Sys.Date(),"%Y")), "month","Suma wpłat w bieżącym miesiącu", MonthPayment())
      paymentsSum_month_reactive$data[[1]]
    })
  })
  
  #Render currency
  observe({
    output$paymentsSum_month_subtitle = renderText({
      paymentsSum_month_reactive$data[[2]]
    })
  })
  
  #Render current year payments to valueBox
  observe({
    output$paymentsSum_year = renderText({
      paymentsSum_year_reactive$data <- currentDate_exists_verification(clients$data, paste0("wplata_roczna_",format(Sys.Date(),"%Y")), "year","Suma wpłat w bieżącym roku", YearPayment())
      paymentsSum_year_reactive$data[[1]]
    })
  })
  
  #Render currency
  observe({
    output$paymentsSum_year_subtitle = renderText({
      paymentsSum_year_reactive$data[[2]]
    })
  })
  
  #Render total number of unpaid contributions
  observe({
    output$notpaid_all = renderText({
      notpaid_all_reactive$data <- anyDateCols_exists_verification(clients$data, 0,"Łączna liczba niedokonanych wpłat")
      notpaid_all_reactive$data[[1]]
    })
  })
  
  #Render currency
  observe({
    output$notpaid_all_subtitle = renderText({
      notpaid_all_reactive$data[[2]]
    })
  })
  
  
  #Render total number of customers
  observe({
    output$number_of_clients = renderText({
      number_of_clients_reactive$data <- clients_exists_verification(clients$data,"Liczba klientów")
      number_of_clients_reactive$data[[1]]
    })
  })
  
  #Render subtitle 
  observe({
    output$number_of_clients_subtitle = renderText({
      number_of_clients_reactive$data[[2]]
    })
  })
  
  #Render plot of contributions
  observe({
    #Verify that the user has logged in
    if(!is.null(res_auth$id)){
      output$barPlot_payments = renderPlotly({
        
        #Assign to variables current year and month
        current_year = format(Sys.Date(), "%Y") %>% as.numeric()
        current_month = format(Sys.Date(), "%m") %>% as.numeric()
        
        #Prepare data to create a plot with people who have paid, failed to pay and paid a part of it
        data = notPaid_partPaid_paid_plot(sort_columns(clients$data,kind = "wplata"),current_year, MonthPayment(),YearPayment(), current_month)
        data = select_paymentsPlot_cols(data = data, prevDates = 2, nextDates = 2, currentMonth = ActualMonth, currentYear = ActualYear)
        
        #If there are no payments columns render empty plot
        if(nrow(data) == 0) {
          data = data.frame(Paid = 1, Date = "Brak danych")
          fig <- plot_ly(data, x = ~Date, y = ~Paid, type = 'bar', name = 'Brak danych') %>%
            layout(xaxis = list(title = 'Data'),yaxis = list(title = 'Liczba osób'))  %>%
            config(displayModeBar = FALSE)
          
        } else {
          fig <- plot_ly(data, x = ~Date, y = ~Paid, type = 'bar', name = 'wpłaciły') %>%
            add_trace(y = ~Not_paid, name = 'nie wpłaciły') %>%
            add_trace(y =~Part_paid, name = 'wpłaciły część') %>% 
            layout(xaxis = list(title = 'Data'),yaxis = list(title = 'Liczba osób'), barmode = 'group') %>%
            config(displayModeBar = FALSE)
        }
        
      })
    }
  })
  
  #Render calendar
  output$calendar <- renderCalendar({
    calendar(defaultView = "month")
  })
  
  #List the tables of users who have not paid their monthly contributions
  observe({
    output$notPaidMonth_list = renderDataTable({
      datatable(tables_exists_verification(0, clients$data, "month",kind="wplata"), extensions = c ("Responsive"))
    })
    
  })
  
  #List the tables of users who have not paid their annual contribution
  observe({
    output$notPaidYear_list = renderDataTable({
      datatable(tables_exists_verification(0, clients$data, "year",kind="wplata"), extensions = c ("Responsive"))
      
    })
  })
  
  #List the tables of users who have not paid their share of the annual payment
  observe({
    output$partPaidYear_list = renderDataTable({
      datatable(tables_exists_verification(c(0, YearPayment()), clients$data,"year",kind="wplata"), extensions = c ("Responsive"))
      
    })
  })
  
  #List the tables of users who have not paid a portion of their monthly payments
  observe({
    output$partPaidMonth_list = renderDataTable({
      datatable(tables_exists_verification(c(0,MonthPayment()), clients$data,"month",kind="wplata"), extensions = c ("Responsive"))
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
    #Selected UserPanelPosition because it is position of user panel at sidebar menu
    updatebs4TabItems(
      session,
      inputId = "sidebarMenu",
      selected = 8
    )
  })
  
  #Render Sidebar user panel / Image, name and surname 
  output[["sidebar_userPanel"]] <- renderUI({
    bs4SidebarUserPanel(
      img =  res_auth$img_url,
      text = actionLink(inputId = "userPanelLink", label = textOutput("userName"))
    )
  })
  
  #Render User card at user panel tab
  output[["userPanel"]] <- renderUI({
    bs4UserCard(
      src = res_auth$img_url,
      status = "info",
      title = textOutput("userPanelName"),
      elevation = 4,
      htmlOutput("userInformations"),
      br(),
      fluidRow(
        column(
          width = 6,
          actionButton("editData", "Edytuj dane", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          actionButton("changePassword", "Zmień hasło", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      )
    )
  })
  
  #Render name and surname based on credentials for sidebar panel
  output$userName <- renderText({
    paste(res_auth$name, res_auth$surname, collapse = " ")
  })
  
  #Render name and surname based on credentials for user card
  output$userPanelName <- renderText({
    paste("Użytkownik:", res_auth$name, res_auth$surname, collapse = " ")
  })
  
  #Render information shown in the user card
  output$userInformations <- renderUI({
    HTML(paste(paste0("<b>Imie: </b> ", res_auth$name), paste0("<b>Nazwisko: </b>", res_auth$surname),
               paste0("<b>Email: </b> ", res_auth$email),paste0("<b>Login: </b> ", res_auth$user),
               sep = "<br/>"))
  })
  
  #If the user clicks on the data edition, show a modal with the form
  observeEvent(input$editData, {
    showModal(modalDialog(
      title = "Edytuj dane",
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
  
  #Change the user data
  observeEvent(input$confirmChangedData, {
    res_auth$name = input$changeName
    res_auth$surname = input$changeSurname
    res_auth$user = input$changeLogin
    #If the user uploaded a picture, set it in the application 
    if(length(input$changeImg) != 0)
    {
      #Delete previous image from Dropbox
      drop_delete(paste0("Baza Danych/Profilowe/",res_auth$email, "/",res_auth$image), dtoken = token)
      
      #File name correction
      changeImg = file.rename(input$changeImg$datapath, input$changeImg$name)
      changeImg = input$changeImg$name
      
      #Upload file to dropbox
      drop_upload(changeImg, path = paste0(ProfileDir,res_auth$email), mode = "overwrite", dtoken = token)
      
      #Add image name and url to credentials
      res_auth$image <- changeImg
      res_auth$img_url <- drop_share(paste0(ProfileDir,res_auth$email,"/",res_auth$image), dtoken = token)$url %>% str_replace(., "dl=0","raw=1")
      
      #Save changes to global dataframe
      Credentials$image[Credentials$id_uzytkownika == res_auth$id] <<- res_auth$image
      Credentials$img_url[Credentials$id_uzytkownika == res_auth$id] <<- res_auth$img_url
    }
    #Save rest of changes to global dataframe
    Credentials$imie[Credentials$id_uzytkownika == res_auth$id] <<- res_auth$name
    Credentials$nazwisko[Credentials$id_uzytkownika == res_auth$id] <<- res_auth$surname
    Credentials$uzytkownik[Credentials$id_uzytkownika == res_auth$id] <<- res_auth$user
    
    #Write changes to database
    dbWriteTable(databaseConnection,"wlasciciel_konta",
                 Tonvarchar(Credentials,columns_names=c("uzytkownik","mail","imie","nazwisko","image", "img_url", "gmail_passwd", "drugi_mail")),
                 overwrite=T,field.types = c(uzytkownik = "nvarchar(500)",mail = "nvarchar(500)",imie = "nvarchar(500)",nazwisko = "nvarchar(500)",
                                             image="nvarchar(500)",img_url="nvarchar(500)",gmail_passwd="nvarchar(500)",drugi_mail="nvarchar(500)"))
    
    informationAlert(session,"Dane konta zostały zmienione","success")
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
  
  
  observeEvent(input$confirmChangedPassword, {
    #Verification that the user has entered the correct current password 
    if(input$actualPassword ==  Credentials$haslo[Credentials$id_uzytkownika == res_auth$id])
    {
      #Verification that the user has entered the same new and current password
      if(input$newPassword == input$confirmNewPassword) {
        if(input$actualPassword != input$newPassword)
        {
          if(nchar(input$newPassword) >= 2){
            #Save password in global dataframe
            Credentials$haslo[Credentials$id_uzytkownika == res_auth$id] <<- input$newPassword
            
            #Write changes to database
            dbWriteTable(databaseConnection,"wlasciciel_konta",
                         Tonvarchar(Credentials,columns_names=c("uzytkownik","mail","imie","nazwisko","image", "img_url", "gmail_passwd", "drugi_mail")),
                         overwrite=T,field.types = c(uzytkownik = "nvarchar(500)",mail = "nvarchar(500)",imie = "nvarchar(500)",nazwisko = "nvarchar(500)",
                                                     image="nvarchar(500)",img_url="nvarchar(500)",gmail_passwd="nvarchar(500)",drugi_mail="nvarchar(500)"))
            
            informationAlert(session,"Hasło zostało zmienione", "success")
            removeModal()
            
          } else {
            informationAlert(session,"Nowe hasło musi mieć co najmniej 2 znaki", "error")
          }
          
        }
        else
          informationAlert(session,"Nowe hasło i stare nie różnią się", "error")
      }
      else {
        informationAlert(session,"Nowe hasło i jego potwierdzenie różnią się", "error")
      }
    }
    else {
      informationAlert(session,"Aktualne hasło nie zgadza się", "error")
    }
  })
  
  #OKKOMENTARZE##################################Sidebar#######################
  
  #Create a "picker" for groups
  output[["selectedGroup"]] <- renderUI({
    groups_picker("selectGroup_sidebar", "Wybierz grupę", groups$data, FALSE, "wszyscy")
  })
  
  #Create a "numericInput" for your number of classes per week
  output[["number_of_classes"]] <- renderUI({
    numericInput("NumberOfClasses", value = NumberOfClasses(), min = 1,"Liczba zajęć w miesiącu")
  })
  
  #Create a "numericInput" for your monthly payment
  output[["monthPayment_input"]] <- renderUI({
    numericInput("MonthPayment", value = MonthPayment(), min = 1,"Wysokość wpłaty miesięcznej")
  })
  
  #Create a "numericInput" for your max monthly payment
  output[["monthPayment_input_max"]] <- renderUI({
    numericInput("MonthPayment_max", value = MaxMonthPayment(), min = 1,"Maksymalna wysokość wpłaty miesięcznej")
  })
  
  #Create a "numericInput" for your max annual payment
  output[["yearPayment_input"]] <- renderUI({
    numericInput("YearPayment", value = YearPayment(), min = 1,"Wysokość wpłaty rocznej")
  })
  
  #Create a "numericInput" for your annual payment
  output[["yearPayment_input_max"]] <- renderUI({
    numericInput("YearPayment_max", value = MaxYearPayment(), min = 1,"Maksymalna wysokość wpłaty rocznej")
  })
  
}
