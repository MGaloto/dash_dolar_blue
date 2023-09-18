
library(dplyr)  
library(rvest)
library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)


URL = 'https://dolarhistorico.com/cotizacion-dolar-blue/mes/'
URL_BLUE = "https://dolarhoy.com/cotizaciondolarblue"
URL_MEP = "https://dolarhoy.com/cotizaciondolarbolsa"
URL_OFICIAL = "https://dolarhoy.com/cotizaciondolaroficial"


today = format(
  with_tz(Sys.time() , 
          tzone = "America/Argentina/Buenos_Aires"), 
  format = "%Y-%m-%d"
  )

last_update <- paste("Last update: ", today(), sep = "")






get_dolarhoy = function(url){
  webpage <- read_html(url)
  compra_node <- webpage %>%
    html_nodes(xpath = "//div[@class='topic'][text()='Compra']/following-sibling::div[@class='value']")
  venta_node <- webpage %>%
    html_nodes(xpath = "//div[@class='topic'][text()='Venta']/following-sibling::div[@class='value']")
  compra_valor <- round(as.numeric(gsub("\\$","", html_text(compra_node))),2)
  venta_valor <- round(as.numeric(gsub("\\$","", html_text(venta_node))),2)
  promedio <- round(((venta_valor+compra_valor)/2),2)
  return(c("compra" = compra_valor, "venta" = venta_valor, "promedio" = promedio))
  
  
}

edit_dolar_historico = function(df){
  return(
    df %>% mutate(
      Fecha = as.Date(Fecha, format = "%d/%m/%Y"),
      Compra = as.numeric(Compra),
      Venta = as.numeric(Venta),
      Promedio = as.numeric(Promedio),
      variacion = as.numeric(variacion))
  )
}


data = edit_dolar_historico(read_excel('data/dolar.xlsx') %>% select(Fecha,Compra,Venta,Promedio,variacion)) %>% arrange(Fecha)
datamep = edit_dolar_historico(read_excel('data/dolarmep.xlsx') %>% select(Fecha,Compra,Venta,Promedio,variacion)) %>% arrange(Fecha)
dataoficial = edit_dolar_historico(read_excel('data/dolaroficial.xlsx') %>% select(Fecha,Compra,Venta,Promedio,variacion)) %>% arrange(Fecha)


get_ts_dolar = function(data, URL) {
  from_year=year(max(data$Fecha))
  from_month=month(max(data$Fecha))
  vector_years = c(from_year:year(today()))
  months = c('enero', 'febrero', 'marzo', 
             'abril', 'mayo', 'junio', 
             'julio', 'agosto', 'septiembre', 
             'octubre', 'noviembre', 'diciembre'
  )
  vector_months = months[from_month:length(months)]
  
  vector_final = c()
  datalist = list()
  datalist = vector("list")
  
  counter = 1
  for (i in 1:length(vector_years)) {
    counter = counter + 1
    year = vector_years[i]
    
    for (i in 1:length(vector_months)) {
      counter = counter + 1
      month = vector_months[i]
      vector_final[counter] = paste0(month, '-', year)
    }
  }
  
  for (i in 1:length(vector_final)) {
    skip_to_next <- FALSE
    link = paste0(
      URL,vector_final[i])
    data = as.data.frame(rvest::html_table(read_html(link)))
    datalist[[i]] <- data
    tryCatch(print(b), error = function(e) {skip_to_next <<- TRUE})
    if( skip_to_next ) { next }    
  }
  dolar = do.call(rbind, datalist)
  return(
    dolar
  )
}



edit_dolar = function(df){
  return(
    data.frame(df) %>% mutate(
      Fecha = as.Date(Fecha, format = "%d/%m/%Y"),
      Compra = round(as.numeric(gsub(",", ".", Compra)),2),
      Venta = round(as.numeric(gsub(",", ".", Venta)),2),
      Promedio = ((Compra + Venta) / 2),
      variacion = round(as.numeric(gsub(",", ".", gsub("%", "", Variación))),2) ) %>%
      select(Fecha, Compra, Venta, Promedio, variacion)
  ) 
}


get_tibble = function(dolarhoy, data, today){
  today_before = as.Date(max(data$Fecha))-1
  return(
    tibble(
      Fecha = as.Date(today),
      Compra = dolarhoy["compra"][[1]],
      Venta = dolarhoy["venta"][[1]],
      Promedio = dolarhoy["promedio"][[1]],
      variacion = round(
        ((dolarhoy["promedio"][[1]] - 
            data$Promedio[data$Fecha==today_before] ) / 
           data$Promedio[data$Fecha==today_before]),2
      ) * 100
    )
  )
}


if (today>=max(data$Fecha)){
  if (today == max(data$Fecha)+1 |
      today == max(data$Fecha) |
      weekdays(as.Date(today)-1) == "domingo"){
    dolarhoy_blue= get_dolarhoy(URL_BLUE)
    dolar = unique(bind_rows(
      data %>% filter(Fecha < today),
      get_tibble(dolarhoy_blue, data, today)
    ) %>% arrange(Fecha)
    )
  } else {
    dolar = unique(bind_rows(data, edit_dolar(get_ts_dolar(data, URL))) %>% arrange(Fecha))
  }
  write.xlsx(dolar, 'data/dolar.xlsx', rowNames = FALSE, overwrite = TRUE)
  rm(data)
} else {
  dolar <- data %>% arrange(Fecha)
  rm(data)
}




if (today>=max(datamep$Fecha)  |
    weekdays(as.Date(today)-1) == "domingo")){
  dolarhoy_mep= get_dolarhoy(URL_MEP)
  dolarmep = unique(bind_rows(
    datamep %>% filter(Fecha < today),
    get_tibble(dolarhoy_mep, datamep, today)
  ) %>% arrange(Fecha)
  )
  write.xlsx(dolarmep, 'data/dolarmep.xlsx', rowNames = FALSE, overwrite = TRUE)
  rm(datamep)
} else {
  dolarmep <- datamep %>% arrange(Fecha)
  rm(datamep)
}



if (today>=max(dataoficial$Fecha) |
    weekdays(as.Date(today)-1) == "domingo"){
  dolarhoy_oficial= get_dolarhoy(URL_OFICIAL)
  dolaroficial = unique(bind_rows(
    dataoficial %>% filter(Fecha < today),
    get_tibble(dolarhoy_oficial, dataoficial, today)
  ) %>% arrange(Fecha)
  )
  write.xlsx(dolaroficial, 'data/dolaroficial.xlsx', rowNames = FALSE, overwrite = TRUE)
  rm(dataoficial)
} else {
  dolaroficial <- dataoficial %>% arrange(Fecha)
  rm(dataoficial)
}
