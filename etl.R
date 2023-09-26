
library(dplyr)  
library(rvest)
library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
library(highcharter)


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
    weekdays(as.Date(today)-1) == "domingo"){
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

















library(httr)
library(rvest)



today = format(
  with_tz(Sys.time() , 
          tzone = "America/Argentina/Buenos_Aires"), 
  format = "%Y-%m-%d"
)

from = as.Date(today) - years(1)
from_historic = from
to = today



get_json <- function(url) {
  tryCatch({
    response <- httr::GET(url, headers = c("sec-ch-ua" = "^Chromium^;v=^116^, ^Not"))
    status <- httr::http_status(response)$reason 
    if (status == "OK") {
      response_content <- httr::content(response, "text")
      tryCatch({
        response_json <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
        return(response_json)
      }, error = function(e) {
        cat("Error al analizar el JSON:", e$message, "\n")
        return(NULL)
      })
    } else {
      cat("La solicitud no se pudo completar. Código de estado:", status, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error en la solicitud HTTP:", e$message, "\n")
    return(NULL)
  })
}


ultima_actualizacion = function(ccl_day, oficial_day, informal_day, mep_day) {
  ultima_actualizacion = rbind(ccl_day, oficial_day, informal_day, mep_day) %>% select(Actualizacion) %>% arrange(Actualizacion)
  valor = ultima_actualizacion[nrow(ultima_actualizacion),]$Actualizacion
  return(valor)
}




format_df = function(df){
  if ("Promedio" %in% colnames(df)) {
    return(df %>%
             arrange(Fecha) %>% 
             mutate(Promedio = round(as.numeric(gsub(",", ".", Promedio)),2)) %>% 
             mutate(variacion = round((as.numeric(Promedio) - lag(as.numeric(Promedio), n = 1)) / lag(as.numeric(Promedio), n = 1),4)*100) %>% 
             select(Fecha, Promedio, variacion))
  }  else if ("valor_cierre_ant" %in% colnames(df)) {
    return(
      df %>% mutate(
        Fecha = as.Date(strsplit(fecha, " - ")[[1]][1], format = "%d/%m/%Y"),
        Compra = round(as.numeric(gsub(",", ".", compra)),2),
        Venta = round(as.numeric(gsub(",", ".", venta)),2),
        Promedio = ((Compra + Venta) / 2),
        variacion = round(as.numeric(gsub(",", ".", gsub("%", "", variacion))),4)) %>%
        select(Fecha, Compra, Venta, Promedio, variacion)
    )
  } else {
    return(
      df %>% arrange(Fecha) %>% mutate(
        Fecha = as.Date(Fecha, format = "%d/%m/%Y"),
        Compra = round(as.numeric(gsub(",", ".", Compra)),2),
        Venta = round(as.numeric(gsub(",", ".", Venta)),2),
        Promedio = ((Compra + Venta) / 2),
        variacion = round((Promedio - lag(Promedio, n = 1)) / lag(Promedio, n = 1),4)*100) %>%
        select(Fecha, Compra, Venta, Promedio, variacion)
    ) 
  }
}







format_df_diario = function(df){
  return(
    df %>% mutate(
      Fecha = as.Date(strsplit(fecha, " - ")[[1]][1], format = "%d/%m/%Y"),
      Actualizacion = strsplit(fecha, " - ")[[1]][2],
      Compra = round(as.numeric(gsub(",", ".", compra)),2),
      Venta = round(as.numeric(gsub(",", ".", venta)),2),
      Promedio = ((Compra + Venta) / 2),
      variacion = round(as.numeric(gsub(",", ".", gsub("%", "", variacion))),4)) %>%
      select(Fecha, Compra, Venta, Actualizacion, Promedio, variacion)
  )
}




get_dolar = function(url, from, to){
  url_historico <- paste0(url, from,"/",to)
  response_json = get_json(url_historico)
  data_frame <- response_json %>%
    purrr::map_dfr(~setNames(as.list(.x), c("Fecha", "Compra", "Venta")))
  mi_tibble <- as_tibble(data_frame)[-1, ]
  mi_tibble <- mi_tibble %>% 
    mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>%
    arrange(Fecha) %>%
    group_by(Fecha) %>%
    filter(Venta == max(Venta)) %>%
    ungroup() %>% 
    distinct()
  return(format_df(mi_tibble))
}


url_informal = "https://mercados.ambito.com/dolar/informal/historico-general/"
informal_historico = get_dolar(url_informal, from, to)

url_informal_diario <- "https://mercados.ambito.com/dolar/informal/variacion"

informal_diario <- format_df(as_tibble(get_json(url_informal_diario)))

informal = rbind(informal_historico %>% filter(Fecha != today), informal_diario) %>% arrange(Fecha)
informal_day = format_df_diario(as_tibble(get_json(url_informal_diario)))

rm(informal_historico)
rm(informal_diario)





get_mep = function(url, from, to){
  url_historico <- paste0(url, from,"/",to)
  response_json = get_json(url_historico)
  data_frame <- data.frame(matrix(unlist(response_json), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(data_frame) <- c("Fecha", "Promedio")
  mi_tibble <- as_tibble(data_frame)[-1, ]
  mi_tibble <- mi_tibble %>% 
    mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>%
    arrange(Fecha) %>%
    group_by(Fecha) %>%
    filter(Promedio == max(Promedio)) %>%
    ungroup() %>% 
    distinct()
  return(format_df(mi_tibble))
}



url_mep = "https://mercados.ambito.com//dolarrava/mep/grafico/"
mep_historico = get_mep(url_mep, from, to)

url_mep_diario <- "https://mercados.ambito.com///dolarrava/mep/variacion"

mep_diario <- format_df(as_tibble(get_json(url_mep_diario)))

mep = rbind(mep_historico %>% filter(Fecha != today), mep_diario %>% select(Fecha, Promedio, variacion)) %>% arrange(Fecha)
mep_day = format_df_diario(as_tibble(get_json(url_mep_diario)))
rm(mep_historico)
rm(mep_diario)



get_ccl = function(url, from, to){
  url_historico <- paste0(url, from,"/",to)
  response_json = get_json(url_historico)
  data_frame <- data.frame(matrix(unlist(response_json), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(data_frame) <- c("Fecha", "Promedio")
  mi_tibble <- na.omit(as_tibble(data_frame)[-1, ])
  mi_tibble <- mi_tibble %>% 
    mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>%
    arrange(Fecha) %>%
    group_by(Fecha) %>%
    filter(Promedio == max(Promedio)) %>%
    ungroup() %>% 
    distinct()
  return(format_df(mi_tibble))
}



url_ccl = "https://mercados.ambito.com//dolarrava/cl/historico-general/"
ccl_historico = get_ccl(url_ccl, from, to)

url_ccl_diario <- "https://mercados.ambito.com//dolarrava/cl/variacion"

ccl_diario <- format_df(as_tibble(get_json(url_ccl_diario)))

ccl = rbind(ccl_historico %>% filter(Fecha != today), ccl_diario %>% select(Fecha, Promedio, variacion)) %>% arrange(Fecha)
ccl_day = format_df_diario(as_tibble(get_json(url_mep_diario)))
rm(ccl_historico)
rm(ccl_diario)








url_oficial = "https://mercados.ambito.com//dolar/oficial/historico-general/"
oficial_historico = get_dolar(url_oficial, from, to)

url_oficial_diario <- "https://mercados.ambito.com//dolar/oficial/variacion"

oficial_diario <- format_df(as_tibble(get_json(url_oficial_diario)))

oficial = rbind(oficial_historico, oficial_diario) %>% arrange(Fecha)
oficial_day = format_df_diario(as_tibble(get_json(url_oficial_diario)))
rm(oficial_historico)
rm(oficial_diario)





ultima_hora = ultima_actualizacion(ccl_day, oficial_day, informal_day, mep_day)








dolar <- merge(
  merge(
    merge(
      informal %>% select(Fecha, Promedio) %>% rename(informal = Promedio), 
      ccl %>% select(Fecha, Promedio) %>% rename(ccl = Promedio), by = "Fecha", all = TRUE), 
    mep %>% select(Fecha, Promedio) %>% rename(mep = Promedio), by = "Fecha", all = TRUE), 
  oficial %>% select(Fecha, Promedio) %>% rename(oficial = Promedio), by = "Fecha", all = TRUE)





get_df_fill = function(df, from_historic){
  df = df %>% filter(Fecha >= from_historic)
  columns = c("mep","ccl","informal","oficial")
  for (col in columns){
    posiciones_na <- which(is.na(df[, col]))
    for (pos in posiciones_na){
      numero = pos
      while (TRUE) {
        numero <- numero + 1
        valor_anterior = df[[col]][numero]
        if (!is.na(valor_anterior)) {
          df[[col]][pos] = valor_anterior
          break
        } 
      }
    }
  }
  return(df %>% distinct())
}


d=get_df_fill(dolar,from_historic)





highchart() %>%
  hc_xAxis(categories = d$Fecha) %>%
  hc_add_series(d$ccl, 
                name = "CCL", 
                type = "line",
                color = "#2dcc72",
                tooltip = list(valueDecimals = 2, valueSuffix = " (Serie 1)")) %>%
  hc_add_series(d$mep, 
                name = "Mep",
                type = "line",
                color = "#1b7bc4",
                tooltip = list(valueDecimals = 2, valueSuffix = " (Serie 1)")) %>%
  hc_add_series(d$informal,
                name = 'Informal', 
                type = "line",
                color = "#5dadbd",
                tooltip = list(valueDecimals = 2, valueSuffix = " (Serie 1)")) %>%
  hc_add_series(d$oficial, 
                name = "Oficial",
                type = "line",
                color = "#cfc5b7",
                tooltip = list(valueDecimals = 2, valueSuffix = " (Serie 1)")) %>%
  hc_title(text = "Gráfico de Líneas con Tres Series de Tiempo") %>%
  hc_yAxis(title = list(text = "Valor")) %>%
  hc_tooltip(shared = T) %>%
  hc_plotOptions(series = list(dataLabels = list(enabled = F))) %>%
  highcharter::hc_legend(enabled = T) %>%
  highcharter::hc_yAxis(
    title = list(text = "Precio",
                 style = list(color = "black", fontWeight = "bold")),
    gridLineWidth = 0,
    reversed = FALSE,
    labels = list(
      style = list(color = "black", fontWeight = "bold")
    )
  ) %>%
  highcharter::hc_caption(
    text = paste0("Serie Temporal de distintos tipos de cambio en Argentina."),
    style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
  hc_title(
    text = paste0('Dolar Argentina'),
    style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) %>% 
hc_add_theme(hc_theme_google())






acumulado_df = function(df){
  columns = c("mep","ccl","informal","oficial")
  max_year_filter = year(max(df$Fecha))
  data_filter = df %>% filter(year(Fecha)==max_year_filter) %>% select(Fecha) 
  min_date_filter = min(data_filter$Fecha)
  max_date_filter = max(data_filter$Fecha)
  names = c()
  values = c()
  for (i in 1:length(columns)){
    actual_value = df[df[["Fecha"]] == max_date_filter, columns[i]]
    before_value = df[df[["Fecha"]] == min_date_filter, columns[i]]
    interanual_rate = round((actual_value - before_value) / before_value, 4) * 100
    
    names[i] <- columns[i]
    values[i] <- interanual_rate
  }
  return(na.omit(tibble(tc = names, tasa = values)))
}


a = acumulado_df(d)



highcharter::hchart(
  a, 
  type = "column",  
  highcharter::hcaes(x = "tc", y = "tasa"),
  color = "#007bff",
  name = "a", 
  showInLegend = F,
  maxSize = "15%",
  dataLabels = list(enabled = TRUE,
                    format = '{point.y:.2f} %')) %>%
  hc_legend(enabled = FALSE) %>% 
  highcharter::hc_tooltip(
    crosshairs = FALSE, 
    pointFormat = "Inflacion: {point.y:.2f} %") %>%
  highcharter::hc_xAxis(
    title = list(text = ""),
    gridLineWidth = 0,
    dateTimeLabelFormats = list(day = '%Y'),
    type = "date",
    reversed = FALSE,
    labels = list(
      style = list(color = "black", fontWeight = "bold")
    )
  ) %>%
  highcharter::hc_yAxis(
    title = list(text = "% Acumulado",
                 style = list(color = "black", fontWeight = "bold")),
    gridLineWidth = 0,
    reversed = FALSE,
    labels = list(
      style = list(color = "black", fontWeight = "bold")
    )
  ) %>%
  
  highcharter::hc_caption(
    text = paste0("Grafico de barras de . <br>Fuente: Indec."),
    style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
  hc_title(
    text = paste0('Inflacion '),
    style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) %>% 
  highcharter::hc_tooltip(
    crosshairs = TRUE,
    backgroundColor = "#F0F0F0",
    shared = TRUE, 
    borderWidth =1
  ) %>% 
  hc_credits(enabled = TRUE, text = "INDEC",align = "right",verticalAlign = "bottom",
             style = list(color = "#2b908f", fontSize = '10px'),
             href = " https://www.indec.gob.ar/")






a <- a %>%
  mutate(colores = ifelse(tc == "mep", "#1b7bc4",
              ifelse(tc == "ccl", "#2dcc72",
                     ifelse(tc == "informal", "#5dadbd",
                            ifelse(tc == "oficial", "#cfc5b7", NA))))) %>% 
  arrange(desc(tasa))


highchart() %>%
  hc_chart(type = "column") %>%
  hc_xAxis(categories = a$tc) %>%
  hc_add_series(data = a$tasa, name = "Precio", colorByPoint = T, colors = a$colores)%>% 
  highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Porcentaje Acumulado en el Año: % {point.y}") %>%
  highcharter::hc_legend(enabled = FALSE) %>%
  highcharter::hc_xAxis(
    title = list(text = ""),
    reversed = FALSE,
    labels = list(
      style = list(color = "black", fontWeight = "bold")
    )
  ) %>%
  highcharter::hc_yAxis(
    title = list(text = "%",
                 style = list(color = "black", fontWeight = "bold")),
    gridLineWidth = 0,
    reversed = FALSE,
    labels = list(
      style = list(color = "black", fontWeight = "bold")
    )
  ) %>%
  
  highcharter::hc_caption(
    text = paste0("Tipos de Cambio"),
    style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
  highcharter::hc_tooltip(
    crosshairs = TRUE,
    backgroundColor = "#F0F0F0",
    shared = TRUE, 
    borderWidth = 5
  ) %>% 
  hc_title(
    text = paste0("Variacion % acumulado en" , as.character(year(to))),
    style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 