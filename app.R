
library(bs4Dash)
library(tidyverse)
library(dplyr)
library(shinycssloaders)
library(waiter)
library(readxl)
library(highcharter)
library(shiny)
library(lubridate)
library(reactable)
library(htmltools)
library(openxlsx)
library(httr)
library(rvest)



today = as.Date(format(
  with_tz(Sys.time() , 
          tzone = "America/Argentina/Buenos_Aires"), 
  format = "%Y-%m-%d"
))


current_hour <- hour(with_tz(Sys.time(), tzone = "America/Argentina/Buenos_Aires"))



get_day_to = function(today, current_hour){
  day_of_week <- weekdays(today)
  if (day_of_week == "sábado" || day_of_week == "Saturday") {
    today <- format(as.Date(today) - days(1), format = "%Y-%m-%d")
    return(as.Date(today))
  } else if (day_of_week == "domingo"|| day_of_week == "Sunday") {
    today <- format(as.Date(today) - days(2), format = "%Y-%m-%d")
    return(as.Date(today))
  }
  
  else if (current_hour >= 0 && current_hour <= 11) {
    
    if (day_of_week == "lunes" || day_of_week == "Monday"){
      today <- format(as.Date(today) - days(3), format = "%Y-%m-%d")
      return(as.Date(today))
    } else {
      today <- format(as.Date(today) - days(1), format = "%Y-%m-%d")
      return(as.Date(today))
    }
    
  }
  
  else{
    return(as.Date(today))
  }

}




to = get_day_to(today, current_hour)
from = to - years(1)
from_historic = from



url_ccl = "https://mercados.ambito.com//dolarrava/cl/historico-general/"
url_ccl_diario <- "https://mercados.ambito.com//dolarrava/cl/variacion"
url_informal = "https://mercados.ambito.com/dolar/informal/historico-general/"
url_informal_diario <- "https://mercados.ambito.com/dolar/informal/variacion"
url_mep = "https://mercados.ambito.com//dolarrava/mep/grafico/"
url_mep_diario <- "https://mercados.ambito.com///dolarrava/mep/variacion"
url_oficial = "https://mercados.ambito.com//dolar/oficial/historico-general/"
url_oficial_diario <- "https://mercados.ambito.com//dolar/oficial/variacion"



                     
CCL_COLOR = "#2dcc72"
MEP_COLOR = "#1b7bc4"
INFORMAL_COLOR = "#5dadbd"
OFICIAL_COLOR = "#cfc5b7"


get_df_fill = function(df, from_historic){
  df = df %>% filter(Fecha >= from_historic)
  columns = c("mep","ccl","informal","oficial")
  for (col in columns){
    
    ult_value = df[nrow(df),col]
    if (is.na(ult_value)){
      ante_ult_value = dolar[nrow(dolar) - 1,col]
      df[nrow(df),col] = ante_ult_value
    } 
    
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
        cat("Error al analizar el JSON:", e$message,url, "\n")
        return(NULL)
      })
    } else {
      cat("La solicitud no se pudo completar. Código de estado:", status,url, "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error en la solicitud HTTP:", e$message, "\n")
    return(NULL)
  })
}


ultima_actualizacion = function(ccl_day, oficial_day, informal_day, mep_day) {
  act= rbind(ccl_day, oficial_day, informal_day, mep_day) %>% select(Actualizacion) %>% arrange(Actualizacion)
  valor = act[nrow(act),]$Actualizacion
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






get_icon_arrow = function(df){
  actual_value = round(df$brecha[nrow(df)],2)
  before_value = round(df$brecha[nrow(df)-1],2)
  if (actual_value > before_value){
    return(
      c("danger", "arrow-up")
    )
  } else if (actual_value < before_value) {
    return(
      c("primary", "arrow-down")
    )
  } else {
    c("teal","equals")
  }
  
}



get_table = function(df){
  return(
    reactable::reactable(
      na.omit(df) %>% arrange(desc(Fecha)),
      columns = list(
        Fecha = colDef(
          name = "Fecha",
          align = "center",
          minWidth = 60
        ),
        Promedio = colDef(
          name = "Promedio",
          align = "left",
          minWidth = 60
        ),
        variacion = colDef(
          name = "variacion",
          align = "left",
          minWidth = 60
        )
      ),
      searchable = TRUE,
      striped = TRUE,
      defaultPageSize = 8)
  )
}






ui <- dashboardPage(
  options = F,
  header = dashboardHeader(
    skin = "dark",
    fixed = T,
    title = dashboardBrand(
      title ="Dolar Argentina",
      image = "https://w7.pngwing.com/pngs/563/791/png-transparent-dollar-sign-illustration-united-states-dollar-icon-design-icon-dollar-sign-text-logo-number.png")
  ),
  sidebar = dashboardSidebar(
    background = "lightgray",
    background_dark = "darkgray",
    skin = "light",
    width = 500,
    sidebarMenu(
      menuItem(
        text = "Principal",
        tabName = "dashboard",
        startExpanded = T,
        icon = icon("home")
      ),
      menuItem(
        text = "Brechas",
        tabName = "brechas",
        startExpanded = T,
        icon = icon("chart-bar")
      ),
      menuItem(
        text = "Datos",
        tabName = "datos",
        startExpanded = T,
        icon = icon("database")
      ),
      menuItem(
        text = "Acerca de",
        tabName = "about",
        icon = icon("info-circle"))
    )
  ),
  body = dashboardBody(
    tags$style(
      type = 'text/css', 
      '.rt-align-left {color: black!important; }',
      '.rt-align-center {color: black!important; }',
      '.ReactTable {color: black!important; }',
      '.sidebar .shiny-input-container {padding: 0px 21px 0px 16px!important; }',
      '.btn-primary {color: #fff!important; background-color: #5d6b7a!important; border-color: #007bff!important; box-shadow: none!important;}',
      '.small-box .inner {padding: 12px!important; text-align: center!important; font-size: 37px!important;}'
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        h3("Precios Dolar Economia Argentina"),
        p("Dashboard sobre el precio del dolar en ", 
          span("Argentina", style = "color: green;"), paste0(". Datos actualizados hasta: ",to)),
        fluidRow(
          bs4ValueBoxOutput("valuebox_1",width = 4),
          bs4ValueBoxOutput("valuebox_2",width = 4),
          bs4ValueBoxOutput("valuebox_3",width = 4)
        ),
        fluidRow(
          column(
            width = 7,
            box(
              title = "Serie Temporal",
              width = 12,
              status = "teal",
              icon = icon("chart-line"),
              solidHeader = TRUE,
              tabPanel(
                title = "Var",
                fluidRow(
                  column(6, dateRangeInput(
                    "daterange", 
                    label = "Rango de Fechas",
                    start  = from,
                    end    = to,
                    min    = from,
                    max    = to,
                    format = "dd/mm/yyyy",
                    separator = " - ",
                    language = "es"
                  )),
                  column(6, radioButtons(
                    "opciones",
                    "Opciones:",
                    choices = c("Anual", "Ultima Semana"),
                    inline = T,
                    selected = "Anual"))
                )
                ,
                withSpinner(
                  highchartOutput("timeserie"),
                  type = 1
                )
              )
            )
          ),
          column(
            width = 5,
            box(
              title = "Bar Plot",
              status = "teal",
              icon = icon("keyboard"),
              solidHeader = TRUE,
              width = 12,
              tabPanel(
                title = "Time Serie",
                withSpinner(
                  highchartOutput("barplot"),
                  type = 1
                )
              )
            )
          ),
          column(
            width = 12,
            box(
              title = "Variacion Dolar",
              width = 12,
              status = "teal",
              icon = icon("chart-line"),
              solidHeader = TRUE,
              tabPanel(
                title = "Variacion Dolar Blue",
                radioButtons("tc", "Seleccionar el Tipo de Cambio",
                             c("Informal" , "Ccl", "Mep" , "Oficial"), 
                             inline = T),
                withSpinner(
                  highchartOutput("varplot"),
                  type = 1
                )
              )
            )
          )
        )),
      
      tabItem(
        tabName = "brechas",
        p("Los colores indican un cambio en la brecha cambiaria respecto al dia anterior.",
          span("Rojo", style = "color: red;"), paste0(" indica un incemento de la brecha."),
          span("Azul", style = "color: blue;"), paste0(" indica una caida y"),
          span(" Verde", style = "color: green;"), paste0(" que permanece constante.")),
        fluidRow(
          bs4ValueBoxOutput("valuebox_brechas_1",width = 4),
          bs4ValueBoxOutput("valuebox_brechas_2",width = 4),
          bs4ValueBoxOutput("valuebox_brechas_3",width = 4),
        ),
        fluidRow(
          column(12,
                 tabBox(title = "",
                        status = "gray",
                        width = 12, 
                        solidHeader = TRUE,
                        maximizable = T,
                        tabPanel(
                          title = "Blue/Mep",
                          withSpinner(
                            highchartOutput("barplot_bluevsmep"))
                        ),
                        tabPanel(
                          title = "Mep/Oficial",
                          withSpinner(
                            highchartOutput("barplot_mepvsoficial"))
                        ),
                        tabPanel(
                          title = "Blue/Oficial",
                          withSpinner(
                            highchartOutput("barplot_bluevsoficial"))
                        ),
                        tabPanel(
                          title = "CCL/Oficial",
                          withSpinner(
                            highchartOutput("barplot_cclvsoficial"))
                        )
                 )
          )
        )
      ),
      tabItem(
        tabName = "datos",
        p("Datos de los 4 Tipos de Cambio."),
        fluidRow(
          column(12,
                 tabBox(
                   title = "",
                   status = "gray",
                   width = 12, 
                   solidHeader = TRUE,
                   maximizable = T,
                   tabPanel(
                     title = "Dolar Blue",
                     reactable::reactableOutput("table_blue_output")
                   ),
                   tabPanel(
                     title = "Dolar Mep",
                     reactable::reactableOutput("table_mep_output")
                   ),
                   tabPanel(
                     title = "Dolar CCL",
                     reactable::reactableOutput("table_ccl_output")
                   ),
                   tabPanel(
                     title = "Dolar Oficial",
                     reactable::reactableOutput("table_oficial_output")
                   )
                 )
          )
        )
      ),
      tabItem(
        tabName = "about",
        h2("Acerca de"),
        fluidRow(
          box(
            title = strong("Dashboard sobre los tipos de cambio en Argentina"), solidHeader = TRUE,
            p("Este Dashboard tiene la finalidad de mostrar la variación de los distintos tipos de cambio en  ", strong("Argentina"), "."),
            p("Podemos ver las siguientes métricas:"),
            tags$ol(
              tags$li("Variación interanual del tipo de cambio paralelo"),
              tags$li("Variación diaria del tipo de cambio paralelo, mep y oficial."),
              tags$li("Variación diaria de las brechas cambiarias. Blue vs Mep, Mep vs Oficial y Blue vs Oficial")
            ),
            p("Packages:"),
            tags$ul(
              tags$li(tags$a(href="https://jkunst.com/highcharter/index.html", "highcharter")),
              tags$li(tags$a(href="https://rinterface.github.io/bs4Dash/index.html", "bs4Dash")),
              tags$li(tags$a(href="https://dplyr.tidyverse.org/", "dplyr"))
            )
          ),
          box(
            title = strong("Trabajo hecho por:"), solidHeader = TRUE,
            h5("Galoto Maximiliano"),
            tags$ul(
              tags$li(tags$a(href="https://www.linkedin.com/in/maximilianogaloto", "Linkedin")),
              tags$li(tags$a(href="https://github.com/MGaloto", "GitHub")))
          )
        )
      )
    )
  )
)

# SV ----------------------------------------------------------------------
server <- function(input, output,session) {
  
  
  
  informal = reactiveVal(rbind(
    get_dolar(url_informal, from, to) %>% filter(Fecha != to, Fecha != today), 
    format_df(as_tibble(get_json(url_informal_diario)))
    ) %>% arrange(Fecha) %>% distinct()
  )
  
  informal_day = reactive(format_df_diario(
    as_tibble(
      get_json(url_informal_diario)
      )
    )
  )
  
  

  mep = reactiveVal(rbind(
    get_mep(url_mep, from, to) %>% filter(Fecha != to,Fecha != today), 
    format_df(as_tibble(get_json(url_mep_diario))) %>% select(Fecha, Promedio, variacion)
    ) %>% arrange(Fecha) %>% distinct()
  )
  
  mep_day = reactive(format_df_diario(
    as_tibble(
      get_json(url_mep_diario)
      )
    )
  )
  
  
  
  ccl = reactiveVal(rbind(
    get_ccl(url_ccl, from, to) %>% filter(Fecha != to,Fecha != today), 
    format_df(as_tibble(get_json(url_ccl_diario))) %>% select(Fecha, Promedio, variacion)
    ) %>% arrange(Fecha) %>% distinct()
  )
  
  ccl_day = reactive(format_df_diario(
    as_tibble(
      get_json(url_ccl_diario)
      )
    )
  )

  
  
  oficial = reactiveVal(rbind(
    get_dolar(url_oficial, from, to) %>% filter(Fecha != to,Fecha != today), 
    format_df(as_tibble(get_json(url_oficial_diario)))
    ) %>% arrange(Fecha) %>% distinct()
  )
  
  oficial_day = reactive(format_df_diario(
    as_tibble(
      get_json(url_oficial_diario)
      )
    )
  )


  
  ultima_hora = reactive(ultima_actualizacion(
    ccl_day(), oficial_day(), informal_day(), mep_day()
    )
  )
  
  
  
  
  
  dolar <- reactive(merge(
    merge(
      merge(
        informal() %>% select(Fecha, Promedio) %>% rename(informal = Promedio), 
        ccl() %>% select(Fecha, Promedio) %>% rename(ccl = Promedio), by = "Fecha", all = TRUE), 
      mep() %>% select(Fecha, Promedio) %>% rename(mep = Promedio), by = "Fecha", all = TRUE), 
    oficial() %>% select(Fecha, Promedio) %>% rename(oficial = Promedio), by = "Fecha", all = TRUE))
  
  
  
  dolarmepcierre = reactive(mep()$Promedio[nrow(mep())])
  dolaroficialcierre = reactive(oficial()$Promedio[nrow(oficial())])
  dolarbluecierre = reactive(informal()$Promedio[nrow(informal())])
  dolarcclcierre = reactive(ccl()$Promedio[nrow(ccl())])
  
  
  mepvsoficial = reactive(
    merge(
    na.omit(mep() %>% mutate(prommep = Promedio) %>% select(Fecha,prommep)), 
    na.omit(oficial() %>% mutate(promoficial = Promedio) %>% select(Fecha,promoficial)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((prommep-promoficial) / promoficial,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  )
  
  bluevsoficial = reactive(
    merge(
    na.omit(informal() %>% mutate(promblue = Promedio) %>% select(Fecha,promblue)), 
    na.omit(oficial() %>% mutate(promoficial = Promedio) %>% select(Fecha,promoficial)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((promblue-promoficial) / promoficial,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  )
  
  bluevsmep = reactive(
    merge(
    na.omit(informal() %>% mutate(promblue = Promedio) %>% select(Fecha,promblue)), 
    na.omit(mep() %>% mutate(prommep = Promedio) %>% select(Fecha,prommep)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((promblue-prommep) / prommep,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  )
  
  cclvsoficial = reactive(
    merge(
    na.omit(ccl() %>% mutate(promccl = Promedio) %>% select(Fecha,promccl)), 
    na.omit(oficial() %>% mutate(promoficial = Promedio) %>% select(Fecha,promoficial)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((promccl-promoficial) / promoficial,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  )
  
  
  
  info_mepvsoficial = reactive(get_icon_arrow(mepvsoficial()))
  info_bluevsoficial = reactive(get_icon_arrow(bluevsoficial()))
  info_bluevsmep = reactive(get_icon_arrow(bluevsmep()))
  info_cclvsoficial = reactive(get_icon_arrow(cclvsoficial()))
  
  
  dolar_merge = reactive(get_df_fill(
    dolar(), from_historic)
  )
  
  df_acumulado_anual = reactive(acumulado_df(dolar_merge()))
  
  
  
  
  output$table_blue_output <- reactable::renderReactable({
    get_table(informal())
  })
  output$table_mep_output <- reactable::renderReactable({
    get_table(mep())
  })
  output$table_oficial_output <- reactable::renderReactable({
    get_table(oficial())
  })
  output$table_ccl_output <- reactable::renderReactable({
    get_table(ccl())
  })
  
  
  output$barplot <- renderHighchart({
    df_with_colors <- df_acumulado_anual() %>%
      mutate(colores = ifelse(tc == "mep", MEP_COLOR,
                              ifelse(tc == "ccl", CCL_COLOR,
                                     ifelse(tc == "informal", INFORMAL_COLOR,
                                            ifelse(tc == "oficial", OFICIAL_COLOR, NA))))) %>% 
      arrange(desc(tasa))
    
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = df_with_colors$tc) %>%
      hc_add_series(data = df_with_colors$tasa, name = "Precio", colorByPoint = TRUE, colors = df_with_colors$colores) %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Porcentaje Acumulado en el Año: % {point.y}",valueDecimals=2) %>%
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
        text = paste0("Variacion % acumulado en " , as.character(year(to))),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
  })
  
  
  
  output$varplot <- renderHighchart({
    
    if (input$tc == "Informal"){
      df = informal()
    } else if (input$tc == "Mep") {
      df = mep()
    } else if (input$tc == "Ccl") {
      df = ccl()
    } else {
      df = informal()
    }
    
    highcharter::hchart(
      df %>% 
        filter(Fecha >= from, 
               Fecha <= to) %>% 
        select(Fecha, variacion), 
      type = "line",  
      highcharter::hcaes(x = "Fecha", y = "variacion"),
      color = "#007bff",
      name = "ver", 
      id = "trend",
      showInLegend = TRUE) %>%
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Variacion: {point.y}%",valueDecimals=2) %>%
      highcharter::hc_plotOptions(
        line = list(
          color = "blue",
          marker = list(
            fillColor = "white",
            lineWidth = 2,
            radius=1,
            lineColor = NULL
          )
        )
      ) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_xAxis(
        title = list(text = ""),
        gridLineWidth = 0,
        dateTimeLabelFormats = list(day = '%Y'),
        type = "datetime",
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
        text = paste0("Variacion del Dolar ", input$tc),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Dolar ',input$tc,' Variacion Argentina'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
  })
  
  
  
  
  
  output$timeserie <- renderHighchart({
    
    if (input$opciones == "Anual"){
      dolar_merge_filter = dolar_merge() %>% 
        filter(Fecha >= input$daterange[1], Fecha <= input$daterange[2])
    } else {
      dolar_merge_filter = dolar_merge() %>% arrange(Fecha)
      dolar_merge_filter = tail(dolar_merge_filter, 7)
    }
    
    
    
    highchart() %>%
      hc_xAxis(categories = dolar_merge_filter$Fecha) %>%
      hc_add_series(dolar_merge_filter$ccl, 
                    name = "CCL", 
                    type = "line",
                    color = CCL_COLOR,
                    tooltip = list(valueDecimals = 2)) %>%
      hc_add_series(dolar_merge_filter$mep, 
                    name = "Mep",
                    type = "line",
                    color = MEP_COLOR,
                    tooltip = list(valueDecimals = 2)) %>%
      hc_add_series(dolar_merge_filter$informal,
                    name = 'Informal', 
                    type = "line",
                    color = INFORMAL_COLOR,
                    tooltip = list(valueDecimals = 2)) %>%
      hc_add_series(dolar_merge_filter$oficial, 
                    name = "Oficial",
                    type = "line",
                    color = OFICIAL_COLOR,
                    tooltip = list(valueDecimals = 2)) %>%
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
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black"))
    
  })
  
  
  
  
  output$barplot_mepvsoficial <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Brecha Dolar Mep vs Oficial") %>%
      hc_xAxis(categories = mepvsoficial()$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = mepvsoficial()$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}",valueDecimals=2) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_xAxis(
        title = list(text = ""),
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "% Brecha",
                     style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0,
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      
      highcharter::hc_caption(
        text = paste0("Brecha Mep vs Oficial."),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Brecha Cambiaria.'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
  })
  
  
  
  
  output$barplot_bluevsoficial <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Brecha Dolar Blue vs Oficial") %>%
      hc_xAxis(categories = bluevsoficial()$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = bluevsoficial()$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}",valueDecimals=2) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_xAxis(
        title = list(text = ""),
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "% Brecha",
                     style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0,
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      highcharter::hc_caption(
        text = paste0("Brecha Blue vs Oficial."),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Brecha Cambiaria.'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
  })
  
  
  
  
  
  
  output$barplot_bluevsmep <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Brecha Dolar Blue vs Mep") %>%
      hc_xAxis(categories = bluevsmep()$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = bluevsmep()$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}",valueDecimals=2) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_xAxis(
        title = list(text = ""),
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "% Brecha",
                     style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0,
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      
      highcharter::hc_caption(
        text = paste0("Brecha Blue vs Mep"),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Brecha Cambiaria.'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 

  })
  
  
  
  
  output$barplot_cclvsoficial <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Brecha Dolar CCL vs Oficial") %>%
      hc_xAxis(categories = cclvsoficial()$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = cclvsoficial()$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}",valueDecimals=2) %>%
      highcharter::hc_legend(enabled = FALSE) %>%
      highcharter::hc_xAxis(
        title = list(text = ""),
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      highcharter::hc_yAxis(
        title = list(text = "% Brecha",
                     style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0,
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      
      highcharter::hc_caption(
        text = paste0("Brecha CCL vs Oficial"),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Brecha Cambiaria.'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
    
  })
  
  
  
  
  
  output$valuebox_1 <- renderbs4ValueBox({
    bs4ValueBox(
      value = "",
      subtitle = HTML(
        "Blue: $", as.character(dolarbluecierre()),"<br>",
        "Mep: $", as.character(dolarmepcierre()),"<br>",
        "Ccl: $", as.character(dolarcclcierre()),"<br>",
        "Oficial: $", as.character(dolaroficialcierre())),
      icon = icon("dollar-sign"),
      color = "teal",
      width = 3,
      footer = div(paste0("Precios al ", to , " ", ultima_hora()))
    )
  })
  
  output$valuebox_3 <- renderbs4ValueBox({
    bs4ValueBox(
      value = "",
      subtitle = HTML(
        "Blue: %", as.character(round(((informal()$Promedio[informal()$Fecha == max(informal()$Fecha)]-informal()$Promedio[informal()$Fecha == min(informal()$Fecha)])/informal()$Promedio[informal()$Fecha == min(informal()$Fecha)])*100,2)),"<br>",
        "Mep: %", as.character(round(((mep()$Promedio[mep()$Fecha == max(mep()$Fecha)]-mep()$Promedio[mep()$Fecha == min(mep()$Fecha)])/mep()$Promedio[mep()$Fecha == min(mep()$Fecha)])*100,2)),"<br>",
        "Ccl: %", as.character(round(((ccl()$Promedio[ccl()$Fecha == max(ccl()$Fecha)]-ccl()$Promedio[ccl()$Fecha == min(ccl()$Fecha)])/ccl()$Promedio[ccl()$Fecha == min(ccl()$Fecha)])*100,2)),"<br>",
        "Oficial: %", as.character(round(((oficial()$Promedio[oficial()$Fecha == max(oficial()$Fecha)]-oficial()$Promedio[oficial()$Fecha == min(oficial()$Fecha)])/oficial()$Promedio[oficial()$Fecha == min(oficial()$Fecha)])*100,2))),
      icon = icon("info"),
      color = "teal",
      width = 3,
      footer = div(
        paste0(
          "De: ", from,
          " a: ", to
        )  
      )
    )
    
  })
  
  
  output$valuebox_2 <- renderbs4ValueBox({
    
    bs4ValueBox(
      value = "",
      subtitle = HTML(
        "Blue: ", as.character(informal_day()$variacion)," %<br>",
        "Mep: ", as.character(mep_day()$variacion)," %<br>",
        "Ccl: ", as.character(ccl_day()$variacion)," %<br>",
        "Oficial: ", as.character(oficial_day()$variacion)," %"),
      icon = icon("coins"),
      color = "teal",
      width = 3,
      footer = div(paste0("Variacion % respecto al dia anterior."))
    )
  })
  
  
  
  output$valuebox_brechas_1 <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(
        as.character(
          bluevsmep()$brecha[nrow(bluevsmep())]
        ), " %"),
      subtitle = paste0("Brecha Blue vs Mep"),
      icon = icon(info_bluevsmep()[2]),
      color = info_bluevsmep()[1],
      width = 3,
      footer = div(
        paste0(
          max(bluevsmep()$Fecha)
        )  
      )
    )
  })
  
  output$valuebox_brechas_2 <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(
        as.character(
          mepvsoficial()$brecha[nrow(mepvsoficial())]
        ), " %"),
      subtitle = paste0("Brecha Mep vs Oficial"),
      icon = icon(info_mepvsoficial()[2]),
      color = info_mepvsoficial()[1],
      width = 3,
      footer = div(
        paste0(
          max(mepvsoficial()$Fecha)
        )  
      )
    )
  })
  
  output$valuebox_brechas_3 <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(
        as.character(
          bluevsoficial()$brecha[nrow(bluevsoficial())]
        ), " %"),
      subtitle = paste0("Brecha Blue vs Oficial"),
      icon = icon(info_bluevsoficial()[2]),
      color = info_bluevsoficial()[1],
      width = 3,
      footer = div(
        paste0(
          max(bluevsoficial()$Fecha)
        )  
      )
    )
  })
  

  
}

shinyApp(ui = ui, server = server)
