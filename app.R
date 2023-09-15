
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



today = format(
  with_tz(
    Sys.time() ,
    tzone = "America/Argentina/Buenos_Aires"), 
  format = "%Y-%m-%d"
)



edit_dolar_historico = function(df){
  return(
    df %>% mutate(
      Fecha = as.Date(Fecha, format = "%d/%m/%Y"),
      Compra = round(as.numeric(Compra),2),
      Venta = round(as.numeric(Venta),2),
      Promedio = round(as.numeric(Promedio),2),
      variacion = round(as.numeric(variacion),2)
    ))
}


get_date_interanual = function(df){
  fecha_max = max(df$Fecha)
  fecha_before = as.Date(fecha_max) - years(1)
  while (TRUE) {
    dolar_before = df$Promedio[df$Fecha == fecha_before]
    if (length(dolar_before) > 0) {
      break  
    } else {
      fecha_before = fecha_before - 1
    }
  }
  return(as.Date(fecha_before))
}

df = edit_dolar_historico(read_excel('data/dolar.xlsx') %>% 
                            select(Fecha,Compra,Venta,Promedio,variacion)) %>% arrange(Fecha)
max_date = max(df$Fecha)
min_history_date = min(df$Fecha)
min_date = get_date_interanual(df)



get_dolar_interanual = function(df){
  fecha_max = max(df$Fecha)
  fecha_before = as.Date(fecha_max) - years(1)
  while (TRUE) {
    dolar_before = df$Promedio[df$Fecha == fecha_before]
    if (length(dolar_before) > 0) {
      break  
    } else {
      fecha_before = fecha_before - 1
    }
  }
  dolar_actual = df$Promedio[nrow(df)]
  interanual_rate = round(((dolar_actual-dolar_before)/dolar_before)*100,2)
  return(interanual_rate)
}


get_dolar_interanual_input = function(df, date_min, date_max){
  fecha_max = as.Date(date_max)
  fecha_before = as.Date(date_min)
  while (TRUE) {
    dolar_before = df$Promedio[df$Fecha == fecha_before]
    if (length(dolar_before) > 0) {
      break  
    } else {
      fecha_before = fecha_before - 1
    }
  }
  while (TRUE) {
    dolar_actual = df$Promedio[df$Fecha == fecha_max]
    if (length(dolar_actual) > 0) {
      break  
    } else {
      fecha_max = fecha_max - 1
    }
  }
  interanual_rate = round(((dolar_actual-dolar_before)/dolar_before)*100,2)
  return(interanual_rate)
}



get_dolar_var = function(df, dias=1){
  return(
    round((df$Promedio[df$Fecha==max(df$Fecha)] - 
             df$Promedio[nrow(df) - dias]) /
            df$Promedio[nrow(df) - dias],4)*100
  )
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
          minWidth = 120
        ),
        Compra = colDef(
          name = "Compra",
          align = "center",
          minWidth = 60
        ),
        Venta = colDef(
          name = "Venta",
          align = "left",
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




get_date = function(df, date){
  fecha = as.Date(date)
  while (TRUE) {
    dolar = df$Promedio[df$Fecha == fecha]
    if (length(dolar) > 0) {
      break  
    } else {
      fecha = fecha - 1
    }
  }
  return(as.Date(fecha))
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
          span("Argentina", style = "color: green;"), paste0(". Datos actualizados hasta: ",today)),
        fluidRow(
          bs4ValueBoxOutput("valuebox_1",width = 4),
          bs4ValueBoxOutput("valuebox_2",width = 4),
          bs4ValueBoxOutput("valuebox_3",width = 4)
        ),
        fluidRow(
          column(
            width = 4,
            box(
              title = "Inputs",
              status = "teal",
              icon = icon("keyboard"),
              solidHeader = TRUE,
              width = 12,
              tabPanel(
                title = "Time Serie",

                dateRangeInput(
                  "daterange3", 
                  label = HTML(
                    "Rango de Fechas: <br><br>",
                    "Seleccionar un rango de fechas para ver la variacion del Tipo de Cambio Blue, la Serie Temporal del precio y de la variacion respecto al dia anterior."),
                  start  = min_date,
                  end    = max_date,
                  min    = min_history_date,
                  max    = max_date,
                  format = "dd/mm/yyyy",
                  separator = " - ",
                  language = "es"
                )
              )
            )
          ),
          column(
            width = 8,
            box(
              title = "Serie Temporal",
              width = 12,
              status = "teal",
              icon = icon("chart-line"),
              solidHeader = TRUE,
              tabPanel(
                title = "Var",
                withSpinner(
                  highchartOutput("timeserie"),
                  type = 1
                )
              )
            )
          ),
          column(
            width = 12,
            box(
              title = "Variacion Dolar Blue",
              width = 12,
              status = "teal",
              icon = icon("chart-line"),
              solidHeader = TRUE,
              tabPanel(
                title = "Variacion Dolar Blue",
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
          bs4ValueBoxOutput("valuebox_brechas_3",width = 4)
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
                        )
                 )
          )
        )
      ),
      tabItem(
        tabName = "datos",
        p("Datos de los 3 Tipos de Cambio."),
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
  
  
  
  dolar = edit_dolar_historico(
    read_excel('data/dolar.xlsx') %>% 
      arrange(Fecha) %>% 
      select(Fecha,Compra,Venta,Promedio,variacion))
  
  dolarmep= edit_dolar_historico(
    read_excel('data/dolarmep.xlsx') %>% 
      arrange(Fecha) %>% 
      select(Fecha,Compra,Venta,Promedio,variacion))
  
  dolaroficial=edit_dolar_historico(
    read_excel('data/dolaroficial.xlsx') %>% 
      arrange(Fecha) %>% 
      select(Fecha,Compra,Venta,Promedio,variacion))
  
  dolarmepcierre = dolarmep$Promedio[nrow(dolarmep)]
  dolaroficialcierre = dolaroficial$Promedio[nrow(dolaroficial)]
  dolarbluecierre = dolar$Promedio[nrow(dolar)]
  
  
  mepvsoficial = merge(
    na.omit(dolarmep %>% mutate(prommep = Promedio) %>% select(Fecha,prommep)), 
    na.omit(dolaroficial %>% mutate(promoficial = Promedio) %>% select(Fecha,promoficial)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((prommep-promoficial) / promoficial,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  
  bluevsoficial = merge(
    na.omit(dolar %>% mutate(promblue = Promedio) %>% select(Fecha,promblue)), 
    na.omit(dolaroficial %>% mutate(promoficial = Promedio) %>% select(Fecha,promoficial)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((promblue-promoficial) / promoficial,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  
  bluevsmep = merge(
    na.omit(dolar %>% mutate(promblue = Promedio) %>% select(Fecha,promblue)), 
    na.omit(dolarmep %>% mutate(prommep = Promedio) %>% select(Fecha,prommep)), 
    by = "Fecha", 
    all = FALSE
  ) %>% mutate(
    brecha = round((promblue-prommep) / prommep,2)*100
  ) %>% select(Fecha, brecha) %>% arrange(Fecha)
  
  info_mepvsoficial = get_icon_arrow(mepvsoficial)
  info_bluevsoficial = get_icon_arrow(bluevsoficial)
  info_bluevsmep = get_icon_arrow(bluevsmep)
  
  
  rv <- reactiveValues()
  
  
  observeEvent(input$variaciondias, {
    
    rv$dias_variacion = as.numeric(input$variaciondias)
    rv$var_dolar_blue = get_dolar_var(dolar)
    rv$var_dolar_oficial = get_dolar_var(dolaroficial)
    rv$var_dolar_mep = get_dolar_var(dolarmep)
    
  }, ignoreNULL = FALSE)
  
  output$table_blue_output <- reactable::renderReactable({
    get_table(dolar)
    
  })
  output$table_mep_output <- reactable::renderReactable({
    get_table(dolarmep)
    
  })
  output$table_oficial_output <- reactable::renderReactable({
    get_table(dolaroficial)
    
  })
  
  
  
  output$varplot <- renderHighchart({
    
    date_from = get_date(dolar,as.Date(input$daterange3[1]))
    date_to = get_date(dolar,as.Date(input$daterange3[2]))
    
    highcharter::hchart(
      dolar %>% 
        filter(Fecha >= date_from, 
               Fecha <= date_to) %>% 
        select(Fecha, variacion), 
      type = "line",  
      highcharter::hcaes(x = "Fecha", y = "variacion"),
      color = "#007bff",
      name = "ver", 
      id = "trend",
      showInLegend = TRUE) %>%
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Variacion: {point.y}%") %>%
      
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
        text = paste0("Variacion del Dolar Blue"),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Dolar Blue Variacion Argentina'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
  })
  
  
  
  
  output$timeserie <- renderHighchart({
    
    highcharter::hchart(
      dolar %>% 
        filter(Fecha >= as.Date(input$daterange3[1]), Fecha <= as.Date(input$daterange3[2])) %>% 
        select(Fecha, Promedio), 
      type = "line",  
      highcharter::hcaes(x = "Fecha", y = "Promedio"),
      color = "#007bff",
      name = "ver", 
      id = "trend",
      showInLegend = TRUE) %>%
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Dolar Blue: ${point.y}") %>%
      
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
        title = list(text = "Precio",
                     style = list(color = "black", fontWeight = "bold")),
        gridLineWidth = 0,
        reversed = FALSE,
        labels = list(
          style = list(color = "black", fontWeight = "bold")
        )
      ) %>%
      
      highcharter::hc_caption(
        text = paste0("Serie Temporal del Dolar Paralelo."),
        style = list(fontSize = '12px', fontWeight = 'bold', color = "black")) %>%
      highcharter::hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        shared = TRUE, 
        borderWidth = 5
      ) %>% 
      hc_title(
        text = paste0('Dolar Paralelo Argentina'),
        style = list(fontSize = '16px', fontWeight = 'bold', color = "black")) 
  })
  
  
  output$barplot_mepvsoficial <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Brecha Dolar Mep vs Oficial") %>%
      hc_xAxis(categories = mepvsoficial$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = mepvsoficial$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}") %>%
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
      hc_xAxis(categories = bluevsoficial$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = bluevsoficial$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}") %>%
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
      hc_xAxis(categories = bluevsmep$Fecha) %>%
      hc_yAxis(title = list(text = "Brecha")) %>%
      hc_add_series(name = "brecha", 
                    data = bluevsmep$brecha,
                    color = "#007bff") %>% 
      highcharter::hc_tooltip(crosshairs = TRUE, pointFormat = "Brecha: % {point.y}") %>%
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
  
  
  
  
  
  
  output$valuebox_1 <- renderbs4ValueBox({
    
    bs4ValueBox(
      value = "",
      subtitle = HTML(
        "Blue: $", as.character(dolarbluecierre),"<br>",
        "Mep: $", as.character(dolarmepcierre),"<br>",
        "Oficial: $", as.character(dolaroficialcierre)),
      icon = icon("dollar-sign"),
      color = "teal",
      width = 3,
      footer = div(paste0("Precios al ",max_date))
    )
  })
  
  output$valuebox_3 <- renderbs4ValueBox({
    
    bs4ValueBox(
      value = paste0(
        as.character(
          get_dolar_interanual_input(
            dolar, input$daterange3[1], input$daterange3[2]
          )
        ), " %"),
      subtitle = paste0("Variacion % Dolar Blue"),
      icon = icon("info"),
      color = "teal",
      width = 3,
      footer = div(
        paste0(
          "De: ", input$daterange3[1],
          " a: ",input$daterange3[2]
        )  
      )
    )
    
  })
  
  
  output$valuebox_2 <- renderbs4ValueBox({
    
    bs4ValueBox(
      value = "",
      subtitle = HTML(
        "Blue: ", as.character(rv$var_dolar_blue)," %<br>",
        "Mep: ", as.character(rv$var_dolar_mep)," %<br>",
        "Oficial: ", as.character(rv$var_dolar_oficial)," %"),
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
          bluevsmep$brecha[nrow(bluevsmep)]
        ), " %"),
      subtitle = paste0("Brecha Blue vs Mep"),
      icon = icon(info_bluevsmep[2]),
      color = info_bluevsmep[1],
      width = 3,
      footer = div(
        paste0(
          max(bluevsmep$Fecha)
        )  
      )
    )
  })
  
  output$valuebox_brechas_2 <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(
        as.character(
          mepvsoficial$brecha[nrow(mepvsoficial)]
        ), " %"),
      subtitle = paste0("Brecha Mep vs Oficial"),
      icon = icon(info_mepvsoficial[2]),
      color = info_mepvsoficial[1],
      width = 3,
      footer = div(
        paste0(
          max(mepvsoficial$Fecha)
        )  
      )
    )
  })
  
  output$valuebox_brechas_3 <- renderbs4ValueBox({
    bs4ValueBox(
      value = paste0(
        as.character(
          bluevsoficial$brecha[nrow(bluevsoficial)]
        ), " %"),
      subtitle = paste0("Brecha Blue vs Oficial"),
      icon = icon(info_bluevsoficial[2]),
      color = info_bluevsoficial[1],
      width = 3,
      footer = div(
        paste0(
          max(bluevsoficial$Fecha)
        )  
      )
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
