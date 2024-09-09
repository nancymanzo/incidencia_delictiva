library(googledrive)
library(googlesheets4)
library(shiny)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(RColorBrewer) 
library(janitor)
library(mxmaps)
library(stringr)
library(wordcloud2)
library(RColorBrewer)
library(shinydashboard)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)
library(extrafont)
library(showtext)
library(jsonlite)
library(data.table)
library(shinyjs)
library(leaflet)
library(mxmaps)
library(openxlsx)
library(shinyWidgets)
library(gt)
library(gtExtras)


# setwd("~/Justicia/SESNSP/incidencia_delictiva")

# Total federacion-------------------------------------------------------------
sesnp_federacion<-read.csv("IDEFF_mar24.csv",check.names = T, encoding = "latin1") %>% 
  filter(AÑO >= 2015 & AÑO <= 2024) %>% 
  clean_names()

sesnp_federacion$concepto <- str_to_sentence(sesnp_federacion$concepto)
sesnp_federacion$entidad <- str_to_sentence(sesnp_federacion$entidad)


sesnp_federacion %>% 
  gather(mes, carpetas, enero:diciembre) %>% 
  group_by(ano) %>% 
  summarise(total=sum(carpetas, na.rm = T))  ->sesnp_federacion_nacional

entidad<- c("Federación")
cbind(entidad, sesnp_federacion_nacional)->sesnp_federacion_nacional
names(sesnp_federacion_nacional)[names(sesnp_federacion_nacional) == "...1"] <- "entidad"


sesnp_federacion %>% 
  gather(mes, carpetas, enero:diciembre) %>% 
  group_by(entidad,ano) %>% 
  summarise(total=sum(carpetas, na.rm = T)) %>% 
  rbind(sesnp_federacion_nacional)->sesnp_federacion_nacional



# Federación ------------------------------------------------------------------

paleta<- c("#543553","#8f5478", "#3b5063", "#6e273b", "#8056ba")

fed_concepto_tipo<-read.csv("IDEFF_mar24.csv",check.names = T, encoding = "latin1") %>% 
  filter(AÑO >= 2015 & AÑO <= 2024) %>% 
  clean_names()

fed_concepto_tipo$concepto <- str_to_sentence(fed_concepto_tipo$concepto)
fed_concepto_tipo$entidad <- str_to_sentence(fed_concepto_tipo$entidad)


fed_concepto_tipo %>% 
  filter(ano %in% c(2022,2023)) %>% 
  gather(mes, carpetas, enero:diciembre) %>% 
  group_by(ano, concepto, tipo) %>% 
  summarise(total=sum(carpetas, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "ano",
              values_from = "total") %>% 
  mutate(variacion_porcentual = round(((`2023`- `2022`)/ `2022`) * 100, digits=2))->fed_concepto_tipo_nacional

entidad<- c("Federación")
cbind(entidad, fed_concepto_tipo_nacional)->fed_concepto_tipo_nacional
names(fed_concepto_tipo_nacional)[names(fed_concepto_tipo_nacional) == "...1"] <- "entidad"


fed_concepto_tipo %>% 
  filter(ano %in% c(2022,2023)) %>% 
  gather(mes, carpetas, enero:diciembre) %>% 
  group_by(entidad,ano, concepto, tipo) %>% 
  summarise(total=sum(carpetas, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "ano",
              values_from = "total") %>% 
  mutate(variacion_porcentual = round(((`2023`- `2022`)/ `2022`) * 100, digits=2)) %>% 
  rbind(fed_concepto_tipo_nacional)->fed_concepto_tipo


fed_concepto_tipo <- fed_concepto_tipo %>%
  mutate(concepto = factor(concepto, levels = c("Ley federal contra la delincuencia organizada (l.f.c.d.o.)", "Otros delitos", "Otras leyes y codigos", "Ley general de salud (l.g.s.)", "Contra la salud"))) %>%
  arrange(concepto, desc(variacion_porcentual))

fed_concepto_tipo$tipo <- factor(fed_concepto_tipo$tipo, levels = unique(fed_concepto_tipo$tipo))
fed_concepto_tipo$tipo <- fct_rev(fed_concepto_tipo$tipo)


# Tasa fata frame---------------------------------------------------------------
df_tasa <- read_excel("df_tasa.xlsx") %>% 
  select(Entidad, Año, Subtipo.de.delito, tasa)


#Data--------------------------------------------------------------------------------------------------
secretariado_df<-read.csv("IDEFC_NM_ene24.csv",check.names = T, encoding = "latin1")

#Subdata: concetrado nacional --------------------------------------------------
secretariado_df %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Año, Subtipo.de.delito) %>% 
  summarise(value=sum(Carpetas, na.rm = T)) ->nacional_secretariado

Entidad<- c("Nacional")
cbind(Entidad, nacional_secretariado)->nacional_secretariado
names(nacional_secretariado)[names(nacional_secretariado) == "...1"] <- "Entidad"

secretariado<-secretariado_df %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Entidad, Año, Subtipo.de.delito) %>% 
  summarise(value=sum(Carpetas, na.rm = T)) %>% 
  rbind(nacional_secretariado)

secretariado_bien_nacional<-secretariado_df %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Año, Bien.jurídico.afectado, Subtipo.de.delito) %>% 
  summarise(value=sum(Carpetas, na.rm = T)) %>%
  mutate(porcentaje =(value/sum(value)))

Entidad<- c("Nacional")
cbind(Entidad, secretariado_bien_nacional)->secretariado_bien_nacional
names(secretariado_bien_nacional)[names(secretariado_bien_nacional) == "...1"] <- "Entidad"

secretariado_bien<-secretariado_df %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Entidad, Año, Bien.jurídico.afectado, Subtipo.de.delito) %>% 
  summarise(value=sum(Carpetas, na.rm = T)) %>%
  mutate(porcentaje =(value/sum(value))) %>% 
  rbind(secretariado_bien_nacional)
  # arrange(-value)


# THEME ESTILO -----------------------------------------------------------------
theme_1<-theme_minimal()+
  theme(text=element_text(family = gt::google_font("Montserrat")),
        plot.title = element_text(family = gt::google_font("Montserrat"),
                                  # face = "bold",
                                  size = 18, #vjust = -2,
                                  hjust = 0),
        plot.subtitle = element_text(family = gt::google_font("Montserrat"),
                                     size = 13,
                                     hjust = 0,
                                     colour = "grey40"),
        plot.caption = element_text(family = gt::google_font("Montserrat"),
                                    size = 10,
                                    colour = "grey40"),
        axis.text.x = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = 12,
                                   colour = "black"),
        axis.text.y = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",   
                                   size = 12,
                                   colour = "black"),
        legend.title = element_text(family = gt::google_font("Montserrat"),
                                    face = "bold",
                                    size = 13,
                                    colour = "black",
                                    hjust = 1),
        legend.title.align = 0.5,
        legend.text = element_text(family = gt::google_font("Montserrat"),
                                   # face = "bold",
                                   size = "8px",
                                   colour = "black",
                                   hjust = 1),
        legend.text.align = 0.5,
        legend.key.size = unit(25, "pt")) ->estilo_theme
data("df_mxstate_2020")
merge(secretariado, df_mxstate_2020, by.x = "Entidad", by.y="state_name_official")->mx_nominal
merge(df_tasa, df_mxstate_2020, by.x = "Entidad", by.y="state_name_official")->mx_tasa
mx_tasa$value<-mx_tasa$tasa


#UI ------------------------------------------------------------------------
ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(
      class = 'p-0',
      tags$head(
        tags$style(HTML("
        .p-0 {
       padding: 0px!important;
      }
      .small-box h3 {
    font-size: 38px;
    font-weight: 700;
    margin: 0 0 10px 0;
    white-space: normal!important;
    padding: 0;
    }
    @media (min-width: 768px) {
  .d-flex {
    display: flex;
  }
    }
    .small-box{
    border-radius: 2px;
    position: relative;
    display: block;
    margin-bottom: 20px;
    box-shadow: 0 1px 1px rgb(0 0 0 / 10%);
    height: calc(100% - 20px);
    }
    # .html-widget{min-width: 300px;
    # }
    .mb-2{ 
    margin-bottom:20px;
    }
    .p-2{ 
    padding: 20px;     
    }x|
    #table_muertes{overflow: scroll; 
    }   
    
  .small-box.bg-fuchsia {
   background-color: #b06497 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-purple {
   background-color: #B14C71 !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;

   }

   .small-box.bg-maroon {
     background-color: #8F5199 !important; 
   color: white !important;       
   font-family: Nutmeg-Light !important;

   }
   .small-box.bg-light-blue {
   background-color: #5d3d6c !important; 
   color: white !important; 
   font-family: Nutmeg-Light !important;
   }
    
                        ")),
        tags$script(HTML("window.addEventListener('message', event => {
    // IMPORTANT: check the origin of the data!
    console.log('recibi un mensaje', event);
    if (event.origin.includes('https://igualdad.jalisco.gob.mx')) {
        // The data was sent from your site.
        // Data sent with postMessage is stored in event.data:

        let height = 0;
        if (document.body) {
            if (document.body.scrollHeight) {
                height= document.body.scrollHeight;
            }
        }

        event.source.postMessage(height, event.origin);
    } 

    return;
});")),
        tags$script('
           var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
      add_busy_spinner(onstart = F, spin = "fading-circle", color = "#E34F70"),
      navbarPage(header=busy_start_up(
        loader = spin_epic("flower", color = "#8F5199"),
        text = "Cargando",
        timeout = 1500,
        color = "#8F5199",
        background = " white"),
        useShinydashboard(),
    # - - - - - - - - -- - - - - - - - - - - - - - - - - -
    tabPanel(title = "Delitos anual", class="p-2",
             sidebarPanel(width = 3, "Seleccione algunas características \n", class=".mb-2",
                          selectInput(
                            inputId = "incidencia_año",
                            label = "Año",
                            choices = unique(sort(secretariado$Año)),
                            multiple = TRUE,
                            selected = c(2015:2023)
                           ),                                                               
                           selectInput(
                             inputId = "incidencia_entidad",
                             label = "Entidad",
                             choices = unique(sort(secretariado$Entidad)),
                             multiple = F,
                             selected = "Nacional"
                             ),
                           selectInput(
                             inputId = "incidencia_delito",
                             label = "Tipo de delito",
                             choices = unique(sort(secretariado$Subtipo.de.delito)),
                             multiple = F,
                             selected = "Homicidio doloso"
                           ),
                          tags$head(tags$style(".btn { horizontal-align: middle; 
                                    height: 30px; width: 90%;font-size: 10px;
                                    color: black;background-color:#bdbdbd;}")),
                          downloadButton(
                            outputId = "download_grafico_anual",
                            label = "Gráfico barras"),
                          downloadButton(
                            outputId = "download_grafico_anual_lineas",
                            label = "Gráfico líneas"),
                          # downloadButton("downloadData_medidas", "Descarga (.csv)")
                          ),
             mainPanel(width=9,
                       plotOutput("grafico_anual"), br(), hr(),
                       # splitLayout(cellWidths = c("40%", "60%"),
                                   dataTableOutput("tabla_anual"),
                                   plotOutput("grafico_anual_lineas"))),
             # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
             tabPanel(title = "Tasa por cada 100 mil habitantes", class="p-2",
                      sidebarPanel(width = 3, "Seleccione algunas características \n", class=".mb-2",
                                   selectInput(
                                     inputId = "incidencia_año_tasa",
                                     label = "Año",
                                     choices = unique(sort(df_tasa$Año)),
                                     multiple = TRUE,
                                     selected = c(2015:2023)
                                   ),                                                               
                                   selectInput(
                                     inputId = "incidencia_entidad_tasa",
                                     label = "Entidad",
                                     choices = unique(sort(df_tasa$Entidad)),
                                     multiple = F,
                                     selected = "Aguascalientes"
                                   ),
                                   selectInput(
                                     inputId = "incidencia_delito_tasa",
                                     label = "Tipo de delito",
                                     choices = unique(sort(df_tasa$Subtipo.de.delito)),
                                     multiple = F,
                                     selected = "Homicidio doloso"
                                   ),
                                   tags$head(tags$style(".btn { horizontal-align: middle; 
                                    height: 30px; width: 90%;font-size: 10px;
                                    color: black;background-color:#bdbdbd;}")),
                                   downloadButton(
                                     outputId = "download_grafico_anual_tasa",
                                     label = "Descargar gráfico 1 (barras)"),
                                   downloadButton(
                                     outputId = "download_grafico_anual_lineas_tasa",
                                     label = "Descargar gráfico 2 (líneas)")),
                      mainPanel(width=9,
                                plotOutput("grafico_anual_tasa"), br(), hr(),
                                # splitLayout(cellWidths = c("40%", "60%"),
                                            dataTableOutput("tabla_anual_tasa"),
                                            plotOutput("grafico_anual_lineas_tasa")#)
                      )),
    
    # - - - - - - - - -- - - - - - - - - - - - - - - - - -
    
    tabPanel("Mapas",
             # h2(),
             tabPanel("Mapa absoluto",
             sidebarPanel(width = 3, "Seleccione algunas características \n", class=".mb-2",
             selectInput(inputId = "incidencia_año_mapa", 
                         label = "Seleccione el año" ,
                         choices = unique(sort(mx_nominal$Año),
                         selected = c(2023),  
                         multiple = FALSE)),
             selectInput(inputId = "incidencia_delito_mapa", 
                         label = "Seleccione el delito" ,
                         choices = unique(sort(mx_nominal$Subtipo.de.delito)),
                         selected = "Homicidio doloso",  multiple = FALSE),
             downloadButton(
               outputId = "download_mapa",
               label = "Descargar mapa (absoluto)")),
             mainPanel(width=9,
                       splitLayout(cellWidths = c("50%", "50%"),
                       plotOutput("mapa"),
                       dataTableOutput("tabla_mapa")
                       ))),
             br(), hr(),
             tags$hr(),  # Línea horizontal entre los gráficos
             column(12,
                    br(), hr(),
                    
             tabPanel("Mapa tasa",
                      sidebarPanel(width = 3, "Seleccione algunas características \n", class=".mb-2",
                                   selectInput(inputId = "incidencia_año_mapa_tasa", 
                                               label = "Seleccione el año" ,
                                               choices = unique(sort(mx_tasa$Año),
                                                                 selected = c(2023),  
                                                                multiple = FALSE)),
                                   selectInput(inputId = "incidencia_delito_mapa_tasa", 
                                               label = "Seleccione el delito" ,
                                               choices = unique(sort(mx_tasa$Subtipo.de.delito)),
                                               selected = "Homicidio doloso",  multiple = FALSE),
                                   downloadButton(
                                     outputId = "download_mapa_tasa",
                                     label = "Descargar mapa (tasa)")),
                      mainPanel(width=9,
                                splitLayout(cellWidths = c("50%", "50%"),
                                            plotOutput("mapa_tasa"),
                                            dataTableOutput("tabla_mapa_tasa")
                                ))))
                      
                      
                      ),
    # - - - - - - - - -- - - - - - - - - - - - - - - - - -
    tabPanel(title = "Bien jurídico", class="p-2",
             sidebarPanel(width = 5, "Seleccione algunas características \n", class=".mb-2",
                          selectInput(
                            inputId = "incidencia_año_bien",
                            label = "Año",
                            choices = unique(sort(secretariado_bien$Año)),
                            multiple = TRUE,
                            selected = c(2023)
                          ),                                                               
                          selectInput(
                            inputId = "incidencia_entidad_bien",
                            label = "Entidad",
                            choices = unique(sort(secretariado_bien$Entidad)),
                            multiple = F,
                            selected = "Nacional"
                          ),
                          selectInput(
                            inputId = "incidencia_bienjuridico_bien", #incidencia_bienjuridico_bien
                            label = "Tipo de bien jurídico",
                            choices = unique(sort(secretariado_bien$Bien.jurídico.afectado)),
                            multiple = T,
                            selected = 
                              c("El patrimonio" , "La familia", "La libertad y la seguridad sexual",  
                                "La sociedad", "La vida y la Integridad corporal", "Libertad personal", 
                                "Otros bienes jurídicos afectados (del fuero común)")
                          ),
                          selectInput(
                            inputId = "incidencia_delito_bien",
                            label = "Tipo de delito",
                            choices = unique(sort(secretariado_bien$Subtipo.de.delito)),
                            multiple = T
                          ),
                          tags$head(tags$style(".btn { horizontal-align: middle;
                                    height: 30px; width: 90%;font-size: 10px;
                                    color: black;background-color:#bdbdbd;}")),
                          downloadButton(
                            outputId = "download_tabla_bien",
                            label = "Descargar tabla"),
                          # downloadButton(
                          #   outputId = "download_grafico_anual_lineas_bien",
                          #   label = "Descargar gráfico 2 (líneas)"),
                          # # downloadButton("downloadData_medidas", "Descarga (.csv)")
             ),
             mainPanel(width=7,
                       gt_output("tabla_bien"),
                       br(), hr())),
    
    # Federación---------------------------------------------------------------
    tabPanel(title = "Federación",
         tabsetPanel(
            tabPanel(title = "Variación anual", class="p-2",
             sidebarPanel(width = 3, "Seleccione algunas características \n", class=".mb-2",
                          selectInput(
                            inputId = "federacion_entidad",
                            label = "Entidad",
                            choices = unique(sort(fed_concepto_tipo$entidad)),
                            multiple = F,
                            selected = "Federación"
                            ),
                          tags$head(tags$style(".btn { horizontal-align: middle;
                                    height: 30px; width: 90%;font-size: 10px;
                                    color: black;background-color:#bdbdbd;}")),
                          downloadButton(
                            outputId = "download_gr_federacion",
                            label = "Descargar gráfico"),
                          # downloadButton(
                          #   outputId = "download_grafico_anual_lineas_bien",
                          #   label = "Descargar gráfico 2 (líneas)"),
                          # # downloadButton("downloadData_medidas", "Descarga (.csv)")
             ),
             mainPanel(width=9,
                       plotOutput("gr_federacion",height = "1000px"),
                       br(), hr())),
         tabPanel(title="Total anual",class="p-2",
                  sidebarPanel(width = 3, "Seleccione algunas características \n", class=".mb-2",
                               selectInput(
                                 inputId = "sesnsp_federacion_año",
                                 label = "Año",
                                 choices = unique(sort(sesnp_federacion_nacional$ano)),
                                 multiple = TRUE,
                                 selected = c(2015:2023)
                               ),                                                               
                               selectInput(
                                 inputId = "sesnsp_federacion_entidad",
                                 label = "Entidad",
                                 choices = unique(sort(sesnp_federacion_nacional$entidad)),
                                 multiple = F,
                                 selected = "Federación"
                               ),
                               # selectInput(
                               #   inputId = "sesnp_federacion_concepto",
                               #   label = "Concepto",
                               #   choices = unique(sort(sesnp_federacion_nacional$concepto)),
                               #   multiple = T
                               # ),
                               tags$head(tags$style(".btn { horizontal-align: middle; 
                                    height: 30px; width: 90%;font-size: 10px;
                                    color: black;background-color:#bdbdbd;}")),
                               downloadButton(
                                 outputId = "download_sesnsp_federacion",
                                 label = "Gráfico barras")
                  ),
                  mainPanel(width=9,
                            plotOutput("grafico_sesnsp_federacion"), br(), hr())))
           
    ))))
  )


# Server ----------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  
  output$sesnsp_federacion_año <- renderUI({
    selectInput("sesnsp_federacion_año",
                label =  "Seleccione el año",
                choices = sort(unique(sesnp_federacion_nacional$ano)),
                multiple = T)
  })
  
  output$sesnsp_federacion_entidad <- renderUI({
    selectInput("sesnsp_federacion_entidad",
                label =  "Seleccione la Entidad",
                choices = sort(unique(sesnp_federacion_nacional$entidad)),
                multiple = T)
  })
  
  # output$sesnp_federacion_concepto <- renderUI({
  #   selectInput("sesnp_federacion_concepto",
  #               label =  "Selecciona el concepto",
  #               choices = sort(unique(sesnp_federacion_nacional$concepto)),
  #               multiple = T)
  # })
  # 
  sesnp_federacion_reactive <- reactive({
    
    sesnp_federacion_nacional %>%
      filter(
        if(!is.null(input$sesnsp_federacion_año))                       ano %in% input$sesnsp_federacion_año        else ano != "",
        if(!is.null(input$sesnsp_federacion_entidad))               entidad %in% input$sesnsp_federacion_entidad    else entidad != "") 
    })
  
  
  output$grafico_sesnsp_federacion <- renderPlot ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    sesnp_federacion_reactive() %>% 
      ggplot() +
      aes(x =as.factor(ano), y = total) +
      geom_col(fill = "#8056ba") +
      geom_label(aes(label=scales::comma(total)), size=4, vjust = 0.5, label.size = .1) +
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="", fill="Tipo", color="Tipo",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de carpetas de investigación (2015 a 2023)}")),
           subtitle = paste0("Datos correspondiente al fuero federal: ", sesnp_federacion_reactive()$entidad[1]),
           caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP.| @mexeva")+
      theme_bw()+
      theme(legend.position = "bottom")+
      estilo_theme->grafico_sesnsp_federacion
    
    grafico_sesnsp_federacion
  })  
  
  
  output$download_sesnsp_federacion <- downloadHandler(
    filename = function() {
      paste0("grafico_federacion_anual_", tolower(sesnp_federacion_reactive()$entidad[1]),".jpeg") # Nombre del archivo de descarga
    },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        sesnp_federacion_reactive() %>% 
          ggplot() +
          aes(x =as.factor(ano), y = total) +
          geom_col(fill = "#8056ba") +
          geom_label(aes(label=total), size=4, vjust = 0.5, label.size = .1) +
          scale_y_continuous(labels = scales::comma) +
          labs(x="", y="", fill="Tipo", color="Tipo",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Total de carpetas de investigación (2015 a 2023)}")),
               subtitle = paste0(sesnp_federacion_reactive()$entidad[1]),
               caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP.| @mexeva")+
          theme_bw()+
          theme(legend.position = "bottom")+
          estilo_theme) 
      dev.off()
    }
  )
  
  
  plotWidth <- reactive({session$clientData[["output_user-muni_graf_width"]]})
  
  plotHeight <- function(){
    width <- plotWidth()
    h <- ifelse(width > 425, width*0.54, width*0.75)
    return(h)}
  
  fontbase <- 8
  
  textFunction <- function(){
    width <- plotWidth()
    textSize <- ifelse(width > 425, fontbase, 0.5*fontbase)
    return(textSize)
  }
  
  #--------------------------------------------------------------------------
  

  output$federacion_entidad <- renderUI({
    selectInput("federacion_entidad",
                label =  "Seleccione la Entidad",
                choices = sort(unique(fed_concepto_tipo$Entidad)),
                multiple = T)
  })
  

  federacion_reactive <- reactive({
    
    fed_concepto_tipo %>%
      filter(
        if(!is.null(input$incidencia_entidad))  entidad %in% input$federacion_entidad else entidad != "") })

  
  output$gr_federacion <- renderPlot ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    federacion_reactive() %>% 
      ggplot(aes(x = tipo, y = variacion_porcentual, fill = concepto)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(variacion_porcentual, 2), "%"), hjust = ifelse(variacion_porcentual < 0, 1.2, -0.2)),
                size = 4, color = "grey19") +  
      labs(
        title = latex2exp::TeX(paste("Gráfico: \\textbf{Variación anual por tipo de delito: ", federacion_reactive()$entidad[1], ".}")),
        # title = expression(paste("\nGráfico: ",bold("Variación porcentual por tipo de delito"))),
        subtitle = "Datos correspondiente al fuero federal, 2023",
        x = "",
        y = "",
        fill = "",
        caption = "Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP. | @mexevalua") +
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +  
      scale_fill_manual(values = paleta) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 65)) +
      coord_flip()+
      theme_minimal()+
      theme_1+
      guides(fill = guide_legend(ncol = 1))+
      theme(legend.position = "bottom",
            plot.title = element_text(family = gt::google_font("Montserrat"),
                                       # face = "bold",
                                       size = 12,
                                       colour = "black"),
            plot.subtitle = element_text(family = gt::google_font("Montserrat"),
                                         size = 10,
                                         hjust = 0,
                                         colour = "grey40"),
            axis.text.y = element_text(family = gt::google_font("Montserrat"),
                                       # face = "bold",
                                       size = 9,
                                       colour = "black"),
            legend.title = element_text(family = gt::google_font("Montserrat"),
                                        face = "bold",
                                        size = 5,
                                        colour = "black",
                                        hjust = 0),
            legend.text.align = 0)->grafico_federacion
    
    grafico_federacion
  })  
  
  
  output$download_gr_federacion <- downloadHandler(
    filename = function() {
      paste0("grafico_federacion_", tolower(federacion_reactive()$entidad[1]),".jpeg") # Nombre del archivo de descarga
    },
    
    content = function(file) {
      jpeg(file, width = 1800, height = 1000) 
      print(
        federacion_reactive() %>% 
          ggplot(aes(x = tipo, y = variacion_porcentual, fill = concepto)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste(round(variacion_porcentual, 2), "%"), hjust = ifelse(variacion_porcentual < 0, 1.2, -0.2)),
                    size = 4, color = "grey19") +  
          labs(
            title = latex2exp::TeX(paste("Gráfico: \\textbf{Variación anual por tipo de delito: ", federacion_reactive()$entidad[1], ".}")),
            # title = expression(paste("\nGráfico: ",bold("Variación porcentual por tipo de delito"))),
            subtitle = "Datos correspondiente al fuero federal, 2023",
            x = "",
            y = "",
            fill = "",
            caption = "Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP. | @mexevalua") +
          scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +  
          scale_fill_manual(values = paleta) +
          scale_x_discrete(labels = function(x) str_wrap(x, width = 60)) +
          coord_flip()+
          theme_minimal()+
          theme_1+
          theme(legend.position = "bottom",
                axis.text.x = element_text(family = gt::google_font("Montserrat"),
                                           # face = "bold",
                                           size = 9,
                                           colour = "black")))
      dev.off()
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
  
  output$incidencia_año <- renderUI({
    selectInput("incidencia_año",
                label =  "Seleccione el año",
                choices = sort(unique(secretariado$Año)),
                multiple = T)
  })
  
  output$incidencia_entidad <- renderUI({
    selectInput("incidencia_entidad",
                label =  "Seleccione la Entidad",
                choices = sort(unique(secretariado$Entidad)),
                multiple = T)
  })

  output$incidencia_delito <- renderUI({
    selectInput("incidencia_delito",
                label =  "Selecciona el delito",
                choices = sort(unique(secretariado$Subtipo.de.delito)),
                multiple = T)
  })
  
  incidencia_reactive <- reactive({
    
    secretariado %>%
      filter(
        if(!is.null(input$incidencia_año))                              Año %in% input$incidencia_año              else Año != "",
        if(!is.null(input$incidencia_entidad))                       Entidad %in% input$incidencia_entidad         else Entidad != "",
        if(!is.null(input$incidencia_delito))              Subtipo.de.delito %in% input$incidencia_delito          else Subtipo.de.delito != "") })
  

  output$grafico_anual <- renderPlot ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive() %>% 
    ggplot() +
      aes(x =as.factor(Año), y = value) +
      geom_col(fill = "#8f5478") +
      geom_label(aes(label=comma(value)), size=4, vjust = 0.5, label.size = .1)+
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{", incidencia_reactive()$Subtipo.de.delito[1],"de 2015 a 2023}")),
           # title=paste0("Gráfico 1. Investigación iniciadas \n por el delito de ", ),
           subtitle = paste0(incidencia_reactive()$Entidad[1]),
           caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP| @mexeva")+
      theme(legend.position = "bottom")+
      estilo_theme->grafico_anual
    
    grafico_anual
  })  
  
  
  output$download_grafico_anual <- downloadHandler(
    filename = function() {
      paste0("grafico_delitos_anual_", tolower(incidencia_reactive()$Subtipo.de.delito[1]),
             "_",tolower(incidencia_reactive()$Entidad[1]),".jpeg") # Nombre del archivo de descarga
    },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        incidencia_reactive() %>% 
          ggplot() +
          aes(x =as.factor(Año), y = value) +
          geom_col(fill = "#8f5478") +
          geom_label(aes(label=comma(value)), size=4, vjust = 0.5, label.size = .1)+
          scale_y_continuous(labels = scales::comma) +
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{", incidencia_reactive()$Subtipo.de.delito[1], "de 2015 a 2023}")),
               
               # title=paste0("Gráfico 1. Investigación iniciadas \n por el delito de ", ),
               subtitle = paste0(incidencia_reactive()$Entidad[1]),
               caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP| @mexeva")+
          theme_bw()+
          theme(legend.position = "bottom")+
          estilo_theme ) 
      dev.off()
    }
  )


  
  output$grafico_anual_lineas <- renderPlot ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive() %>%
      ggplot() +
      aes(x =as.factor(Año), y = value, group=1) +
      geom_line(fill = "#8f5478", colour = "#8f5478", size=1.3) +
      geom_point(fill = "#8f5478", colour = "#8f5478", size=8, alpha=0.6) +
      geom_text(aes(label=comma(value)), size=4, vjust = 0.5, label.size = .1, colour="black")+
      scale_y_continuous(labels = scales::comma) +     
      labs(x="", y="", fill="", color="",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{", incidencia_reactive()$Subtipo.de.delito[1],"de 2015 a 2023}")),
           
           # title=paste0("Gráfico 1. Investigación iniciadas \n por el delito de ", ),
           subtitle = paste0(incidencia_reactive()$Entidad[1]),
           caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP| @mexeva")+
      theme_minimal()+
      theme(legend.position = "none")+
      estilo_theme->grafico_anual_lineas
     
     grafico_anual_lineas
  })  
  
  
  output$download_grafico_anual_lineas <- downloadHandler(
    filename = function() {
      paste0("grafico_delitos_anual_lineas", tolower(incidencia_reactive()$Subtipo.de.delito[1]),
             "_",tolower(incidencia_reactive()$Entidad[1]),".jpeg") 
      },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        incidencia_reactive() %>% 
          ggplot() +
          aes(x =as.factor(Año), y = value, group=1) +
          geom_line(fill = "#8f5478", colour = "#8f5478", size=1.3) +
          geom_point(fill = "#8f5478", colour = "#8f5478", size=8, alpha=0.6) +
          geom_text(aes(label=comma(value)), size=4, vjust = 0.5, label.size = .1, colour="black")+
          scale_y_continuous(labels = scales::comma) +
          labs(x="", y="", fill="", color="",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{", incidencia_reactive()$Subtipo.de.delito[1],"de 2015 a 2023}")),
               
               # title=paste0("Gráfico 1. Investigación iniciadas \n por el delito de ", ),
               subtitle = paste0(incidencia_reactive()$Entidad[1]),
               caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP| @mexeva")+
          theme_minimal()+
          theme(legend.position = "bottom")+
          estilo_theme
      ) 
      dev.off()
    }
  )


  output$incidencia_año <- renderUI({
    selectInput("incidencia_año",
                label =  "Seleccione el año",
                choices = sort(unique(secretariado$Año)),
                multiple = T)
  })
  
  output$incidencia_entidad <- renderUI({
    selectInput("incidencia_entidad",
                label =  "Seleccione la Entidad",
                choices = sort(unique(secretariado$Entidad)),
                multiple = T)
  })

  output$incidencia_delito <- renderUI({
    selectInput("incidencia_delito",
                label =  "Selecciona el delito",
                choices = sort(unique(secretariado$Subtipo.de.delito)),
                multiple = T)
  })
  
  # incidencia_reactive <- reactive({
  #   
  #   secretariado %>%
  #     filter(
  #       if(!is.null(input$incidencia_año))                              Año %in% input$incidencia_año              else Año != "",
  #       if(!is.null(input$incidencia_entidad))                       Entidad %in% input$incidencia_entidad         else Entidad != "",
  #       if(!is.null(input$incidencia_delito))              Subtipo.de.delito %in% input$incidencia_delito          else Subtipo.de.delito != "") })
  # 
  # 
  # output$grafico_anual <- renderPlot ({
  #   width <- session$clientData$output_plot_responsive_width
  #   height <- session$clientData$output_plot_responsive_height
  #   
  #   incidencia_reactive() %>% 
  #   ggplot() +
  #     aes(x =as.factor(Año), y = value) +
  #     geom_col(fill = "#8f5478") +
  #     geom_label(aes(label=comma(value)), size=4, vjust = 0.5, label.size = .1)+
  #     scale_y_continuous(labels = scales::comma) +
  #     labs(x="", y="", fill="Tipo", color="Tipo",
  #          title=paste0("\n Gráfico 1. Carpetas de investigación por el delito de \n", 
  #          tolower(incidencia_reactive()$Subtipo.de.delito[1]), " en la entidad de ", 
  #          incidencia_reactive()$Entidad[1], "\n"),
  #          caption="Fuente: Elaboración propia con datos del SESNSP, actualizado a enero 2024| @mexeva")+
  #     theme_bw()+
  #     theme(legend.position = "bottom")+
  #     estilo_theme->grafico_anual
  #   
  #   grafico_anual
  # })  
  # 
  # 
  # output$download_grafico_anual <- downloadHandler(
  #   filename = function() {
  #     paste0("grafico_delitos_anual_", tolower(incidencia_reactive()$Subtipo.de.delito[1]),
  #            "_",tolower(incidencia_reactive()$Entidad[1]),".jpeg") # Nombre del archivo de descarga
  #   },
  #   
  #   content = function(file) {
  #     jpeg(file, width = 800, height = 600) 
  #     print(
  #       incidencia_reactive() %>% 
  #         ggplot() +
  #         aes(x =as.factor(Año), y = value) +
  #         geom_col(fill = "#8f5478") +
  #         geom_label(aes(label=comma(value)), size=4, vjust = 0.5, label.size = .1)+
  #         scale_y_continuous(labels = scales::comma) +
  #         labs(x="", y="", fill="Tipo", color="Tipo",
  #              title=paste0("\n Gráfico 1. Carpetas de investigación por el delito de \n", 
  #                           tolower(incidencia_reactive()$Subtipo.de.delito[1]), " en la entidad de ", 
  #                           incidencia_reactive()$Entidad[1], "\n"),
  #              caption="Fuente: Elaboración propia con datos del SESNSP, actualizado a enero 2024| @mexeva")+
  #         theme_bw()+
  #         theme(legend.position = "bottom")+
  #         estilo_theme ) 
  #     dev.off()
  #   }
  # )


  # 
  # output$grafico_anual_lineas <- renderPlot ({
  #   width <- session$clientData$output_plot_responsive_width
  #   height <- session$clientData$output_plot_responsive_height
  #   
  #   incidencia_reactive() %>%
  #     ggplot() +
  #     aes(x =as.factor(Año), y = value, group=1) +
  #     geom_line(fill = "#8f5478", colour = "#8f5478", size=1.3) +
  #     geom_point(fill = "#8f5478", colour = "#8f5478", size=8, alpha=0.6) +
  #     geom_text(aes(label=comma(value)), size=3, vjust = 0.5, label.size = .1, colour="black")+
  #     scale_y_continuous(labels = scales::comma) +
  #     labs(x="", y="", fill="Tipo", color="Tipo",
  #          title=paste0("\n Gráfico 2. Carpetas de investigación \n por el delito de ", 
  #                        tolower(incidencia_reactive()$Subtipo.de.delito[1]), "\n en la entidad de ", 
  #                        incidencia_reactive()$Entidad[1], 
  #                       "\n"),
  #          caption="Fuente: Elaboración propia con datos del SESNSP,\n actualizado a enero 2024| @mexeva")+
  #     theme_minimal()+
  #     theme(legend.position = "bottom")+
  #     estilo_theme->grafico_anual_lineas
  #    
  #    grafico_anual_lineas
  # })  
  # 
  # 
  # output$download_grafico_anual_lineas <- downloadHandler(
  #   filename = function() {
  #     paste0("grafico_delitos_anual_lineas", tolower(incidencia_reactive()$Subtipo.de.delito[1]),
  #            "_",tolower(incidencia_reactive()$Entidad[1]),".jpeg") 
  #     },
  #   
  #   content = function(file) {
  #     jpeg(file, width = 800, height = 600) 
  #     print(
  #       incidencia_reactive() %>% 
  #         ggplot() +
  #         aes(x =as.factor(Año), y = value, group=1) +
  #         geom_line(fill = "#8f5478", colour = "#8f5478", size=1.3) +
  #         geom_point(fill = "#8f5478", colour = "#8f5478", size=8, alpha=0.6) +
  #         geom_text(aes(label=comma(value)), size=3, vjust = 0.5, label.size = .1, colour="black")+
  #         scale_y_continuous(labels = scales::comma) +
  #         labs(x="", y="", fill="Tipo", color="Tipo",
  #              title=paste0("\n Gráfico 2. Carpetas de investigación por el delito de \n", 
  #                           tolower(incidencia_reactive()$Subtipo.de.delito[1]), " en la entidad de ", 
  #                           incidencia_reactive()$Entidad[1], 
  #                           "\n"),
  #              caption="Fuente: Elaboración propia con datos del SESNSP, actualizado a enero 2024| @mexeva")+
  #         theme_minimal()+
  #         theme(legend.position = "bottom")+
  #         estilo_theme
  #     ) 
  #     dev.off()
  #   }
  # )
  # 
  
  output$tabla_anual <- renderDataTable ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive() %>%
      ungroup() %>% 
      mutate(variacion = scales::percent((value - lag(value))/lag(value),0.1)) %>% 
      arrange(-Año) %>% 
      select(Año, value, variacion) %>% 
      datatable(
        caption = htmltools::tags$caption(paste0("Variación anual del delito de \n", 
                                                  tolower(incidencia_reactive()$Subtipo.de.delito[1]), " en ", incidencia_reactive()$Entidad[1], 
                                                  "."), style = 'caption-side: top; text-align: center; color:black;  font-size:80%; font-weight: bold;'),
       
        colnames = c('Año', 
                     'Total', 'Variación'), 
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#8f5478', 'color': '#fff', 'font-size':'12px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(7,1,5,10,20, "All"),
                                         c(7,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatCurrency('value',currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "13px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })
  
  # ...........................................................................
  output$incidencia_año_tasa <- renderUI({
    selectInput("incidencia_año_tasa",
                label =  "Seleccione el año",
                choices = sort(unique(df_tasa$Año)),
                multiple = T)
  })
  
  output$incidencia_entidad_tasa <- renderUI({
    selectInput("incidencia_entidad_tasa",
                label =  "Seleccione la Entidad",
                choices = sort(unique(df_tasa$Entidad)),
                multiple = T)
  })
  
  output$incidencia_delito_tasa <- renderUI({
    selectInput("incidencia_delito_tasa",
                label =  "Selecciona el delito",
                choices = sort(unique(df_tasa$Subtipo.de.delito)),
                multiple = T)
  })
  
  incidencia_reactive_tasa <- reactive({
    
    df_tasa %>%
      filter(
        if(!is.null(input$incidencia_año_tasa))                              Año %in% input$incidencia_año_tasa              else Año != "",
        if(!is.null(input$incidencia_entidad_tasa))                       Entidad %in% input$incidencia_entidad_tasa         else Entidad != "",
        if(!is.null(input$incidencia_delito_tasa))              Subtipo.de.delito %in% input$incidencia_delito_tasa          else Subtipo.de.delito != "") })
  
  
  output$grafico_anual_tasa <- renderPlot ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
      incidencia_reactive_tasa() %>% 
    # df_tasa %>% 
    #   filter(Entidad=="Jalisco",
    #          Subtipo.de.delito=="Feminicidio") %>% 
      ggplot() +
      aes(x =as.factor(Año), y = tasa) +
      geom_col(fill = "#8056ba") +
      geom_label(aes(label=round(tasa, digits = 2)), size=4, vjust = 0.5, label.size = .1)+
      scale_y_continuous(labels = scales::comma) +
      labs(x="", y="", fill="Tipo", color="Tipo",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{Tasa de ", tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]),"por cada 100 mil habitantes (2015 a 2023)}")),
           # title=paste0("\n Gráfico 3. Carpetas de investigación por cada 100 mil habitantes \n por el delito de ", 
           #              paste(tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]))),
           subtitle = paste0(incidencia_reactive_tasa()$Entidad[1]),
           caption="Fuente: Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP y Población de CONAPO | @mexeva")+
        theme_bw()+
      theme(legend.position = "bottom")+
      estilo_theme->grafico_anual_tasa
    
    grafico_anual_tasa
  })  
  
  
  output$download_grafico_anual_tasa <- downloadHandler(
    filename = function() {
      paste0("grafico_delitos_anual_tasa_", tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]),
             "_",tolower(incidencia_reactive_tasa()$Entidad[1]),".jpeg") 
      },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        incidencia_reactive_tasa() %>% 
          ggplot() +
          aes(x =as.factor(Año), y = tasa) +
          geom_col(fill = "#8056ba") +
          geom_label(aes(round(tasa, digits = 2)), size=4, vjust = 0.5, label.size = .1)+
          scale_y_continuous(labels = scales::comma) +
          labs(x="", y="", fill="Tipo", color="Tipo",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Tasa de ", tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]),"por cada 100 mil habitantes (2015 a 2023)}")),
               # title=paste0("\n Gráfico 3. Carpetas de investigación por cada 100 mil habitantes \n por el delito de ", 
               #              paste(tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]))),
               subtitle = paste0(incidencia_reactive_tasa()$Entidad[1]),
               caption="Fuente: Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP y Población de CONAPO | @mexeva")+
          theme_bw()+
          theme(legend.position = "bottom")+
          estilo_theme ) 
      dev.off()
    }
  )
  
  
  
  output$grafico_anual_lineas_tasa <- renderPlot ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
     incidencia_reactive_tasa() %>%
      # df_tasa %>%
      #   filter(Entidad=="Jalisco",Subtipo.de.delito=="Feminicidio") %>%
      ggplot() +
      aes(x =as.factor(Año), y = tasa, group=1) +
      geom_line(fill = "#8056ba", colour = "#8056ba", size=1.3) +
      geom_point(fill = "#8056ba", colour = "#8056ba", size=8) +
      geom_text(aes(label=round(tasa, digits = 2)), size=3, vjust = 0.5, colour="grey87")+
      scale_y_continuous()+
      labs(x="", y="", fill="Tipo", color="Tipo",
           title = latex2exp::TeX(paste("Gráfico: \\textbf{Tasa de ", tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]),"por cada 100 mil habitantes (2015 a 2023)}")),
           # title=paste0("\n Gráfico 4. Carpetas de investigación \n por cada 100 mil habitantes:"),
           subtitle = paste(incidencia_reactive_tasa()$Entidad[1]),
           caption="Fuente: Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP y Población de CONAPO | @mexeva")+
       theme_minimal()+
      theme(legend.position = "bottom")+
      estilo_theme->grafico_anual_lineas_tasa
    
    grafico_anual_lineas_tasa
  })  
  
  
  output$download_grafico_anual_lineas_tasa <- downloadHandler(
    filename = function() {
      paste0("grafico_delitos_anual_lineas_tasa_", tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]),
             "_",tolower(incidencia_reactive_tasa()$Entidad[1]),".jpeg") 
    },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        incidencia_reactive_tasa() %>%
        # df_tasa %>%
        #   filter(Entidad=="Jalisco",Subtipo.de.delito=="Feminicidio") %>%
          ggplot() +
          aes(x =as.factor(Año), y = tasa, group=1) +
          geom_line(fill = "#8056ba", colour = "#8056ba", size=1.3) +
          geom_point(fill = "#8056ba", colour = "#8056ba", size=8) +
          geom_text(aes(label=round(tasa, digits = 2)), size=3, vjust = 0.5, colour="grey87")+
          scale_y_continuous()+
          labs(x="", y="", fill="Tipo", color="Tipo",
               title = latex2exp::TeX(paste("Gráfico: \\textbf{Tasa de ", tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]),"por cada 100 mil habitantes (2015 a 2023)}")),
               # title=paste0("\n Gráfico 4. Carpetas de investigación \n por cada 100 mil habitantes:"),
               subtitle = paste(incidencia_reactive_tasa()$Entidad[1]),
               caption="Fuente: Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP y Población de CONAPO | @mexeva")+
          theme_minimal()+
          theme(legend.position = "bottom")+
          estilo_theme
      ) 
      dev.off()
    }
  )
  
  
  output$tabla_anual_tasa <- renderDataTable ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive_tasa() %>%
      ungroup() %>% 
      mutate(variacion = scales::percent((tasa - lag(tasa))/lag(tasa),0.1)) %>% 
      arrange(-Año) %>% 
      select(Año, tasa, variacion) %>% 
      datatable(
        caption = htmltools::tags$caption(paste0("Tasa de variación anual del delito de \n", 
                                                 tolower(incidencia_reactive_tasa()$Subtipo.de.delito[1]), " en ", incidencia_reactive_tasa()$Entidad[1], 
                                                 "."), style = 'caption-side: top; text-align: center; color:black;  font-size:80%; font-weight: bold;'),
        
        
        # filter = 'top',
        colnames = c('Año','Tasa por cada 100 mil hab.', 'Variación'), 
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#8056ba', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(7,1,5,10,20, "All"),
                                         c(7,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:3)))) %>%  
      formatCurrency('variacion',currency = "", interval = 3, mark = ",", digits = 1) %>%
      formatCurrency('tasa',currency = "", interval = 3, mark = ",", digits = 2) %>%
      formatStyle(
        columns = c(1:3),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })
  
  # ...........................................................................
  output$incidencia_año_tasa_bien <- renderUI({
    selectInput("incidencia_año_bien",
                label =  "Seleccione el año",
                choices = sort(unique(secretariado_bien$Año)),
                multiple = T)
  })
  
  output$incidencia_entidad_bien <- renderUI({
    selectInput("incidencia_entidad_bien",
                label =  "Seleccione la Entidad",
                choices = sort(unique(secretariado_bien$Entidad)),
                multiple = T)
  })
  
  output$incidencia_bienjuridico_bien <- renderUI({
    selectInput("incidencia_bienjuridico_bien",
                label =  "Selecciona el bien jurídico",
                choices = sort(unique(secretariado_bien$Bien.jurídico.afectado)),
                multiple = T)
  })
  
  output$incidencia_delito_bien <- renderUI({
    selectInput("incidencia_delito_bien",
                label =  "Selecciona el delito",
                choices = sort(unique(secretariado_bien$Subtipo.de.delito)),
                multiple = T)
  })
  
  incidencia_reactive_bien <- reactive({
    
    secretariado_bien %>%
      filter(
        if(!is.null(input$incidencia_año_bien))                               Año %in% input$incidencia_año_bien             else Año != "",
        if(!is.null(input$incidencia_entidad_bien))                       Entidad %in% input$incidencia_entidad_bien         else Entidad != "",
        if(!is.null(input$incidencia_bienjuridico_bien))         Bien.jurídico.afectado %in% input$incidencia_bienjuridico_bien    else Bien.juridico.afectado != "",
        if(!is.null(input$incidencia_delito_bien))              Subtipo.de.delito %in% input$incidencia_delito_bien          else Subtipo.de.delito != "") })
  
  
  output$tabla_bien <- render_gt ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive_bien() %>%
    # secretariado_bien %>% 
    #   filter(Entidad=="Jalisco", Año==2023,
    #          Bien.jurídico.afectado%in%c("La familia", "La sociedad")) %>% 
      arrange(-value) %>% 
      dplyr::top_n(n=3) %>%
      arrange(Bien.jurídico.afectado) %>%
      ungroup() %>% 
      dplyr::select(Bien.jurídico.afectado, Subtipo.de.delito, value, porcentaje) %>% 
      gt(#rowname_col = c("Entidad"),
        groupname_col = c("Bien.jurídico.afectado"
                          #, "Año"
                          )) %>% 
      tab_header(title = md("**Principales delitos por bien jurídico**"),
                 subtitle = paste0(incidencia_reactive_bien()$Entidad[1],
                                    ", ",
                                   incidencia_reactive_bien()$Año[1])
                 ) %>% 
      tab_options(
        heading.background.color = "#3d2a57",
        heading.border.bottom.color = "#3d2a57",
        heading.border.bottom.width = "5px",
        heading.border.lr.color = "#3d2a57",
        heading.border.lr.width = "50px") %>%
      tab_options(column_labels.background.color = "#7d6f8f") %>% 
      tab_style(
        style = list(style = "dashed",
          cell_borders(sides = c("top", "bottom"), weight = px(1))),
        locations = list(
          cells_body(columns = Bien.jurídico.afectado,rows = is_empty(Bien.jurídico.afectado)))) %>% 
      cols_align(align = "center", columns = 1) %>%
      tab_style(style = list(cell_borders(sides = c("top", "bottom", "left", "right"),
                                          color = "white",
                                          weight = px(5))),
                locations = cells_body(columns = c(1)))  %>% 
      fmt_number(columns = c(value), decimals = 0) %>%
      fmt_percent(columns = c(porcentaje), decimals = 2) %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_row_groups()) %>% 
      tab_source_note(source_note = md("Elaboración propia con base en datos del SESNSP | @mexevalua")) %>% 
      cols_label(`Subtipo.de.delito` = md("**Bien jurídico | Delito**"),
                 `Bien.jurídico.afectado` = md("**Bien<br>jurídico**"),
                 `value` = md("**Total**"),
                 `porcentaje` = md("**Porcentaje**"))->tabla_bien
    
    tabla_bien
     
  })
  
  output$download_tabla_bien <- downloadHandler(
    filename = function() {
      paste0("tabla_bien_juridico_", tolower(incidencia_reactive_bien()$Entidad[1]),".png") # Nombre del archivo de descarga
    },
    
    content = function(file) {
      png(file, width = 800, height = 600) 
      print(
        incidencia_reactive_bien() %>%
          arrange(-value) %>% 
          dplyr::top_n(n=3) %>%
          arrange(Bien.jurídico.afectado) %>%
          ungroup() %>% 
          select(Bien.jurídico.afectado, Subtipo.de.delito, value) %>% 
          gt(#rowname_col = c("Entidad"),
            groupname_col = c("Bien.jurídico.afectado"
                              #, "Año"
            )) %>% 
          tab_header(title = md("**Principales delitos por bien jurídico**"),
                     subtitle = paste0(incidencia_reactive_bien()$Entidad[1], 
                                       ", ", 
                                       incidencia_reactive_bien()$Año[1])) %>% 
          tab_options(
            heading.background.color = "#3d2a57",
            heading.border.bottom.color = "#3d2a57",
            heading.border.bottom.width = "5px",
            heading.border.lr.color = "#3d2a57",
            heading.border.lr.width = "50px") %>%
          tab_options(column_labels.background.color = "#7d6f8f") %>% 
          tab_style(
            style = list(style = "dashed",
                         cell_borders(sides = c("top", "bottom"), weight = px(1))),
            locations = list(
              cells_body(columns = Bien.jurídico.afectado,rows = is_empty(Bien.jurídico.afectado)))) %>% 
          cols_align(align = "center", columns = 1) %>%
          tab_style(style = list(cell_borders(sides = c("top", "bottom", "left", "right"),
                                              color = "white",
                                              weight = px(5))),
                    locations = cells_body(columns = c(1)))  %>% 
          fmt_number(columns = c(value), decimals = 0) %>%
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_row_groups()) %>% 
          tab_source_note(source_note = md("Elaboración propia con base en datos del SESNSP | @mexevalua")) %>% 
          cols_label(`Subtipo.de.delito` = md("**Delito**"),
                     `Bien.jurídico.afectado` = md("**Bien<br>jurídico**"),
                     `value` = md("**Total**"))) 
      dev.off()
    }
  )
  
  ##############################################################################
  ##############################################################################
  ##############################################################################
  
  # TASA ...........................................................................
  output$incidencia_año_mapa_tasa <- renderUI({
    selectInput("incidencia_año_mapa_tasa",
                label =  "Seleccione el año",
                choices = sort(unique(mx_tasa$Año)),
                multiple = T)
  })
  
  output$incidencia_delito_mapa_tasa <- renderUI({
    selectInput("incidencia_delito_mapa_tasa",
                label =  "Selecciona el delito",
                choices = sort(unique(mx_tasa$Subtipo.de.delito)),
                multiple = T)
  })
  
  incidencia_reactive_mapa_tasa <- reactive({
    
    mx_tasa %>%
      filter(
        if(!is.null(input$incidencia_año_mapa_tasa))                               Año %in% input$incidencia_año_mapa_tasa             else Año != "",
        if(!is.null(input$incidencia_delito_mapa_tasa))              Subtipo.de.delito %in% input$incidencia_delito_mapa_tasa          else Subtipo.de.delito != "") 
    })
  
  output$mapa_tasa <- renderPlot ({
    
    incidencia_reactive_mapa_tasa() %>% 
      # filter(Año==incidencia_reactive_mapa()$Año[1],
      #        Subtipo.de.delito==incidencia_reactive_mapa()$Subtipo.de.delito[1]) %>% 
      mxhexbin_choropleth(num_colors = 1) +
      scale_fill_gradient(
        low = "#c7b0f5", 
        high = "#8056ba",
        guide = "colourbar",
        label=comma)+
      labs(x="", y="", fill="Total", color="Total",
           title = latex2exp::TeX(paste("Mapa: \\textbf{Tasa de", tolower(incidencia_reactive_mapa()$Subtipo.de.delito[1]), incidencia_reactive_mapa()$Año[1],".}")),
           # title=paste0("\n Mapa 1. Carpetas de investigación: "),
           subtitle=paste0(incidencia_reactive_mapa()$Subtipo.de.delito[1]),
           caption="Fuente: elaboración propia con base en la \nincidencia delictiva reportada al SESNSP. | @mexeva")+
      theme_bw() +
      estilo_theme-> mapa_tasa
    
    mapa_tasa
  })
  
  output$download_mapa_tasa <- downloadHandler(
    filename = function() {
      paste0("mapa_", tolower(incidencia_reactive_mapa_tasa()$Subtipo.de.delito[1]),
             "_",tolower(incidencia_reactive_mapa_tasa()$Año[1]),".jpeg") # Nombre del archivo de descarga
    },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        incidencia_reactive_mapa_tasa() %>% 
          # filter(Año==incidencia_reactive_mapa()$Año[1],
          #        Subtipo.de.delito==incidencia_reactive_mapa()$Subtipo.de.delito[1]) %>% 
          mxhexbin_choropleth(num_colors = 1) +
          scale_fill_gradient(
            low = "#c7b0f5", 
            high = "#8056ba",
            guide = "colourbar",
            label=comma)+
          labs(x="", y="", fill="Total", color="Total",
               title = latex2exp::TeX(paste("Mapa: \\textbf{Tasa de", tolower(incidencia_reactive_mapa()$Subtipo.de.delito[1]), "por cada 100 mil habitantes,", incidencia_reactive_mapa()$Año[1],".}")),
               # title=paste0("\n Mapa 1. Carpetas de investigación: "),
               subtitle=paste0(incidencia_reactive_mapa()$Subtipo.de.delito[1]),
               caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP. | @mexeva")+
          theme_bw() +
          estilo_theme) 
      dev.off()
    }
  )
  
  output$tabla_mapa_tasa <- renderDataTable ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive_mapa_tasa() %>%
      ungroup() %>% 
      select(Año, Subtipo.de.delito, Entidad, value) %>% 
      datatable(
        caption = htmltools::tags$caption(paste0(incidencia_reactive_mapa_tasa()$Subtipo.de.delito[1], ", ", incidencia_reactive_mapa_tasa()$Año[1], 
                                                 "."), style = 'caption-side: top; text-align: center; color:black;  font-size:80%; font-weight: bold;'),
        
        
        # filter = 'top',
        colnames = c('Año','Delito', 'Entidad', 'Tasa'), 
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#8056ba', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, "All"),
                                         c(5,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:4)))) %>%  
      formatCurrency('value',currency = "", interval = 3, mark = ",", digits = 2) %>%
      formatStyle(
        columns = c(1:4),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })
  
  # Absoluto ...........................................................................
  output$incidencia_año_mapa <- renderUI({
    selectInput("incidencia_año_mapa",
                label =  "Seleccione el año",
                choices = sort(unique(mx_nominal$Año)),
                multiple = T)
  })
  
  output$incidencia_delito_mapa <- renderUI({
    selectInput("incidencia_delito_mapa",
                label =  "Selecciona el delito",
                choices = sort(unique(mx_nominal$Subtipo.de.delito)),
                multiple = T)
  })
  
  incidencia_reactive_mapa <- reactive({
    
    mx_nominal %>%
      filter(
        if(!is.null(input$incidencia_año_mapa))                               Año %in% input$incidencia_año_mapa             else Año != "",
        if(!is.null(input$incidencia_delito_mapa))              Subtipo.de.delito %in% input$incidencia_delito_mapa          else Subtipo.de.delito != "") 
  })
  
  output$mapa <- renderPlot ({
    
    incidencia_reactive_mapa() %>% 
      # filter(Año==incidencia_reactive_mapa()$Año[1],
      #        Subtipo.de.delito==incidencia_reactive_mapa()$Subtipo.de.delito[1]) %>% 
      mxhexbin_choropleth(num_colors = 1) +
      scale_fill_gradient(
        low = "#b897aa", 
        high = "#8f5478",
        guide = "colourbar",
        label=comma)+
      labs(x="", y="", fill="Total", color="Total",
           title = latex2exp::TeX(paste("Mapa: \\textbf{", incidencia_reactive_mapa()$Subtipo.de.delito[1],incidencia_reactive_mapa()$Año[1],".}")),
           # title=paste0("\n Mapa 1. Carpetas de investigación: "),
           subtitle=paste0(incidencia_reactive_mapa()$Subtipo.de.delito[1]),
           caption="Fuente: elaboración propia con base en la \nincidencia delictiva reportada al SESNSP. | @mexeva")+
      theme_bw() +
      estilo_theme-> mapa
    
    mapa
  })
  
  output$download_mapa <- downloadHandler(
    filename = function() {
      paste0("mapa_", tolower(incidencia_reactive_mapa()$Subtipo.de.delito[1]),
             "_",tolower(incidencia_reactive_mapa()$Año[1]),".jpeg") # Nombre del archivo de descarga
    },
    
    content = function(file) {
      jpeg(file, width = 800, height = 600) 
      print(
        incidencia_reactive_mapa() %>% 
          # filter(Año==incidencia_reactive_mapa()$Año[1],
          #        Subtipo.de.delito==incidencia_reactive_mapa()$Subtipo.de.delito[1]) %>% 
          mxhexbin_choropleth(num_colors = 1) +
          scale_fill_gradient(
            low = "#b897aa", 
            high = "#8f5478",
            guide = "colourbar",
            label=comma)+
          labs(x="", y="", fill="Total", color="Total",
               title = latex2exp::TeX(paste("Mapa: \\textbf{", incidencia_reactive_mapa()$Subtipo.de.delito[1],incidencia_reactive_mapa()$Año[1],".}")),
               # title=paste0("\n Mapa 1. Carpetas de investigación: "),
               subtitle=paste0(incidencia_reactive_mapa()$Subtipo.de.delito[1]),
               caption="Fuente: elaboración propia con base en la incidencia delictiva reportada al SESNSP. | @mexeva")+
          theme_bw() +
          estilo_theme) 
      dev.off()
    }
  )
  
  output$tabla_mapa <- renderDataTable ({
    width <- session$clientData$output_plot_responsive_width
    height <- session$clientData$output_plot_responsive_height
    
    incidencia_reactive_mapa() %>%
      ungroup() %>% 
      select(Año, Subtipo.de.delito, Entidad, value) %>% 
      datatable(
        caption = htmltools::tags$caption(paste0(incidencia_reactive_mapa()$Subtipo.de.delito[1], " en ", incidencia_reactive_mapa()$Año[1], 
                                                 "."), style = 'caption-side: top; text-align: center; color:black;  font-size:80%; font-weight: bold;'),
        
        
        # filter = 'top',
        colnames = c('Año','Delito', 'Entidad', 'Total'), 
        
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',
                       buttons = c('copy', 'excel', 'print'),
                       language = list(info = ' ', paginate = list(previous = 'Anterior', `next` = 'Siguiente')),
                       initComplete = JS("function(settings, json) {","$(this.api().table().header()).css(
                            {'background-color': '#8f5478', 'color': '#fff', 'font-size':'11px','align':'center'});","}"),
                       dom = "tip",
                       buttons = c('copy', 'excel', 'print'),
                       lengthMenu = list(c(5,1,5,10,20, "All"),
                                         c(5,1,5,10,20, "All")),
                       columnDefs = list(list(className = 'dt-center', targets = 1:4)))) %>%  
      formatCurrency('value',currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatStyle(
        columns = c(1:4),
        fontFamily = "Nutmeg-Light",
        fontSize = "11px",
        fontWeight = 'plain',
        borderRightWidth = "1px",
        borderRightStyle = "solid",
        borderRightColor = "white",
        borderBottomColor = "#ffffff",
        borderBottomStyle = "solid",
        borderBottomWidth = "0.5px",
        verticalAlign = "middle",
        textAlign = "center",
        wordWrap = "break-word")
    
    
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)