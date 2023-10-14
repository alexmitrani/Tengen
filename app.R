

# dependencies ------------------------------------------------------------

library(tidyverse)
library(gsheet)
library(shiny)
library(bslib)
library(thematic)
library(DT)
library(lubridate)
library(ggplot2)
library(plotly)
library(PlayerRatings)

# theme -------------------------------------------------------------------

my_theme <- bs_theme(bootswatch = "darkly",
                     base_font = font_google("Inconsolata"),
                     version = 5)

thematic_shiny(font = "auto")


# supuestos ---------------------------------------------------------------

# factores de equivalencia de piedras handicap para distintos tamaños de tablero.
# detalles sobre los supuestos adoptados y discusión de otras opciones:
# https://web.archive.org/web/20231014034254/https://forums.online-go.com/t/ranking-and-handicaps/17739/26?u=alemitrani

handicap_factor_9x9 <- 4
handicap_factor_13x13 <- (16/9)
handicap_factor_19x19 <- 1

# cantidad mínima de partidas para incluir en la página de ratings:
partidas_requeridas_para_rating <- 3

# referencia: https://github.com/online-go/online-go.com/blob/2e9ccea12b16fefeba8fb86e0312875964e16857/src/lib/rank_utils.ts#L50C1-L51C17
const_a <- 525
const_c <- 23.15

# ajustado considerando los rangos de Avelio, Nico, Alex y Pandora
const_d <- -28.5


# data processing ---------------------------------------------------------

  mydf <- gsheet2tbl('docs.google.com/spreadsheets/d/1WSHIHtk_oKzA5kruweD_3g3OH7DseTsgTLhEFTHsvFQ')

  colnames(mydf)[1]="fecha_hora"
  colnames(mydf)[3]="persona.1"
  colnames(mydf)[4]="persona.2"
  colnames(mydf)[5]="tablero"
  colnames(mydf)[6]="handicap"
  colnames(mydf)[7]="victoria"
  colnames(mydf)[8]="comentario"

  mydf <- mydf %>%
    select(fecha_hora, persona.1, persona.2, tablero, handicap, victoria, comentario)

  mydf$fecha_hora <- as.POSIXct(mydf$fecha_hora, format="%d/%m/%Y %H:%M:%S", tz = "UTC")

  mydf <- mydf %>%
    mutate(victoria = ifelse(victoria=="Negro", 1, 2))

  mydf <- mydf %>%
    pivot_longer(cols = c(persona.1, persona.2), names_to = "color", values_to = "persona")

  mydf <- mydf %>%
    mutate(color = ifelse(color=="persona.1", 1, 2))

  mydf <- mydf %>%
    mutate(victoria=ifelse(color==victoria, 1, 0))

  # corrections to persona names

  mydf <- mydf %>%
    mutate(persona = ifelse(persona=="Anibal", "Aníbal", persona))

  mydf <- mydf %>%
    mutate(persona = ifelse(persona=="Nicolás", "Nico", persona))

  mydf <- mydf %>%
    mutate(persona = ifelse(persona=="Panchito", "Francisco", persona))

  # merge on oponente name

  mydf2 <- mydf %>%
    select(fecha_hora, persona, color) %>%
    rename(oponente = persona) %>%
    mutate(color = if_else(color==1, 2, 1))

  mydf <- mydf %>%
    left_join(mydf2) %>%
    mutate(color = ifelse(color==1, "negro", "blanco"))

  rm(mydf2)

  # prepare final games data

  mydf <- mydf %>%
    mutate(comentario = ifelse(substr(comentario, 1, 4)=="http", paste0("<a href='",  comentario, "' target='_blank'>", comentario, "</a>"), comentario))

  mydf <- mydf %>%
    select(fecha_hora, tablero, handicap, persona, color, oponente, victoria, comentario)


  # aplicación de factores de equivalencia de piedras handicap para distintos tamaños de tablero.

  mydf <- mydf %>%
    mutate(handicap_factor =
             case_when(
               tablero == "9 x 9" ~ handicap_factor_9x9,
               tablero == "13 x 13" ~ handicap_factor_13x13,
               tablero == "19 x 19" ~ handicap_factor_19x19
               )
           )

  mydf <- mydf %>%
    mutate(handicap_adj = handicap*handicap_factor)

  mydf <- mydf %>%
    arrange(desc(fecha_hora))

  # data for ratings calculation

  ratings_data <- mydf %>%
    mutate(tp = as.integer(format(mydf$fecha_hora, "%Y%m%d"))) %>%
    filter(color=="negro") %>%
    select(tp, persona, oponente, victoria)

  handicap_vector <- as.vector(mydf$handicap_adj)
  sobj <- glicko2(ratings_data, init = c(1500,350,0.06), gamma = handicap_vector, history = TRUE)
  resultados <- sobj[["ratings"]]

  resultados <- resultados %>%
    rename(persona = Player,
           rating = Rating,
           desviación = Deviation,
           volatilidad = Volatility,
           partidas = Games,
           victorias = Win,
           derrotas = Loss)

  resultados <- resultados %>%
    mutate(rango = round((log(rating/const_a)*const_c + const_d),1))

  resultados <- resultados %>%
    select(persona, rating, rango, desviación, partidas, victorias, derrotas)

  resultados <- resultados %>%
    mutate(rating = round(rating, 0),
           desviación = round(desviación, 0))

  resultados <- resultados %>%
    filter(partidas>=partidas_requeridas_para_rating)

  history <- as.data.frame(sobj[["history"]])
  history_long <- as.data.frame(t(history))
  history_long <- rownames_to_column(history_long, var = "periodo") %>% as_tibble()
  history_long <- history_long %>% filter(grepl('Rating', periodo))
  history_long <- history_long %>%
    mutate(periodo = str_replace(history_long$periodo, ".Rating", ""))

  history_long2 <- history_long %>%
    pivot_longer(cols = !periodo)

  fecha <- ratings_data %>% group_by(tp) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    arrange(tp) %>%
    mutate(periodo = row_number()) %>%
    select(periodo, tp)

  history_long2 <- history_long2 %>%
    mutate(periodo = as.numeric(periodo)) %>%
    rename(persona = name,
           rating = value)

  history_long2 <- history_long2 %>%
    left_join(fecha)

  history_long2 <- history_long2 %>%
    mutate(fecha = as.Date(as.character(tp), "%Y%m%d")) %>%
    select(fecha, persona, rating)


# Application -------------------------------------------------------------


# user interface ----------------------------------------------------------

ui <- fluidPage(

  theme = my_theme,

  tags$head(includeHTML(("google-analytics.html"))),

  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),

  tags$div(
    tags$br(),
    tags$a(href="https://online-go.com/group/615", target='_blank', "Club Tengen de Go"),
    tags$br(),
    tags$br(),
    img(src = "tengen.png", height = 114, width = 344),
    tags$br(),
    tags$br(),
    # Para buscar nuevos datos de la planilla de entrada de datos
    actionButton("actualizar", "Actualizar"),
    tags$br(),
    tags$br(),
  ),

  mainPanel(

      tags$div(
        textOutput("numero_partidas", inline = TRUE), " partidas, ",
        textOutput("numero_personas", inline = TRUE), " personas, actualizado ",
        textOutput("date_time_update", inline = TRUE), "."
      ),

      tags$br(),

      fluidRow(

        column(12,
               selectizeInput("persona_Input_games", "personas:",
                              sort(unique(mydf$persona)),
                              selected=NULL, multiple =TRUE)),
        # column(6,
        #        uiOutput("oponente_Input_games")
        # )

      ),

      # fluidRow(
      #
      #   column(6,
      #          uiOutput("tablero_Input_games")
      #   )
      #
      # ),


     tabsetPanel(type = "tabs",

                 # rating -----------------------------------------------------------------


                 tabPanel("rating",

                          tags$br(),


                          fluidPage(

                            # Graph

                            fluidRow(
                              column(12,
                                     plotlyOutput("history_plot")
                              )
                            ),

                            hr(),


                            fluidRow(

                              tags$br(),

                              column(12,

                                     # Create a new row for the table.
                                     DT::dataTableOutput("rating_data_table")

                              )

                            )

                          )

                 ),


                # resumen -----------------------------------------------------------------


                 tabPanel("resumen",

                          tags$br(),


                          fluidPage(


                            fluidRow(

                              tags$br(),

                              column(12,

                                     # Create a new row for the table.
                                     DT::dataTableOutput("resumen_data_table")

                              )

                            )

                          )

                 ),


                 # games -------------------------------------------------------------------

                 tabPanel("partidas",

                          tags$br(),


                          fluidPage(


                              fluidRow(

                                tags$br(),

                                column(12,

                                       # Create a new row for the table.
                                       DT::dataTableOutput("games_data_table")

                                )

                              )

                            )

                          )

                 )

     ),

  tags$div(
    tags$br(),
    tags$a(href="https://github.com/alexmitrani/Tengen", "Tengen-app GitHub"),
    tags$br()
  )

  )


# server -----------------------------------------------------


server <- function(input, output, session) {

  session$onSessionEnded(stopApp)


# actualizar datos --------------------------------------------------------

  observeEvent(input$actualizar, {

    mydf <- gsheet2tbl('docs.google.com/spreadsheets/d/1WSHIHtk_oKzA5kruweD_3g3OH7DseTsgTLhEFTHsvFQ')

    colnames(mydf)[1]="fecha_hora"
    colnames(mydf)[3]="persona.1"
    colnames(mydf)[4]="persona.2"
    colnames(mydf)[5]="tablero"
    colnames(mydf)[6]="handicap"
    colnames(mydf)[7]="victoria"
    colnames(mydf)[8]="comentario"

    mydf <- mydf %>%
      select(fecha_hora, persona.1, persona.2, tablero, handicap, victoria, comentario)

    mydf$fecha_hora <- as.POSIXct(mydf$fecha_hora, format="%d/%m/%Y %H:%M:%S", tz = "UTC")

    mydf <- mydf %>%
      mutate(victoria = ifelse(victoria=="Negro", 1, 2))

    numero_partidas <- nrow(mydf)

    mydf <- mydf %>%
      pivot_longer(cols = c(persona.1, persona.2), names_to = "color", values_to = "persona")

    mydf <- mydf %>%
      mutate(color = ifelse(color=="persona.1", 1, 2))

    mydf <- mydf %>%
      mutate(victoria=ifelse(color==victoria, 1, 0))

    # corrections to persona names

    mydf <- mydf %>%
      mutate(persona = ifelse(persona=="Anibal", "Aníbal", persona))

    mydf <- mydf %>%
      mutate(persona = ifelse(persona=="Nicolás", "Nico", persona))

    mydf <- mydf %>%
      mutate(persona = ifelse(persona=="Panchito", "Francisco", persona))

    # count people

    numero_personas <- nrow(mydf %>% group_by(persona) %>% summarize(count = n()) %>% ungroup())

    # merge on oponente name

    mydf2 <- mydf %>%
      select(fecha_hora, persona, color) %>%
      rename(oponente = persona) %>%
      mutate(color = if_else(color==1, 2, 1))

    mydf <- mydf %>%
      left_join(mydf2) %>%
      mutate(color = ifelse(color==1, "negro", "blanco"))

    rm(mydf2)

    # prepare final games data

    mydf <- mydf %>%
      mutate(comentario = ifelse(substr(comentario, 1, 4)=="http", paste0("<a href='",  comentario, "' target='_blank'>", comentario, "</a>"), comentario))

    mydf <- mydf %>%
      select(fecha_hora, tablero, handicap, persona, color, oponente, victoria, comentario)

    mydf <- mydf %>%
      arrange(desc(fecha_hora))

    date_time_update <- as.character(mydf$fecha_hora[1])

    # Resumen de datos

    summary_text <- paste0("Hay ", numero_partidas, " partidas de ", numero_personas, " personas.")
    update_text <- paste0("Datos actualizados ", date_time_update, ".")

  })

  # menus --------------------------------------------------------------------

  # output$oponente_Input_games <- renderUI({
  #
  #   menudata <- mydf %>%
  #     arrange(desc(fecha_hora))
  #
  #   if (is.null(input$persona_Input_games)==FALSE) {
  #     menudata <- menudata %>%
  #       filter(persona %in% input$persona_Input_games) %>%
  #       arrange(desc(fecha_hora))
  #   }
  #
  #   selectizeInput("oponente_Input_games", "oponente:",
  #                  choices = c(unique(menudata$oponente)), multiple =TRUE)
  #
  # })
  #
  # output$tablero_Input_games <- renderUI({
  #
  #   menudata <- mydf %>%
  #     arrange(desc(fecha_hora))
  #
  #   if (is.null(input$persona_Input_games)==FALSE) {
  #     menudata <- menudata %>%
  #       filter(persona %in% input$persona_Input_games) %>%
  #       arrange(desc(fecha_hora))
  #   }
  #
  #   if (is.null(input$oponente_Input_games)==FALSE) {
  #     menudata <- menudata %>%
  #       filter(oponente %in% input$oponente_Input_games) %>%
  #       arrange(desc(fecha_hora))
  #   }
  #
  #   selectizeInput("tablero_Input_games", "tablero:",
  #                  choices = c(unique(menudata$tablero)), multiple =TRUE)
  #
  # })


  # estado ------------------------------------------------------------------

  # datos generales

  output$numero_personas <- renderText({
    numero_personas <- nrow(mydf %>% group_by(persona) %>% summarize(count = n()) %>% ungroup())
    numero_personas
  })

  output$numero_partidas <- renderText({
    numero_partidas <- nrow(mydf %>% group_by(fecha_hora) %>% summarize(count = n()) %>% ungroup())
    numero_partidas
  })

  output$date_time_update <- renderText({
    date_time_update <- as.character(mydf$fecha_hora[1])
    date_time_update
  })

  # rating -----------------------------------------------------------------


  history_data <- reactive({

    history_data <- history_long2

    if (is.null(input$persona_Input_games)==FALSE) {
      history_data <- history_data %>%
        filter(persona %in% input$persona_Input_games)

    }

    history_data

  })

  output$history_plot <- renderPlotly({

    p <- ggplot(history_data(), aes(x = fecha, y = rating, color = persona)) +
      geom_line() +
      xlab("fecha") +
      ylab("rating")

    plotly::ggplotly(p)

  })

  rating_data <- reactive({

    rating_data <- resultados

    if (is.null(input$persona_Input_games)==FALSE) {
      rating_data <- rating_data %>%
        filter(persona %in% input$persona_Input_games)

    }

    rating_data

  })

  output$rating_data_table <- DT::renderDataTable(DT::datatable({

    data <- rating_data() %>%
      arrange(desc(rating))

    data

  },
  style = "bootstrap"))



  # resumen -----------------------------------------------------------------

  resumen_data <- reactive({

    resumen_data <- mydf

    if (is.null(input$persona_Input_games)==FALSE) {
      resumen_data <- resumen_data %>%
        filter(persona %in% input$persona_Input_games)

    }

    # if (is.null(input$oponente_Input_games)==FALSE) {
    #   resumen_data <- resumen_data %>%
    #     filter(oponente %in% input$oponente_Input_games)
    #
    # }
    #
    # if (is.null(input$tablero_Input_games)==FALSE) {
    #   resumen_data <- resumen_data %>%
    #     filter(tablero %in% input$tablero_Input_games)
    #
    # }

    resumen_data <- resumen_data %>%
      group_by(persona, oponente, tablero, handicap) %>%
      summarize(partidas = n(), victorias = sum(victoria), tasa_victoria = round(victorias / partidas, 2)) %>%
      ungroup()

    resumen_data

  })

  output$resumen_data_table <- DT::renderDataTable(DT::datatable({

    data <- resumen_data() %>%
      arrange(desc(partidas), desc(tasa_victoria))

    data

  },
  style = "bootstrap"))



  # games -------------------------------------------------------------------

  games_data <- reactive({


    games_data <- mydf

    if (is.null(input$persona_Input_games)==FALSE) {
      games_data <- games_data %>%
        filter(persona %in% input$persona_Input_games)

    }

    # if (is.null(input$oponente_Input_games)==FALSE) {
    #   games_data <- games_data %>%
    #     filter(oponente %in% input$oponente_Input_games)
    #
    # }
    #
    # if (is.null(input$tablero_Input_games)==FALSE) {
    #   games_data <- games_data %>%
    #     filter(tablero %in% input$tablero_Input_games)
    #
    # }

    games_data

  })

  output$games_data_table <- DT::renderDataTable(DT::datatable({

    data <- games_data() %>%
      arrange(desc(fecha_hora))

    data

  }, escape = c(-9),
  style = "bootstrap"))

  # end of server -----------------------------------------------------------


}


# shinyapp ----------------------------------------------------------------



shinyApp(ui = ui, server = server)




#









