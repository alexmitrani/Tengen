

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

rating_por_unidad_handicap <- 66.21465
handicap_factor_9x9 <- 4
handicap_factor_13x13 <- (16/9)
handicap_factor_19x19 <- 1

# referencia: https://github.com/online-go/online-go.com/blob/2e9ccea12b16fefeba8fb86e0312875964e16857/src/lib/rank_utils.ts#L50C1-L51C17
const_a <- 525
const_c <- 23.15

# ajustado considerando que rating 1918 sea rango 0 al igual ue en OGS. Valores mayores o iguales a 1918 se consideran dan, menores kyu.
const_d <- -30

# rango = log(rating/const_a)*const_c + const_d

# máximo de personas para mostrar a la vez en el gráfico
personas_max_ratings_grafico <- 10

# cantidad minimo de partidos para distinguir entre grupos 1 y 2
minimo_partidos <- 10


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
    mutate(handicap_rating = handicap*handicap_factor*rating_por_unidad_handicap)

  mydf <- mydf %>%
    arrange(desc(fecha_hora))

  # data for games and summary pages
  gamesdf <- mydf %>%
    select(fecha_hora, tablero, handicap, persona, color, oponente, victoria, comentario)

  # data for ratings calculation

  mydf <- mydf %>%
    filter(color=="negro")

  handicap_vector <- as.vector(mydf$handicap_rating)

  ratings_data <- mydf %>%
    mutate(tp = as.integer(format(fecha_hora, "%Y%m%d"))) %>%
    select(tp, persona, oponente, victoria)

  sobj <- glicko2(ratings_data, init = c(1500,350,0.06), gamma = handicap_vector, history = TRUE)
  resultados <- sobj[["ratings"]] %>%
    mutate(grupo = ifelse(Games>=minimo_partidos,1,2))

  resultados <- resultados %>%
    rename(persona = Player,
           rating = Rating,
           desviación = Deviation,
           volatilidad = Volatility,
           partidos = Games,
           victorias = Win,
           derrotas = Loss)

  resultados <- resultados %>%
    mutate(rango = round((log(rating/const_a)*const_c + const_d),1))

  # mínimo rango 30 kyu
  resultados <- resultados %>%
    mutate(rango = ifelse(is.na(rango)==TRUE | rango < -30, -30, rango))

  resultados <- resultados %>%
    select(grupo, persona, rating, rango, desviación, partidos, victorias, derrotas)

  resultados <- resultados %>%
    mutate(rating = round(rating, 0),
           desviación = round(desviación, 0),
           tasa_victoria = round(victorias/partidos, 2))

  persona_incluir <- resultados %>%
    arrange(desc(partidos)) %>%
    mutate(incluir = ifelse(row_number()<=personas_max_ratings_grafico, 1, 0)) %>%
    select(persona, incluir)

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
    mutate(rango = round((log(rating/const_a)*const_c + const_d),1)) %>%
    select(fecha, persona, rating, rango)

  history_long2 <- history_long2 %>%
    left_join(persona_incluir)


# application -------------------------------------------------------------


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
    tags$br()
  ),

  mainPanel(

      tags$div(
        textOutput("numero_partidos", inline = TRUE), " partidos, ",
        textOutput("numero_personas", inline = TRUE), " personas, actualizado ",
        textOutput("date_time_update", inline = TRUE), "."
      ),

      tags$br(),

      fluidRow(

        column(12,
               selectizeInput("persona_Input_games", "personas:",
                              sort(unique(gamesdf$persona)),
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

                 tabPanel("partidos",

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

  # estado ------------------------------------------------------------------

  # datos generales

  output$numero_personas <- renderText({
    numero_personas <- nrow(resultados %>% group_by(persona) %>% summarize(count = n()) %>% ungroup())
    numero_personas
  })

  output$numero_partidos <- renderText({
    numero_partidos <- nrow(mydf %>% group_by(fecha_hora) %>% summarize(count = n()) %>% ungroup())
    numero_partidos
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

    } else {

      history_data <- history_data %>%
        filter(incluir==1)

    }

    history_data

  })

  output$history_plot <- renderPlotly({

    ymin <- floor(min(history_data()$rango))
    ymax <- ceiling(max(history_data()$rango))

    p <- ggplot(history_data(), aes(x = fecha, y = rango, color = persona)) +
      geom_line() +
      xlab("fecha") +
      ylab("rango") +
      scale_y_continuous(breaks = seq(ymin, ymax, by = 1))

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
      arrange(grupo, desc(rating))

    data

  },
  style = "bootstrap"))



  # resumen -----------------------------------------------------------------

  resumen_data <- reactive({

    resumen_data <- gamesdf

    if (is.null(input$persona_Input_games)==FALSE) {
      resumen_data <- resumen_data %>%
        filter(persona %in% input$persona_Input_games)

    }

    resumen_data <- resumen_data %>%
      group_by(persona, oponente, tablero, handicap) %>%
      summarize(partidos = n(), victorias = sum(victoria), tasa_victoria = round(victorias / partidos, 2)) %>%
      ungroup()

    resumen_data

  })

  output$resumen_data_table <- DT::renderDataTable(DT::datatable({

    data <- resumen_data() %>%
      arrange(desc(partidos), desc(tasa_victoria))

    data

  },
  style = "bootstrap"))



  # partidos -------------------------------------------------------------------

  games_data <- reactive({


    games_data <- gamesdf

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









