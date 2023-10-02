

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(gsheet)
library(shiny)
library(bslib)
library(thematic)
library(DT)
library(lubridate)

# theme -------------------------------------------------------------------

my_theme <- bs_theme(bootswatch = "darkly",
                     base_font = font_google("Inconsolata"),
                     version = 5)

thematic_shiny(font = "auto")


# Data processing ---------------------------------------------------------



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

# merge on opponente name

mydf2 <- mydf %>%
  select(fecha_hora, persona, color) %>%
  rename(opponente = persona) %>%
  mutate(color = if_else(color==1, 2, 1))

mydf <- mydf %>%
  left_join(mydf2) %>%
  mutate(color = ifelse(color==1, "negro", "blanco"))

rm(mydf2)

# prepare final games data

mydf <- mydf %>%
  mutate(comentario = ifelse(substr(comentario, 1, 4)=="http", paste0("<a href='",  comentario, "' target='_blank'>", comentario, "</a>"), comentario))

mydf <- mydf %>%
  select(fecha_hora, tablero, handicap, persona, color, opponente, victoria, comentario)

mydf <- mydf %>%
  arrange(desc(fecha_hora))



# Application -------------------------------------------------------------


# user interface ----------------------------------------------------------

ui <- fluidPage(

  theme = my_theme,

  tags$head(includeHTML(("google-analytics.html"))),

  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),

  h1("Tengen-app"),

  tags$div(
    tags$a(href="https://www.instagram.com/clubtengen/", "Club Tengen de Go"),
    tags$br(),
    tags$a(href="https://docs.google.com/forms/d/1iPusyVKjL9lTLXJ8PBK7oaNCWR6n8sT2tM0FybLwvVg/edit", "Registrar una Partida"),
    tags$br(),
  ),


  mainPanel(

      tags$br(),

      fluidRow(

        column(6,
               selectizeInput("persona_Input_games", "persona:",
                              sort(unique(mydf$persona)),
                              selected=NULL, multiple =TRUE)),
        column(6,
               uiOutput("opponente_Input_games")
        )

      ),

      fluidRow(

        column(6,
               uiOutput("tablero_Input_games")
        )

      ),


     tabsetPanel(type = "tabs",


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
    "Contact the developer on ",
    tags$a(rel="me", href="https://mastodon.online/@alex_mitrani_es", "Mastodon"),
    tags$br()
  )

  )


# server -----------------------------------------------------


server <- function(input, output, session) {

  session$onSessionEnded(stopApp)


  # menus --------------------------------------------------------------------

  output$opponente_Input_games <- renderUI({

    menudata <- mydf %>%
      arrange(desc(fecha_hora))

    if (is.null(input$persona_Input_games)==FALSE) {
      menudata <- menudata %>%
        filter(persona %in% input$persona_Input_games) %>%
        arrange(desc(fecha_hora))
    }

    selectizeInput("opponente_Input_games", "opponente:",
                   choices = c(unique(menudata$opponente)), multiple =TRUE)

  })

  output$tablero_Input_games <- renderUI({

    menudata <- mydf %>%
      arrange(desc(fecha_hora))

    if (is.null(input$persona_Input_games)==FALSE) {
      menudata <- menudata %>%
        filter(persona %in% input$persona_Input_games) %>%
        arrange(desc(fecha_hora))
    }

    if (is.null(input$opponente_Input_games)==FALSE) {
      menudata <- menudata %>%
        filter(opponente %in% input$opponente_Input_games) %>%
        arrange(desc(fecha_hora))
    }

    selectizeInput("tablero_Input_games", "tablero:",
                   choices = c(unique(menudata$tablero)), multiple =TRUE)

  })


  # resumen -----------------------------------------------------------------

  resumen_data <- reactive({

    resumen_data <- mydf

    if (is.null(input$persona_Input_games)==FALSE) {
      resumen_data <- resumen_data %>%
        filter(persona %in% input$persona_Input_games)

    }

    if (is.null(input$opponente_Input_games)==FALSE) {
      resumen_data <- resumen_data %>%
        filter(opponente %in% input$opponente_Input_games)

    }

    if (is.null(input$tablero_Input_games)==FALSE) {
      resumen_data <- resumen_data %>%
        filter(tablero %in% input$tablero_Input_games)

    }

    resumen_data <- resumen_data %>%
      group_by(persona, opponente, tablero, handicap) %>%
      summarize(games = n(), victorias = sum(victoria), tasa_victoria = round(victorias / games, 2)) %>%
      ungroup()

    resumen_data

  })

  output$resumen_data_table <- DT::renderDataTable(DT::datatable({

    data <- resumen_data() %>%
      arrange(persona, opponente, tablero)

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

    if (is.null(input$opponente_Input_games)==FALSE) {
      games_data <- games_data %>%
        filter(opponente %in% input$opponente_Input_games)

    }

    if (is.null(input$tablero_Input_games)==FALSE) {
      games_data <- games_data %>%
        filter(tablero %in% input$tablero_Input_games)

    }

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









