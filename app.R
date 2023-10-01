

# Dependencies ------------------------------------------------------------

library(tidyverse)
library(gsheet)
library(shiny)
library(bslib)
library(thematic)
library(DT)

# theme -------------------------------------------------------------------

my_theme <- bs_theme(bootswatch = "darkly",
                     base_font = font_google("Inconsolata"),
                     version = 5)

thematic_shiny(font = "auto")


# Data processing ---------------------------------------------------------



mydf <- gsheet2tbl('docs.google.com/spreadsheets/d/1WSHIHtk_oKzA5kruweD_3g3OH7DseTsgTLhEFTHsvFQ')

colnames(mydf)[3]="player.1"
colnames(mydf)[4]="player.2"
colnames(mydf)[5]="board_size"
colnames(mydf)[6]="handicap"
colnames(mydf)[7]="player_won"

mydf <- mydf %>%
  select(Timestamp, player.1, player.2, board_size, handicap, player_won)

mydf <- mydf %>%
  mutate(player_won = ifelse(player_won=="Negro", 1, 2))

mydf <- mydf %>%
  pivot_longer(cols = c(player.1, player.2), names_to = "player", values_to = "name")

mydf <- mydf %>%
  mutate(player = ifelse(player=="player.1", 1, 2))

mydf <- mydf %>%
  mutate(win=ifelse(player==player_won, 1, 0))

mydf <- mydf %>%
  mutate(loss=1-win)

mydf <- mydf %>%
  mutate(name = ifelse(name=="Anibal", "Aníbal", name))


# Application -------------------------------------------------------------


# user interface ----------------------------------------------------------

ui <- fluidPage(

  theme = my_theme,

  # tags$head(includeHTML(("google-analytics.html"))),

  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),

  h1("Tengen-app"),

  tags$div(
    "Registro de Partidas de ",
    tags$a(href="https://www.clubtengen.cl/", "Club Tengen de Go"),
    tags$br(),
    tags$br(),
  ),


  mainPanel(


     tabsetPanel(type = "tabs",

                 # shows -------------------------------------------------------------------

                 tabPanel("games",


                          fluidPage(

                            tags$br(),

                              fluidRow(
                                column(6,
                                       selectizeInput("playerInput_games", "name:",
                                                      sort(unique(mydf$name)),
                                                      selected=NULL, multiple =TRUE)),
                                column(6,
                                       selectizeInput("board_sizeInput_games", "board_size:",
                                                      sort(unique(mydf$board_size)),
                                                      selected=NULL, multiple =TRUE))

                              ),

                              fluidRow(

                                tags$br(),

                                column(12,

                                       # Create a new row for the table.
                                       DT::dataTableOutput("games_datatable")

                                )

                              )

                            )

                          )

                 )

     )

  )


# server -----------------------------------------------------


server <- function(input, output, session) {

  session$onSessionEnded(stopApp)


  # menus --------------------------------------------------------------------




  # games -------------------------------------------------------------------

  games_data <- reactive({


    games_data <- mydf

    if (is.null(input$playerInput_games)==FALSE) {
      games_data <- games_data %>%
        filter(name %in% input$playerInput_games)

    }

    if (is.null(input$board_sizeInput_games)==FALSE) {
      games_data <- games_data %>%
        filter(board_size %in% input$board_sizeInput_games)

    }

    games_data

  })

  output$games_datatable <- DT::renderDataTable(DT::datatable({

    data <- games_data() %>%
      arrange(desc(Timestamp))

    data

  },
  style = "bootstrap"))

  # end of server -----------------------------------------------------------


}


# shinyapp ----------------------------------------------------------------



shinyApp(ui = ui, server = server)




#









