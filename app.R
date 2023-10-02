

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

colnames(mydf)[1]="timestamp"
colnames(mydf)[3]="player.1"
colnames(mydf)[4]="player.2"
colnames(mydf)[5]="board_size"
colnames(mydf)[6]="handicap"
colnames(mydf)[7]="player_won"
colnames(mydf)[8]="comment"

mydf <- mydf %>%
  select(timestamp, player.1, player.2, board_size, handicap, player_won, comment)

mydf$timestamp <- as.POSIXct(mydf$timestamp, format="%d/%m/%Y %H:%M:%S", tz = "UTC")

mydf <- mydf %>%
  mutate(player_won = ifelse(player_won=="Negro", 1, 2))

mydf <- mydf %>%
  pivot_longer(cols = c(player.1, player.2), names_to = "player", values_to = "player_name")

mydf <- mydf %>%
  mutate(player = ifelse(player=="player.1", 1, 2))

mydf <- mydf %>%
  mutate(player_win=ifelse(player==player_won, 1, 0))

# corrections to player names

mydf <- mydf %>%
  mutate(player_name = ifelse(player_name=="Anibal", "Aníbal", player_name))

# merge on opponent name

mydf2 <- mydf %>%
  select(timestamp, player, player_name) %>%
  rename(opponent_name = player_name) %>%
  mutate(player = if_else(player==1, 2, 1))

mydf <- mydf %>%
  left_join(mydf2) %>%
  mutate(player_colour = ifelse(player==1, "black", "white"))

# prepare final data

mydf <- mydf %>%
  mutate(comment = ifelse(substr(comment, 1, 4)=="http", paste0("<a href='",  comment, "' target='_blank'>", comment, "</a>"), comment))

mydf <- mydf %>%
  select(timestamp, board_size, handicap, player_name, player_colour, opponent_name, player_win, comment)

mydf <- mydf %>%
  arrange(desc(timestamp))


# Application -------------------------------------------------------------


# user interface ----------------------------------------------------------

ui <- fluidPage(

  theme = my_theme,

  tags$head(includeHTML(("google-analytics.html"))),

  tags$style(type = "text/css", "html, body {width:100%; height:100%}"),

  h1("Tengen-app"),

  tags$div(
    "Club Tengen de Go ",
    tags$a(href="https://docs.google.com/forms/d/1iPusyVKjL9lTLXJ8PBK7oaNCWR6n8sT2tM0FybLwvVg/edit", "Registro de Partidas"),
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
                                       selectizeInput("player_name_Input_games", "player_name:",
                                                      sort(unique(mydf$player_name)),
                                                      selected=NULL, multiple =TRUE)),
                                column(6,
                                       uiOutput("opponent_name_Input_games")
                                )

                              ),

                              fluidRow(
                                column(6,
                                       selectizeInput("board_size_Input_games", "board_size:",
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

  output$opponent_name_Input_games <- renderUI({

    if (is.null(input$player_name_Input_games)==FALSE) {
      menudata <- mydf %>%
        filter(player_name %in% input$player_name_Input_games) %>%
        arrange(desc(timestamp))
    } else {
      menudata <- mydf %>%
        arrange(desc(timestamp))
    }

    selectizeInput("opponent_name_Input_games", "opponent_name:",
                   choices = c(unique(menudata$opponent_name)), multiple =TRUE)

  })


  # games -------------------------------------------------------------------

  games_data <- reactive({


    games_data <- mydf

    if (is.null(input$player_name_Input_games)==FALSE) {
      games_data <- games_data %>%
        filter(player_name %in% input$player_name_Input_games)

    }

    if (is.null(input$opponent_name_Input_games)==FALSE) {
      games_data <- games_data %>%
        filter(opponent_name %in% input$opponent_name_Input_games)

    }

    if (is.null(input$board_size_Input_games)==FALSE) {
      games_data <- games_data %>%
        filter(board_size %in% input$board_size_Input_games)

    }

    games_data

  })

  output$games_datatable <- DT::renderDataTable(DT::datatable({

    data <- games_data() %>%
      arrange(desc(timestamp))

    data

  }, escape = c(-9),
  style = "bootstrap"))

  # end of server -----------------------------------------------------------


}


# shinyapp ----------------------------------------------------------------



shinyApp(ui = ui, server = server)




#









