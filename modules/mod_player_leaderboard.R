#' player_leaderboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_player_leaderboard_ui <- function(id) {
  ns <- NS(id)
  tags$style(HTML('.card {border: none;}'))
  tagList(
    page_fluid(
      navset_card_tab(
        nav_panel("Thrower Stats",
          sliderInput(ns("min_possessions_thrower"), "Minimum Offensive Possessions", min = 0, max = 500, value = 50, step = 1),
          DT::dataTableOutput(ns("thrower_table")) |> withSpinner()
        ),
        nav_panel("Defense Stats",
          sliderInput(ns("min_possessions_defense"), "Minimum Defensive Possessions", min = 0, max = 500, value = 50, step = 1),
          DT::dataTableOutput(ns("defense_table")) |> withSpinner()
        ),
        nav_panel("Receiver Stats",
        sliderInput(ns("min_possessions_receiver"), "Minimum Offensive Possessions", min = 0, max = 500, value = 50, step = 1),
          DT::dataTableOutput(ns("receiver_table")) |> withSpinner()
        ),
        nav_panel("Team Impact Stats",
        sliderInput(ns("min_possessions_team_impact"), "Minimum Offensive Possessions", min = 0, max = 500, value = 50, step = 1),
          DT::dataTableOutput(ns("team_impact_table")) |> withSpinner()
        )
      )
    )
  )
}
    
#' player_leaderboard Server Functions
#'
#' @noRd 
mod_player_leaderboard_server <- function(id, all_player_stats){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    player_link_name <- reactiveValues(player_name = "")

    cached_metric <- reactiveVal(NULL)
    observeEvent(input$metric_selector, {
      if(input$metric_selector != ""){
        cached_metric(input$metric_selector)
      }
    })

    thrower_table <- reactiveVal(NULL)
    receiver_table <- reactiveVal(NULL)
    defense_table <- reactiveVal(NULL)
    team_impact_table <- reactiveVal(NULL)
    
    observe({
      req(input$min_possessions_thrower, all_player_stats())
      thrower_table(get_thrower_table(input, all_player_stats()))
    })

    observe({
      req(input$min_possessions_receiver, all_player_stats())
      receiver_table(get_receiver_table(input, all_player_stats()))
    })

    observe({
      req(input$min_possessions_defense, all_player_stats())
      defense_table(get_defense_table(input, all_player_stats()))
    })

    observe({
      req(input$min_possessions_team_impact, all_player_stats())
      team_impact_table(get_team_impact_table(input, all_player_stats()))
    })

    output$team_impact_table <- DT::renderDT(server=FALSE, {
      req(input$min_possessions_team_impact, team_impact_table())
      format_dt(team_impact_table())
    })

    output$defense_table <- DT::renderDT(server=FALSE, {
      req(input$min_possessions_defense, defense_table())
      format_dt(defense_table())
    })

    output$receiver_table <- DT::renderDT(server=FALSE, {
      req(input$min_possessions_receiver, receiver_table())
      format_dt(receiver_table())
    })

    output$thrower_table <- DT::renderDT(server=FALSE, {
      req(input$min_possessions_thrower, thrower_table())
      format_dt(thrower_table())
    })


    observe({
      selected_row <- input$thrower_table_rows_selected
      if (length(selected_row) > 0) {
        selected_name <- thrower_table()[selected_row, "full_name"]
        player_link_name$player_name <- selected_name
      }
    })

    return(reactive(player_link_name$player_name))

  })
}
