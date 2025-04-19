
function(input, output, session) {
  mod_player_leaderboard_server("player_leaderboard", all_player_stats)
  observeEvent(input$go_home, {
    # Change the active panel to "Home" when the logo is clicked
    updateNavbarPage(session, "page_navbar", selected = "Home")
  })
  pool <- get_db_pool()
  all_player_stats <- reactiveVal(NULL)
  loading_stats <- reactiveVal(TRUE) # To track loading state

  # Start the database query in the background
  future({
    get_all_player_stats(pool)
  }) %...>% {
    all_player_stats(.)
    loading_stats(FALSE) # Set loading to false when done
  } %...!% {
    # Handle potential errors here
    showNotification(paste("Error loading player stats:", .), type = "error")
    loading_stats(FALSE) # Ensure loading state is updated on error
  } 
  
}