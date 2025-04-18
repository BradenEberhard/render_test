# Helper function to format the datatable
format_dt <- function(metric_table) {
  if (is.null(metric_table)) {
    # Return a message or empty data frame when the table is empty
    return(DT::datatable(
      data.frame(Message = "No data available"),
      rownames = FALSE,
      colnames = "Message",
      options = list(
        pageLength = 1, 
        autoWidth = TRUE,
        select = list(
          style = "os",
          selector = 'tr>td:nth-child(1)'
        )
      ),
      extensions = c("Select"),
      selection = 'none',
      class = "compact"
    ) %>%
      DT::formatStyle(
        columns = "Message", 
        fontSize = '12px', 
        textAlign = 'center'
      )
    )
  }

  column_mapping <- c(
    full_name = "Name",
    thrower_aec = "T-aEC",
    thrower_aec_percentile = "T-aEC %",
    receiver_aec = "R-aEC",
    receiver_aec_percentile = "R-aEC %",
    Assists = "Assists",
    Assists_percentile = "Assists %",
    Goals = "Goals",
    Goals_percentile = "Goals %",
    Blocks = "Blocks",
    Blocks_percentile = "Blocks %",
    cpoe = "CPOE",
    cpoe_percentile = "CPOE %",
    turnovers = "Turnovers",
    turnovers_percentile = "Turnovers %",
    defensive_efficiency = "DE",
    defensive_efficiency_percentile = "DE %",
    offensive_efficiency = "OE",
    offensive_efficiency_percentile = "OE %",
    offensive_involvement = "O-Involvement",
    offensive_involvement_percentile = "O-Involvement %",
    total_aec = "Total aEC",
    total_aec_percentile = "Total aEC %"
  )

  colnames(metric_table) <- column_mapping[colnames(metric_table)]

  DT::datatable(
    metric_table,
    rownames = FALSE,
    options = list(
      pageLength = 10, 
      autoWidth = TRUE,
      select = list(
        style = "os",
        selector = 'tr>td:nth-child(1)'
      )
    ),
    extensions = c("Select"),
    selection = 'none',
    class = "compact"
  ) %>%
    DT::formatStyle(
      columns = colnames(metric_table), 
      fontSize = '12px', 
      lineHeight = '70%'
    ) %>%
    DT::formatStyle(
      columns = "Name", # Apply style only to the "Name" column
      cursor = "pointer",
      `white-space` = "nowrap", # Prevent text wrapping
      `overflow` = "hidden",    # Hide overflowing text
      `text-overflow` = "ellipsis", # Display ellipses for overflowed text
      `textDecoration` = "underline"
    )
}



get_thrower_table <- function(input, all_player_stats) {
  filtered_stats <- all_player_stats %>% filter(Year == "2024")
  filtered_stats <- filtered_stats %>% filter(num_possessions >= input$min_possessions_thrower)
  filtered_stats <- add_percentiles(filtered_stats)

  metric_table <- filtered_stats %>% select(full_name, thrower_aec, thrower_aec_percentile, Assists, Assists_percentile, cpoe, cpoe_percentile, turnovers, turnovers_percentile)
  if(nrow(metric_table) == 0) {
    return(NULL)
  }
  metric_table <- metric_table %>%
    mutate(turnovers_percentile = 100 - turnovers_percentile) %>%
    mutate(across(where(is.numeric), \(x) round(x, 1)))
  return(metric_table)
}

get_receiver_table <- function(input, all_player_stats) {
  filtered_stats <- all_player_stats %>% filter(Year == "2024")
  filtered_stats <- filtered_stats %>% filter(num_possessions >= input$min_possessions_receiver)
  filtered_stats <- add_percentiles(filtered_stats)

  metric_table <- filtered_stats %>% select(full_name, receiver_aec, receiver_aec_percentile, Goals, Goals_percentile)
  if(nrow(metric_table) == 0) {
    return(NULL)
  }
  metric_table <- metric_table %>%
    mutate(across(where(is.numeric), \(x) round(x, 1)))
  return(metric_table)
}

get_defense_table <- function(input, all_player_stats) {
  filtered_stats <- all_player_stats %>% filter(Year == "2024")
  filtered_stats <- filtered_stats %>% filter(num_possessions >= input$min_possessions_defense)
  filtered_stats <- add_percentiles(filtered_stats)

  metric_table <- filtered_stats %>% select(full_name, Blocks, Blocks_percentile, defensive_efficiency, defensive_efficiency_percentile)
  if(nrow(metric_table) == 0) {
    return(NULL)
  }
  metric_table <- metric_table %>%
    mutate(across(where(is.numeric), \(x) round(x, 1)))
  return(metric_table)
}

get_team_impact_table <- function(input, all_player_stats) {
  filtered_stats <- all_player_stats %>% filter(Year == "2024")
  filtered_stats <- filtered_stats %>% filter(num_possessions >= input$min_possessions_team_impact)
  filtered_stats <- add_percentiles(filtered_stats)

  metric_table <- filtered_stats %>% select(full_name, offensive_involvement, offensive_involvement_percentile, total_aec, total_aec_percentile, offensive_efficiency, offensive_efficiency_percentile)
  if(nrow(metric_table) == 0) {
    return(NULL)
  }
  metric_table <- metric_table %>%
    mutate(across(where(is.numeric), \(x) round(x, 1)))
  return(metric_table)
}