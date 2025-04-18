### UI functions
# card formatting for grade
generate_grade_card <- function(ns, category, role) {
  card(
    class = "mx-0 px-0 ml-0 pl-0 text-center", 
    card_header(paste0(category, ":"), style = "padding: 5px; margin-bottom: 0 px;"),
    div(
      style = "display: flex; justify-content: center; align-items: baseline; gap: 10px; padding: 0;",
      h2(
        textOutput(ns(paste0(role, "_", tolower(category), "_grade"))),
        style = "margin: 0;"  # Remove extra space around the grade text
      ),
      div(
        textOutput(ns(paste0(role, "_", tolower(category), "_percentile"))),
        style = "font-size: smaller; margin: 0;"  # Make the percentile smaller
      )
    )
  )
}

# UI for grade panel
generate_grade_panel <- function(ns, role, grade_categories) {
  bslib::accordion_panel(
    title = paste0(tools::toTitleCase(role), " Grade:"),
    id = ns(paste(role, "_grade")),  # ID for this accordion item
    page_fluid(
      layout_column_wrap(
        fillable = FALSE,
        div(
          fluidRow(
            generate_grade_card(ns, "Overall", role)
          ),
          fluidRow(
            layout_column_wrap(
              class = "mx-0 px-0",
              width = 1/2,
              !!!lapply(grade_categories, function(category) generate_grade_card(ns, category, role))
            )
          )
        ),
        plotOutput(ns(paste0(role, "_radial_histogram_plot"))) |> withSpinner() |> bslib::as_fill_carrier(),
        width = 1/2
      ),
      layout_columns(
        !!!lapply(grade_categories, function(category) {
          girafeOutput(ns(paste0(role, "_", tolower(category), "_plot"))) |> withSpinner() |> bslib::as_fill_carrier()
        })
      )      
    )
  )
}



# Logic to update years with a new player
update_year_selector <- function(player_selector, all_player_stats, session) {
  req(player_selector)
  stats <- all_player_stats %>% filter(.data$full_name == player_selector)
  valid_years <- stats$Year[stats$Year >= 2021]
  updateSelectInput(session, "year_selector", 
    choices = sort(valid_years), 
    selected = max(valid_years))
}

# Logic for switches
create_switch <- function(input_name, condition, true_label, false_label, selected_player_stats, ns) {
  req(selected_player_stats())  # Ensure selected_player_stats is available
  label <- ifelse(condition, true_label, false_label)
  input_switch(id = ns(input_name), label = label, value = FALSE)
}


### Server functions

# filters a player stats df, reformats and renames it
convert_to_metric_df <- function(input, df, all_metrics, selected_player=NULL, all_years=FALSE) {
  if (is.null(selected_player)) {
    selected_player <- input$player_selector
  }
  handler_value <- ifelse(is.null(input$handler_switch_value), FALSE, input$handler_switch_value)
  offense_value <- ifelse(is.null(input$offense_switch_value), FALSE, input$offense_switch_value)
  if (!all_years) {
    df <- adjust_for_year(df, input$year_selector)
  }
  df <- adjust_for_role(input, df)

  metric_data <- list()
  if (all_years) {

    unique_years <- setdiff(unique(df$Year), "Career")
    metric_data <- lapply(unique_years, function(Year) {
      year_df <- adjust_for_year(df, Year) # Adjust the data for the specific year
      lapply(all_metrics, function(metric) {
        result <- process_metric(metric, year_df, selected_player)
        if (!is.null(result)) {
          result$Year <- Year
          return(result)
        }
      })
    })
    # Flatten the nested list structure
    metric_data <- do.call(c, metric_data)
  } else {
    metric_data <- lapply(all_metrics, function(metric) {
      result <- process_metric(metric, df, selected_player)
      if (!is.null(result)) {
        return(result)
      }
    })
  }
  return(do.call(rbind, lapply(metric_data, function(x) data.frame(x, stringsAsFactors = FALSE))))
}


# Fetch selected player stats
get_selected_player_stats <- function(player_selector, year_selector, all_player_stats) {
  all_player_stats %>%
    filter(.data$full_name == player_selector & .data$Year == year_selector)
}

### Plots 

# ggiraph plots for percentiles over years
#' @importFrom ggiraph geom_line_interactive geom_segment_interactive geom_text_interactive geom_point_interactive girafe girafe_options opts_hover opts_hover_inv opts_toolbar opts_sizing opts_selection
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales comma
generate_yearly_percentile_plot <- function(metric_df, title) {
  metric_df$Year <- as.numeric(metric_df$Year)
  metric_df <- metric_df %>% filter(Year >= 2021)
  metric_df <- metric_df %>% filter(!is.na(value)) %>%
    filter(!(metric_full_name %in% c("Games Played", "Throwing Yards Per 100 Possessions", "Receiving Yards Per 100 Possessions", "Defensive Points Per Game", "Receptions Per 100 Possessions", "Offensive Points Per Game") & Year < 2021))

  last_points <- metric_df %>%
    group_by(.data$metric) %>%
    filter(.data$Year == max(.data$Year))
  text_repel_offset <- ifelse(length(unique(metric_df$Year)) > 1, (max(metric_df$Year) - min(metric_df$Year)) / 10, 0)

  # Create the ggplot
  plot <- ggplot(
    metric_df, 
    aes(
      x = .data$Year, 
      y = .data$percentile, 
      color = .data$metric, 
      group=.data$metric,
      data_id = .data$metric
    )
  ) +
    geom_line_interactive(aes(tooltip = .data$metric_full_name), size = 1.2) +
    geom_point_interactive(aes(tooltip = paste(.data$metric_abbreviation, ": ", round(.data$value,2), "\nPercentile: ", .data$percentile)), size = 3) +
    geom_text_repel(
      data=last_points,
      aes(color = .data$metric, label = ifelse(.data$metric_abbreviation %in% c("C/100P", "R/100P", "B/100P"), 
      .data$metric_abbreviation, 
        gsub("/100P", "",  gsub("/GP", "", .data$metric_abbreviation)))),  
      family = "Lato",  # Adjust the font family
      size = 8,
      direction = "y",  # Direction of text (either 'x', 'y', or 'both')
      xlim = c(max(metric_df$Year) + text_repel_offset, NA),  # Limit for x-axis labels
      hjust = 0,
      vjust = 0,
      segment.size = 0.7,
      segment.alpha = 0.5,
      segment.linetype = "dotted",
      box.padding = 0.4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20
    ) +
    labs(title = title, x = "Year", y = "Percentile") +
    scale_y_continuous(limits = c(0, 102), breaks = seq(0, 100, 20)) +
    scale_x_continuous(
      limits = if (length(unique(metric_df$Year)) > 1) {
        c(min(metric_df$Year), max(metric_df$Year) + 1)
      } else {
        c(min(metric_df$Year), max(metric_df$Year))
      },
      breaks = seq(min(metric_df$Year), max(metric_df$Year), 1)
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(10, 50, 10, 10)
    ) + coord_cartesian(clip = "off")
  
  # Convert to interactive plot using ggiraph
  interactive_plot <- girafe(ggobj = plot)
  interactive_plot <- girafe_options(
    interactive_plot,
    opts_hover(
      css = "stroke:attr(color); stroke-width:4px; r:4px; transition: all 0.1s ease;"
    ),
    opts_hover_inv(css = "opacity:0.5; filter:saturate(10%);"),
    opts_toolbar(saveaspng = FALSE, hidden = c("selection")),
    opts_selection(type = "none")
  )

  return(interactive_plot)
}

# Plot for main skill percentiles
#' @importFrom tidyr drop_na
create_skill_percentiles_plot <- function(stat_category, plot_data) {
  title_suffix <- ifelse(stat_category == "Total", "", ifelse(stat_category == "Per Game", "Per Game", "Per 100 Possessions"))
  plot_data$metric_abbreviation <- sub("/.*", "", plot_data$metric_abbreviation)
  plot_data <- plot_data %>% drop_na()

  gg <- ggplot(plot_data, aes(
    x = percentile, 
    y = reorder(metric_abbreviation, percentile)
  )) +
    geom_segment_interactive(
      aes(
        x = 0, 
        xend = percentile, 
        yend = reorder(metric_abbreviation, percentile),
        tooltip = paste(
          metric_abbreviation, ":", 
          sub("\\.0+$", "", comma(value, accuracy = 0.01)), 
          "<br>Percentile:", percentile
        )
      ),
      color = "black", size = 1
    ) +
  geom_point_interactive(
    aes(
      tooltip = paste(
        metric_abbreviation, ":", 
        sub("\\.0+$", "", comma(value, accuracy = 0.01)), 
        "<br>Percentile:", percentile
      )
    ),
    size = 3, color = "black"
  ) +
    geom_text_interactive(
      aes(
        x = -2,  
        label = metric_abbreviation,
        tooltip = paste(metric_full_name, ":\n", metric_description)  # Custom tooltip for labels
      ),
      hjust = 1, size = 5, color = "black"
    ) +
    labs(x = "Percentile", y = paste("Metric", title_suffix)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 14),
      axis.text.y = element_blank(),
      axis.title.y = element_text(margin = margin(r = 25)),
      panel.grid.major.y = element_blank()
    ) + coord_cartesian(clip="off") +
      scale_x_continuous(limits=c(NA, 100))
  
  interactive_plot <- girafe(ggobj = gg, width_svg = 6, height_svg = 4)
  interactive_plot <- girafe_options(
    interactive_plot,
    opts_hover(
      css = "stroke:black; stroke-width:20px; r:20px; transition: all 0.1s ease;"
    ),
    opts_hover_inv(css = "opacity:0.5; filter:saturate(10%);"),
    opts_toolbar(saveaspng = FALSE, hidden = c("selection")),
    opts_selection(type = "none")
  )

  return(interactive_plot)
}

# radial histogram plot
radial_histogram_plot <- function(passes, bin_width = 24, role = "Thrower") {
  bin_cutoffs <- seq(-180, 180, by = bin_width)
  max_y <- max(table(cut(passes, breaks = bin_cutoffs)))

  ggplot(data.frame(passes), aes(x = passes)) +
    geom_histogram(breaks = bin_cutoffs, fill = "#052e85", color = "white", boundary = 0) +
    coord_polar(start = pi, clip = "off") +
    scale_x_continuous(limits = c(-180, 180), breaks = seq(-180, 180, 45)) +
    theme_minimal() +
    annotate("text", x = 0, y = max_y+5, label = "Forward", size = 5, hjust=0.5, vjust=0) +
    annotate("text", x = -90, y = max_y+5, label = "Left", size = 5, hjust=1, vjust=0.5) +
    annotate("text", x = 90, y = max_y+5, label = "Right", size = 5, hjust=0, vjust=0.5) +
    annotate("text", x = 180, y = max_y+5, label = "Backward", size = 5, hjust=0.5, vjust=1) +
    theme(
      axis.title.y = element_text(size = 14, margin = margin(r = 10), face = "bold"),  # Adjust y-axis label size
      axis.text.y = element_text(size = 12, margin = margin(r = 20)),
      axis.title.x = element_text(size = 14, face = "bold"),  # Adjust y-axis label size
      axis.text.x = element_blank(),  # Hides angle labels
      axis.ticks = element_blank(),
      plot.title = element_text(size = 18, face = "bold"), 
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)  
    ) +
    labs(
      title = paste0(tools::toTitleCase(role), " Tendencies"),
      x = "Angle",
      y = "Number of Passes"
    )
}

### Helper Functions

# calculates percentils
calc_percentile <- function(player_value, all_values) {
  return(mean(all_values <= player_value, na.rm = TRUE) * 100)
}

# gets per possession or game metric names based on input
get_metrics <- function(category) {
  addition <- ifelse(category == "Per Possession", "_per_possession", 
                     ifelse(category == "Per Game", "_per_game", ""))
  counting_metrics <- c("goals", "assists", "blocks", "completions", "hockeyAssists", 
                        "yardsThrown", "yardsReceived", "thrower_aec", "receiver_aec", "plus_minus")
  counting_metrics <- paste0(counting_metrics, addition)
  percentage_metrics <- c("offensive_efficiency", "offensive_efficiency_involved", "offensive_involvement", "defensive_efficiency", "involved_efficiency_improvement", "completion_percentage", "cpoe", "xcp", "offensive_efficiency_above_replacement")
  return(c(counting_metrics, percentage_metrics))
}

# filters df by selected year
adjust_for_year <- function(df, year) {
  return(df[df$Year == year, ])
}

# filters df by selected role (handler, cutter, defense, offense) if selected
adjust_for_role <- function(input, df) {
  handler_value <- ifelse(is.null(input$handler_switch_value), FALSE, input$handler_switch_value)
  offense_value <- ifelse(is.null(input$offense_switch_value), FALSE, input$offense_switch_value)
  if (handler_value) {
    player_handler <- df %>% filter(.data$full_name == input$player_selector  & .data$Year == input$year_selector) %>% pull(.data$handler)
    df <- df %>% filter(.data$handler == player_handler | .data$full_name == input$player_selector)
  }
  if (offense_value) {
    player_offense <- df %>% filter(.data$full_name == input$player_selector  & .data$Year == input$year_selector) %>% pull(.data$offense)
    df <- df %>% filter(.data$offense == player_offense | .data$full_name == input$player_selector)
  }
  return(df)
}

# function to rename metrics and reformat metric dataframe
process_metric <- function(metric, df, player_full_name) {
  percentage_metrics <- c("offensive_efficiency", "defensive_efficiency", "completion_percentage", "cpoe", "xcp")
  metric_info <- map_metrics_to_formula(df, list(metric))[[metric]]
  player_value <- map_metrics_to_formula(df[df$full_name == player_full_name,], list(metric))[[metric]]$value
  df_final_players <- filter_for_eligible_players(df)
  if (length(player_value) > 0) {
    percentile_value <- calc_percentile(player_value, metric_info$value) %>% round(2)
    if ("turnover" %in% metric) {
      percentile_value <- 100 - percentile_value
    }
    # Return the metric data along with its full name and abbreviation
    return(list(
      metric = metric,
      percentile = percentile_value,
      value = player_value,
      metric_full_name = metric_info$display_name,
      metric_abbreviation = metric_info$abbreviation,
      metric_description = metric_info$description
    ))
  }
  return(NULL)
}

# make sure players have some stats to be used during percentile calculations
filter_for_eligible_players <- function(df) {
  return(df %>% filter(.data$Games >= 3))
}

#  adds full_name column to player_stats df
get_playerID_by_fullName <- function(input, data) {
  # Filter the dataframe by full_name
  filtered_data <- data %>%
    filter(.data$full_name == input$player_selector) %>%
    slice_head(n = 1)  # Get the first row
  
  # Return the playerID from the first row, or NA if no match found
  if (nrow(filtered_data) > 0) {
    return(filtered_data$PlayerID)
  } else {
    return(NA)  # Return NA if fullName does not exist
  }
}

### Config

# large config for metrics with how to calculate, what to name and their abbreviation
map_metrics_to_formula <- function(df, metric_names) {
  metric_map <- list(
    ## Overall
    "Receptions" = list(
      formula = function(df) df$Catches,
      display_name = "Receptions",
      abbreviation = "R",
      description = "Total number of catches"
    ),
    "Drops" = list(
      formula = function(df) df$Drops,
      display_name = "Drops",
      abbreviation = "D",
      description = "Total number of drops"
    ),
    "receiver_ec" = list(
      formula = function(df) df$receiver_ec,
      display_name = "Receiver Expected Contribution",
      abbreviation = "R-EC",
      description = "Contribution towards a score from passes caught"
    ),
    "receiver_aec" = list(
      formula = function(df) df$receiver_aec,
      display_name = "Receiver Adjusted Expected Contribution",
      abbreviation = "R-aEC",
      description = "Contribution towards a score from passes caught adjusted for comparison to goals"
    ),
    "plus_minus" = list(
      formula = function(df) df$Goals + df$Assists + df$Blocks + (df$HockeyAssists * 0.5 ) - df$Throwaways - df$Stalls - df$Drops,
      display_name = "Plus Minus",
      abbreviation = "PM",
      description = "goals + assists + blocks + hockey assists/2 - throwaways - stalls - drops"
    ),
    "Assists" = list(
      formula = function(df) df$Assists,
      display_name = "Assists",
      abbreviation = "A",
      description = "Total number of assists"
    ),
    "HockeyAssists" = list(
      formula = function(df) df$HockeyAssists,
      display_name = "Hockey Assists",
      abbreviation = "HA",
      description = "Total number of hockey assists"
    ),
    "Goals" = list(
      formula = function(df) df$Goals,
      display_name = "Goals",
      abbreviation = "G",
      description = "Total number of goals scored"
    ),
    "Blocks" = list(
      formula = function(df) df$Blocks,
      display_name = "Blocks",
      abbreviation = "B",
      description = "Total number of blocks"
    ),
    "thrower_ec" = list(
      formula = function(df) df$thrower_ec,
      display_name = "Thrower Expected Contribution",
      abbreviation = "T-EC",
      description = "Contribution towards a score from thrown passes"
    ),
    "thrower_aec" = list(
      formula = function(df) df$thrower_aec,
      display_name = "Thrower Adjusted Expected Contribution",
      abbreviation = "T-aEC",
      description = "Contribution towards a score from thrown passes adjusted for comparison to assists"
    ),
    "total_aec" = list(
      formula = function(df) df$thrower_aec + df$receiver_aec,
      display_name = "Total Adjusted Expected Contribution",
      abbreviation = "Tot-aEC",
      description = "Contribution towards a score from both thrown passes and receptions"
    ),
    "YardsThrown" = list(
      formula = function(df) df$YardsThrown,
      display_name = "Throwing Yards",
      abbreviation = "TY",
      description = "Total yards thrown"
    ),
    "YardsReceived" = list(
      formula = function(df) df$YardsReceived,  
      display_name = "Receiving Yards",
      abbreviation = "RY",
      description = "Total yards received"
    ),
    "Games" = list(
      formula = function(df) df$Games,  
      display_name = "Games Played",
      abbreviation = "GP",
      description = "Total number of games played"
    ),
    "Completions" = list(
      formula = function(df) df$Completions,  
      display_name = "Completions",
      abbreviation = "C",
      description = "Total number of successful pass completions"
    ),
    "OOpportunities" = list(
      formula = function(df) df$OOpportunities,  
      display_name = "Possessions",
      abbreviation = "P",
      description = "Total number of offensive possessions"
    ),
    "DOpportunities" = list(
      formula = function(df) df$DOpportunities,  
      display_name = "Defensive Possessions",
      abbreviation = "DP",
      description = "Total number of defensive possessions"
    ),
    "OPointsPlayed" = list(
      formula = function(df) df$OPointsPlayed,  
      display_name = "Offensive Points",
      abbreviation = "OP",
      description = "Total offensive points played"
    ),
    "DPointsPlayed" = list(
      formula = function(df) df$DPointsPlayed,  
      display_name = "Defensive Points",
      abbreviation = "DP",
      description = "Total defensive points played"
    ),
    "turnovers" = list(
      formula = function(df) df$Throwaways + df$Drops + df$Stalls,
      display_name = "Turnovers",
      abbreviation = "Turns",
      description = "Average turnovers per 100 offensive possessions."
    ),
    ## Rate Based
    "xcp" = list(
      formula = function(df) df$xcp * 100,  
      display_name = "Expected Completion Percentage",
      abbreviation = "xCP",
      description = "Percentage of passes expected to be completed based on throw risk"
    ),
    "cpoe" = list(
      formula = function(df) df$cpoe * 100,  
      display_name = "Completion Percentage Over Expected",
      abbreviation = "CPOE",
      description = "Difference between actual completion percentage and expected based on throw risk"
    ),
    "completion_percentage" = list(
      formula = function(df) (df$Completions / df$ThrowAttempts) * 100,
      display_name = "Completion Percentage",
      abbreviation = "CP",
      description = "Percentage of passes completed"
    ),
    "offensive_efficiency" = list(
      formula = function(df) (df$num_possessions_scored / df$num_possessions) * 100,  
      display_name = "Offensive Efficiency",
      abbreviation = "OE",
      description = "Offensive success rate, based on percentage of scores vs opportunities"
    ),
    "offensive_efficiency_involved" = list(
      formula = function(df) (df$num_possessions_involved_scored / df$num_possessions_involved) * 100,  
      display_name = "Involved Offensive Efficiency",
      abbreviation = "IOE",
      description = "Involved offensive success rate, based on percentage of scores vs opportunities counting only possessions with one throw or catch"
    ),
    "involved_efficiency_improvement" = list(
      formula = function(df) ((df$num_possessions_involved_scored / df$num_possessions_involved) - (df$num_possessions_scored / df$num_possessions)) * 100,  
      display_name = "Involved Efficiency Improvement",
      abbreviation = "IEI",
      description = "Improvement in offensive efficiency when involved in the play"
    ),
    "offensive_involvement" = list(
      formula = function(df) (df$num_possessions_involved / df$num_possessions) * 100,  
      display_name = "Offensive Involvement",
      abbreviation = "OI",
      description = "Involvement in offensive possessions, based on number of possessions with at least one throw/catch per total possessions"
    ),
    "offensive_efficiency_above_replacement" = list(
      formula = function(df) df$OE_adjusted * 100,  
      display_name = "Offensive Efficiency Above Replacement",
      abbreviation = "OE-AR",
      description = "Involved offensive success rate, based on percentage of scores vs opportunities counting only possessions with one throw or catch"
    ),
    "defensive_efficiency" = list(
      formula = function(df) (df$num_d_possessions_stopped / df$num_d_possessions) * 100,  
      display_name = "Defensive Efficiency",
      abbreviation = "DE",
      description = "Defensive success rate, based on percentage of stops vs opportunities"
    ),
    ## Per Game
    "receptions_per_game" = list(
      formula = function(df) df$Catches / df$Games,
      display_name = "Receptions Per Game",
      abbreviation = "R/GP",
      description = "Average number of receptions per game."
    ),
    "drops_per_game" = list(
      formula = function(df) df$Drops / df$Games,
      display_name = "Drops Per Game",
      abbreviation = "D/GP",
      description = "Average number of drops per game."
    ),
    "receiver_ec_per_game" = list(
      formula = function(df) df$receiver_ec / df$Games,
      display_name = "Receiver Expected Contribution Per Game",
      abbreviation = "R-EC/GP",
      description = "Average contribution towards a score from passes caught per game."
    ),
    "receiver_aec_per_game" = list(
      formula = function(df) df$receiver_aec / df$Games,
      display_name = "Receiver Adjusted Expected Contribution Per Game",
      abbreviation = "R-aEC/GP",
      description = "Average contribution towards a score from passes caught adjusted for comparison to goals per game."
    ),
    "turnovers_per_game" = list(
      formula = function(df) (df$Throwaways + df$Drops + df$Stalls) / df$Games,
      display_name = "Turnovers Per Game",
      abbreviation = "Turns/GP",
      description = "Average number of turnovers per game."
    ),
    "assists_per_game" = list(
      formula = function(df) df$Assists / df$Games,
      display_name = "Assists Per Game",
      abbreviation = "A/GP",
      description = "Average number of assists per game."
    ),
    "hockeyAssists_per_game" = list(
      formula = function(df) df$HockeyAssists / df$Games,
      display_name = "Hockey Assists Per Game",
      abbreviation = "HA/GP",
      description = "Average number of hockey assists per game."
    ),
    "goals_per_game" = list(
      formula = function(df) df$Goals / df$Games,
      display_name = "Goals Per Game",
      abbreviation = "G/GP",
      description = "Average number of goals per game."
    ),
    "blocks_per_game" = list(
      formula = function(df) df$Blocks / df$Games,
      display_name = "Blocks Per Game",
      abbreviation = "B/GP",
      description = "Average number of blocks per game."
    ),
    "thrower_ec_per_game" = list(
      formula = function(df) df$thrower_ec / df$Games,
      display_name = "Thrower Expected Contribution Per Game",
      abbreviation = "T-EC/GP",
      description = "Average contribution towards a score from thrown passes per game."
    ),
    "thrower_aec_per_game" = list(
      formula = function(df) df$thrower_aec / df$Games,
      display_name = "Thrower Adjusted Expected Contribution Per Game",
      abbreviation = "T-aEC/GP",
      description = "Average contribution towards a score from thrown passes adjusted for comparison to assists per game."
    ),
    "yardsThrown_per_game" = list(
      formula = function(df) df$YardsThrown / df$Games,
      display_name = "Throwing Yards Per Game",
      abbreviation = "TY/GP",
      description = "Average throwing yards per game."
    ),
    "completions_per_game" = list(
      formula = function(df) df$Completions / df$Games,
      display_name = "Completions Per Game",
      abbreviation = "C/GP",
      description = "Average number of completions per game."
    ),
    "defensive_possessions_per_game" = list(
      formula = function(df) df$DOpportunities / df$Games,
      display_name = "Defensive Possessions Per Game",
      abbreviation = "DP/GP",
      description = "Average number of defensive possessions per game."
    ),
    "yardsReceived_per_game" = list(
      formula = function(df) df$YardsReceived / df$Games,
      display_name = "Receiving Yards Per Game",
      abbreviation = "RY/GP",
      description = "Average receiving yards per game."
    ),
    "possessions_per_game" = list(
      formula = function(df) df$OOpportunities / df$Games,
      display_name = "Possessions Per Game",
      abbreviation = "P/GP",
      description = "Average number of offensive possessions per game."
    ),
    "plus_minus_per_game" = list(
      formula = function(df) (df$Goals + df$Assists + df$Blocks + (df$HockeyAssists * 0.5 ) - df$Throwaways - df$Stalls - df$Drops) / df$Games,
      display_name = "Plus Minus Per Game",
      abbreviation = "PM/GP",
      description = "Average plus-minus score per game."
    ),
    "offensive_points_per_game" = list(
      formula = function(df) df$OPointsPlayed / df$Games,
      display_name = "Offensive Points Per Game",
      abbreviation = "OP/GP",
      description = "Average number of offensive points per game."
    ),
    "defensive_points_per_game" = list(
      formula = function(df) df$DPointsPlayed / df$Games,
      display_name = "Defensive Points Per Game",
      abbreviation = "DP/GP",
      description = "Average number of defensive points per game."
    ),
    ## Per 100 Possessions
    "receptions_per_possession" = list(
      formula = function(df) df$Catches / df$Games,
      display_name = "Receptions Per 100 Possessions",
      abbreviation = "R/100P",
      description = "Average number of receptions per 100 offensive possessions."
    ),
    "drops_per_possession" = list(
      formula = function(df) (df$Drops / df$num_possessions) * 100,
      display_name = "Drops Per 100 Possessions",
      abbreviation = "D/100P",
      description = "Average number of drops per 100 offensive possessions."
    ),
    "receiver_ec_per_possession" = list(
      formula = function(df) (df$receiver_ec / df$num_possessions) * 100,
      display_name = "Receiver Expected Contribution Per 100 Possessions",
      abbreviation = "R-EC/100P",
      description = "Average contribution towards a score from catches per 100 offensive possessions."
    ),
    "receiver_aec_per_possession" = list(
      formula = function(df) (df$receiver_aec / df$num_possessions) * 100,
      display_name = "Receiver Adjusted Expected Contribution Per 100 Possessions",
      abbreviation = "R-aEC/100P",
      description = "Average contribution towards a score from catches, adjusted for comparison to goals per 100 offensive possessions."
    ),
    "thrower_ec_per_possession" = list(
      formula = function(df) (df$thrower_ec / df$num_possessions) * 100,
      display_name = "Thrower Expected Contribution Per 100 Possessions",
      abbreviation = "T-EC/100P",
      description = "Average contribution towards a score from thrown passes per 100 offensive possessions."
    ),
    "thrower_aec_per_possession" = list(
      formula = function(df) (df$thrower_aec / df$num_possessions) * 100,
      display_name = "Thrower Adjusted Expected Contribution Per 100 Possessions",
      abbreviation = "T-aEC/100P",
      description = "Average contribution towards a score from thrown passes, adjusted for comparison to assists per 100 offensive possessions."
    ),
    "yardsThrown_per_possession" = list(
      formula = function(df) (df$YardsThrown / df$num_possessions) * 100,
      display_name = "Throwing Yards Per 100 Possessions",
      abbreviation = "TY/100P",
      description = "Average throwing yards per 100 offensive possessions."
    ),
    "completions_per_possession" = list(
      formula = function(df) (df$Completions / df$num_possessions) * 100,
      display_name = "Completions Per 100 Possessions",
      abbreviation = "C/100P",
      description = "Average number of completions per 100 offensive possessions."
    ),
    "yardsReceived_per_possession" = list(
      formula = function(df) (df$YardsReceived / df$num_possessions) * 100,
      display_name = "Receiving Yards Per 100 Possessions",
      abbreviation = "RY/100P",
      description = "Average receiving yards per 100 offensive possessions."
    ),
    "assists_per_possession" = list(
      formula = function(df) (df$Assists / df$num_possessions) * 100,
      display_name = "Assists Per 100 Possessions",
      abbreviation = "A/100P",
      description = "Average assists per 100 offensive possessions."
    ),
    "hockeyAssists_per_possession" = list(
      formula = function(df) (df$HockeyAssists / df$num_possessions) * 100,
      display_name = "Hockey Assists Per 100 Possessions",
      abbreviation = "HA/100P",
      description = "Average hockey assists per 100 offensive possessions."
    ),
    "goals_per_possession" = list(
      formula = function(df) (df$Goals / df$num_possessions) * 100,
      display_name = "Goals Per 100 Possessions",
      abbreviation = "G/100P",
      description = "Average goals per 100 offensive possessions."
    ),
    "blocks_per_possession" = list(
      formula = function(df) (df$Blocks / df$num_d_possessions) * 100,
      display_name = "Blocks Per 100 Possessions",
      abbreviation = "B/100P",
      description = "Average blocks per 100 defensive possessions."
    ),
    "turnovers_per_possession" = list(
      formula = function(df) ((df$Throwaways + df$Drops + df$Stalls) / df$num_possessions) * 100,
      display_name = "Turnovers Per 100 Possessions",
      abbreviation = "Turns/100P",
      description = "Average turnovers per 100 offensive possessions."
    ),
    "plus_minus_per_possession" = list(
      formula = function(df) (df$Goals/df$num_possessions + df$Assists/df$num_possessions + df$Blocks/df$num_d_possessions + (df$HockeyAssists * 0.5)/df$num_possessions - df$Throwaways/df$num_possessions - df$Stalls/df$num_possessions - df$Drops/df$num_possessions) * 100,
      display_name = "Plus Minus Per 100 Possessions",
      abbreviation = "PM/100P",
      description = "Average plus-minus score per 100 offensive possessions."
    )
  )
  
  result <- list()
  
  # Loop through each metric name in the input list
  for (metric_name in metric_names) {
    if (metric_name %in% names(metric_map)) {
      # Retrieve the formula, display name, and abbreviation for the metric
      metric_info <- metric_map[[metric_name]]
      
      # Calculate the value of the metric using the formula
      metric_value <- metric_info$formula(df)
      
      # Add the result to the list
      result[[metric_name]] <- list(
        value = metric_value,
        display_name = metric_info$display_name,
        abbreviation = metric_info$abbreviation,
        description = metric_info$description
      )
    } else {
      # If the metric name doesn't exist in the map, return an error
      result[[metric_name]] <- list(error = "Metric not found")
    }
  }
  return(result)
}


# transforms numeric percentile to letter grades
get_letter_grade <- function(percentile) {
  if (percentile >= 90) {
    return("A+")
  } else if (percentile >= 85) {
    return("A")
  } else if (percentile >= 80) {
    return("A-")
  } else if (percentile >= 75) {
    return("B+")
  } else if (percentile >= 70) {
    return("B")
  } else if (percentile >= 65) {
    return("B-")
  } else if (percentile >= 60) {
    return("C+")
  } else if (percentile >= 55) {
    return("C")
  } else if (percentile >= 50) {
    return("C-")
  } else if (percentile >= 45) {
    return("D+")
  } else if (percentile >= 40) {
    return("D")
  } else if (percentile >= 35) {
    return("D-")
  } else {
    return("F")
  }
}

