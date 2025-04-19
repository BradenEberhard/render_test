
test_text <- "
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed fermentum, metus ut cursus dapibus, risus nisl fringilla libero, eget malesuada ligula nulla et eros. Vivamus porttitor lorem sed neque blandit, id dapibus lorem elementum. Proin sit amet eros tincidunt, tincidunt est ac, pulvinar justo. Curabitur congue convallis diam a ultrices.

Integer sollicitudin nisl vel mi lacinia malesuada. Suspendisse vestibulum vitae justo sed feugiat. Etiam consequat mi a felis vulputate, ut varius augue tincidunt. Sed a libero nec nulla malesuada mattis. Fusce fermentum magna nec finibus viverra. Sed at nulla non quam dapibus imperdiet. Pellentesque vitae purus at nulla vestibulum pulvinar.

Maecenas gravida neque vel ante tincidunt, at vulputate magna tincidunt. Nulla facilisi. Aenean eget justo a nisl fringilla eleifend. Cras ut nisl mauris. Suspendisse lobortis nisi id luctus hendrerit. Integer quis orci sem. Pellentesque sit amet semper nulla. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae.

Sed eu metus at nisi volutpat sagittis. Praesent ac nisi nec ipsum dignissim tempor. Nunc lacinia, sapien nec euismod rutrum, lectus velit commodo lorem, et convallis velit justo vel lacus. Duis fermentum ante sit amet neque vehicula dapibus. Donec congue lorem a turpis imperdiet volutpat.

Curabitur vestibulum tristique nulla, sit amet pulvinar ex volutpat at. Nulla eget tortor sed nunc vehicula malesuada. Morbi sed sapien sit amet orci facilisis tincidunt. Nam volutpat porttitor augue. Cras sit amet ligula felis. Fusce sodales vestibulum arcu ac volutpat. Donec malesuada fermentum diam, ut congue purus sodales nec.
"
# team_message <- Sys.getenv("DB_HOST", unset = "Default message")

shinyUI(
  tagList(
    # Favicon in the head
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "result.png"),
      tags$style(HTML('
      .card {
        border: 0 !important;        
        border-radius: 0 !important; 
        box-shadow: none !important;
      }
    '))
    ),



    # Main UI layout
    page_navbar(
      title = span(
        img(src = "logo.png", height = 40),
        strong(" Comanalysis") # Added a space for better visual separation
      ),
      padding = 0,
      theme = bs_theme(brand = TRUE),
      navbar_options = navbar_options(
        bg = abc_black,
        position = "static-top"
      ),
      nav_spacer(),

      nav_panel(
        title = "Home", align = "left", p("home content"),
        div(
          class = "container",
          style = "
            max-width: 700px;
            margin: 2rem auto;
            font-size: 1.1rem;
            line-height: 1.6;
            color: #333;
          ",
          p(test_text)
        )

      ),
      nav_panel(title = "Game Center", align = "left", p("Game content")),
      # nav_panel(title = "Teams", align = "left", p(team_message)),
      nav_panel(title = "Players", align = "left", page_navbar(
        position = "static-top",
        bg = abc_gray,
        nav_panel(title = "leaderboard", align = "left", mod_player_leaderboard_ui("player_leaderboard"))
      )),

      nav_spacer(),
      nav_item(tags$a(fa(name = "bluesky"), href = "https://twitter.com/Comet_Miller")),
      nav_item(tags$a(fa(name = "newspaper"), href = "https://analysisbycomet.substack.com/"))
    )
  )
)