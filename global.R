# Welcome to the Comanalysis website

# Dependencies ------------------------------------------------------------

library(shiny)
library(bslib)
library(fontawesome)
library(tidyverse)
library(shinyWidgets)
library(sysfonts)
library(showtext)
library(ggiraph)
library(ggimage)
library(shinycssloaders)
library(dotenv)
library(pool)
library(RPostgres)
library(glue)
library(future)
library(promises)
library(DT)

# Setup -------------------------------------------------------------------
font_add("Azeret Mono", "./fonts/AzeretMono-Regular.ttf")
font_add("Mina", "./fonts/Mina-Regular.ttf")

showtext_auto()

theme_set(theme_minimal(
  base_family = "Mina"
))

asp_ratio <- 1.618

## Colors

abc_white <- "#FCFAFA"
abc_black <- "#1B1E26"
abc_blue <- "#033860"
abc_teal <- "#62929E"
abc_gold <- "#E6AF2E"
abc_gray <- "#6E8387"

# Source Files ------------------------------------------------------------
# Source functions from the utils folder
# list.files("utils", pattern = "\\.R$", full.names = TRUE) %>%
#   purrr::walk(source)

# # Source functions from the fcts folder
# list.files("fcts", pattern = "\\.R$", full.names = TRUE) %>%
#   purrr::walk(source)

# # Source module logic from the modules folder
# list.files("modules", pattern = "\\.R$", full.names = TRUE) %>%
#   purrr::walk(source)

# # Source module logic from the proxies folder
list.files("proxies", pattern = "\\.R$", full.names = TRUE) %>%
  purrr::walk(source)

# Data --------------------------------------------------------------------

load_dot_env()

# Playground --------------------------------------------------------------


