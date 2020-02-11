library(shiny)
library(shinyWidgets)
library(tidyverse)

options(shiny.autoreload = TRUE)

df <- read_csv(list.files(pattern = ".csv")[[1]])
levels <- c("All levels", distinct(df, Level) %>% arrange(Level) %>% pull(Level))
ssas <- c("All sector subject areas", distinct(df, `Sector Subject Area`) %>%
  arrange(`Sector Subject Area`) %>%
  pull(`Sector Subject Area`))

pts <- list("All qualifications",
            `Included in performance tables` = "Yes",
            `Not included in performance tables` = "No")
types <- list("All qualification types" = "all",
             "Excluding GCSE, AS and A levels" = "excl")

ui <- fluidPage(
  h3("This interactive visualisation shows the current qualification landscape for qualifications regulated by Ofqual"),
  br(),
  p("The availability of qualifications is obtained by blah blah blah"),
  fluidRow(
    column(4, selectInput("level", "Qualification level", levels, width = "100%")),
    column(4, selectInput("ssa", "Sector subject area:", ssas, width = "100%")),
    column(4, selectInput("pt", "Performance tables:", pts, width = "100%"))
  ),
  br(),
  fluidRow(
    column(4, selectInput("type", "Qualification type:", types, width = "100%")),
    column(4, selectInput("order", "Order by:" , c("Certificates", "Available qualifications"), width = "100%")),
    column(4, searchInput("search", "Search for term in qualification title:",
                          btnSearch = icon("search"), btnReset = icon("remove"),
                          width = "100%"))
  ),
  fluidRow(
    plotOutput("plot", width = "100%", height = "590px")
  ),
  br(),
  fluidRow(
    DT::dataTableOutput("table")
  ),
  br(),
  fluidRow(
    downloadButton("filtered_table", label = "Download this table")
  ),
  fluidRow(
    p("Made by Mark for James")
  ),
  tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 950px; }"))
)