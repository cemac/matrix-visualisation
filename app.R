library(shiny)
source("helpers.R")

regions <- getOptions("Region")
transitions <- getOptions("Transition")

matvis <- function(title, level, region = "Africa") {
  group_cols <- c(
    "Region",
    "Transition",
    "Ad/Mit",
    "Intervention - Level 1"
  )
  if (level == 2) {
    group_cols <- c(group_cols,
                    "Intervention - Level 2")
  }
  data <- getFlatsheetData(level, region) %>%
    dplyr::nest_by(!!!syms(group_cols))
  list(title = title, data = data)
}

matvisOutput <- function(id) {
  el <- shiny::tags$div(
    id = id, class = "matvis",
    h1(id = sprintf("%s-matvis-value", id), class = "matvis-value"),
    p(id = sprintf("%s-matvis-title", id), class = "matvis-title"),
    shiny::tags$table(id = sprintf("%s-matvis-table", id), 
                      class = "matvis-table",
                      shiny::tags$tr(
                        shiny::tags$th("Region"),
                        shiny::tags$th("Transition"),
                        shiny::tags$th("Ad/Mit"),
                        shiny::tags$th("Intervention - Level 1"),
                        shiny::tags$th("Co-benefits")
                      ))
  )
  
  path <- normalizePath("assets")
  
  deps <- list(
    htmltools::htmlDependency(
      name = "matvis",
      version = "1.0.0",
      src = c(file = path),
      script = c("binding.js"),
      stylesheet = "styles.css"
    )
  )
  
  htmltools::attachDependencies(el, deps)
}

renderMatvis <- function(expr, env = parent.frame(), 
                        quoted = FALSE) {
  # Convert the expression + environment into a function
  func <- shiny::exprToFunction(expr, env, quoted)
  
  function(){
    func()
  }
}

ui <- fluidPage(
  
  # Application title
  titlePanel("Matrix Co-benefits Visualisation"),
  
  fluidRow(
    column(4,
           selectInput("region",
                       "Region",
                       regions)
    ),
    column(4,
           checkboxGroupInput("transition",
                              label = "Transition",
                              choices = transitions,
                              selected = transitions)
    ),
    column(4,
           selectInput("level",
                       "Intervention level",
                       1:2)
    )
  ),
  
  tabsetPanel(
    tabPanel("Level 1", matvisOutput("level1")),
    tabPanel("Level 2", matvisOutput("level2"))
  )
)

server <- function(input, output){
  output$level1 <- renderMatvis({
    matvis("Level 1", 1)
  })

  output$level2 <- renderMatvis({
    matvis("Level 2", 2)
  })
}

shinyApp(ui, server)