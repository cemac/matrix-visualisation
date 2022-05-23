library(shiny)
source("helpers.R")

regions <- getOptions("Region")
transitions <- getOptions("Transition")
ad_mit <- getOptions("Ad/Mit")

matvis <- function(title, level, input) {
  group_cols <- c(
    "Transition",
    "Ad/Mit",
    "Intervention - Level 1"
  )
  if (level == 2) {
    group_cols <- c(group_cols,
                    "Intervention - Level 2")
  }
  # Add co-benefit categories to grouping
  group_cols <- c(group_cols, "Co-benefit category")
  data <- getFlatsheetData(level, input$region) %>%
    dplyr::filter(Transition %in% input$transition,
                  `Ad/Mit` %in% input$ad_mit) %>%
    dplyr::nest_by(!!!syms(group_cols), .key = "Co-benefits") %>%
    tidyr::pivot_wider(names_from = `Co-benefit category`,
                       values_from = `Co-benefits`)

  # Combine intervention information
  data <- tidyr::unite(data,
                       "Intervention",
                       `Ad/Mit`,
                       `Intervention - Level 1`,
                       sep = ";")
  if (level == 2) {
    data <- tidyr::unite(data,
                         "Intervention",
                         Intervention,
                         `Intervention - Level 2`,
                         sep = ": ")
  }
  list(title = input$region, data = data, groups = group_cols)
}

matvisOutput <- function(id) {
  el <- shiny::tags$div(
    id = id, class = "matvis",
    h1(id = sprintf("%s-matvis-title", id), class = "matvis-title"),
    p(id = sprintf("%s-matvis-desc", id), class = "matvis-desc"),
    shiny::tags$div(id = sprintf("%s-matvis-legend", id), class = "matvis-legend"),
    shiny::tags$div(id = sprintf("%s-matvis-table", id))
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
  titlePanel("Assessment of Global and Regional Co-benefits and Trade-offs
             of Climate Mitigation and Adaptation Options"),
  
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
           checkboxGroupInput("ad_mit",
                              label = "Adaptation/Mitigation",
                              choices = ad_mit,
                              selected = ad_mit)
    )
  ),
  
  tabsetPanel(
    tabPanel("Level 1", matvisOutput("level1")),
    tabPanel("Level 2", matvisOutput("level2")),
    tabPanel("About", htmlTemplate("static/about.html"))
  )
)

server <- function(input, output){
  output$level1 <- renderMatvis({
    matvis("Level 1", 1, input = input)
  })

  output$level2 <- renderMatvis({
    matvis("Level 2", 2, input = input)
  })
}

shinyApp(ui, server)