library(shiny)
source("helpers.R")

regions <- getOptions("Region")
transitions <- getOptions("Transition")
ad_mit <- getOptions("Ad/Mit")
tll <- getOptions("Traffic light co-impact")
tlc <- getOptions("Traffic light confidence")
cs <- getOptions("Context sensitivity")

matvis <- function(title, level, input) {
  getGroupedData(level, input)
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
  titlePanel("Co-benefits and Trade-offs of Climate Mitigation and Adaptation"),
  h3("Global and Regional Assessment"),

  helpText(htmlTemplate("static/banner.html")),

  fluidRow(
    column(2,
           selectInput("region",
                       "Region",
                       regions)
    ),
    column(2,
           checkboxGroupInput("transition",
                              label = "Transition",
                              choices = transitions,
                              selected = transitions)
    ),
    column(2,
           checkboxGroupInput("ad_mit",
                              label = "Adaptation/Mitigation",
                              choices = ad_mit,
                              selected = ad_mit)
    ),
    column(2,
           checkboxGroupInput("tll",
                              label = "Potential for trade-offs or co-benefits",
                              choices = tll,
                              selected = tll)
    ),
    column(2,
           checkboxGroupInput("tlc",
                              label = "Confidence",
                              choices = tlc,
                              selected = tlc)
    ),
    column(2,
           checkboxGroupInput("cs",
                              label = "Context sensitivity",
                              choices = cs,
                              selected = cs)
    )
  ),
  
  tabsetPanel(
    tabPanel("Level 1", matvisOutput("level1")),
    tabPanel("Level 2", matvisOutput("level2")),
    tabPanel("Combined", matvisOutput("combined")),
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

  output$combined <- renderMatvis({
    matvis("Combined", 1:2, input = input)
  })
}

shinyApp(ui, server)