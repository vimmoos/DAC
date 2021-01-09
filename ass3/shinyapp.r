require(shiny)
require(shinydashboard)
require(plotly)
require(shinycssloaders)
require(tidyverse)


## set firefox as browser for testing
options(browser = "/usr/bin/firefox")



pre_proc <- function()
  read.csv("disruptions-2019-Q4.csv") %>%
    as_tibble() %>%
    rename(
      id = rdt_id, delay = duration_minutes,
      cause = statistical_cause_en,
      names = rdt_station_names,
      group = cause_group) %>%
    mutate(delay = delay / 60) %>%
    dplyr::select(id, delay, cause, names, group) %>%
    separate(names, as.character(c(1:33)),
      sep = ",") %>%
    gather(
      "sid", "name",
      -delay, -cause,
      -id, -group)

se <- function(col)
  col %>%
    na.omit() %>%
    (function(x) sd(x) / sqrt(length(x)))

make_graph <- function(dat, col, xaxis)
  substitute(
    dat %>%
      group_by(col) %>%
      summarize(
        m = mean(delay, na.rm = TRUE),
        stde = se(delay)) %>%
      data.frame() %>% plot_ly(
        y = ~m,
        type = "bar",
        x = ~col,
        error_y = ~ list(array = stde, color = "#000000"),
        color = ~col) %>%
      layout(
        yaxis = list(title = "Delay in hours", zeroline = F),
        showlegend = FALSE,
        xaxis = list(
          title = xlabel)),
    list(col = col, dat = substitute(dat), xlabel = xaxis))

server <- function(input, output, session) {

  data <- pre_proc()

  updateSelectizeInput(session, "stations",
    choices = levels(factor(data$name)),
    server = TRUE)

  filt_data <- reactive(
    if (is.null(input$stations)
        | length(input$stations) == 0) data
    else data [data$name %in% input$stations, ])

  delay_data <- reactive({
    req(input$delay_range)
    if (is.null(input$delay_range)) filt_data()
    else filter(filt_data(), delay > input$delay_range [1]
                            & delay < input$delay_range [2])}) %>%
  debounce(500)

  cause_data <- reactive({
    req(input$cause)
    tmp <- delay_data()
    if (input$cause == "ALL" | input$cause == "") tmp
    else tmp [tmp$group == input$cause, ]})


  xaxis <- reactive({
    req(input$cause)
    if (input$cause == "ALL") "Cause group"
    else "Cause"})


  updateSelectizeInput(
    session, "cause",
    choices = prepend("ALL", levels(factor(data$group))),
    selected = "ALL", server = TRUE)

  xaxis.col <- reactive({
    req(input$cause)
    if (input$cause == "ALL"
        | input$cause == "") quote(group)
    else quote(cause)})


  output$delay_graph <-
    renderPlotly({
        g <- eval(make_graph(
                    cause_data(),
                    xaxis.col(),
                    xaxis()))
        Sys.sleep(0.3)
        g})


  output$delay_slider <- renderUI(
    sliderInput("delay_range",
      label = tags$div(
        h2("Delay range"),
        tags$p("The delay is expressed in hours")),
      min = 0, max = floor(max(filt_data()$delay, na.rm = TRUE)),
      value = c(min, max)))}

ui <- dashboardPage(
  dashboardHeader(title = "DAC Dutch Railways"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML(
      "div.box-header  { text-align:center}"))),
    box(
      width = 3,
      title = tags$p("Settings",
        style = "font-size:300%"),
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      selectizeInput("cause",
        choices = NULL,
        label = tags$div(
          h2("Cause group"),
          tags$p("Select a group to inspect it")),
        selected = NULL,
        options = list(
          closeAfterSelect = TRUE,
          placeholder =
            "Select a cause group to inspect",
          allowEmptyOption = FALSE)),
      uiOutput("delay_slider"),
      selectizeInput("stations",
        choices = NULL,
        multiple = TRUE,
        label = tags$div(
          h2("Stations"),
          tags$p("select potentially multiple station")),
        options = list(
          closeAfterSelect = TRUE,
          placeholder = "Write which stations to select, by default ALL",
          allowEmptyOption = FALSE))),
    box(
      width = 9,
      height = "60rem",
      title = tags$p("Causes of delays",
        style = "font-size:300%;"),
      status = "danger",
      solidHeader = TRUE,
      withSpinner(plotlyOutput("delay_graph", height = "auto")))),
  skin = "red")

runApp(list(ui = ui, server = server),
  launch.browser = TRUE,
  port = getOption("shiny.port", 8080),
  host = getOption("shiny.host", "127.0.0.1"))
