library(shiny)
library(leaflet)
library(dplyr)

`%||%` <- function(a,b) ifelse(length(a)==0 || is.na(a) || a=="", b, a)
titlecase <- function(x) stringr::str_to_title(x)

ui <- fluidPage(
  titlePanel("Wine Explorer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("type_ui"),
      hr(),
      h4(textOutput("selected_region"), placeholder = TRUE),
      selectInput("wine_select","Wine", choices = NULL),
      tags$hr(),
      tags$strong("Selected wine"),
      verbatimTextOutput("wine_info")
    ),
    mainPanel(
      leafletOutput("map", height = 560),
      p(em("Click a region marker and pick a wine. Filter by one or more wine types."))
    )
  )
)

server <- function(input, output, session) {
  # Load data
  wines  <- vinexplorer::load_wine_data()
  coords <- vinexplorer::load_region_coords()   # requires inst/extdata/RegionCoordinates.csv
  
  # Type filter (all types selected by default)
  all_types <- sort(unique(wines$type[!is.na(wines$type)]))
  pretty_types <- setNames(all_types, titlecase(all_types))
  
  output$type_ui <- renderUI({
    selectizeInput(
      "type_filter", "Type(s)",
      choices  = pretty_types,
      selected = all_types,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })
  
  # Apply type filtering
  filtered <- reactive({
    req(input$type_filter)
    vinexplorer::filter_wines(wines, types = input$type_filter)
  })
  
  # Regions with coordinates + counts
  regions_df <- reactive({
    filtered() |>
      dplyr::mutate(region = stringr::str_trim(.data$region)) |>
      dplyr::inner_join(coords, by = "region") |>
      dplyr::filter(is.finite(lat), is.finite(lon)) |>
      dplyr::group_by(region, lat, lon) |>
      dplyr::summarize(n_wines = dplyr::n(), .groups = "drop")
  })
  
  # Base map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 10, lat = 45, zoom = 3)
  })
  
  # Add/refresh region markers when filters change
  observe({
    df <- regions_df()
    validate(need(nrow(df) > 0, "No regions to show with the current filter."))
    leafletProxy("map") |>
      clearMarkers() |>
      addCircleMarkers(
        lng = df$lon, lat = df$lat,
        layerId = df$region,
        label = paste0(df$region, " (", df$n_wines, ")"),
        radius = 7, opacity = 1, fillOpacity = 0.85
      )
  })
  
  # Region click → update wines list
  observeEvent(input$map_marker_click, {
    reg <- input$map_marker_click$id
    if (is.null(reg)) return()
    output$selected_region <- renderText(paste("Region:", reg))
    
    reg_wines <- filtered() |> dplyr::filter(.data$region == reg)
    updateSelectInput(session, "wine_select",
                      choices = setNames(reg_wines$wine_id, reg_wines$wine_name),
                      selected = if (nrow(reg_wines)) reg_wines$wine_id[1] else character(0))
  })
  
  # Show selected wine details
  output$wine_info <- renderText({
    req(input$wine_select)
    w <- filtered() |> dplyr::filter(.data$wine_id == as.integer(input$wine_select))
    if (!nrow(w)) return("No wine selected.")
    
    paste0(
      w$wine_name, " (", titlecase(w$type), ")\n",
      "Grape: ", w$grape %||% "—",
      if (!is.na(w$secondary_grapes) && w$secondary_grapes != "") paste0(" | Secondary: ", w$secondary_grapes) else "",
      "\nVintage: ", w$vintage %||% "—",
      " | Price: ", ifelse(is.na(w$price), "—", w$price),
      "\nRegion: ", w$region,
      if (!is.na(w$appellation) && w$appellation != "") paste0(" (Appellation: ", w$appellation, ")") else "",
      if (!is.na(w$style) && w$style != "") paste0("\nStyle: ", w$style) else "",
      if (!is.na(w$characteristics) && w$characteristics != "") paste0("\nCharacteristics: ", w$characteristics) else ""
    )
  })
}

shinyApp(ui, server)
