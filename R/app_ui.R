#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  all_years <- unique(qbgbaExtraData::AllHospitals$year)

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    dashboardPage(
      title = "Basic Dashboard",
      fullscreen = TRUE,
      header = dashboardHeader(
        title = dashboardBrand(
          title = "Klinik-DB",
          color = "primary",
          href = "https://klinik-db.de",
          image = "www/favicon.png",
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE,
        leftUi = NULL,
        rightUi = NULL
      ),
      sidebar = dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        sidebarMenu(
          sidebarHeader("Available Tools"),
          menuItem(
            "Mapping all hospitals",
            tabName = "mapping",
            icon = icon("map-marked-alt")
          ),
          menuItem(
            "Analyse hospital KPIs",
            tabName = "drilling",
            icon = icon("layer-group")
          )
        )
      ),
      controlbar = NULL,
      footer = NULL,
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName = "mapping",
            shinycssloaders::withSpinner(leafletOutput("map", width = "100%", height = "90vh"), size = 2, color = "#0080b7"),

            absolutePanel(top = 80, right = 20,
                          #id = "input_control",
                          draggable = TRUE,
                          width = "auto",

                          selectInput("year", "Select Year:",
                                      choices = all_years,
                                      selected = all_years[[1]]),

                          sliderInput("rangeBeds", "Hospital Beds",
                                      min(qbgbaExtraData::AllHospitals$quantityBeds),
                                      max(qbgbaExtraData::AllHospitals$quantityBeds),
                                      value = range(qbgbaExtraData::AllHospitals$quantityBeds), step = 1),

                          #textOutput("sliderError"),

                          tableOutput("details")

            ),

            # conditionalPanel("isNaN(input.map_shape_click)", uiOutput("unclick")),

            absolutePanel(bottom = 80, right = 80,
                          div(style = "display:inline-block; float:left",
                              actionButton("reset", "Reset Map",
                                           class = "btn-success"))
            )
          ),
          tabItem(
            tabName = "drilling",
            box(
              title = "Drill",
              width = 6,
              h1("Hallo Drilling")
            )
          )
        )
      )
    )

    #   body = dashboardBody(
    #
    #     tags$head(
    #       tags$style(HTML(".leaflet-container { background: #fff; height: 100%}")),
    #       # workaround for the NA in leaflet legend see https://github.com/rstudio/leaflet/issues/615
    #       tags$style(HTML(".leaflet-control div:last-child {clear: both;}")),
    #       tags$style(HTML(".card {height: 100%;}")),
    #       tags$style(HTML(".col-sm-12:last-child .card {margin-bottom: 0 !important;}")),
    #       tags$style(HTML("#leafdown {height: 80% !important; margin-top: 10px; margin-bottom: 10px;}"))
    #     ),
    #
    #     tabItems(
    #       tabItem(
    #         tabName = "Map",
          #
          # ),
          # tabItem(
          #   tabName = "Drilling",
          #   box(
          #     title = "Leafdown",
          #     width = 6,
          #     selectInput("map_sel", "Select what KPI to display on the map:",
          #                 c("Votes" = "votes", "Unemployment" = "unemployment")),
          #     # the two buttons used for drilling
          #     actionButton("drill_down", "Drill Down"),
          #     actionButton("drill_up", "Drill Up"),
          #     # the actual map element
          #     #leafletOutput("leafdown")
          #   )
          #
          #   )
          #
          #
          # )
      #   )
      # )
    )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'qbgbaApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

