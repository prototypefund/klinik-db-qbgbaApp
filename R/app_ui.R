#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  all_years <- unique(qbgbaExtraData::AllHospitals$year)

  all_years_down <- as.numeric(rev(unique(qbgbaExtraData::mapBRDCounties$year)))

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
            "Mapping of all hospitals",
            tabName = "mapping",
            icon = icon("map-marked-alt")
          ),
          menuItem(
            "Analyzing Hospital KPIs",
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

            fluidRow(
              column(width = 9,
                     box(title = "Map", width = 12,
                         shinycssloaders::withSpinner(leafletOutput("map",
                                                                    width = "100%",
                                                                    height = "83vh"),
                                                      size = 1,
                                                      color = "#0080b7")
                         )
              ),
              column(width = 3,
                     box(title = "Controls", width = 12,
                         fluidRow(
                           column(width = 8,
                                  selectInput("year", "Select Year:",
                                              choices = all_years,
                                              selected = all_years[[1]])),
                           column(width = 4,
                                  tags$div(style = "margin-top: 1.9em; display: inline-block;",
                                           actionButton("reset", "Reset Map", icon = icon("sync-alt"),
                                                        class = "btn-success"))
                          )),
                         fluidRow(
                           column(width = 12,
                                  selectInput("owner", "Select Ownership:",
                                              choices = c("Public" = "oeffentlich",
                                                          "Private" = "privat",
                                                          "Unknown" = "unbekannt"),
                                              selected = c("oeffentlich", "privat", "unbekannt"),
                                              multiple = TRUE)
                         )),
                         fluidRow(
                           column(width = 12,
                                  sliderInput("rangeBeds", "Hospital Beds",
                                              min(qbgbaExtraData::AllHospitals$quantityBeds),
                                              max(qbgbaExtraData::AllHospitals$quantityBeds),
                                              value = range(qbgbaExtraData::AllHospitals$quantityBeds), step = 10,
                                              dragRange = TRUE))
                         )),
                     box(title = "Details", width = 12,
                         tableOutput("details"))
              )
            ),
          ),
          tabItem(
            tabName = "drilling",

            tags$head(
              tags$style(HTML(".leaflet-container { background: #fff;}")),
              # workaround for the NA in leaflet legend see https://github.com/rstudio/leaflet/issues/615
              tags$style(HTML(".leaflet-control div:last-child {clear: both;}")),
              #tags$style(HTML(".card {height: 100%;}")),
              tags$style(HTML(".col-sm-12:last-child .card {margin-bottom: 0 !important;}")),
              #tags$style(HTML("#leafdown {height: 80% !important; margin-top: 10px; margin-bottom: 10px;}"))
            ),

            shinyjs::useShinyjs(),

            fluidRow(
              column(width = 6,
                     box(
                       title = "Map",
                       width = 12,
                       # the two buttons used for drilling
                       actionButton("drill_up", "Drill Up"),
                       actionButton("drill_down", "Drill Down"),
                       # the actual map element
                       shinycssloaders::withSpinner(leafletOutput("leafdown", width = "100%",
                                                                  height = "75vh"),
                                                    size = 1,
                                                    color = "#0080b7")
                       )),
              column(width = 6,
                     box(
                       title = "Controls",
                       width = 12,
                       fluidRow(
                         column(width = 4,
                                # a dropdown to select what KPI should be displayed on the map
                                selectInput("yearDown", "Select Year:",
                                            choices = all_years_down,
                                            selected = all_years_down[[1]])
                                ),
                         column(width = 8,
                                selectInput("map_sel", "Select what KPI to display on the map:",
                                            c("Number of Doctors" = "DoctorsSum",
                                              "Number of Attending Doctors" = "AttendingDoctorsSum",
                                              "Number of Nurses" = "NursesSum",
                                              "Number of Beds" = "quantityBedsSum",
                                              "Number of Inpatient Cases" = "quantityCasesFullSum",
                                              "Number of Outpatient Cases" = "quantityCasesOutpatientSum"))
                                )
                         )
                       )
                     ),
                     # box(
                     #   title = "Results",
                     #   width = 12,
                     #   echarts4rOutput("comparison")
                     #   )
                    )
                  )
                )
              )
            )
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

