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

    fluidPage(theme = shinytheme("cerulean"),

              # tags$style(type = "text/css",
              #            "html, body {width:100%; height:100%}"),

              fluidRow(
                column(12,
                       tabsetPanel(id="ui_tab",
                                   tabPanel("Map",
                                            shinycssloaders::withSpinner(leafletOutput("map", width = "100%", height = "95vh"), size = 2, color = "#0080b7"),

                                                                         absolutePanel(top = 45, right = 20,
                                                                                       id = "input_control",
                                                                                       draggable = TRUE,
                                                                                       width = "auto",

                                                                                       selectInput("year", "Select Year:",
                                                                                                   choices = all_years,
                                                                                                   selected = all_years[[1]]),

                                                                                       sliderInput("rangeBeds", "Hospital Beds",
                                                                                                   min(qbgbaExtraData::AllHospitals$quantityBeds),
                                                                                                   max(qbgbaExtraData::AllHospitals$quantityBeds),
                                                                                                   value = range(qbgbaExtraData::AllHospitals$quantityBeds), step = 1),

                                                                                       tableOutput("details")

                                                                         ),

                                                                         # conditionalPanel("isNaN(input.map_shape_click)", uiOutput("unclick")),

                                                                         absolutePanel(bottom = 40, right = 20,
                                                                                       div(style = "display:inline-block; float:right",
                                                                                           actionButton("reset", "Reset Map",
                                                                                                        class="btn btn-sm btn-success"))
                                                                         )
                                   ),

                                   tabPanel("Drilling",
                                            h1("Hallo!")
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

