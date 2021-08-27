#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  all_years <- unique(AllHospitals$year)

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bootstrapPage(

      tags$style(type = "text/css",
                 "html, body {width:100%; height:100%}"),

      leafletOutput("map", width = "100%", height = "100%"),

      absolutePanel(top = 10, right = 10,
                    id = "input_control",
                    draggable = TRUE,
                    width = "25%",

                    selectInput("year", "Select Year:",
                                choices = all_years,
                                selected = all_years[[1]]),

                    sliderInput("rangeBeds", "Hospital Beds",
                                min(AllHospitals$quantityBeds),
                                max(AllHospitals$quantityBeds),
                                value = range(AllHospitals$quantityBeds), step = 1),

                    tableOutput("details")

      ),

      #conditionalPanel()
      # absolutePanel(bottom = 50, left = 10,
      #               tableOutput("details"),
      #               id = "input_control"),

      absolutePanel(bottom = 20, right = 10,
                    div(style = "display:inline-block; float:right",
                        actionButton("reset", "Reset Map",
                                     class="btn btn-sm btn-success"))
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

