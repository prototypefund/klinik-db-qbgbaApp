#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

    filteredData <- reactive({
        qbgbaExtraData::AllHospitals[qbgbaExtraData::AllHospitals$year == input$year &
                                         qbgbaExtraData::AllHospitals$quantityBeds >= input$rangeBeds[1] &
                                         qbgbaExtraData::AllHospitals$quantityBeds <= input$rangeBeds[2], ]
    })

    output$map <- renderLeaflet({

        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lat = 51.16344546735013,
                    lng = 10.447737773401668,
                    zoom = 6)


    })

    observe({

        labels <- sprintf("<strong> %s </strong> <br/>
                  <i>Owner:</i> %s (%s)<br/>
                  <div align='right' style='font-size: 0.8em;;'>Data from %s</div>",
                  filteredData() %>% pull(HospitalName),
                  filteredData() %>% pull(HospitalOperatorName),
                  filteredData() %>% pull(type),
                  rep(input$year, times = nrow(filteredData()))) %>%
            lapply(htmltools::HTML)

        leafletProxy("map") %>%
            clearShapes() %>%
            addCircles(layerId = filteredData() %>% pull(idHospital),
                       lng = filteredData() %>% pull(lon),
                       lat = filteredData() %>% pull(lat),
                       radius = 2,
                       color = "#f34c29",
                       weight = 5,
                       opacity = 1,
                       label = labels,
                       labelOptions = labelOptions(
                           style =
                               list(
                                   "font-weight" = "normal",
                                   "padding" = "2px 4px"
                               ),
                           textsize = "12px", direction = "auto"
                       ))
    })


    observeEvent(input$map_shape_click, {

        p <- input$map_shape_click

        one_clinic <- filteredData() %>%
            filter(idHospital == p$id) %>%
            select(HospitalName,
                   ikNumber,
                   locationNumberOverall,
                   street,
                   housenumber,
                   zip,
                   city,
                   URL,
                   quantityBeds,
                   quantityCasesFull,
                   quantityCasesPartial,
                   quantityCasesOutpatient)

        one_clinic_table <- tibble("NAMES" = c("<strong>Hospital Name</strong>",
                                               "<strong>IK - Location",
                                               "<strong>Address</strong>",
                                               "<strong>Website</strong>",
                                               "<strong>Number of Beds</strong>",
                                               "<strong>Inpatient Cases</strong>",
                                               "<strong>Day Care Cases</strong>",
                                               "<strong>Outpatient Cases</strong>"),

                                   "VALUES" = c(map_chr(strwrap(one_clinic %>% pull(HospitalName),
                                                                width = 42,
                                                                simplify = FALSE),
                                                        paste, collapse = "<br/>"),
                                                paste0(one_clinic %>% pull(ikNumber), " - ", one_clinic %>% pull(locationNumberOverall)),
                                                paste0(one_clinic %>% pull(street), " ",
                                                       one_clinic %>% pull(housenumber), "<br/>",
                                                       one_clinic %>% pull(zip), " ",
                                                       one_clinic %>% pull(city)),
                                                ifelse(one_clinic %>% pull(URL) == "No URL available",
                                                       one_clinic %>% pull(URL),
                                                       paste0('<a href="', one_clinic %>% pull(URL), '">', one_clinic %>% pull(URL), '</a>')),
                                                format(as.numeric(one_clinic %>% pull(quantityBeds)), big.mark = ".", decimal.mark = ","),
                                                format(as.numeric(one_clinic %>% pull(quantityCasesFull)), big.mark = ".", decimal.mark = ","),
                                                format(as.numeric(one_clinic %>% pull(quantityCasesPartial)), big.mark = ".", decimal.mark = ","),
                                                format(as.numeric(one_clinic %>% pull(quantityCasesOutpatient)), big.mark = ".", decimal.mark = ",")))

        output$details <- renderTable(one_clinic_table,
                                      colnames = FALSE,
                                      striped = TRUE,
                                      sanitize.text.function = identity)

    })


    observeEvent(input$reset, {

        leafletProxy("map")   %>%
            setView(lat = 51.16344546735013,
                    lng = 10.447737773401668,
                    zoom = 6)

    })


}
