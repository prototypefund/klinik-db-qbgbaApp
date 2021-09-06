#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

    AllHospitals <- qbgbaExtraData::AllHospitals %>%
        mutate(type = factor(type)) %>%
        mutate(type = forcats::fct_collapse(type,
                                   privat = c("privat",
                                              "GmbH",
                                              "Milde Stiftung privaten Rechts",
                                              "Sana Kliniken Duisburg GmbH",
                                              "Stiftung privaten Rechts",
                                              "Privat",
                                              "Ab 01.11.2019 erfolgte die betriebliche Änderung mit neuem Träger, der CCare AG Darmstadt, und der neuen Bezeichnung: Klinik Ingelheim GmbH.  Bis zum 31.10.2019 war der Träger zu 90% die Universitätsmedizin Mainz und zu 10% die Stadt Ingelheim am Rhein. Der Name der Einrichtung lautete bis dahin Krankenhaus Ingelheim der Universitätsmedizin Mainz GmbH.\r\n"),
                                   oeffentlich = c("freigemeinnützig",
                                                   "öffentlich",
                                                   "öffentlich-rechtlich",
                                                   "öffentlich - Rechtlich",
                                                   "Öffentlich-rechtlich",
                                                   "gemeinnützig",
                                                   "kommunal",
                                                   "in öffentlich-rechtlicher Trägerschaft",
                                                   "freigemeinnützig und öffentlich",
                                                   "freigemeinnützig / kirchlich",
                                                   "Anstalt öffentlichen Rechts",
                                                   "gemeinnütziger Trägerverein",
                                                   "öffentlich, gGmbH",
                                                   "gemeinützig",
                                                   "Körperschaft des öffentlichen Rechts (K.d.ö.R.)",
                                                   "e. V.",
                                                   "öffentlich-rechtliche Trägerschaft",
                                                   "gemeinnützige GmbH",
                                                   "Rechtsfähige Stiftung des bürgerlichen Rechts",
                                                   "freigemeinnützig/kirchlich",
                                                   "Landesgesellschaft"),
                                   unbekannt = c("-",
                                                 "0",
                                                 "Maximalversorger",
                                                 "nicht vorhanden",
                                                 "2 Standorte",
                                                 "zu 50% in öffentlicher und zu 50% in privater Trägerschaft",
                                                 "MusterArt",
                                                 "Grund- und Regelversorgung",
                                                 "m")))

    pal <- colorFactor(
        palette = c('blue', 'red', 'green'),
        levels = unique(as.character(AllHospitals$type))
        )


    filteredData <- reactive({

        AllHospitals_filtered <- AllHospitals[AllHospitals$year == input$year &
                                                  AllHospitals$type %in% input$owner &
                                                  AllHospitals$quantityBeds >= input$rangeBeds[1] &
                                                  AllHospitals$quantityBeds <= input$rangeBeds[2], ]

        validate(
            need(nrow(AllHospitals_filtered) > 0, message = "No Hospitals available - please select a wider range!")#,
            #shinyFeedback::feedbackWarning("rangeBeds", nrow(AllHospitals_filtered) == 0, "No Hospitals available - please select a wider range!")
        )

        AllHospitals_filtered

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
                       color = pal(as.character(filteredData() %>% pull(type))),
                       weight = 5,
                       opacity = 0.75,
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
                   quantityCasesOutpatient,
                   lat,
                   lon)

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
                                                       paste0('<a href="', one_clinic %>% pull(URL), '" target="_blank" rel="noopener noreferrer">',
                                                              one_clinic %>% pull(URL), '</a>')),
                                                format(as.numeric(one_clinic %>% pull(quantityBeds)), big.mark = ".", decimal.mark = ","),
                                                format(as.numeric(one_clinic %>% pull(quantityCasesFull)), big.mark = ".", decimal.mark = ","),
                                                format(as.numeric(one_clinic %>% pull(quantityCasesPartial)), big.mark = ".", decimal.mark = ","),
                                                format(as.numeric(one_clinic %>% pull(quantityCasesOutpatient)), big.mark = ".", decimal.mark = ",")))

        output$details <- renderTable(one_clinic_table,
                                      colnames = FALSE,
                                      striped = TRUE,
                                      spacing = "xs",
                                      sanitize.text.function = identity)

        leafletProxy("map") %>%
            clearMarkers() %>%
            addMarkers(lng = one_clinic$lon, lat = one_clinic$lat)


    })

    # observeEvent(input$map_click,{
    #
    #     output$details <- NULL
    #
    #     leafletProxy("map") %>%
    #         clearMarkers()
    #
    # })

    # output$controls <- renderUI({
    #     req(input$map_marker_click)
    #
    #     absolutePanel(id = "unclick", top = 100, left = 50,
    #                   right = "auto", bottom = "auto", width = "auto", height = "auto",
    #                   actionButton(inputId = "resetChoice", label = "Remove Selection", class = "btn-primary")
    #     )
    # })


    observeEvent(input$reset, {

        output$details <- NULL

        leafletProxy("map")   %>%
            clearMarkers() %>%
            setView(lat = 51.16344546735013,
                    lng = 10.447737773401668,
                    zoom = 6)

    })


    # Leafdown Part -----------------------------------------------------------

    # library(tidyverse)
    # library(sf)

    mapBRDStates <- qbgbaExtraData::mapBRDStates

    mapBRDStates_map <- mapBRDStates %>%
        filter(year == "2019") %>%
        select(AGS_1, GEN_1, BEZ_1)
    mapBRDStates_map <- as_Spatial(mapBRDStates_map)

    mapBRDStates_metadata <- mapBRDStates %>%
        select(-GEN_1, -BEZ_1)
    st_geometry(mapBRDStates_metadata) <- NULL

    mapBRDCounties <- qbgbaExtraData::mapBRDCounties

    mapBRDCounties_map <- mapBRDCounties %>%
        filter(year == "2019") %>%
        select(AGS_1, GEN_1, BEZ_1, AGS_2, GEN_2, BEZ_2)
    mapBRDCounties_map <- as_Spatial(mapBRDCounties_map)

    mapBRDCounties_metadata <- mapBRDCounties %>%
        select(-AGS_1, -GEN_1, -BEZ_1, -GEN_2, -BEZ_2)
    st_geometry(mapBRDCounties_metadata) <- NULL


    rm(mapBRDStates, mapBRDCounties)

    spdfs_list <- list(mapBRDStates_map, mapBRDCounties_map)

    # create leafdown object
    my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input, join_map_levels_by = c("AGS_1" = "AGS_1"))

    rv <- reactiveValues()
    rv$update_leafdown <- 0

    # observers for the drilling buttons
    observeEvent(input$drill_down, {

        my_leafdown$drill_down()

        rv$update_leafdown <- rv$update_leafdown + 1

    })

    observeEvent(input$drill_up, {

        my_leafdown$drill_up()

        rv$update_leafdown <- rv$update_leafdown + 1

    })


    data <- reactive({

        req(rv$update_leafdown)

        # fetch the current metadata from the leafdown object
        current_data <- my_leafdown$curr_data

        if(my_leafdown$curr_map_level == 2) {

            current_data <- current_data %>%
                select(AGS_1, GEN_1, BEZ_1, AGS_2, GEN_2, BEZ_2)

            mapBRDCounties_metadata_current <- mapBRDCounties_metadata %>%
                filter(year == input$yearDown) %>%
                select(-year)

            data <- left_join(current_data, mapBRDCounties_metadata_current, by = "AGS_2")

        } else {

            current_data <- current_data %>%
                select(AGS_1, GEN_1, BEZ_1)

            # message("current_data:\n", paste0(names(current_data), collapse = ", "), "\n")
            # message("current_data:\n", current_data, "\n")
            # message("\nJahr: ", input$yearDown, ", Typ: ", typeof(input$yearDown), "\n")

            mapBRDStates_metadata_current <- mapBRDStates_metadata %>%
                filter(year == input$yearDown) %>%
                select(-year)

            data <- left_join(current_data, mapBRDStates_metadata_current, by = "AGS_1")

        }

        # add the data back to the leafdown object
        my_leafdown$add_data(data)

        data$curr_map_level <- my_leafdown$curr_map_level

        data

    })


    create_labels <- function(data, map_level, role, resident_baseline) {

        labels <- sprintf(
            "<strong>%s</strong><br/>%s %s per %s residents in %s</sup>",
            data[, paste0("GEN_", map_level)],
            prettyNum(round(data$y, digits = 1), big.mark = ","),
            rep(role, times = nrow(data)),
            rep(paste0(as.character(resident_baseline/1000), "K"), times = nrow(data)),
            rep(input$yearDown, times = nrow(data))
        )

        labels %>% lapply(htmltools::HTML)

    }

    # this is where the leafdown magic happens
    output$leafdown <- renderLeaflet({
        req(spdfs_list)
        req(data)

        data <- data()

        curr_map_level <- unique(data$curr_map_level)

        if (curr_map_level == 1) {

            resident_baseline <- 100000

        } else if (curr_map_level == 2) {

            resident_baseline <- 10000

        }

        # depending on the selected KPI in the dropdown we show different data
        if (input$map_sel == "DoctorsSum") {

            data <- data %>%
                mutate(y = DoctorsSum / (male + female) * resident_baseline)
            labels <- create_labels(data, curr_map_level, "Doctors", resident_baseline)
            fillcolor <- leaflet::colorNumeric("Greens", data$y)
            legend_title <- paste0("Number of Doctors per<br/>", resident_baseline/1000, "Kresidents in ", input$yearDown)

        } else if (input$map_sel == "AttendingDoctorsSum") {

            data <- data %>%
                mutate(y = AttendingDoctorsSum / (male + female) * resident_baseline)
            labels <- create_labels(data, curr_map_level, "Attending Doctors", resident_baseline)
            fillcolor <- leaflet::colorNumeric("Reds", data$y)
            legend_title <- paste0("Number of Attending Doctors per<br/>", resident_baseline/1000, "K residents in ", input$yearDown)

        } else if (input$map_sel == "NursesSum") {

            data <- data %>%
                mutate(y = NursesSum / (male + female) * resident_baseline)
            labels <- create_labels(data, curr_map_level, "Nurses", resident_baseline)
            fillcolor <- leaflet::colorNumeric("Blues", data$y)
            legend_title <- paste0("Number of Nurses per<br/>", resident_baseline/1000, "K residents in ", input$yearDown)

        } else if (input$map_sel == "quantityBedsSum") {

            data <- data %>%
                mutate(y = quantityBedsSum / (male + female) * resident_baseline)
            labels <- create_labels(data, curr_map_level, "Beds", resident_baseline)
            fillcolor <- leaflet::colorNumeric("Oranges", data$y)
            legend_title <- paste0("Number of Beds per<br/>", resident_baseline/1000, "K residents in ", input$yearDown)

        } else if (input$map_sel == "quantityCasesFullSum") {

            data <- data %>%
                mutate(y = quantityCasesFullSum / (male + female) * resident_baseline)
            labels <- create_labels(data, curr_map_level, "Inpatient Cases", resident_baseline)
            fillcolor <- leaflet::colorNumeric("BuPu", data$y)
            legend_title <- paste0("Number of Inpatient Cases per<br/>", resident_baseline/1000, "K residents in ", input$yearDown)

        } else if (input$map_sel == "quantityCasesOutpatientSum") {

            data <- data %>%
                mutate(y = quantityCasesOutpatientSum / (male + female) * resident_baseline)
            labels <- create_labels(data, curr_map_level, "Outpatient Cases", resident_baseline)
            fillcolor <- leaflet::colorNumeric("Purples", data$y)
            legend_title <- paste0("Number of Outpatient Cases per<br/>", resident_baseline/1000, "K residents in ", input$yearDown)

        }


        # draw the leafdown object
        my_leafdown$draw_leafdown(
            fillColor = ~fillcolor(data$y),
            weight = 2, fillOpacity = 1, color = "grey", label = labels,
            highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)) %>%
            setView(lat = 51.16344546735013,
                    lng = 10.447737773401668,
                    zoom = 6) %>%
            # add a nice legend
            addLegend(pal = fillcolor,
                      values = ~data$y,
                      title = legend_title,
                      opacity = 1,
                      position = "bottomleft")
    })

    # output$comparison <- renderEcharts4r({
    #
    #     # get the currently selected data from the map
    #     df <- my_leafdown$curr_sel_data()
    #
    #     # check whether any shape is selected, show general election-result if nothing is selected
    #     if(dim(df)[1] > 0){
    #         if(my_leafdown$curr_map_level == 1) {
    #             df <- df[, c("state_abbr", "Democrats2016", "Republicans2016", "Libertarians2016", "Green2016")]
    #             df <- df %>% pivot_longer(2:5, "party") %>% group_by(party)
    #         } else {
    #             df <- df[, c("County", "Democrats2016", "Republicans2016", "Libertarians2016", "Green2016")]
    #             df <- df %>% pivot_longer(2:5, "party") %>% group_by(party)
    #             df$value <- df$value
    #             names(df)[1] <- "state_abbr"
    #         }
    #     } else {
    #         # show general election-result as no state is selected
    #         df <- data.frame(
    #             party = c("Democrats2016", "Republicans2016", "Libertarians2016", "Green2016"),
    #             state_abbr = "USA",
    #             value = c(0.153, 0.634, 0.134, 0.059)) %>% group_by(party)
    #     }
    #     # create the graph
    #     df %>%
    #         e_charts(state_abbr, stack="grp") %>%
    #         e_bar(value) %>%
    #         e_y_axis(formatter = e_axis_formatter("percent", digits = 2)) %>%
    #         e_tooltip(trigger = "axis",axisPointer = list(type = "shadow")) %>%
    #         e_legend(right = 10,top = 10) %>%
    #         e_color(c("#232066", "#E91D0E", "#f3b300", "#006900"))%>%
    #         e_tooltip(formatter = e_tooltip_item_formatter("percent", digits = 2))
    # })





}
