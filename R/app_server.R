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

    AllHospitals <- AllHospitals %>%
        mutate(identifier = paste0(HospitalName, " (", ikNumber, " - ", locationNumberOverall, ")"))

    pal <- colorFactor(
        palette = c('#007bff', '#dc3545', '#28a745'),
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


    filteredDataSearch <- reactive({

        OneHospital_filtered <- AllHospitals[AllHospitals$year == input$year &
                                                 AllHospitals$identifier == input$searchHospital, ]

        validate(
            need(nrow(OneHospital_filtered) > 0, message = "No Hospital available!")#,
        )

        OneHospital_filtered

    })

    observe({

        updateSelectizeInput(session, 'searchHospital',
                             choices = unique(filteredData()$identifier),
                             selected = "",
                             server = TRUE)

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

        data <- filteredData()

        one_clinic <- data %>%
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


    observeEvent(input$searchHospital, {

        if (req(input$searchHospital) != "") {

            oneClinic <- filteredDataSearch()

            oneClinic_table <- tibble("NAME" = c("<strong>Hospital Name</strong>",
                                                 "<strong>IK - Location",
                                                 "<strong>Address</strong>",
                                                 "<strong>Website</strong>",
                                                 "<strong>Number of Beds</strong>",
                                                 "<strong>Inpatient Cases</strong>",
                                                 "<strong>Day Care Cases</strong>",
                                                 "<strong>Outpatient Cases</strong>"),

                                      "VALUE" = c(map_chr(strwrap(oneClinic %>% pull(HospitalName),
                                                                  width = 42,
                                                                  simplify = FALSE),
                                                          paste, collapse = "<br/>"),
                                                  paste0(oneClinic %>% pull(ikNumber), " - ", oneClinic %>% pull(locationNumberOverall)),
                                                  paste0(oneClinic %>% pull(street), " ",
                                                         oneClinic %>% pull(housenumber), "<br/>",
                                                         oneClinic %>% pull(zip), " ",
                                                         oneClinic %>% pull(city)),
                                                  ifelse(oneClinic %>% pull(URL) == "No URL available",
                                                         oneClinic %>% pull(URL),
                                                         paste0('<a href="', oneClinic %>% pull(URL), '" target="_blank" rel="noopener noreferrer">',
                                                                oneClinic %>% pull(URL), '</a>')),
                                                  format(as.numeric(oneClinic %>% pull(quantityBeds)), big.mark = ".", decimal.mark = ","),
                                                  format(as.numeric(oneClinic %>% pull(quantityCasesFull)), big.mark = ".", decimal.mark = ","),
                                                  format(as.numeric(oneClinic %>% pull(quantityCasesPartial)), big.mark = ".", decimal.mark = ","),
                                                  format(as.numeric(oneClinic %>% pull(quantityCasesOutpatient)), big.mark = ".", decimal.mark = ",")))

            output$details <- renderTable(oneClinic_table,
                                          colnames = FALSE,
                                          striped = TRUE,
                                          spacing = "xs",
                                          sanitize.text.function = identity)

            leafletProxy("map") %>%
                clearMarkers() %>%
                setView(lat = oneClinic$lat,
                        lng = oneClinic$lon,
                        zoom = 7) %>%
                addMarkers(lng = oneClinic$lon, lat = oneClinic$lat)

        }

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

        updateSelectizeInput(session, "searchHospital",
                             selected = "")

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

        curr_map_level <- my_leafdown$curr_map_level

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
            legend_title <- paste0("Number of Doctors per<br/>", resident_baseline/1000, "K residents in ", input$yearDown)

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

    output$comparison <- renderEcharts4r({

        # get the currently selected data from the map
        df <- my_leafdown$curr_sel_data()

        # Get currently selected KPI
        kpi_name <- paste0("sum_", input$map_sel)

        curr_map_level <- my_leafdown$curr_map_level

        if (curr_map_level == 1) {

            resident_baseline <- 100000

        } else if (curr_map_level == 2) {

            resident_baseline <- 10000

        }

        # depending on the selected KPI in the dropdown we show different data
        if (input$map_sel == "DoctorsSum") {

            e_legend_title <- paste0("Number of Doctors per ", resident_baseline/1000, "K residents")

        } else if (input$map_sel == "AttendingDoctorsSum") {

            e_legend_title <- paste0("Number of Attending Doctors per ", resident_baseline/1000, "K residents")

        } else if (input$map_sel == "NursesSum") {

            e_legend_title <- paste0("Number of Nurses per ", resident_baseline/1000, "K residents")

        } else if (input$map_sel == "quantityBedsSum") {

            e_legend_title <- paste0("Number of Beds per ", resident_baseline/1000, "K residents")

        } else if (input$map_sel == "quantityCasesFullSum") {

            e_legend_title <- paste0("Number of Inpatient Cases per ", resident_baseline/1000, "K residents")

        } else if (input$map_sel == "quantityCasesOutpatientSum") {

            e_legend_title <- paste0("Number of Outpatient Cases per ", resident_baseline/1000, "K residents")

        }


        # check whether any shape is selected, show general election-result if nothing is selected
        if(dim(df)[1] > 0){

            if(my_leafdown$curr_map_level == 1) {

                plotData <- mapBRDStates_metadata[mapBRDStates_metadata$AGS_1 %in% unique(df$AGS_1), ] %>%
                    left_join(mapBRDStates_map@data[, c("AGS_1", "GEN_1")], by = "AGS_1") %>%
                    select(GEN_1, everything()) %>%
                    mutate(year = factor(year),
                           GEN_1 = factor(GEN_1),
                           AGS_1 = factor(AGS_1)) %>%
                    group_by(year, AGS_1, GEN_1) %>%
                    summarize(across(contains("male"), sum, .names = "sum_{.col}"),
                              numberHospitalsSum = sum(numberHospitals),
                              across(typeRatioPrivat:psychiatricDutyToSupplyRatio, mean, .names = "mean_{.col}"),
                              across(quantityBedsSum:NursesSum, sum, .names = "sum_{.col}"),
                              across(weeklyWH_doctors_mean:weeklyWH_nurses_mean, mean, .names = "mean_{.col}"), .groups = "keep")

                plotData <- plotData %>%
                    select(year, AGS_1, GEN_1, sum_male, sum_female, {{kpi_name}}) %>%
                    group_by(year, AGS_1) %>%
                    mutate(residents = sum_male + sum_female) %>%
                    mutate(year = factor(year))

                plotData <- plotData %>%
                    mutate(DV = .data[[kpi_name]] / residents * resident_baseline) %>%
                    group_by(GEN_1)

            } else {

                plotData <- mapBRDCounties_metadata[mapBRDCounties_metadata$AGS_2 %in% unique(df$AGS_2), ] %>%
                    left_join(mapBRDCounties_map@data[, c("AGS_2", "GEN_2")], by = "AGS_2") %>%
                    select(GEN_2, everything()) %>%
                    mutate(year = factor(year),
                           GEN_2 = factor(GEN_2),
                           AGS_2 = factor(AGS_2)) %>%
                    group_by(year, AGS_2, GEN_2) %>%
                    summarize(across(contains("male"), sum, .names = "sum_{.col}"),
                              numberHospitalsSum = sum(numberHospitals),
                              across(typeRatioPrivat:psychiatricDutyToSupplyRatio, mean, .names = "mean_{.col}"),
                              across(quantityBedsSum:NursesSum, sum, .names = "sum_{.col}"),
                              across(weeklyWH_doctors_mean:weeklyWH_nurses_mean, mean, .names = "mean_{.col}"), .groups = "keep")

                plotData <- plotData %>%
                    select(year, AGS_2, GEN_2, sum_male, sum_female, {{kpi_name}}) %>%
                    group_by(year, AGS_2) %>%
                    mutate(residents = sum_male + sum_female) %>%
                    mutate(year = factor(year))

                plotData <- plotData %>%
                    mutate(DV = .data[[kpi_name]] / residents * resident_baseline) %>%
                    group_by(GEN_2)

            }

        } else {

            plotData <- mapBRDStates_metadata %>%
                mutate(year = factor(year),
                       AGS_1 = factor(AGS_1)) %>%
                group_by(year) %>%
                summarize(across(contains("male"), sum, .names = "sum_{.col}"),
                          numberHospitalsSum = sum(numberHospitals),
                          across(typeRatioPrivat:psychiatricDutyToSupplyRatio, mean, .names = "mean_{.col}"),
                          across(quantityBedsSum:NursesSum, sum, .names = "sum_{.col}"),
                          across(weeklyWH_doctors_mean:weeklyWH_nurses_mean, mean, .names = "mean_{.col}"), .groups = "keep")

            plotData <- plotData %>%
                select(year, sum_male, sum_female, {{kpi_name}}) %>%
                group_by(year) %>%
                mutate(residents = sum_male + sum_female) %>%
                mutate(year = factor(year))
            plotData <- plotData %>%
                mutate(DV = .data[[kpi_name]] / residents * 100000) %>%
                mutate(GEN_0 = "BRD") %>%
                group_by(GEN_0)

        }
        # create the graph
        plotData %>%
            e_charts(year) %>%
            e_bar(DV) %>%
            e_tooltip(trigger = "axis", axisPointer = list(type = "shadow")) %>%
            e_legend(bottom = 0) %>%
            e_x_axis(axisLabel = list(interval = 0),
                     axisTick = list(alignWithLabel = TRUE)) %>%
            e_title(e_legend_title,
                    left = "center", top = 5) %>%
            e_grid(bottom = 100, height = "auto")

    })

}
