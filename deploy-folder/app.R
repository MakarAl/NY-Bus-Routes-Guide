library(shinydashboard)
library(leaflet)
library(RgoogleMaps)
library(RMySQL)
library(lubridate)

source("db_operator2.R")

ui <- dashboardPage(
    dashboardHeader(title = "Manhattan Commute Planner",
                    titleWidth = 300),
    #add tab with ER-diagram
    #add tab with full schedule
    dashboardSidebar(width = 300,
                     sidebarMenuOutput("sidebar_menu")),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "map",
                leafletOutput("busmap", width="100%", height=450),
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                              width = 230, height = "auto",
                              #tweak layout and formatting
                              h4("Select routes"),
                              tags$div("To get the info about the routes, please click on the bus stop you are interested in."),
                              uiOutput("route_table")
                ),
                uiOutput("schedule_table")
            ),
            tabItem(
                tabName = "schedule",
                h3("Route Schedule"),
                fluidRow(
                    column(width = 3,
                           selectInput("route", "Select route", choices = loadAllRoutes()[,1]),
                           selectInput("selected_stop", "Select stop", choices = loadStops()[,4]),
                           uiOutput("route_info")
                    ),
                    column(width = 9,
                           dataTableOutput("route_schedule"))
                )
            ),
            tabItem(
                tabName = "info",
                h3("About the Project"),
                h3(""),
                h4("ER diagram"),
                img(src = "http://s18.postimg.org/xsuflfyop/ER_model.jpg", width = "75%", height = "75%"),
                h1(""),
                h4("Instructions"),
                p("1. User chooses one of the stops by clicking the bus shaped mark on the map,this will provide the list of routes passing by the stop in a checkbox."),
                p("2. User selects one or several routes from the checkbox and the travel date from the calendar, the App will draw the shapes of the selected routes on the designated date on the map."),
                p("3. User then sets the departure time and it will show the list of arrival times of the bus at the selected stop. User can choose to check the arrival times after the designated departure time or the schedule of whole day.")           
            )
        )
    )
)
    
server <- function(input, output, session) {
    #map related output
    map <- leaflet(data = loadStops()) %>% 
        addProviderTiles("Stamen.Toner") %>%
        addMarkers(lng = ~lon, lat = ~lat, 
                   layerId = ~as.character(nam), 
                   popup = ~as.character(nam),
                   icon = icons("bus.png", iconWidth = 24, iconHeight = 24),
                   clusterOptions = markerClusterOptions()) %>% 
        setView(getGeoCode("Columbia University, New York, NY, 10027")[1], 
                getGeoCode("Columbia University, New York, NY, 10027")[2], 
                zoom = 17)
    output$busmap <- renderLeaflet(map)
    observe({
       proxyMap <- leafletProxy("busmap")
       proxyMap %>% clearShapes()
       for (route in input$routes_to_map) {
           info <- loadRouteInfo(route)
           addPolylines(map = proxyMap, 
                        layerId = route,
                        lng = loadShapes(route, input$schedule_date)$shape_pt_lon,
                        lat = loadShapes(route, input$schedule_date)$shape_pt_lat,
                        color = paste0("#",as.character(loadColor(route))),
                        weight = 8,
                        opacity = 0.7,
                        fill = FALSE,
                        popup = paste0("<b><a href='",info[1,4],"'>",info[1,1],"</a></b>
                                       </br>",info[1,2])
                        )
       }
       
    })
    #add search feature
    observe({
        coord <- getGeoCode(paste0(input$searchText,", New York, NY"))
        proxyMap1 <- leafletProxy("busmap")
        setView(map = proxyMap1, lng = coord[2], lat = coord[1], zoom = 16)
    })
    
    #absolute panel & schedule related output
    observe(
        if (is.null(input$busmap_marker_click)) {} 
        else{
            output$route_table <- renderUI({
                checkboxGroupInput("routes_to_map", 
                                    label = paste0("Routes passing through ", as.character(input$busmap_marker_click[1])), 
                                    choices = t(loadRoutes(as.character(input$busmap_marker_click),input$schedule_date)),
                                    selected = t(loadRoutes(as.character(input$busmap_marker_click),input$schedule_date)))
            
            })
            
            output$schedule_table_aux <- renderTable({
                s <- data.frame(time1 = character(),
                                time2 = character(),
                                time3 = character(),
                                time4 = character(),
                                time5 = character(),
                                stringsAsFactors = FALSE)
                for (i in 1:length(input$routes_to_map)) {
                    s[i,] <- Schedule(input$routes_to_map[i], 
                                      input$schedule_date, 
                                      as.character(input$busmap_marker_click[1]),
                                      format(input$schedule_time-5*60*60,"%H:%M:%S"))[1:5,]
                }
                row.names(s) <- input$routes_to_map
                s
            })
            
            output$schedule_table <- renderUI({
                caption <- paste0("Suggested departures from ",as.character(input$busmap_marker_click[1]),
                                  " at ",format(input$schedule_time-5*60*60,"%H:%M:%S"),
                                  " on ",input$schedule_date)
                list(
                    tags$h4(caption),
                    tableOutput("schedule_table_aux")
                )
            })
            
            updateSelectInput(session, "selected_stop", selected = input$busmap_marker_click[1])
        }
    )
    
    #sidebar panel related output
    output$text_stop <- renderText({as.character(input$busmap_marker_click[1])})
    output$sidebar_menu <- renderMenu({
        sidebarMenu(
            sidebarSearchForm(textId = "searchText", 
                              buttonId = "searchButton",
                              label = "Search..."),
            menuItem("Map", tabName = "map", icon = icon("database")),
            dateInput("schedule_date", "Enter date", value = Sys.Date()),
            sliderInput("schedule_time", "Enter time", value = floor_date(Sys.time(), "minute"), 
                        min = Sys.time()-(Sys.time() - floor_date(Sys.time(), "day")),
                        max = Sys.time()-(Sys.time() - floor_date(Sys.time(), "day"))+24*60*60,
                        step = 15,
                        timeFormat = "%H:%M:%S"),
            br(),
            menuItem("Schedule", tabName = "schedule", icon = icon("calendar")),
            menuItem("About", tabName = "info", icon = icon("info-circle"))
        )
    })
    
    #full schedule rendering
    #route selection
    observe({
        if (is.null(input$busmap_shape_click)) {} 
        else{
            updateSelectInput(session, "route", selected = input$busmap_shape_click[1])
        }
    })
    output$route_schedule <- renderDataTable({
        s <- Schedule(input$route, 
                 input$schedule_date, 
                 input$selected_stop,
                 "00:00:00")
        names(s) <- input$selected_stop
        s
    },
    options = list(pageLength = 10))
    
}
    
shinyApp(ui, server)