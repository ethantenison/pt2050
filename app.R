# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
library(visNetwork)
library(shinyWidgets)
library(readr)
library(DT)
library(igraph)
library(sna)
library(qgraph)
library(ggplot2)
library(htmltools)
library(mice)
library(VIM)
library(lavaan)
library(kableExtra)
library(tidyverse)
library(htmlTable)
library(htmlwidgets)
library(ggmap)
library(rsconnect)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(leaflet.extras)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #



g1 <- readRDS("./data/network_full.rds")
g1_equity <- readRDS("./data/network_equity.rds")
g1_resilience <- readRDS("./data/network_resilience.rds")
coord <- read_csv("data/coordinates.csv") |>
    select(name,Orgtype, lon, lat) |> 
    mutate(Orgtype = as.factor(Orgtype))
centrality <- read_csv("data/TX ENGO Centrality.csv") |> 
    select(name, TotalDegree)
data <- list("g1" = g1,
             "g1_equity" = g1_equity,
             "g1_resilience" = g1_resilience)
titles <- list("g1" = "PT 2050 Network" ,
               "g1_resilience" = "PT 2050 Resilience-focused Network",
               "g1_equity" = "PT 2050 equity-focused Network")


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
     var element = document.documentElement,
 enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
 exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
 if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
 enterFS.call(element);
 } else {
 exitFS.call(document);
 }
 }'



header <-
    dashboardHeader(title = h2("Controls", align = "center"))

sidebar <- dashboardSidebar(
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsToggleFS, functions = "toggleFullScreen"),
    sidebarMenu(
        id = "tabs",
        menuItem(
            "Network Graph",
            tabName = "graph",
            icon = icon("project-diagram")
        ),
        conditionalPanel(
            condition = "input.tabs == 'graph'",
            div(id = "focus_select",
            selectInput(
                "focus",
                "Network Focus",
                c(
                    "PT 2050 Network" = "g1",
                    "Resilience-focused Network" = "g1_resilience",
                    "Equity-focused Network" = "g1_equity"
                )
            )),
            hr(style = "margin-top: 5px; margin-bottom: 5px; width:90%"),
        ),
        menuItem("Network Map",
                 tabName = "network_map",
                 icon = icon("map")),
        conditionalPanel(condition = "input.tabs == 'network_map'",
                         selectInput(
                             "map_focus",
                             "Map Focus",
                             c(
                                 "PT 2050 Network" = "g1",
                                 "Resilience-focused Network" = "g1_equity",
                                 "Equity-focused Network" = "g1_resilience"
                             )
                         )),
        hr(style = "margin-top: 5px; margin-bottom: 5px; width:90%"),
        HTML(
            "<h4 style='color:#ffffff; padding: 3px 5px 5px 17px; display:block'><i class='fa fa-toolbox'></i> Dashboard Tools</h4>"
        ),
        div(id = "about_research", 
            actionButton(
            "show",
            "About Research",
            icon = icon("info-circle", class = "fa-pull-left"),
            style = "color: #152934"
        )),
        downloadButton('downloadpdf', label = 'Download', style="display: block; margin: 0 auto; width: 200px;color: #152934;"),
        HTML(
            "<button type='button' class='btn btn-default action-button shiny-bound-input' style='display: block; margin: 6px 5px 6px 15px; width: 200px;color: #152934;' onclick = 'shinyjs.toggleFullScreen();'><i class='fa fa-expand fa-pull-left'></i> Fullscreen</button>"
        ),
        hr(style = "margin-top: 15px; margin-bottom: 5px; width:90%")
        
    )
    
)



body <- dashboardBody(
    tags$head(
        tags$script(src = "wordwrap.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(
            rel = "stylesheet",
            href = "https://use.fontawesome.com/releases/v5.1.0/css/all.css",
            integrity = "sha384-lKuwvrZot6UHsBSfcMvOkWwlCMgc0TaWr+30HWe3a4ltaBwTZhyTEggF5tJv8tbt",
            crossorigin = "anonymous"
        )
    ),
    tags$head(tags$style(
        HTML(
            '
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #97c2fc;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #97c2fc;
        /* Network Title */                      }
        #network_title, #networkmap_title, #network_title_legend{color: #152934;
                                 font-size: 20px;
                                 }'
        )
    )),
    tags$head(tags$style(
        type="text/css",
        "#legend img {max-width: 100%; width: 100%; height: auto}"
    )),
    tabItems(
        tabItem(tabName = "graph",
                fluidRow(
                    h3("Planet Texas 2050"),
                    #HTML('<center><img src="images/logo3.png" width="800"></center>'),
                    hr()
                ),
                fluidRow(column(
                    width = 12,
                    box(
                        title = textOutput("network_title"),
                        width = 12,
                        div(id = "visnetwork", visNetworkOutput("twg_network", height = "700px"))
                    )
                ))),
        tabItem(tabName = "network_map",
                fluidRow(
                    h3("Planet Texas 2050"),
                    #HTML('<center><img src="images/logo2.png" width="700"></center>'),
                    hr()
                ),
                fluidRow(box(
                    title = textOutput("networkmap_title"),
                    width = 12,
                    leafletOutput(
                        "map",
                        height = "700px"
                    )
                )))
    )
)
ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #

server <- function(input, output, session) {
    
    #Network Title ------------------------------------------------------------
    output$network_title <- renderText({
        titles[[input$focus]]
    })
    
    output$networkmap_title <- renderText({
        titles[[input$map_focus]]
    })
    
    
    #VisNetwork ---------------------------------------------------------------
    output$twg_network <- renderVisNetwork({
        gvis <-
            toVisNetworkData(data[[input$focus]])
        
        nodes <- sort(gvis$nodes)
        nodes <- nodes |> mutate(font.size = 20)
        edges <- gvis$edges
        
        lnodes <-
            data.frame(
                label = c(
                    "Non-Governmental",
                    "Municipal/County",
                    "State Agency",
                    "Federal Agency",
                    "University",
                    "Private",
                    "Austin",
                    "Dallas",
                    "Houston",
                    "San Antonio",
                    "Others"
                ),
                color.background = c(
                    "white",
                    "white",
                    "white",
                    "white",
                    "white",
                    "white",
                    "green",
                    "blue",
                    "red",
                    "yellow",
                    "#87CEEB"
                ),
                color.border = c(
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black"
                ),
                shape = c(
                    "dot",
                    "square",
                    "triangle",
                    "triangleDown",
                    "star",
                    "diamond",
                    "square",
                    "square",
                    "square",
                    "square",
                    "square"
                )
            )
        
        
        network <- visNetwork(nodes,
                              edges,
                              #main = titles[[input$sectors]],
                              width = "100%",
                              height = "850px") |>
            visEdges(
                smooth = T,
                arrows = list(to = list(
                    enabled = TRUE, scaleFactor = .5
                )),
                color = list(color = "lightblue", highlight = "black")
            ) |>
            visIgraphLayout(
                smooth = list(enabled = T, type = 'dynamic'),
                physics = list(
                    stabilization = F,
                    solver = "forceAtlas2Based",
                    forceAtlas2Based = list(gravitationalConstant = -500)
                ),
                layout = "layout_nicely",
                randomSeed = 123
            ) |>
            visInteraction(navigationButtons = FALSE) |>
            visOptions(
                selectedBy = list(variable = c("county"), multiple = TRUE),
                highlightNearest = list(enabled = T, hover = T),
                nodesIdSelection = TRUE
            ) |>
            addFontAwesome() |>
            visLegend(
                position = "right",
                addNodes = lnodes,
                useGroups = FALSE,
                stepY = 100
            )
        
        network
        
    })
    
    # Network Map -------------------------------------------------------------
    nodes <- reactive({
        gvis <-
            toVisNetworkData(data[[input$map_focus]])
        
        nodes <- sort(gvis$nodes) |> 
            left_join(coord, by = c("id" = "name")) |> 
            left_join(centrality, by = c("id" = "name")) |> 
            mutate(label = paste(id, "\n", TotalDegree))
        nodes
    })
    
    
    pal <- reactive({ 
        
        colorFactor(
            palette = c('red', 'green', 'blue', 'purple', "yellow", "orange"),
            domain = nodes()$Orgtype
        )
        
        
    })
    
    output$map <- renderLeaflet({
        pal <- pal()
        
        leaflet(nodes()) |> 
            addProviderTiles(providers$Stamen.TonerLite) |> 
            setView(-99.33584086660734, 31.13198248011147, zoom = 6) |> 
            clearControls() |> 
            clearMarkers() |> 
            addCircleMarkers(
                data = nodes(),
                lng =  ~ nodes()$lon,
                lat =  ~ nodes()$lat,
                fillColor = ~pal(nodes()$Orgtype),
                color = ~pal(nodes()$Orgtype),
                radius = nodes()$TotalDegree + 2,
                fillOpacity = 0.5,
                label = nodes()$label
            ) |> 
            addLegend(data = nodes(),
                      "bottomright",
                      pal = pal,
                      values = ~nodes()$Orgtype,
                      title = "")
        

    })
    
    

}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
