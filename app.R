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
library(rintrojs)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #



g1 <- readRDS("./data/network_full.rds")
g1_equity <- readRDS("./data/network_equity.rds")
g1_resilience <- readRDS("./data/network_resilience.rds")


data <- list(
    "g1" = g1,
    "g1_equity" = g1_equity,
    "g1_resilience" = g1_resilience
)





titles <- list(
    "g1" = "PT 2050 Network" ,
    "g1_resilience" = "PT 2050 Resilience-focused Network",
    "g1_equity" = "PT 2050 equity-focused Network"
)


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
    introjsUI(),
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
            div(id = "switches",
            materialSwitch(
                inputId = "geographic",
                label = "Geographic Network",
                status = "default",
                right = TRUE,
                value = FALSE
            ),    
            materialSwitch(
                inputId = "edgenames",
                label = "Connection Names",
                status = "default",
                right = TRUE,
                value = FALSE
            ),
            materialSwitch(
                inputId = "nodenames",
                label = "Organization Names",
                status = "default",
                right = TRUE,
                value = TRUE
            )),
            actionButton(
                "help",
                "Tutorial",
                icon = icon("book-open", class = "fa-pull-left"),
                style = "display: block; margin: 0 auto; width: 200px;color: #152934"
            ),
            hr(style = "margin-top: 5px; margin-bottom: 5px; width:90%"),
        ),
        menuItem("Network Data",
                 tabName = "table",
                 icon = icon("table")),
        conditionalPanel(condition = "input.tabs == 'table'",
                         selectInput(
                             "sectors_table",
                             "Sector",
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
        #network_title, #network_title_legend{color: #152934;
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
        tabItem(tabName = "table",
                fluidRow(
                    #HTML('<center><img src="images/logo2.png" width="700"></center>'),
                    hr()
                ),
                fluidRow(box(
                    width = 12, DT::dataTableOutput("table")
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
    
    
    output$twg_network <- renderVisNetwork({
        gvis <-
            toVisNetworkData(data[[input$focus]])
        
        nodes <- sort(gvis$nodes)
        nodes <- nodes %>% mutate(font.size = 20)
        
        
        edges <- gvis$edges
        
        network <- visNetwork(nodes,
                              edges,
                              #main = titles[[input$sectors]],
                              width = "100%",
                              height = "850px") %>%
            visEdges(
                arrows = list(to = list(
                    enabled = TRUE, scaleFactor = .5
                )),
                color = list(highlight = "black"),
                width = 3,
                label = TRUE
            ) %>%
            visNodes(color = list(
                background = "white",
                border = "black",
                highlight = list(background = "#A9A9A9", border = "black"),
                hover = list(background = "#A9A9A9", border = "black")
            )) %>%
            visIgraphLayout(
                smooth = list(enabled = T, type = 'dynamic'),
                physics = list(
                    stabilization = F,
                    solver = "forceAtlas2Based",
                    forceAtlas2Based = list(gravitationalConstant = -500)
                ),
                layout = "layout_with_kk",
                randomSeed = 27
            ) %>%
            visInteraction(navigationButtons = FALSE) %>%
            visOptions(
                selectedBy = list(variable = c("type"), multiple = TRUE),
                highlightNearest = list(enabled = T, hover = T),
                nodesIdSelection = TRUE
            ) %>%
            addFontAwesome()
        
        network
        
    })
    

}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
