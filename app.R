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
library(bslib)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #

cords <- read_csv("./data/node_coords.csv")
cords <- select(cords, id, x, y)
row.names(cords) <- cords[['id']]

g1 <- readRDS("./data/g1.rds")
g1_agriculture <- readRDS("./data/g1_agriculture.rds")
g1_environment <- readRDS("./data/g1_environment.rds")
g1_flooding <- readRDS("./data/g1_flooding.rds")
g1_groundwater <- readRDS("./data/g1_groundwater.rds")
g1_innovation <- readRDS("./data/g1_innovation.rds")
g1_municipal <- readRDS("./data/g1_municipal.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas.rds")
g1_rural <- readRDS("./data/g1_rural.rds")

data <- list(
    "g1" = g1,
    "g1_agriculture" = g1_agriculture,
    "g1_environment" = g1_environment,
    "g1_flooding" = g1_flooding,
    "g1_groundwater" = g1_groundwater,
    "g1_innovation" = g1_innovation,
    "g1_municipal" = g1_municipal,
    "g1_oilandgas" = g1_oilandgas,
    "g1_rural" = g1_rural
)



g1 <- readRDS("./data/g12.rds")
g1_agriculture <- readRDS("./data/g1_agriculture2.rds")
g1_environment <- readRDS("./data/g1_environment2.rds")
g1_flooding <- readRDS("./data/g1_flooding2.rds")
g1_groundwater <- readRDS("./data/g1_groundwater2.rds")
g1_innovation <- readRDS("./data/g1_innovation2.rds")
g1_municipal <- readRDS("./data/g1_municipal2.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas2.rds")
g1_rural <- readRDS("./data/g1_rural2.rds")

data2 <- list(
    "g1" = g1,
    "g1_agriculture" = g1_agriculture,
    "g1_environment" = g1_environment,
    "g1_flooding" = g1_flooding,
    "g1_groundwater" = g1_groundwater,
    "g1_innovation" = g1_innovation,
    "g1_municipal" = g1_municipal,
    "g1_oilandgas" = g1_oilandgas,
    "g1_rural" = g1_rural
)



g1 <- readRDS("./data/g13.rds")
g1_agriculture <- readRDS("./data/g1_agriculture3.rds")
g1_environment <- readRDS("./data/g1_environment23.rds")
g1_flooding <- readRDS("./data/g1_flooding3.rds")
g1_groundwater <- readRDS("./data/g1_groundwater3.rds")
g1_innovation <- readRDS("./data/g1_innovation3.rds")
g1_municipal <- readRDS("./data/g1_municipal3.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas3.rds")
g1_rural <- readRDS("./data/g1_rural3.rds")

data3 <- list(
    "g1" = g1,
    "g1_agriculture" = g1_agriculture,
    "g1_environment" = g1_environment,
    "g1_flooding" = g1_flooding,
    "g1_groundwater" = g1_groundwater,
    "g1_innovation" = g1_innovation,
    "g1_municipal" = g1_municipal,
    "g1_oilandgas" = g1_oilandgas,
    "g1_rural" = g1_rural
)



combined_data <- list(
    "Connections" = data,
    "Connections and Organizations" = data2,
    "Organizations " = data3
)



titles <- list(
    "g1" = "Network Graph - All Sectors" ,
    "g1_agriculture" = "Network Graph - Agriculture",
    "g1_environment" = "Network Graph - Environment",
    "g1_groundwater" = "Network Graph - Groundwater",
    "g1_oilandgas" = "Network Graph - Oil and Gas",
    "g1_rural" = "Network Graph - Rural Utilities",
    "g1_municipal" = "Network Graph - Municipal",
    "g1_environment" = "Network Graph - Environment",
    "g1_flooding" = "Network Graph - Flooding",
    "g1_innovation" = "Network Graph - Innovation"
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
                    "Connections and Organizations",
                    "Connections",
                    "Organizations "
                )
            )),
            div(id = "sector_select",
            selectInput(
                "sectors",
                "Sector",
                c(
                    "All Sectors" = "g1",
                    "Agriculture" = "g1_agriculture",
                    "Environment" = "g1_environment",
                    "Groundwater" = "g1_groundwater",
                    "Oil and Gas" = "g1_oilandgas",
                    "Rural Utilities" = "g1_rural",
                    "Municipal" = "g1_municipal",
                    "Environment" = "g1_environment",
                    "Flooding" = "g1_flooding",
                    "Innovation" = "g1_innovation"
                )
            )),
            div(id = "switches",
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
                                 "All Sectors" = "g1",
                                 "Agriculture" = "g1_agriculture",
                                 "Environment" = "g1_environment",
                                 "Groundwater" = "g1_groundwater",
                                 "Oil and Gas" = "g1_oilandgas",
                                 "Rural Utilities" = "g1_rural",
                                 "Municipal" = "g1_municipal",
                                 "Flooding" = "g1_flooding",
                                 "Innovation" = "g1_innovation"
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
        hr(style = "margin-top: 15px; margin-bottom: 5px; width:90%"),
        
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
                    HTML('<center><img src="images/logo3.png" width="800"></center>'),
                    hr()
                ),
                fluidRow(column(
                    width = 12,
                    box(
                        title = textOutput("network_title"),
                        width = 12,
                        div(id = "visnetwork", visNetworkOutput("twg_network", height = "700px")),
                        absolutePanel(
                            id = "topbar",
                            fixed = TRUE,
                            draggable = FALSE,
                            top = "70%",
                            left = "78.5%",
                            #right = "1%",
                            #bottom = "20%",
                            imageOutput("legend", height = "300px")
                            
                        )
                    )
                ))),
        tabItem(tabName = "table",
                fluidRow(
                    HTML('<center><img src="images/logo2.png" width="700"></center>'),
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
    
    
    ######Introjs######
    hintjs(session, options = list("hintButtonLabel"="Close This Hint"))
    
    observeEvent(input$help,
                 rintrojs::introjs(session, options = list(
                     steps = data.frame(element = c("#focus_select .form-group ",
                                                    "#sector_select .form-group",
                                                    "#switches .form-group",
                                                    "#visnetwork",
                                                    "#network_data",
                                                    "#about_research .form-group",
                                                    "#download "
                     ),
                     intro = c(includeMarkdown("tooltips/focus_select.md"),
                               includeMarkdown("tooltips/sector_select.md"),
                               includeMarkdown("tooltips/switches.md"),
                               includeMarkdown("tooltips/visnetwork.md"),
                               includeMarkdown("tooltips/network_data.md"),
                               includeMarkdown("tooltips/about_research.md"),
                               includeMarkdown("tooltips/download.md")

                     ),
                     position = c("auto",
                                  "auto",
                                  "auto",
                                  "auto",
                                  "auto",
                                  "auto",
                                  "auto"

                     )
                     ),
                     "nextLabel"="Next",
                     "prevLabel"="Previous",
                     "skipLabel"="Exit")#,
                     #events = list("oncomplete"=I('alert("Thanks! Also, check out the Index for more information about the data represented here.")')
                      #             )
                     )
    )
    
    ######About Research Button######
    observeEvent(input$show, {
        showModal(modalDialog(
            title = "About this Research",
            size = "l",
            #footer = HTML("<a href='https://www.texaswater.org/'> Texas Water Foundation</a>"),
            includeMarkdown("tooltips/about_research_button.md"),
            easyClose = TRUE
        ))
    })
    
    ######Download Research Button######
    output$downloadpdf <- downloadHandler(
        filename = 'LBJTWF_Flows_of_Texas_Water.pdf', 
        content = function(file) {
            file.copy("www/LBJTWF_Flows_of_Texas_Water_Final_Report.pdf", file)
        }
    ) 
    
    
    ######Reactive Titles######
    output$network_title <- renderText({
        titles[[input$sectors]]
    })
    output$network_title_legend <-
        renderText({
            "Network Graph Legend"
        })
    
    output$legend <- renderImage({
        width  <- session$clientData$output_legend_width
        height <- session$clientData$output_legend_height
        
        ######Organizations and Connections######
        if (input$focus == "Connections and Organizations" &
            input$sectors == "g1") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_sector.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_agriculture") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_ag.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_environment") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_env.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_flooding") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_flood.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_groundwater") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_gw.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_innovation") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_in.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_municipal") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_mun.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_oilandgas") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_oilandgas.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1_rural") {
            filename <-
                normalizePath(file.path('./www/images/orgcon_rural.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        ######Just connections ######
        else if (input$focus == "Connections" &
                 input$sectors == "g1") {
            filename <-
                normalizePath(file.path('./www/images/con_sector.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_agriculture") {
            filename <-
                normalizePath(file.path('./www/images/con_ag.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_environment") {
            filename <-
                normalizePath(file.path('./www/images/con_env.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_flooding") {
            filename <-
                normalizePath(file.path('./www/images/con_flooding.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_groundwater") {
            filename <-
                normalizePath(file.path('./www/images/con_gw.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_innovation") {
            filename <-
                normalizePath(file.path('./www/images/con_in.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_municipal") {
            filename <-
                normalizePath(file.path('./www/images/con_mun.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_oilandgas") {
            filename <-
                normalizePath(file.path('./www/images/con_oilandgas.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Connections" &
                 input$sectors == "g1_rural") {
            filename <-
                normalizePath(file.path('./www/images/con_rural.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        ######Just Organizations######
        else if (input$focus == "Organizations " &
                 input$sectors == "g1") {
            filename <-
                normalizePath(file.path('./www/images/org_all.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_agriculture") {
            filename <-
                normalizePath(file.path('./www/images/org_ag.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_environment") {
            filename <-
                normalizePath(file.path('./www/images/org_env.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_flooding") {
            filename <-
                normalizePath(file.path('./www/images/org_flood.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_groundwater") {
            filename <-
                normalizePath(file.path('./www/images/org_gw.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_innovation") {
            filename <-
                normalizePath(file.path('./www/images/org_in.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_municipal") {
            filename <-
                normalizePath(file.path('./www/images/org_mun.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_oilandgas") {
            filename <-
                normalizePath(file.path('./www/images/org_oilandgas.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Organizations " &
                 input$sectors == "g1_rural") {
            filename <-
                normalizePath(file.path('./www/images/org_rural.png'))
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        
    }, deleteFile = FALSE)
    
    output$twg_network <- renderVisNetwork({
        gvis <-
            toVisNetworkData(combined_data[[input$focus]][[input$sectors]])
        
        nodes <- sort(gvis$nodes)
        nodes <- nodes %>% mutate(font.size = 20)
        nodes$title <-
            paste(
                "<p>",
                "Name: ",
                nodes$label,
                "<br>Domain: ",
                nodes$level,
                "<br>Type: ",
                nodes$type,
                "<br>Connections: ",
                nodes$size,
                "</p>"
            )
        nodes$size <- nodes$size + 3 * 2
        
        if (input$nodenames == FALSE) {
            nodes$label <- ""
        }
        
        edges <- gvis$edges
        edges$color[edges$type == "Water"] <- "#97c2fc"
        edges$verb <- ""
        edges$verb[edges$type == "Advocacy/Policy Preference"] <-
            "advocate for"
        edges$verb[edges$type == "Information"] <-
            "send information to"
        edges$verb[edges$type == "Authority to set rules"] <-
            "has the authority to set rules for"
        edges$verb[edges$type == "Contract"] <- "contract"
        edges$verb[edges$type == "Cooperation/coordination"] <-
            "cooperate and/or coordinate with"
        edges$verb[edges$type == "Water rights/regulation of"] <-
            "regulate the water rights for"
        edges$verb[edges$type == "Grants"] <-
            "provide grants to"
        edges$verb[edges$type == "Infrastruture Services"] <-
            "provide infrastruture services to"
        edges$verb[edges$type == "Water"] <- "provide water to"
        edges$verb[edges$type == "Regulation/Oversight"] <-
            "regulate"
        edges$verb[edges$type == "Litigation"] <- "litigate"
        edges$verb[edges$type == "Money"] <- "pay"
        edges$verb[edges$type == "Lobbying"] <- "lobby"
        edges$verb[edges$type == "Membership"] <-
            "are members of"
        edges$verb[edges$type == "Permits/Authorization"] <-
            "issue permits or authorize"
        edges$verb[edges$type == "Ecosystem service"] <-
            "provide ecosystem services to"
        edges$verb[edges$type == "Water disposal"] <-
            "dispose water for"
        edges$verb[edges$type == "Water sales"] <-
            "sell water to"
        edges$verb[edges$type == "Water Savings"] <-
            "save water for"
        edges$verb[edges$type == ""] <-
            "provides unknown services for"
        
        edges$verb[edges$from == "Bureau Economic Geology" &
                       edges$type == "Information"] <-
            "sends information to"
        edges$verb[edges$from == "Aquifer" &
                       edges$type == "Water"] <-
            "provides water to"
        edges$verb[edges$from == "Aquifer" &
                       edges$type == "Water Savings"] <-
            "saves water for"
        
        edges$title <- paste(edges$from, edges$verb, edges$to)
        
        
        if (input$edgenames == FALSE) {
            edges$label <- ""
        }
        else if (input$focus == "Connections and Organizations" &
                 input$sectors == "g1" |
                 input$focus == "Connections" &
                 input$sectors == "g1" &
                 input$edgenames == TRUE) {
            edges$label <- edges$sector
            
        }
        
        else if (input$focus == "Organizations " &
                 input$edgenames == TRUE) {
            edges$label <- edges$sector
        }
        else {
            edges$label <- edges$type
        }
        
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
    
    output$table <- renderDataTable({
        gvis <-
            toVisNetworkData(combined_data[["Connections and Organizations"]][[input$sectors_table]])
        nodelist <- gvis$nodes
        nodelist$size <- as.integer(nodelist$size)
        rownames(nodelist) <- NULL
        nodelist <-
            nodelist %>% dplyr::select('id', 'level', 'type', 'size') %>% dplyr::rename(
                "Organization" = "id",
                "Juristiction" = "level",
                "Organization Type" = "type",
                "Total Connections" = "size"
            ) %>%
            dplyr::arrange(desc(`Total Connections`))
        
        edgelist <- gvis$edges
        edgecount_from <- count(edgelist, vars = from)
        edgecount_to <- count(edgelist, vars = to)
        edgescount <-
            edgecount_from %>% full_join(edgecount_to, by = "vars")
        edgescount[is.na(edgescount)] <- 0
        edgescount <-
            edgescount %>% mutate(connections = n.x + n.y)
        
        nodelist <-
            nodelist %>% left_join(edgescount, by = c("Organization" = "vars")) %>% rename("Connections from" = "n.x",
                                                                                           "Connections to" = "n.y") %>% select(
                                                                                               "Organization",
                                                                                               "Connections from",
                                                                                               "Connections to",
                                                                                               "connections",
                                                                                               "Organization Type",
                                                                                               "Juristiction"
                                                                                           ) %>% rename("Total Connections" = "connections")
        
        
        datatable(nodelist,
                  options = list(pageLength = 15),
                  height = "200px")
    })
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- #
shinyApp(ui = ui, server = server)
