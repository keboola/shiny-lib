#' Page Template for Keboola Shiny Apps
#' 
#' @import DT shiny
#' @export
#' @param bootstrapPage bootstrapePage element
#' @param appTitle Application title
keboolaPage <- function(page, appTitle="Default") {
    addResourcePath(
        prefix = 'components',
        directoryPath = system.file('components', package='keboola.shiny.lib'))
    
    bootstrapPage(
        # basic application container divs
        singleton(tags$head(
            tags$script(src = 'components/kb_common.js'),
            tags$link(rel = 'stylesheet',
                      type = 'text/css',
                      href = 'components/kb_common.css'),
            tags$title(appTitle)
        )),
        div(
            class="container-fluid",
            div(class = "navbar navbar-static-top",
                div(class = "container-fluid",
                    div(class = "navbar-left kb-navbar-top",                        
                        div(class = "kb-logo",
                            a(class = "",
                              href = "https://connection.keboola.com",
                              img(src = "https://d3iz2gfan5zufq.cloudfront.net/images/cloud-services/shiny-32-2.png", alt="Keboola")
                            )    
                        ),
                        div(class = "kb-shiny-app-title", appTitle)
                    ),
                    div(class = "collapse navbar-collapse",
                        div(class = "nav navbar-nav navbar-right ",
                            fluidRow(
                                column(3, class="kb-toolbar-btn",
                                    uiOutput("kb_dataModalButton")     
                                ),
                                column(3, class="kb-toolbar-btn",
                                    uiOutput("kb_settingsModalButton") 
                                ),
                                column(6, class="navbar-right ",
                                    ""
                                )
                            )
                        )
                    )
                )
            ),
            
            conditionalPanel(
                condition = "input.kb_loggedIn == 0",
                div(class="col-md-6 col-md-offset-3",
                    h3(paste0('Welcome to the ', appTitle, ' application.')),
                    div(class = "well",
                        p("You are seeing this message because I didn\'t find your storage api token in the HTTP headers"),
                        p("To continue, please enter your KBC token and click \'Login\'"),
                        p("Cheers!")
                    ),
                    textInput("kb_token", "Enter KBC Token"),
                    selectInput("kb_bucket", "Select a Bucket", choices=c()),
                    uiOutput("kb_loginMsg"),
                    actionButton("kb_login","Login")
                )
            ),
            conditionalPanel(
                condition = "input.kb_loggedIn == 1 && input.kb_loading == 1",
                div(id="kb_init_panel", class="col-md-8 col-md-offset-2",
                    h4("Environment Initialisation"),
                    div(id="kb_progress_panel", class="progress-panel container-fluid",""),
                    uiOutput("kb_detourMessage"),
                    conditionalPanel(
                        condition = "input.kb_detour == 1",
                        id = "kb_detourPanel",
                        uiOutput("kb_problemTables")
                    )
                )
            ),
            conditionalPanel(
                condition = "input.kb_loggedIn == 1 && input.kb_loading == 0",
                page   
            ),
            div(style = "display: none",
                textInput("kb_loggedIn","",value="0"),
                textInput("kb_loading","",value="0"),
                textInput("kb_detour","",value="0")
            )
        )
    )
}
