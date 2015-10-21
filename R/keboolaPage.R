#' Page Template for Keboola Shiny Apps
#' 
#' @param bootstrapPage Bootstrape page element
#' @import DT
#' @import shiny
#' @export
keboolaPage <- function(page, appTitle="Default") {
    addResourcePath(
        prefix = 'components',
        directoryPath = system.file('components', package='keboola.shiny.lib'))
    
    bootstrapPage(
        DT::datatable(data.frame()),
        # basic application container divs
        singleton(tags$head(
            tags$script(src = 'components/common.js'),
            tags$link(rel = 'stylesheet',
                      type = 'text/css',
                      href = 'components/common.css'),
            tags$title(appTitle)
        )),
        div(
            class="container-fluid",
            div(class = "navbar navbar-default navbar-static-top kb-navbar-top",
                textOutput("debug"),
                div(style="display:none;",
                    textInput("readyElem","hidden element", value="0")
                ),
                div(class = "container-fluid",
                    div(class = "navbar-header",
                        a(class = "navbar-brand",
                          href = "https://connection.keboola.com",
                          span(class = "kb-logo"),
                          span("Keboola Connection")
                        )
                    ),
                    
                    
                    div(class = "collapse navbar-collapse",
                        div(class = "nav navbar-nav navbar-right navbar-brand",
                            fluidRow(
                                column(2, class="kb-toolbar-btn",
                                    uiOutput("dataModalButton")     
                                ),
                                column(2, class="kb-toolbar-btn",
                                    uiOutput("settingsModalButton") 
                                ),
                                column(8, class="kb-shiny-app-title",
                                    appTitle
                                )
                            )
                        )
                    )
                )
            ),
            
            conditionalPanel(
                condition = "input.loggedIn == 0",
                div(class="col-md-6 col-md-offset-3",
                    h3(paste0('Welcome to the ', appTitle, ' application.')),
                    div(class = "well",
                        p("You are seeing this message because I didn\'t find your storage api token in the HTTP headers"),
                        p("To continue, please enter your KBC token and click \'Login\'"),
                        p("Cheers!")
                    ),
                    textInput("token", "Enter KBC Token"),
                    uiOutput("loginMsg"),
                    actionButton("login","Login")
                )
            ),
            conditionalPanel(
                condition = "input.loggedIn == 1 && input.loading == 1",
                div(id="init_panel", class="col-md-8 col-md-offset-2",
                    h4("Environment Initialisation"),
                    div(id="progress_panel", class="progress-panel container-fluid",""),
                    conditionalPanel(
                        condition = "input.detour == 1",
                        uiOutput("problemTables")
                    )
                )
            ),
            conditionalPanel(
                condition = "input.loggedIn == 1 && input.loading == 0",
                page   
            ),
            div(style = "display: none",
                textInput("loggedIn","",value="0"),
                textInput("loading","",value="0"),
                textInput("detour","",value="0")
            )
        )
    )
}
