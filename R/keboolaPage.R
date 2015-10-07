#' Page Template for Keboola Shiny Apps
#' 
#' @param bootstrapPage Bootstrape page element
#' @import shiny
#' @import DT
#' @export
keboolaPage <- function(page, appTitle="Default") {
    bootstrapPage(
        DT::datatable(data.frame()),
        # basic application container divs
        tags$head(tags$title(appTitle)),
        div(
            class="container-fluid",
            tags$head(tags$style('
                .navbar .container-fluid {
                  padding-left: 30px;
                  padding-right: 30px;
                }
                
                .navbar-brand {
                    padding: 0;
                }
                
                .kb-shiny-app-title {
                    padding: 15px 15px;
                }
                
                .kb-toolbar-btn {
                    padding-top:7px;
                }

                .kb-logo {
                    background: transparent url(https://connection.keboola.com/app/modules/admin/images/keboola-logo.png) 0 0 no-repeat;
                    display: inline-block;
                    width: 24px;
                    height: 32px;
                    position: relative;
                    top: 10px;
                }
                #loggedIn {
                    margin-top: 10px;
                }
                .shiny-busy {
                    cursor: wait;
                }         
                .error {
                    color: #ff0033;
                }

                .kb-example:before {
                    content: "Example: "
                }
                .kb-example {
                    color: #A4A4A4;
                }
                .kb-hint {
                    border-bottom: 1px dashed #333;
                }
            ')),
            div(class = "navbar navbar-default navbar-static-top kb-navbar-top",
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
                condition = "output.loggedIn == 0",
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
                condition = "output.loggedIn == 1",
                page   
            ),
            div(style = "visibility: hidden",
                textOutput("loggedIn")
            )
        )
    )
}
