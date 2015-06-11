#' Page Template for Keboola Shiny Apps
#' 
#' @param sidebarPanel UI element
#' @param mainPanel UI element
#' @import shiny
#' @export
keboolaPage <- function(bootstrapPage) {
    bootstrapPage(
        # basic application container divs
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
                
                .kb-logo {
                    background: transparent url(https://connection.keboola.com/app/modules/admin/images/keboola-logo.png) 0 0 no-repeat;
                    display: inline-block;
                    width: 24px;
                    height: 32px;
                    position: relative;
                    top: 10px;
                }
                
                .shiny-busy {
                    cursor: wait;
                }
                
                .error {
                    color: #ff0033;
                }')),
            div(class="row",
                
                HTML('<div class="navbar navbar-default navbar-static-top kb-navbar-top">
                    <div class="container-fluid">
                        <div class="navbar-header">
                            <a class="navbar-brand" href="https://connection.keboola.com">
                                <span class="kb-logo"></span>
                                <span>Keboola Connection</span>
                            </a>
                        </div>
                        <div class="collapse navbar-collapse">
                            <div class="nav navbar-nav navbar-right navbar-brand kb-shiny-app-title shiny-text-output shiny-bound-output" id="appName">
                            <!-- App Title will be written here -->
                            </div>
                        </div>
                    </div>
                </div>')
                
            ),
            div(class="row",
                conditionalPanel(
                    condition = "output.loggedIn != ''",
                    div(class="col-md-6 col-md-offset-3",
                        HTML('<h3>Welcome to the shiny App Demo!</h3>
                              <div class="well">
                              <div>You are seeing this message because I didn\'t find your storage api token in the HTTP headers</div>
                              <div>To continue, please enter your token on the left and click "Login"</div>
                              <div>Cheers!</div>
                             </div>'),
                        textInput("token", "Enter KBC Token"),
                        actionButton("login","Login")
                    )
                ),
                conditionalPanel(
                    condition = "output.loggedIn == ''",
                    bootstrapPage    
                ),
                div(class="container-fluid",
                    div(class="row",
                        br(),
                        div(class="well col-md-6 col-md-offset-3 shiny-html-output shiny-bound-output", id="loggedIn")
                    )
                )
            )
        )
    )
}

