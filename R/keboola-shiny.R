#' Helper Library for Keboola Shiny Applications
#' 
#' @import methods
#' @import shiny
#' @import keboola.sapi.r.client
#' @import KeboolaAppConfig
#' @import KeboolaAppData
#' @export KeboolaShiny
#' @exportClass KeboolaShiny

source("KeboolaAppConfig.R")
source("KeboolaAppData.R")
KeboolaShiny <- setRefClass(
    'KeboolaShiny',
    fields = list(
        loggedIn = 'numeric',
        errMsg = 'character',
        loginErrorOutput = 'ANY',  # shiny.tag element containing login error message html
        bucketId = 'character',
        runId = 'character',
        token = 'character',
        appId = 'character',
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        kfig = 'ANY', # keboola.shiny.lib::KeboolaAppConfig
        kdat = 'ANY' # keboola.shiny.lib::KeboolaAppData
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then
        #'  it will be read from command line argument.
        #' @exportMethod
        initialize = function() {
            loggedIn <<- 0
            errMsg <<- ''
            loginErrorOutput <<- ''
            bucketId <<- ''
            runId <<- ''
            appId <<- ''
            token <<- ''
            client <<- NULL
            kfig <<- NULL
            kdat <<- NULL
        },
        
        #' Get the runId from the query string
        #' 
        #' @param Shiny server session object
        #' @return function closure to retrieve the runId
        getRunId = function(clientData) {
            val <- as.character(parseQueryString(clientData$url_search)$runId)
            if (length(val) == 0) {
                val <- ''
            }
            return(val)
        },

        
        #' Get the bucket from the query string
        #' 
        #' @param Shiny server session object
        #' @return function closure to retrieve the runId
        getBucket = function(clientData) {
            val <- as.character(parseQueryString(clientData$url_search)$bucket)
            if (length(val) == 0) {
                val <- ''
            }
            return(val)
        },

        
        #' Get the token from the session headers or input element
        #' 
        #' @param request object to check header for token
        #' @param input object to check text box for token
        #' @param clientData to check url params for token
        #' @return the supplied token
        getToken = function(request, input, clientData) {
            val <- as.character(request$HTTP_X_STORAGEAPI_TOKEN)
            if (length(val) == 0) {
                val <- as.character(parseQueryString(clientData$url_search)$token)
                if (length(val) == 0) {
                    val <- as.character(input$token)
                    if (length(val) == 0) {
                        val <- ''
                    }
                }
            }
            return(val)
        },

        #' Get the AppId from the URL
        #'
        #' @param clientData -- shiny session clientData object
        #' @return appId
        #' @exportMethod
        getAppId = function(clientData) {
            appId <<- unlist(strsplit(clientData$url_pathname,"/"))[1]
            if (.self$appId == "") {
                appId <<- "8f5fc4bcf3d546ad"
            }
            print(paste("got app id",.self$appId))
            .self$appId
        },
        
        #' Returns login status message or empty string if valid
        #' 
        #' @param the ShinySession object
        #' @return html string containing error message and field settings
        #' @exportMethod
        getLogin = function(session) {
            request <- session$request
            input <- session$input
            clientData <- session$clientData
            print("getLogin")
            token <<- .self$getToken(request, input, clientData)
            runId <<- .self$getRunId(clientData)
            bucketId <<- .self$getBucket(clientData)
            appId <<- .self$getAppId(clientData)
            errMsg <<- ""
            if (token != '') {
                tryCatch({
                    client <<- keboola.sapi.r.client::SapiClient$new(token)
                    write("client successful",stderr())
                    kfig <<- KeboolaAppConfig$new(.self$client, .self$bucketId, .self$appId)
                    write("keboola config lib loaded",stderr())
                    kdat <<- KeboolaAppData$new(.self$client, .self$bucketId, .self$runId)
                    write("keboola data lib loaded",stderr())
                }, error = function(e){
                    errMsg <<- paste0("Please make sure that the token is valid (", e, ").")
                    write(paste("client not successful",e),stderr())
                })
                if (.self$runId == "") {
                    errMsg <<- paste(errMsg, "This application requires a valid runId in the query string.")
                }
                if (.self$bucketId == "") {
                    errMsg <<- paste(errMsg, "This application requires a valid bucket in the query string.")
                }
                write(paste("what is error message?",errMsg),stderr())
                if (errMsg != '') {
                    error <- div(
                        class = "alert alert-danger",
                        errMsg,
                        p(
                            tag("label", "Token:"),
                            .self$token
                        ),
                        p(
                            tag("label", "Bucket:"),
                            .self$bucketId
                        ),
                        p(
                            tag("label", "Run ID:"),
                            .self$runId
                        )
                    )
                    print(paste0('error occured ', errMsg))
                    loggedIn <<- 0
                } else {
                    error <- div()
                    print('success')
                    loggedIn <<- 1
                }
            } else {
                print('token empty')
                loggedIn <<- 0
                error <- div(class = 'alert alert-warning', 'Please log in.')
            }
            loginErrorOutput <<- error
            print('getLogin exiting')
            list(
                token = .self$token, 
                runId = .self$runId, 
                bucket = .self$bucketId, 
                loggedIn = .self$loggedIn, 
                errorMsg = error,
                client = .self$client
            )
        },
        
        #' @exportMethod
        loadTables = function(session, tables, options = list()) {
            print("LT input ready")
            return(
                .self$kdat$loadTables(session, tables, options)
            )
        },
        
        #' This method returns a list of all elements from the shared library that are destined for the shiny server output object
        #' They are mainly renderUI functions, but can be any render function available in shiny
        #' 
        #' @param session - the shiny server session object
        #' @param options - list of options to tell what objects to include
        #'          appTitle - the title of the application
        #'          dataToSave - the reactive in server.R that holds the input filtered data, or any data that would want to be saved to sapi
        #'          configCallback - the method which is to be invoked when a configuration is loaded.  This method will generally update inputs according to the selectedConfig values
        #'          description - whether or not to include a description object
        #'          customElements - the method for processing custom elements of the description
        #' @exportMethod
        output = function(session, options = list(appTitle = "", dataToSave = NULL, configCallback = NULL, description = FALSE, customElements = NULL)) {
            ret <- list()
            ret$loginMsg <- renderUI({.self$loginErrorOutput})
            ret$loggedIn <- renderText(as.character(.self$loggedIn))        
            if (.self$loggedIn == 1 && session$input$readyElem != '0') {
                print("progress bar initiated")
                progressBar <- shiny::Progress$new(session, min = 1, max = 100)
                progressBar$set(message = 'Initializing', detail = 'Preparing components...')    
                progressBar$set(value = 2)
                write("we are logged in, and getting output elements",stderr())
                if (!(is.null(options$dataToSave))) {
                    ret$dataModalButton <- renderUI({.self$kdat$dataModalButton(session,options$dataToSave)})
                    ret$saveResultUI <- renderUI({.self$kdat$saveResultUI(session,options$dataToSave)})
                } else {
                    ret$saveResultUI <- renderUI({div(class="warning","Sorry, this app does not support data saving")})    
                }
                progressBar$set(value=40)
                if (options$description) {
                    print("get description in klib")
                    ret$description <- renderUI({.self$kdat$getDescription(options$appTitle, options$customElements, session)})
                }
                progressBar$set(value=60)
                if (!(is.null(options$configCallback))) {
                    print("init configs")
                    ret$settingsModalButton <- renderUI({.self$kfig$settingsModalButton(session)})
                    ret$saveConfigUI <- renderUI({.self$kfig$saveConfigUI(session$input)})
                    ret$saveConfigResultUI <- renderUI({.self$kfig$saveConfigResultUI(session)})
                    ret$loadConfigResultUI <- renderUI({.self$kfig$loadConfigResultUI(session, options$configCallback)})
                    ret$configListUI <- renderUI({.self$kfig$configListUI(session)})
                    ret$deleteConfigResultUI <- renderUI({.self$kfig$deleteConfigResultUI(session)})
                }
                print("closing progress bar")
                progressBar$set(value=100)
                progressBar$close()
            } 
            ret
        }
        
    )
)