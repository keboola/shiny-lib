#' Helper Library for Keboola Shiny Applications
#' 
#' @import methods
#' @import shiny
#' @import keboola.sapi.r.client
#' @export KeboolaShiny
#' @exportClass KeboolaShiny

KeboolaShiny <- setRefClass(
    'KeboolaShiny',
    fields = list(
        loggedIn = 'numeric',
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
        getLogin = function(request, input, clientData) {
            print("getLogin")
            token <<- .self$getToken(request, input, clientData)
            runId <<- .self$getRunId(clientData)
            bucketId <<- .self$getBucket(clientData)
            appId <<- .self$getAppId(clientData)
            errMsg <- ""
            if (token != '') {
                tryCatch({
                    client <<- keboola.sapi.r.client::SapiClient$new(token)
                    print("client successful")
                    kfig <<- KeboolaAppConfig$new(.self$client, .self$bucketId, .self$appId)
                    print("keboola config lib loaded")
                    kdat <<- KeboolaAppData$new(.self$client, .self$bucketId, .self$runId)
                    print("keboola data lib loaded")
                }, error = function(e){
                    errMsg <- paste0("Please make sure that the token is valid (", e, ").")
                    print(paste("client not successful",e))
                })
                if (.self$runId == "") {
                    errMsg <- paste(errMsg, "This application requires a valid runId in the query string.")
                }
                if (.self$bucketId == "") {
                    errMsg <- paste(errMsg, "This application requires a valid bucket in the query string.")
                }
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
        
        ###############################
        ## KeboolaAppConfig Wrappers ##
        ###############################
        #'
        
        
        ###############################
        ##  KeboolaAppData Wrappers  ##
        ###############################
        #'
        
        #' @exportMethod
        loadTables = function(tableNames, progressBar = NULL) {
            .self$kdat$loadTables(tableNames, progressBar)
        },
        
        #' @exportMethod
        getDescription = function(descriptor, customElements) {
            .self$kdat$getDescription(descriptor, customElements)
        },
        
        output = function(session, dataToSave = NULL) {
            ret <- list()
            ret$loginMsg <- renderUI({.self$loginErrorOutput})
            print("attempting login element")
            ret$loggedIn <- renderText(as.character(.self$loggedIn))
            print("login element ok")
            
            if (.self$loggedIn == 1) {
                print("attempting to readyElem")
                updateTextInput(session,"readyElem",value="1")
                print("readyElem ok")
                ret$dataModalButton <- renderUI({.self$kdat$dataModalButton(session)})
                ret$settingsModalButton <- renderUI({.self$kfig$settingsModalButton(session)})    
                ret$saveResultUI <- renderUI({.self$kdat$saveResultUI(session,dataToSave)})
            } else {
                ret$dataModalButton <- renderUI({""})
                ret$settingsModalButton <- renderUI({""})
            }
            ret
        },
        
        #' @exportMethod
        getCleanData = function(columnTypeTable, sourceDataTable) {
            .self$kdat$getCleanData(columnTypeTable, sourceDataTable)
        }
    )
)