#' Helper Library for Keboola Shiny Applications
#' 
#' @import methods
#' @import shiny
#' @import keboola.sapi.r.client
#' @import keboola.provisioning.r.client
#' @import keboola.redshift.r.client
#' @export KeboolaShiny
#' @exportClass KeboolaShiny

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
        db = 'ANY', # keboola.redshift.r.client::RedshiftDriver
        kfig = 'ANY', # keboola.shiny.lib::KeboolaAppConfig
        kdat = 'ANY', # keboola.shiny.lib::KeboolaAppData
        useRunId = 'logical',
        loading = 'numeric',
        startupOptions = 'list' # placeholder for options for when detour is needed
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then
        #'  it will be read from command line argument.
        #' @exportMethod
        initialize = function(requireRunId = TRUE) {
            loggedIn <<- 0
            errMsg <<- ''
            loginErrorOutput <<- ''
            bucketId <<- ''
            runId <<- ''
            appId <<- ''
            token <<- ''
            client <<- NULL
            db <<- NULL
            kfig <<- NULL
            kdat <<- NULL
            useRunId <<- requireRunId
            loading <<- 1
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
        
        #' Get the configId from the URL
        #' 
        #' @param clientData -- shiny session clientData object
        #' @return configId or ""
        #' @exportMethod
        getConfigId = function(clientData) {
            as.character(parseQueryString(clientData$url_search)$config)
        },
        
        #' establish a connection via provisioning client credentials
        dbConnect = function(session) {
            print("in dbConnect")
            provisioningClient <- ProvisioningClient$new('redshift', .self$client$token, .self$runId)
            credentials <- provisioningClient$getCredentials('transformations')$credentials 
            db <<- RedshiftDriver$new()
            db$connect(
                credentials$host, 
                credentials$db,
                credentials$user,
                credentials$password,
                credentials$schema
            )    
            print("initialization complete")
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
            updateTextInput(session,"loading",value="1")
            updateTextInput(session,"loggedIn",value="1")
            
            if (.self$useRunId == TRUE && .self$runId == "") {
                errMsg <<- paste(errMsg, "This application requires a valid runId in the query string.")
            }
            if (.self$bucketId == "") {
                errMsg <<- paste(errMsg, "This application requires a valid bucket in the query string.")
            }
            if (token != '') {
                tryCatch({
                    client <<- keboola.sapi.r.client::SapiClient$new(token)
                }, error = function(e){
                    errMsg <<- paste0("Please make sure that the token is valid (", e, ").")
                    write(paste("client not successful",e),stderr())
                })
                
                write(paste("what is error message?",errMsg),stderr())
                if (errMsg != '') {
                    errorContent <- list(errMsg,
                                      p(
                                          tag("label", "Token:"),
                                          .self$token
                                      ),
                                      p(
                                          tag("label", "Bucket:"),
                                          .self$bucketId
                                      ))
                    if (.self$useRunId) {
                        errorContent[[length(errorContent) + 1]] <-
                            p(
                                tag("label", "Run ID:"),
                                .self$runId
                            )
                    }
                    error <- div(
                        class = "alert alert-danger",
                        errorContent
                    )
                    print(paste0('error occured ', errMsg))
                    loggedIn <<- 0
                } else {
                    # Login has been sucessful
                    updateTextInput(session,"readyElem",value="1")
                    error <- div()
                    print('success')
                    loggedIn <<- 1
                    updateTextInput(session,"loggedIn",value="1")
                    session$sendCustomMessage(
                        type = "updateProgress",
                        message = list(id="authenticating", text="Authenticating", value="Completed", valueClass="text-success"))
                    loading <<- 1
                    
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
                client = .self$client,
                ready = .self$ready(session)
            )
        },
        
        #' @exportMethod
        initLibs = function(session) {
            print("initLibs?")
            tryCatch({
                
                print("connecting via provisioning client")
                .self$dbConnect(session)
                # get database credentials and connect to database
                print("connection established")
                # login was successful
                write("client successful",stderr())
                kfig <<- KeboolaAppConfig$new(.self$client, .self$bucketId, .self$appId)
                write("keboola config lib loaded",stderr())
                kdat <<- KeboolaAppData$new(.self$client, .self$bucketId, .self$runId, .self$db)    
                write("keboola data lib loaded",stderr())
            }, error = function(e){
                write(paste("Error initializing libraries:", e), stderr())  
            })
        },
        
        ready = function(session) {
            if (!(is.null(session$input$readyElem)) && session$input$readyElem != "0") {
                print("READy")
                TRUE
            } else {
                print("NOT READY")
                FALSE
            }
        },
        
        #' @exportMethod
        loadTablesDirect = function(session, tables, options) {
            # add defaults if they are missing
            if (missing(options)) {
                options <- list(
                    cleanData = FALSE,
                    descriptor = FALSE
                )
            }
            # check the sizes of the tables in case any are too big
            # checkTables will return a list of tables that are "too big" to load as is
            problemTables <- .self$kdat$checkTables(tables)
            print(paste("GOT MEM TRIGGER",names(problemTables)))
            if (length(problemTables) > 0) {
                print("update text input")
                updateTextInput(session, "detour", value="1")
                # we found that a table was too huge so we'll initiate diversion...
                print("Some tables are TOO BIG")
                session$output$problemTables <- renderUI(
                    list(
                        .self$kdat$problemTablesTabset(session, problemTables),
                        fluidRow(
                            column(6, div()),
                            column(3, actionButton("cancel","Cancel")),
                            column(3, actionButton("load", "Continue"))
                        )
                    )
                )
                FALSE    
            } else {
                # no table was deemed too big so we go ahead with loading
                print("All tables are not so big")
                .self$kdat$loadTablesDirect(session, tables, options)
                TRUE
            }
        },
        
        concludeStartup = function(session, options) {
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="finalising", text="Just a couple more things...", value="In Progress", valueClass="text-primary"))
            
            if (!(is.null(options$dataToSave))) {
                session$output$dataModalButton <- renderUI({.self$kdat$dataModalButton(session,options$dataToSave)})
                session$output$saveResultUI <- renderUI({.self$kdat$saveResultUI(session,options$dataToSave)})
            } else {
                session$output$saveResultUI <- renderUI({div(class="warning","Sorry, this app does not support data saving")})    
            }
            if (options$description) {
                print("get description in klib")
                session$output$description <- renderUI({.self$kdat$getDescription(options$appTitle, options$customElements, session)})
            }
            
            if (!(is.null(options$configCallback))) {
                print("init configs")
                session$output$settingsModalButton <- renderUI({.self$kfig$settingsModalButton(session)})
                session$output$saveConfigUI <- renderUI({.self$kfig$saveConfigUI(session$input)})
                session$output$saveConfigResultUI <- renderUI({.self$kfig$saveConfigResultUI(session)})
                session$output$loadConfigResultUI <- renderUI({.self$kfig$loadConfigResultUI(session, options$configCallback)})
                session$output$configListUI <- renderUI({.self$kfig$configListUI(session)})
                session$output$deleteConfigResultUI <- renderUI({.self$kfig$deleteConfigResultUI(session)})
            }
            
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="finalising", text="Just a couple more things...", value="Completed", valueClass="text-success"))
        },
        
        #' @exportMethod
        sourceData = function() {
            reactive({
                .self$kdat$sourceData    
            })
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
        startup = function(session, 
                          options = list(
                                        appTitle = "",          # application title
                                        tables = list(),        # list of tables to load from sapi list(localname = sapiname)
                                        cleanData = FALSE,      # whether to use datatype conversion for the cleandata table
                                        dataToSave = NULL,      # name of the reactive method that produces filtered data to save back to sapi
                                        configCallback = NULL,  # callback function which sets inputs when input configuration chosen
                                        description = FALSE,    # get the descriptor?
                                        customElements = NULL   # function to process custom descriptor elements
                                    )
                          ) {
            
            ret <- list()
            sourceData <- NULL
            
            
            session$output$loginMsg <- renderUI({.self$loginErrorOutput})
            
            tableDisp <- lapply(options$tables,function(table){
                fluidRow(
                    column(9,div(paste("Loading Table", options$tables[[table]]))),
                    column(3,textInput(paste0("table_",table),""))
                )
            })
            session$output$loadingUI <- renderUI({tableDisp})
            
            print("doing login")
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="authenticating", text="Authenticating", value="In Progress", valueClass="text-primary"))
            
            ret$loginInfo <- .self$getLogin(session)
            print("login done")
            updateTextInput(session,"loggedIn",value=as.character(.self$loggedIn))
            
            if (.self$loggedIn == 1 && .self$ready(session)) {    
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="authenticating", text="Authenticating", value="Completed", valueClass="text-success"))
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="connecting", text="Establishing Connection", value="In Progress", valueClass="text-primary"))
                
                .self$initLibs(session)
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="connecting", text="Establishing Connection", value="Completed", valueClass="text-success"))
                
                print("connected")
                
                loadDataOptions <- list(cleanData = options$cleanData)
                if (options$description) {
                    loadDataOptions$descriptor = TRUE
                }
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="data_retrieval", text="Fetching Data", value="In Progress", valueClass="text-primary"))
                
                # load the requested data
                dataHasLoaded <- loadTablesDirect(session, options$tables, options = loadDataOptions)
                
                print("DO WE EVER GET HERE????")
                
                if (dataHasLoaded == TRUE) { 
                    # The load tables method has been successful, 
                    # resume startup processing
                    session$sendCustomMessage(
                        type = "updateProgress",
                        message = list(id="data_retrieval", text="Fetching Data", value="Completed", valueClass="text-success"))
                    
                    # Finish the startup process
                    concludeStartup(session,options)
                    
                    updateTextInput(session,"loading",value="0")    
                } else {
                    # Some tables were found to be too large to import.
                    # The detour workflow is in progress.  
                    # We want to remember the startup options, so they can be used in the resumeStartup method.
                    startupOptions <<- options
                }
            } 
            ret       
        }        
    )
)