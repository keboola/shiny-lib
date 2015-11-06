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
        session = 'ANY',  # shiny server session
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
        loading = 'numeric'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then
        #'  it will be read from command line argument.
        #' @exportMethod
        initialize = function(requireRunId = TRUE, session = getDefaultReactiveDomain()) {
            
            if (!inherits(session, "ShinySession"))
                stop("'session' is not a ShinySession object.")
            
            session <<- session
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
        #' @return runId
        getRunId = function() {
            val <- as.character(parseQueryString(session$clientData$url_search)$runId)
            if (length(val) == 0) {
                val <- ''
            }
            return(val)
        },

        
        #' Get the bucket from the query string
        #' 
        #' @return bucket
        getBucket = function() {
            val <- as.character(parseQueryString(session$clientData$url_search)$bucket)
            if (length(val) == 0) {
                val <- ''
            }
            return(val)
        },

        
        #' Get the token from the session headers or input element
        #' 
        #' @return the supplied token
        getToken = function() {
            val <- as.character(session$request$HTTP_X_STORAGEAPI_TOKEN)
            if (length(val) == 0) {
                val <- as.character(parseQueryString(session$clientData$url_search)$token)
                if (length(val) == 0) {
                    val <- as.character(session$input$kb_token)
                    if (length(val) == 0) {
                        val <- ''
                    }
                }
            }
            return(val)
        },

        #' Get the AppId from the URL
        #'
        #' @return appId
        #' @exportMethod
        getAppId = function() {
            appId <<- unlist(strsplit(session$clientData$url_pathname,"/"))[1]
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
        getConfigId = function() {
            as.character(parseQueryString(session$clientData$url_search)$config)
        },
        
        #' establish a connection via provisioning client credentials
        dbConnect = function() {
            print("in dbConnect")
            provisioningClient <- ProvisioningClient$new('redshift', .self$client$token, .self$runId)
            credentials <- provisioningClient$getCredentials('luckyguess')$credentials 
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
        #' @return html string containing error message and field settings
        #' @exportMethod
        getLogin = function() {
            print("getLogin")
            token <<- .self$getToken()
            runId <<- .self$getRunId()
            bucketId <<- .self$getBucket()
            appId <<- .self$getAppId()
            errMsg <<- ""
            updateTextInput(session,"kb_loading",value="1")
            updateTextInput(session,"kb_loggedIn",value="1")
            
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
                    updateTextInput(session,"kb_readyElem",value="1")
                    error <- div()
                    print('success')
                    loggedIn <<- 1
                    updateTextInput(session,"kb_loggedIn",value="1")
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
                ready = .self$ready()
            )
        },
        
        initLibs = function() {
            print("initLibs?")
            tryCatch({
                
                print("connecting via provisioning client")
                .self$dbConnect()
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
        
        ready = function() {
            if (!(is.null(session$input$kb_readyElem)) && session$input$kb_readyElem != "0") {
                print("READy")
                TRUE
            } else {
                print("NOT READY")
                FALSE
            }
        },
        
        #' @exportMethod
        sourceData = function() {
            reactive({
                if (!is.null(session$input$kb_continue) && session$input$kb_continue > 0) {
                    print("TRYING TO CONCLUDE STARTUP")
                    .self$concludeStartup(session,NULL)
                }
                .self$kdat$sourceData    
            })                
        },
        
        #' Exposed wrapper for Kdat's method of the same name.
        #'
        #' @exportMethod
        loadTable = function(prettyName, name) {
            .self$kdat$loadTable(prettyName, name)
        },
        
        #' @exportMethod
        loadTables = function(tables, options) {
            # add defaults if they are missing
            if (missing(options)) {
                options <- list(
                    cleanData = FALSE,
                    description = FALSE
                )
            }
            # check the sizes of the tables in case any are too big
            # checkTables will return a list of tables that are "too big" to load as is
            problemTables <- .self$kdat$checkTables(tables)
            print(paste("GOT MEM TRIGGER",names(problemTables)))
            if (!is.null(problemTables)) {
                # Set the UI detour flag
                updateTextInput(session, "kb_detour", value="1")
                
                # we found that a table was too huge so we'll initiate diversion...
                print(paste("Some tables are TOO BIG", names(problemTables)))
                print(problemTables)
                session$output$kb_problemTables <- renderUI(
                    .self$kdat$problemTablesUI(problemTables)
                )
                FALSE    
            } else {
                # no table was deemed too big so we go ahead with loading
                print("Tables are not so big")
                .self$kdat$loadTables(tables, options)
                TRUE
            }
        },
        
        concludeStartup = function(options) {
            
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="finalising", text="Just a couple more things...", value="In Progress", valueClass="text-primary"))
                
                if (!(is.null(options$dataToSave))) {
                    session$output$kb_dataModalButton <- renderUI({.self$kdat$dataModalButton(options$dataToSave)})
                    session$output$kb_saveResultUI <- renderUI({.self$kdat$saveResultUI(options$dataToSave)})
                } else {
                    session$output$kb_saveResultUI <- renderUI({div(class="warning","Sorry, this app does not support data saving")})    
                }
                print(paste("post data to save", names(.self$kdat$sourceData)))
                print(paste("options cleandata?", options$cleanData))
                print(c("cleanTable", "columnTypes") %in% names(.self$kdat$sourceData))
                print("was that true?")
                if (options$cleanData == TRUE && c("cleanTable", "columnTypes") %in% names(.self$kdat$sourceData)) {
                    kdat$sourceData$columnTypes <<- .self$kdat$sourceData$columnTypes[,!names(.self$kdat$sourceData$columnTypes) %in% c("run_id")]
                    print(paste("GETTING CLEAN DATA",names(.self$kdat$sourceData$cleanTable)))
                    kdat$sourceData$cleanData <<- .self$kdat$getCleanData(.self$kdat$sourceData$columnTypes, .self$kdat$sourceData$cleanTable)
                }
                
                print("data cleaned")
                if (options$description == TRUE) {
                    print("finished detour, get description")
                    kdat$sourceData$descriptor <<- .self$kdat$getDescriptor()
                    session$output$description <- renderUI({.self$kdat$getDescription(options$appTitle, options$customElements)})
                }
                print("description")
                if (!(is.null(options$configCallback))) {
                    print("init configs")
                    session$output$kb_settingsModalButton <- renderUI({.self$kfig$settingsModalButton()})
                    session$output$kb_saveConfigUI <- renderUI({.self$kfig$saveConfigUI()})
                    session$output$kb_saveConfigResultUI <- renderUI({.self$kfig$saveConfigResultUI()})
                    session$output$kb_loadConfigResultUI <- renderUI({.self$kfig$loadConfigResultUI(options$configCallback)})
                    session$output$kb_configListUI <- renderUI({.self$kfig$configListUI()})
                    session$output$kb_deleteConfigResultUI <- renderUI({.self$kfig$deleteConfigResultUI()})
                }
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="finalising", text="Just a couple more things...", value="Completed", valueClass="text-success"))
                
                updateTextInput(session,"kb_detour", value="0")
                updateTextInput(session,"kb_loading",value="0")    
                print("STARTUP CONClUDED")
        },
        
        #' This method returns a list of all elements from the shared library that are destined for the shiny server output object
        #' They are mainly renderUI functions, but can be any render function available in shiny
        #' 
        #' @param appTitle - the title of the application
        #' @param tables - list of tables to load from sapi list(localname = sapiname)
        #' @param dataToSave - the reactive in server.R that holds the input filtered data, or any data that would want to be saved to sapi
        #' @param configCallback - the method which is to be invoked when a configuration is loaded.  This method will generally update inputs according to the selectedConfig values
        #' @param description - whether or not to include a description object
        #' @param customElements - the method for processing custom elements of the description
        #' @exportMethod
        startup = function(options = list(
                                appTitle = "",          # application title
                               tables = list(),        # list of tables to load from sapi list(localname = sapiname)
                               cleanData = FALSE,      # whether to use datatype conversion for the cleandata table
                               dataToSave = NULL,      # name of the reactive method that produces filtered data to save back to sapi
                               configCallback = NULL,  # callback function which sets inputs when input configuration chosen
                               description = FALSE,    # get the descriptor?
                               customElements = NULL   # function to process custom descriptor elements           
                        )
        ){
                            
            ret <- list()
            if (.self$loggedIn == 1 && .self$loading == 1 && !(is.null(.self$kdat)) && .self$kdat$allLoaded == TRUE) {
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="data_retrieval", text="Fetching Data", value="Completed", valueClass="text-success"))
                
                # Finish the startup process  TODO, better way to pass through parameters, maybe ...
                concludeStartup(options)
                print("RETURNING ABORT")
                return()
            }
            session$output$kb_loginMsg <- renderUI({.self$loginErrorOutput})
            
            print("doing login")
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="authenticating", text="Authenticating", value="In Progress", valueClass="text-primary"))
            print("getLogin method")
            ret$loginInfo <- .self$getLogin()
            print("login done")
            updateTextInput(session,"kb_loggedIn",value=as.character(.self$loggedIn))
            
            if (.self$loggedIn == 1 && .self$ready()) {    
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="authenticating", text="Authenticating", value="Completed", valueClass="text-success"))
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="connecting", text="Establishing Connection", value="In Progress", valueClass="text-primary"))
                
                .self$initLibs()
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="connecting", text="Establishing Connection", value="Completed", valueClass="text-success"))
                
                print("Begin Data Loading")
                
                loadDataOptions <- options
                
                if (options$tables == "recipeTables") {
                    options$tables <- .self$kdat$getRecipeTables(options)
                }
                
                if (!is.null(options$tables) && length(options$tables) > 0) {
                    session$sendCustomMessage(
                        type = "updateProgress",
                        message = list(id="data_retrieval", text="Fetching Data", value="In Progress", valueClass="text-primary"))
                    
                    # load the requested data
                    dataHasLoaded <- loadTables(options$tables, options = loadDataOptions)
                    
                    if (dataHasLoaded == TRUE) { 
                        # The load tables method has been successful, 
                        # resume startup processing
                        session$sendCustomMessage(
                            type = "updateProgress",
                            message = list(id="data_retrieval", text="Fetching Data", value="Completed", valueClass="text-success"))
                        
                        # Finish the startup process
                        concludeStartup(options)
                        
                    } else {
                        # Some tables were found to be too large to import.
                        # The detour workflow is in progress.  
                        # do nothing
                    }    
                } else {
                    # Finish the startup process
                    concludeStartup(options)
                }    
            } 
            ret       
        }        
    )
)