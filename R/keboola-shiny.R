#' Helper Library for Keboola Shiny Applications
#' 
#' @import methods
#' @import shiny
#' @import keboola.sapi.r.client
#' @import keboola.provisioning.r.client
#' @import keboola.redshift.r.client
#' @field session Shiny session object
#' @field loggedIn 1 if the user is logged in, 0 if not
#' @field errMsg Error encountered when logging in
#' @field loginErrrorOutput HTML element with login error
#' @field bucketId ID of the current bucket
#' @field token Current KBC token
#' @field client Instance of Storage API client
#' @field db Instance of Backend Driver (Redshift or Snowflake) 
#' @field type of backend to use
#' @field kfig Instance of Application Config
#' @field kdata Instance of Application Data
#' @field loading Flow control flag set to 1/true while initializing
#' @export KeboolaShiny
#' @exportClass KeboolaShiny
KeboolaShiny <- setRefClass(
    'KeboolaShiny',
    fields = list(
        session = 'ANY',  # shiny server session
        loggedIn = 'numeric',
        errMsg = 'character',
        loginErrorOutput = 'ANY',  # shiny.tag element containing login error message html
        appConfig = 'ANY',
        token = 'character',
        bucket = 'character',
        componentId = 'character',
        configId = 'character',
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        db = 'ANY', # keboola.backend.r.client::BackendDriver
        backendType = 'character',
        kfig = 'ANY', # keboola.shiny.lib::KeboolaAppConfig
        kdat = 'ANY', # keboola.shiny.lib::KeboolaAppData
        loading = 'numeric'
    ),
    methods = list(
        initialize = function(session = getDefaultReactiveDomain()) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{session} Shiny session object.}
            }}"
            if (!inherits(session, "ShinySession"))
                stop("'session' is not a ShinySession object.")
            
            session <<- session
            loggedIn <<- 0
            errMsg <<- ''
            loginErrorOutput <<- ''
            token <<- ''
            configId <<- ''
            componentId <<- ''
            bucket <<- ''
            appConfig <<- NULL
            client <<- NULL
            db <<- NULL
            backendType <<- 'redshift'
            kfig <<- NULL
            kdat <<- NULL
            loading <<- 1
        },
        
        getToken = function() {
            "Get the token from the session headers or input element.
            \\subsection{Return Value}{String KBC token}"
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
        
        getAppConfig = function() {
            "Get the appConfigId from the query string.
             Apps created via the kbc ui will have config starting kbc_
             Apps created via LG will start with lg_
            \\subsection{Return Value}{String storage Bucket ID.}"
            val <- as.character(parseQueryString(session$clientData$url_search)$id)
            if (length(val) == 0) {
                NULL    
            } else {
                if (length(grep("^kbc_", val)) > 0) {
                    # app registered via kbc UI
                    componentId <<- "shiny"
                    configId <<- unlist(strsplit(val,"^kbc_"))[2]
                } else if (length(grep("^lg_", val)) > 0) {
                    # app registered via LG
                    componentId <<- "lg-shiny"
                    configId <<- unlist(strsplit(val,"^lg_"))[2]
                } else {
                    # there wasn't a valid config given
                    stop("Sorry, I need a valid configuration in the url to continue.")
                }
                tryCatch({
                    config <- .self$client$getComponentConfiguration(componentId,configId)    
                }, error = function(e) {
                    # this will most likely be a 404
                    stop(paste("Error retrieving component", componentId, "configuration", configId, e),stderr())
                })
                return(config$configuration)    
            }
        },
        getBucket = function() {
            if (is.null(.self$appConfig$bucket)) {
                if (length(session$input$kb_bucket) == 0) {
                    # bucket not found in config or select input
                    NULL    
                } else {
                    # bucket not found in config, but is set in select input
                    session$input$kb_bucket
                }
            } else {
                # bucket found in config
                .self$appConfig$bucket
            }
        },
        
        getBuckets = function() {
            "Return a list of accessible buckets.
            \\subsection{Return Value}{Character vector of bucketIDs}"
            lapply(.self$client$listBuckets(),function(bucket){ bucket$id })
        },
        
        dbConnect = function() {
            "Establish a connection via provisioning client credentials.
            \\subsection{Return Value}{TRUE}"
            write("Establishing Database Connection", stdout())
            runId <- if (is.null(.self$appConfig$runId)) "" else .self$appConfig$runId
            provisioningClient <- ProvisioningClient$new(.self$backendType, .self$client$token, runId)
            credentials <- provisioningClient$getCredentials('luckyguess')$credentials 
            db <<- BackendDriver$new()
            db$connect(
                credentials$hostname, 
                credentials$db,
                credentials$user,
                credentials$password,
                credentials$schema,
                .self$backendType
            )    
            write("DB Connection Established", stdout())
            TRUE
        },
        
        getLogin = function() {
            "Verify login information and initialize application.
            \\subsection{Return Value}{List with items:
            token, runId, bucket, loggedIn, errMsg, client, ready
            }"
            token <<- .self$getToken()
            errMsg <<- ""
            updateTextInput(session,"kb_loading",value="1")
            updateTextInput(session,"kb_loggedIn",value="1")
            
            # check for a valid token
            if (token != '') {
                tryCatch({
                    client <<- keboola.sapi.r.client::SapiClient$new(token)
                }, error = function(e){
                    errMsg <<- paste0("Please make sure that the token is valid (", e, ").")
                    write(paste("client not successful",e),stderr())
                })
            } else {
                print('token empty')
                loggedIn <<- 0
            }
            error <- div()
            if (!is.null(.self$client)) {
                appConfig <<- .self$getAppConfig()
                bucket <<- .self$getBucket()
                # check to see if appConfig is in the URL
                if (is.null(.self$appConfig)) {
                    errMsg <<- paste(errMsg, "Could not find a valid app configuration {url id parameter}.")
                } else {
                    # we have some configuration
                    if (is.null(.self$bucket)) {
                        # the configuration doesn't have a bucket set
                        # so we'll populate the buckets select input with all buckets that the token has access to.
                        updateTextInput(session,"kb_token",value=.self$token)
                        updateSelectInput(session,"kb_bucket",choices=c("",.self$getBuckets()))
                        errMsg <<- paste(errMsg, "This application requires a valid bucket, please select one.")
                    } 
                }
                if (errMsg == '') {
                    # Login has been sucessful
                    error <- div()
                    loggedIn <<- 1
                    updateTextInput(session,"kb_loggedIn",value="1")
                    session$sendCustomMessage(
                        type = "updateProgress",
                        message = list(id="authenticating", text="Authenticating", value="Completed", valueClass="text-success"))
                    loading <<- 1   
                } else {
                    errorContent <- list(errMsg,
                                         p(
                                             tag("label", "Token:"),
                                             .self$token
                                         ),
                                         p(
                                             tag("label", "Bucket:"),
                                             .self$bucket
                                         ))
                    if (!is.null(.self$appConfig$runId)) {
                        errorContent[[length(errorContent) + 1]] <-
                            p(
                                tag("label", "Run ID:"),
                                .self$appConfig$runId
                            )
                    }
                    error <- div(
                        class = "alert alert-danger",
                        errorContent
                    )
                    write(paste0('Login error occured ', errMsg), stdout())
                    loggedIn <<- 0
                }
            }
            loginErrorOutput <<- error
            
            print('getLogin exiting')
            list(
                token = .self$token, 
                appConfig = .self$appConfig,
                loggedIn = .self$loggedIn, 
                errorMsg = error,
                client = .self$client,
                ready = .self$ready()
            )
        },
        
        initLibs = function() {
            "connect to DB and initialise the keboolaAppData and keboolaAppConfig libraries
            \\subsection{Return Value}{void}"
            tryCatch({
                .self$dbConnect()
                # get database credentials and connect to database
                print("connection established")
                # login was successful
                write("client successful",stdout())
                kfig <<- KeboolaAppConfig$new(.self$client, .self$componentId, .self$configId)
                write("keboola config lib loaded",stdout())
                kdat <<- KeboolaAppData$new(.self$client, .self$appConfig, .self$db)    
                write("keboola data lib loaded",stdout())
            }, error = function(e){
                write(paste("Error initializing libraries:", e), stderr())  
            })
        },
        
        
        ready = function() {
            "The DOM element id = kb_loggedIn is set to 1 when login is successful meaning that data loading can start
            \\subsection{Return Value}{TRUE or FALSE}"
            if (!(is.null(session$input$kb_loggedIn)) && session$input$kb_loggedIn != "0") {
                print("loggedIn, ready.")
                TRUE
            } else {
                print("not loggedIn, not ready.")
                FALSE
            }
        },
        
        
        sourceData = function() {
            "This is a hack used to catch the case when the data-too-large detour has been completed and 
             to resume the startup operations in that case
            \\subsection{Return Value}{list of data.frames containing sourceData}"
            reactive({
                if (!is.null(session$input$kb_continue) && session$input$kb_continue > 0) {
                    print("TRYING TO CONCLUDE STARTUP")
                    .self$concludeStartup(session,NULL)
                }
                .self$kdat$sourceData    
            })                
        },
        
        
        loadTable = function(prettyName, name) {
            "Load table from Storage. 
            (Exposed wrapper for \\code{KeboolaAppData} method).
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{prettyName} Table name to be used in labels throughout the app.}
            \\item{\\code{name} Name of table in SAPI.}
            }}
            \\subsection{Return Value}{data.frame with table data}"
            .self$kdat$loadTable(prettyName, name)
        },
        
        
        loadTables = function(tables, options) {
            "load tables specified in the parameter list tables from storage
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{tables} list of tables to load}
            \\item{\\code{options} startup method options}
            }}
            \\subsection{Return Value}{TRUE if tables succesfully loaded.  FALSE if data was too big and reduction is required.}"
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
            
            if (!is.null(problemTables)) {
                # Set the UI detour flag
                updateTextInput(session, "kb_detour", value="1")
                
                # we found that a table was too huge so we'll initiate diversion...
                print(paste("Some tables are TOO BIG:", names(problemTables)))
                session$output$kb_problemTables <- renderUI({
                    .self$kdat$problemTablesUI(problemTables)
                })  
                FALSE    
            } else {
                # no table was deemed too big so we go ahead with loading
                print("Tables are not so big")
                print(paste("loading",tables))
                .self$kdat$loadTables(tables, options)
                TRUE
            }
        },
        
        concludeStartup = function(options) {
            "Resume the startup tasks after data loading
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{options} initial startup options}
            }}
            \\subsection{Return Value}{void}"
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="finalising", text="Just a couple more things...", value="In Progress", valueClass="text-primary")
            )
            
            # if the data storing option flag is set, we load our output elements with the UI functions.
            if (!(is.null(options$dataToSave))) {
                session$output$kb_dataModalButton <- renderUI({.self$kdat$dataModalButton(options$dataToSave)})
                session$output$kb_saveResultUI <- renderUI({.self$kdat$saveResultUI(options$dataToSave)})
            } else {
                session$output$kb_saveResultUI <- renderUI({div(class="warning","Sorry, this app does not support data saving")})    
            }
            
            if (options$cleanData == TRUE && c("cleanData", "columnTypes") %in% names(.self$kdat$sourceData)) {
                kdat$sourceData$columnTypes <<- .self$kdat$sourceData$columnTypes[,!names(.self$kdat$sourceData$columnTypes) %in% c("run_id", "_timestamp")]
                print(paste("GETTING CLEAN DATA",names(.self$kdat$sourceData$cleanData)))
                kdat$sourceData$cleanData <<- .self$kdat$getCleanData(.self$kdat$sourceData$columnTypes, .self$kdat$sourceData$cleanData)
            }
            
            if (options$description == TRUE) {
                print("finished detour, get description")
                kdat$sourceData$descriptor <<- .self$kdat$getDescriptor()
                session$output$description <- renderUI({.self$kdat$getDescription(options$appTitle, options$customElements)})
            }
            
            # if the input config option (the callback function) is given,
            # we need to populate the session output object with the UI elements for the modal.
            if (!(is.null(options$configCallback))) {
                print("init configs")
                session$output$kb_settingsModalButton <- renderUI({.self$kfig$settingsModalButton()})
                session$output$kb_saveConfigUI <- renderUI({.self$kfig$saveConfigUI()})
                session$output$kb_saveConfigResultUI <- renderUI({.self$kfig$saveConfigResultUI()})
                session$output$kb_loadConfigResultUI <- renderUI({.self$kfig$loadConfigResultUI(options$configCallback)})
                session$output$kb_configSelectorUI <- renderUI({.self$kfig$configSelectorUI()})
                session$output$kb_deleteConfigResultUI <- renderUI({.self$kfig$deleteConfigResultUI()})
            }
            
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="finalising", text="Just a couple more things...", value="Completed", valueClass="text-success"))
            
            updateTextInput(session,"kb_detour", value="0")
            updateTextInput(session,"kb_loading",value="0")    
            print("STARTUP CONClUDED")
        },
        
        
        startup = function(options = list(
                appTitle = "",          # application title
                tables = list(),        # list of tables to load from sapi list(localname = sapiname)
                cleanData = FALSE,      # whether to use datatype conversion for the cleandata table
                dataToSave = NULL,      # name of the reactive method that produces filtered data to save back to sapi
                configCallback = NULL,  # callback function which sets inputs when input configuration chosen
                description = FALSE,    # get the descriptor?
                customElements = NULL,  # function to process custom descriptor elements
                backend = "redshift"    # type of backend, redshift or snowflake
            )
        ){
            "This is the main KBC app entry point for all initialisation housekeeping such as authentication and data retrieval
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{options} List with items:
            appTitle - the title of the application
            tables - list of tables to load from sapi list(localname = sapiname)
            dataToSave - the reactive in server.R that holds the input filtered data, or any data that would want to be saved to sapi
            configCallback - the method which is to be invoked when a configuration is loaded.  This method will generally update inputs according to the selectedConfig values
            description - whether or not to include a description object
            customElements - the method for processing custom elements of the description}
            }}
            \\subsection{Return Value}{list containing loginInfo}"
            ret <- list()
            if (.self$loggedIn == 1 && .self$loading == 1 && !(is.null(.self$kdat)) && .self$kdat$allLoaded == TRUE) {
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="data_retrieval", text="Fetching Data", value="Completed", valueClass="text-success"))
                
                # Finish the startup process 
                concludeStartup(options)
                write("Startup Finished.", stdout())
                return()
            }
            session$output$kb_loginMsg <- renderUI({.self$loginErrorOutput})
            
            session$sendCustomMessage(
                type = "updateProgress",
                message = list(id="authenticating", text="Authenticating", value="In Progress", valueClass="text-primary"))
            
            ret$loginInfo <- .self$getLogin()
            updateTextInput(session,"kb_loggedIn",value=as.character(.self$loggedIn))
            
            if (.self$loggedIn == 1 && .self$ready()) {    
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="authenticating", text="Authenticating", value="Completed", valueClass="text-success"))
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="connecting", text="Establishing Connection", value="In Progress", valueClass="text-primary"))
                backendType <<- backend    
                .self$initLibs()
                
                session$sendCustomMessage(
                    type = "updateProgress",
                    message = list(id="connecting", text="Establishing Connection", value="Completed", valueClass="text-success"))
                
                print("Begin Data Loading")
                
                loadDataOptions <- options
                
                if (!is.null(options$tables) && options$tables == "recipeTables") {
                    options$tables <- .self$kdat$getRecipeTables(options)
                }
                
                if (is.null(options$tables)) {
                    # Finish the startup process
                    print("No tables given to load")
                    concludeStartup(options)
                } else {
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
                }    
            } 
            ret       
        }        
    )
)