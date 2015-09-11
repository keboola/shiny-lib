#' This is the configuration component library for managing app configurations
#'
#' @import methods
#' @import shiny
#' @import jsonlite
#' @import keboola.sapi.r.client
#' @export KeboolaAppConfig
#' @exportClass KeboolaAppConfig
KeboolaAppConfig <- setRefClass(
    'KeboolaAppConfig',
    fields = list(
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        bucket = 'character',
        runId = 'character',
        appId = 'character',
        cleanForm = 'logical',
        lastLoadConfigValue = 'numeric',
        lastSaveConfigValue = 'numeric',
        lastDeleteConfigValue = 'numeric',
        configDeleted = 'logical',
        configSaved = 'logical',
        configLoaded = 'logical',
        shinyBaseUrl = 'character'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param sapiClient - Keboola.sapi.r.client::SapiClient
        #' @param bucket - Bucket where config table is stored
        #' @param shinyUrl - Shiny Bundle API home URL
        #'  it will be read from command line argument.
        #' @exportMethod
        initialize = function(sapiClient, bucketId, appId, shinyUrl = "http://shiny.kbc-devel-02.keboola.com/app_dev.php/shiny/") {
            if (is.null(client)) {
                stop("Can not initialize KeboolaAppConfig.  No valid Sapi Client available.")
            }   
            lastSaveConfigValue <<- 0
            lastLoadConfigValue <<- 0
            lastDeleteConfigValue <<- 0
            cleanForm <<- TRUE
            configDeleted <<- FALSE
            configSaved <<- FALSE
            configLoaded <<- FALSE
            client <<- sapiClient
            bucket <<- bucketId
            appId <<- appId
            shinyBaseUrl <<- shinyUrl
        },
       
        configs = function(session) {
            reactive({
                .self$getConfigs(session)
            })
        },
        
        configChoices = function(session) {
            reactive({
                print("getting configs")
                configs <- .self$configs(session)()
                choices <- list()
                for (config in configs) {
                    choices[[paste(config$comment,config$dateCreated,sep=" -- ")]] = config$configId
                }
                print(paste("choices", choices))
                choices
            })
        },
        
        selectedConfig = function(session) {    
            configId <- session$input$config
            configs <- .self$getConfigs(session)
            config <- lapply(configs,function(config) {
                if (config$configId == configId) {
                    jsonlite::fromJSON(config$configuration)
                } else {
                    NULL
                }
            })
            Filter(Negate(is.null),config)[[1]]
        },
        
        getConfigs = function(session) {
            tryCatch({
                configs <- .self$client$genericGet(
                    paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config"),
                    query = list(bucket = .self$bucket)
                )
                return(configs)
            }, error = function(e) {
                # convert the error to a more descriptive message
                stop(paste0("Error loading app configs (", e, ')'))
            })    
        },
        
        #' This method stores the entire session$input object in the kbc storage
        #' in the bucket being used by the app
        #' 
        #' @param Object - Shiny server session object
        #' @return nothing.  Will throw an error if something goes wrong
        saveConfig = function(session) {
           if (is.null(.self$client)) {
               stop("Not connected to SAPI.")
           }
           tryCatch({
               configs <- list()
               for (name in names(session$input)) {
                   configs[[name]] <- session$input[[name]]  
               }
               print(configs)
               obj <- list()
               obj$comment <- configs$configComment
               obj$bucket <- .self$bucket
               obj$config <- configs
               print(paste("saving config post",paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config")))
               print(jsonlite::toJSON(obj))
               resp <- .self$client$genericPost(
                   paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config"),
                   jsonlite::toJSON(obj, auto_unbox=TRUE))
               print("got resp")
               print(resp)
               configSaved <<- TRUE
               return(TRUE)
           }, error = function(e) {
               # convert the error to a more descriptive message
               stop(paste0("Error saving config (", e, ')'))
           })
        },
        
        deleteConfig = function(session, configId) {
            print(paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config/", configId))
            resp <- .self$client$genericDelete(
                paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config/", configId), 
                query = list(bucket = .self$bucket)
            )
            configDeleted <<- TRUE
            print("config deleted!")
            resp
        },
        
        #' @exportMethod
        settingsModalButton = function(session) {
            list(
                keboolaModalButton(
                    "configModal",
                    label = "",
                    icon = icon("gear"),
                    title = "Configuration Settings",
                    content = .self$configSettingsUI(session)
                )
            )
        },
        
        configSettingsUI = function(session) {
            input <- session$input
            input$configModal
            ret <- div(style = 'margin-top: 20px',
                       wellPanel(
                           uiOutput("loadConfigResultUI"),
                           uiOutput("deleteConfigResultUI"),
                           selectInput("config","Configuration",c("None",configChoices(session)())),
                           actionButton("loadConfig", "Load Selected Configuration"),
                           actionButton("deleteConfig", "Delete Selected Configuration", class="btn-danger"),
                           div(style="text-align:right;margin-top:20px;",
                               actionButton("saveConfig", "Save Current Settings", class="btn-primary")
                           ),
                           uiOutput("saveConfigUI")
                       ))
            print("got configs")
            ret    
        },
        
        saveConfigUI = function(input) {
            ret <- list()
            if ((input$saveConfig > 0) && (input$saveConfig %% 2 == 1)) {
                ret <- wellPanel(
                    uiOutput("saveConfigResultUI"),
                    div(
                        textInput("configComment", "Add a comment:"),
                        actionButton("saveConfigForReal", "Save")
                    )
                )
            }
            ret
        },
        
        saveConfigResultUI = function(session) {
            input <- session$input
            ret <- list()
            if (input$saveConfigForReal > 0 && input$saveConfigForReal > .self$lastSaveConfigValue) {
                lastSaveConfigValue <<- as.numeric(input$saveConfigForReal)
                if (nchar(input$configComment) > 0) {
                    write(paste("Saving configuration", lastSaveConfigValue), stderr())
                    tryCatch({
                        print("saving config")
                        .self$saveConfig(session)
                        print("config saved")
                        updateSelectInput(session,"config", choices=c("None",configChoices(session)()))
                        ret <- list(ret,list(div(class = 'alert alert-success', "Configuration successfully saved.")))
                    }, error = function(e) {
                        write(paste("There was a goddamned error", e), stderr())
                        ret <- list(ret,list(div(class = 'alert alert-danger', paste0("Error saving configuration: ", e))))
                        print(paste("error ret"))
                    })
                } else {
                    ret <- list(ret,list(div(class = 'alert alert-warning', "Please enter a comment.")))
                }
            }    
            
            print("saveConfigResult end")
            return(ret) 
        },
        
        loadConfigResultUI = function(session,callback) {
            input <- session$input
            print(paste("loadConfig", input$loadConfig, "last load config value", .self$lastLoadConfigValue, "config", input$config))
            ret <- list()
                if (input$loadConfig > 0 && input$loadConfig > .self$lastLoadConfigValue && input$config != "None") {
                    print("should load")
                    lastLoadConfigValue <<- as.numeric(input$loadConfig)
                    tryCatch({
                        print("getting selected config")
                        config <- .self$selectedConfig(session)
                        print("calling the callback function")
                        print(callback)
                        callback(session, config)
                        print("callback executed")
                        ret <- list(ret,list(div(class = 'alert alert-success', "Configuration successfully loaded.")))
                    }, error = function(e) {
                        ret <- list(ret,list(div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))))
                    })
                }    
            
            ret
        },
        
        deleteConfigResultUI = function(session) {
            input <- session$input
            ret <- list()
            if (input$deleteConfig > 0 && input$deleteConfig > .self$lastDeleteConfigValue) {
                lastDeleteConfigValue <<- as.numeric(input$deleteConfig)
                tryCatch({
                    print(paste("deleting config", input$config))
                    resp <- .self$deleteConfig(session, input$config)
                    print(paste("deleted config", input$config))
                    print(resp)
                    configDeleted <<- TRUE
                    updateSelectInput(session,"config", choices=c("None",configChoices(session)()))
                    ret <- div(class = 'alert alert-success', "Configuration successfully deleted.")
                }, error = function(e) {
                    ret <- div(class = 'alert alert-danger', paste0("Error deleting configuration: ", e))
                })
            }
            ret
        }
    )
)
           
           
           
           