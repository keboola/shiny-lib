#' This is the configuration component library for managing app configurations
#'
#' @import methods
#' @import shiny
#' @import keboola.shiny.lib
#' @export KeboolaAppConfig
#' @exportClass KeboolaAppConfig
KeboolaAppConfig <- setRefClass(
    'KeboolaAppConfig',
    fields = list(
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        bucket = 'character',
        runId = 'character',
        appId = 'character',
        lastLoadConfigValue = 'numeric',
        lastSaveConfigValue = 'numeric',
        lastDeleteConfigValue = 'numeric',
        configDeleted = 'logical',
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
            configDeleted <<- FALSE
            client <<- sapiClient
            bucket <<- bucketId
            appId <<- appId
            shinyBaseUrl <<- shinyUrl
        },
       
        saveConfig = function(session) {
           if (is.null(.self$client)) {
               stop("Not connected to SAPI.")
           }
           tryCatch({
               resp <- .self$client$genericPost(
                   paste0(.self$shinyBaseUrl,"apps/",.self$getAppId(session$clientData),"/config"),
                   jsonlite::toJSON(input))
               return(TRUE)
           }, error = function(e) {
               # convert the error to a more descriptive message
               stop(paste0("Error saving config (", e, ')'))
           })
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
            if (input$saveConfigForReal > .self$lastSaveConfigValue) {
                if (nchar(input$configComment) > 0) {
                    lastSaveConfigValue <<- input$saveConfigForReal
                    write(paste("Saving configuration", lastSaveConfigValue), stderr())
                    tryCatch({
                        print("saving config")
                        .self$saveConfig(session)
                        print("config saved")
                    }, error = function(e) {
                        ret <- div(class = 'alert alert-danger', paste0("Error saving configuration: ", e))
                    })
                    ret <- div(class = 'alert alert-success', "Configuration successfully saved.")
                } else {
                    ret <- div(class = 'alert alert-warning', "Please enter a comment.")
                }
            } else {
                ret <- div()
            }
            print("saveConfigResult end")
            return(list(ret)) 
        },
        
        configSettingsUI = function(session) {
            input <- session$input
            print("getting configs")
            configs <- .self$configs(session)
            choices <- list()
            for (config in configs) {
                choices[[paste(config$comment,config$dateCreated,sep=" -- ")]] = config$configId
            }
            print(paste("choices", choices))
            ret <- div(style = 'margin-top: 20px',
                       wellPanel(
                           uiOutput("loadConfigResultUI"),
                           uiOutput("deleteConfigResultUI"),
                           selectInput("config","Configuration",c("None",choices)),
                           actionButton("loadConfig", "Load Selected Configuration"),
                           actionButton("deleteConfig", "Delete Selected Configuration", class="btn-warning"),
                           div(style="text-align:right;margin-top:20px;",
                               actionButton("saveConfig", "Save Current Settings", class="btn-primary")
                           ),
                           uiOutput("saveConfigUI")
                       ))
            print("got configs")
            ret
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
        
        #' @exportMethod
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
        
        selectedConfig = function(input) {
            if (input$loadConfig > 0 && input$loadConfig > lastLoadConfigValue) {
                lastLoadConfigValue <<- input$loadConfig
                configId <- input$config
                configs <- configs()
                config <- lapply(configs,function(config) {
                    if (config$configId == configId) {
                        jsonlite::fromJSON(config$configuration)
                    } else {
                        NULL
                    }
                })
                Filter(Negate(is.null),config)[[1]]
            }
        },
        
        loadConfigResultUI = function(session,callback) {
            input <- session$input
            print(paste("loadConfig", input$loadConfig, "config", input$config))
            ret <- list()
            if (input$loadConfig > 0 && input$loadConfig > lastLoadConfig) {
                tryCatch({
                    config <- selectedConfig()
                    callback(session, config)
                }, error = function(e) {
                    ret <- div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))
                })
                ret <- div(class = 'alert alert-success', "Configuration successfully loaded.")
                print("what? FU")
                session$sendCustomMessage(type="background_task", message=list(message="Loaded up the selected configuration.",type="alert"))
            }
            ret
        }
    )
)
           
           
           
           