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
        session = 'ANY', # shiny server session
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        bucket = 'character',
        runId = 'character',
        appId = 'character',
        lastModalButtonValue = 'numeric',
        lastLoadConfigValue = 'numeric',
        lastSaveConfigValue = 'numeric',
        lastDeleteConfigValue = 'numeric',
        lastConfirmDeleteValue = 'numeric',
        lastConfirmCancelValue = 'numeric',
        clearModal = 'logical',
        shinyBaseUrl = 'character'
    ),
    methods = list(
        initialize = function(sapiClient, bucketId, appId, shinyUrl = "https://shiny.keboola.com/shiny/", session = getDefaultReactiveDomain()) {
            "Constructor.
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{sapiClient} Storage API client.}
            \\item{\\code{bucket} Bucket where config table is stored.}
            \\item{\\code{shinyUrl} Shiny Bundle API home URL.
            It will be read from command line argument.}
            }}"
            if (!inherits(session, "ShinySession"))
                stop("'session' is not a ShinySession object.")
            
            if (is.null(client)) {
                stop("Can not initialize KeboolaAppConfig.  No valid Sapi Client available.")
            }   
            session <<- session
            lastSaveConfigValue <<- 0
            lastLoadConfigValue <<- 0
            lastDeleteConfigValue <<- 0
            lastConfirmDeleteValue <<- 0
            lastConfirmCancelValue <<- 0
            lastModalButtonValue <<- 0
            clearModal <<- FALSE
            client <<- sapiClient
            bucket <<- bucketId
            appId <<- appId
            shinyBaseUrl <<- shinyUrl
        },
       
        configs = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            reactive({
                .self$getConfigs()
            })
        },
        
        configChoices = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            reactive({
                configs <- .self$configs()()
                choices <- list()
                for (config in configs) {
                    choices[[paste(config$comment,config$dateCreated,sep=" -- ")]] <- config$configId
                }
                choices    
            })
        },
        
        selectedConfig = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            configId <- session$input$kb_config
            if (is.null(configId) || configId == "None") return(NULL)
            configs <- .self$configs()()
            config <- lapply(configs,function(config) {
                if (config$configId == configId) {
                    jsonlite::fromJSON(config$configuration)
                } else {
                    NULL
                }
            })
            Filter(Negate(is.null),config)[[1]]   
        },
        
        getConfigs = function() {
            "Get app configurations from the Shiny Bundle.
            \\subsection{Return Value}{List of app configurations.}"
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
        
        saveConfig = function() {
            "This method stores the entire session$input object in the kbc storage
            in the bucket being used by the app
            \\subsection{Return Value}{TRUE, will throw an error if something goes wrong.}"
           if (is.null(.self$client)) {
               stop("Not connected to SAPI.")
           }
           tryCatch({
               configs <- list()
               for (name in names(session$input)) {
                   # only store non-system inputs
                   if (length(grep("^kb_", name)) == 0) {
                       configs[[name]] <- session$input[[name]]      
                   }
               }
               obj <- list()
               obj$comment <- configs$kb_configComment
               obj$bucket <- .self$bucket
               obj$config <- configs
               resp <- .self$client$genericPost(
                   paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config"),
                   jsonlite::toJSON(obj, auto_unbox=TRUE))
               return(TRUE)
           }, error = function(e) {
               # convert the error to a more descriptive message
               stop(paste0("Error saving config (", e, ')'))
           })
        },
        
        deleteConfig = function(configId) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{configId} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            resp <- .self$client$genericDelete(
                paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config/", configId), 
                query = list(bucket = .self$bucket)
            )
            print("Config", configId, "deleted!")
            resp
        },
        
        settingsModalButton = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            list(
                keboolaModalButton(
                    "kb_configModal",
                    label = "",
                    icon = icon("gear"),
                    title = "Configuration Settings",
                    content = .self$configSettingsUI()
                )
            )
        },
        
        clearForm = function(input) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{input} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            reactive({
                input$kb_configModalButton
                input$kb_config
                isolate({
                    if (!(is.null(input$kb_configModalButton)) && 
                            input$kb_configModalButton > 0 && 
                            input$kb_configModalButton > .self$lastModalButtonValue) {
                        lastModalButtonValue <<- as.numeric(input$kb_configModalButton)
                        clearModal <<- TRUE
                        return(TRUE)
                    } else {
                        clearModal <<- FALSE
                        return(FALSE)
                    }    
                })
            })
        },
        
        configSettingsUI = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            input <- session$input
            ret <- list(
                       div(style="text-align:right;padding:0 19px 15px 0;",
                           actionButton("kb_saveConfig", "Save Current Settings", class="btn-primary")
                       ),
                       uiOutput("kb_saveConfigUI"),
                       wellPanel(
                            uiOutput("kb_loadConfigResultUI"),
                            uiOutput("kb_deleteConfigResultUI"),
                            uiOutput("kb_configListUI"),
                            fluidRow(
                                column(6,actionButton("kb_loadConfig", "Load Selected Configuration",
                                                      `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#loadConfigResultUI")),
                                column(6,actionButton("kb_deleteConfig", "Delete Selected Configuration", 
                                                      class="btn-danger", `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#deleteConfigResultUI"),
                                        class=" text-right")
                            )
                       )
                    )
            ret    
        },
        
        configListUI = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            selectInput("kb_config","Configuration",c("None",configChoices()()))
        },
        
        saveConfigUI = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            ret <- list()
            input <- session$input
            .self$clearForm(input)()
            if ((input$kb_saveConfig > 0) && (input$kb_saveConfig %% 2 == 1) && !.self$clearModal) {
                ret <- wellPanel(
                    uiOutput("kb_saveConfigResultUI"),
                    div(
                        textInput("kb_configComment", "Add a comment:"),
                        actionButton("kb_saveConfigForReal", "Save")
                    )
                )
            }
            ret
        },
        
        saveConfigResultUI = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            input <- session$input
            ret <- list()
            .self$clearForm(input)()
            if (input$kb_saveConfigForReal > 0 && input$kb_saveConfigForReal > .self$lastSaveConfigValue && !.self$clearModal) {
                lastSaveConfigValue <<- as.numeric(input$kb_saveConfigForReal)
                if (nchar(input$kb_configComment) > 0) {
                    tryCatch({
                        print("saving config")
                        .self$saveConfig(session)
                        print("config saved")
                        updateSelectInput(session,"kb_config", choices=c("None",configChoices()()))
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-success', "Configuration successfully saved.")))
                    }, error = function(e) {
                        write(paste("There was an error saving the config", e), stderr())
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-danger', paste0("Error saving configuration: ", e))))
                    })
                } else {
                    ret <- list(ret,list(div(class = 'kfig-alert alert alert-warning', "Please enter a comment.")))
                }
            }    
            return(ret) 
        },
        
        loadConfigResultUI = function(callback) {
            "TODO
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{callback} TODO}
            }}
            \\subsection{Return Value}{TODO}"
            input <- session$input
            ret <- list()
            .self$clearForm(input)()
            if (input$kb_loadConfig > 0 && input$kb_loadConfig > .self$lastLoadConfigValue && input$kb_config != "None" && !.self$clearModal) {
                tryCatch({
                    print("getting selected config")
                    config <- .self$selectedConfig()
                    print("calling the callback function")
                    callback(config)
                    print("callback executed")
                    ret <- list(ret,list(div(class = 'alert alert-success', "Configuration successfully loaded.")))
                }, error = function(e) {
                    ret <- list(ret,list(div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))))
                })
                lastLoadConfigValue <<- as.numeric(input$kb_loadConfig)
            }
            ret
        },
        
        deleteConfigResultUI = function() {
            "TODO
            \\subsection{Return Value}{TODO}"
            ret <- list()
            session$input$kb_deleteConfig
            session$input$kb_confirmDelete
            session$input$kb_confirmCancel
            session$input$kb_configModalButton
            isolate({
                input <- session$input
                .self$clearForm(input)()
                if (input$kb_deleteConfig > 0 && input$kb_deleteConfig %% 2 == 1
                    && (is.null(input$kb_confirmDelete) || input$kb_confirmDelete == .self$lastConfirmDeleteValue) 
                    && (is.null(input$kb_confirmCancel) || input$kb_confirmCancel == .self$lastConfirmCancelValue) 
                    && !.self$clearModal) {
                    
                    choices <- configChoices()()
                    mtch <- match(input$kb_config,unlist(choices))
                    choice <- names(choices)[mtch[1]]
                    ret <- div(class = 'alert alert-warning', paste("Are you sure you want to delete '", choice, "'?",sep=''),
                               actionButton("kb_confirmDelete",'Yes'),
                               actionButton("kb_confirmCancel",'No'))
                } else if (!is.null(input$kb_confirmDelete) && input$kb_confirmDelete > .self$lastConfirmDeleteValue && !.self$clearModal) {
                    print("delete confirmed")
                    lastConfirmDeleteValue <<- as.numeric(input$kb_confirmDelete)
                    tryCatch({
                        print(paste("deleting config", input$kb_config))
                        resp <- .self$deleteConfig(input$kb_config)
                        print(paste("deleted config", input$kb_config))
                        updateSelectInput(session,"kb_config", choices=c("None",configChoices()()))
                        ret <- div(class = 'alert alert-success', "Configuration successfully deleted.")
                    }, error = function(e) {
                        ret <- div(class = 'alert alert-danger', paste0("Error deleting configuration: ", e))
                    })
                } else if (!is.null(input$kb_confirmCancel) && input$kb_confirmCancel > .self$lastConfirmCancelValue) {
                    lastConfirmCancelValue <<- as.numeric(input$kb_confirmCancel)
                    # Do nothing
                }    
            })
            ret
        }
    )
)
