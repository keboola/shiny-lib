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
        #' Constructor.
        #'
        #' @param sapiClient - Keboola.sapi.r.client::SapiClient
        #' @param bucket - Bucket where config table is stored
        #' @param shinyUrl - Shiny Bundle API home URL
        #'  it will be read from command line argument.
        #'  "http://shiny.kbc-devel-02.keboola.com/app_dev.php/shiny/"
        #' @exportMethod
        initialize = function(sapiClient, bucketId, appId, shinyUrl = "https://shiny.keboola.com/shiny/") {
            if (is.null(client)) {
                stop("Can not initialize KeboolaAppConfig.  No valid Sapi Client available.")
            }   
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
            reactive({
                .self$getConfigs()
            })
        },
        
        configChoices = function(session) {        
            reactive({
                print("getting configs")
                configs <- .self$configs()()
                choices <- list()
                for (config in configs) {
                    choices[[paste(config$comment,config$dateCreated,sep=" -- ")]] <- config$configId
                }
                print(paste("choices", choices))
                choices    
            })
        },
        
        selectedConfig = function(session) {
            input <- session$input    
            configId <- session$input$config
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
        
        #' Get app configurations from the Shiny Bundle
        #' 
        #' @return list of app configurations
        getConfigs = function() {
            tryCatch({
                configs <- .self$client$genericGet(
                    paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config"),
                    query = list(bucket = .self$bucket)
                )
                print(paste("got", length(configs),"configs"))
                print(configs)
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
               obj <- list()
               obj$comment <- configs$configComment
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
        
        deleteConfig = function(session, configId) {
            resp <- .self$client$genericDelete(
                paste0(.self$shinyBaseUrl,"apps/",.self$appId,"/config/", configId), 
                query = list(bucket = .self$bucket)
            )
            print("config", configId, "deleted!")
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
        
        clearForm = function(input) {
            reactive({
                input$configModalButton
                input$config
                isolate({
                    if (!(is.null(input$configModalButton)) && input$configModalButton > 0 && input$configModalButton > .self$lastModalButtonValue) {
                        lastModalButtonValue <<- as.numeric(input$configModalButton)
                        clearModal <<- TRUE
                        return(TRUE)
                    } else {
                        clearModal <<- FALSE
                        return(FALSE)
                    }    
                })
            })
        },
        
        configSettingsUI = function(session) {
            input <- session$input
            ret <- list(
                       helpText("Save your current input configuration to share with others or reload later. 
                                You can also load previously saved configurations, or delete them as necessary."),
                        div(style="text-align:right;padding:0 19px 15px 0;",
                           actionButton("saveConfig", "Save Current Settings", class="btn-primary")
                       ),
                       uiOutput("saveConfigUI"),
                       wellPanel(
                            uiOutput("loadConfigResultUI"),
                            uiOutput("deleteConfigResultUI"),
                            uiOutput("configListUI"),
                            fluidRow(
                                column(6,actionButton("loadConfig", "Load Selected Configuration",
                                                      `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#loadConfigResultUI")),
                                column(6,actionButton("deleteConfig", "Delete Selected Configuration", 
                                                      class="btn-danger", `data-toggle` = "kfig-alert", 
                                                      `data-target` = "#deleteConfigResultUI"),
                                        class=" text-right")
                            )
                       )
                    )
            print("got configs")
            ret    
        },
        
        configListUI = function(session) {
            selectInput("config","Configuration",c("None",configChoices(session)()))
        },
        
        saveConfigUI = function(input) {
            ret <- list()
            .self$clearForm(input)()
            if ((input$saveConfig > 0) && (input$saveConfig %% 2 == 1) && !.self$clearModal) {
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
            .self$clearForm(input)()
            print("Entered SAVE RESULTS")
            if (input$saveConfigForReal > 0 && input$saveConfigForReal > .self$lastSaveConfigValue && !.self$clearModal) {
                lastSaveConfigValue <<- as.numeric(input$saveConfigForReal)
                if (nchar(input$configComment) > 0) {
                    tryCatch({
                        print("saving config")
                        .self$saveConfig(session)
                        print("config saved")
                        updateSelectInput(session,"config", choices=c("None",configChoices(session)()))
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-success', "Configuration successfully saved.")))
                    }, error = function(e) {
                        write(paste("There was an error saving the config", e), stderr())
                        ret <- list(ret,list(div(class = 'kfig-alert alert alert-danger', paste0("Error saving configuration: ", e))))
                    })
                } else {
                    ret <- list(ret,list(div(class = 'kfig-alert alert alert-warning', "Please enter a comment.")))
                }
            }    
            
            print("saveConfigResult end")
            return(ret) 
        },
        
        loadConfigResultUI = function(session,callback) {
            input <- session$input
            ret <- list()
            .self$clearForm(input)()
            if (input$loadConfig > 0 && input$loadConfig > .self$lastLoadConfigValue && input$config != "None" && !.self$clearModal) {
                print("loading config")
                tryCatch({
                    print("getting selected config")
                    config <- .self$selectedConfig(session)
                    print("calling the callback function")
                    callback(session, config)
                    print("callback executed")
                    ret <- list(ret,list(div(class = 'alert alert-success', "Configuration successfully loaded.")))
                }, error = function(e) {
                    ret <- list(ret,list(div(class = 'alert alert-danger', paste0("Error loading configuration: ", e))))
                })
                lastLoadConfigValue <<- as.numeric(input$loadConfig)
            }
            ret
        },
        
        deleteConfigResultUI = function(session) {
            print("DELETE CONFIG RESULTS UI")
            ret <- list()
            session$input$deleteConfig
            session$input$confirmDelete
            session$input$confirmCancel
            session$input$configModalButton
            isolate({
                input <- session$input
                .self$clearForm(input)()
                if (input$deleteConfig > 0 && input$deleteConfig %% 2 == 1
                    && (is.null(input$confirmDelete) || input$confirmDelete == .self$lastConfirmDeleteValue) 
                    && (is.null(input$confirmCancel) || input$confirmCancel == .self$lastConfirmCancelValue) 
                    && !.self$clearModal) {
                    
                    choices <- configChoices(session)()
                    mtch <- match(input$config,unlist(choices))
                    choice <- names(choices)[mtch[1]]
                    print(paste("choice to delete ", choice))
                    ret <- div(class = 'alert alert-warning', paste("Are you sure you want to delete '", choice, "'?",sep=''),
                               actionButton("confirmDelete",'Yes'),
                               actionButton("confirmCancel",'No'))
                } else if (!is.null(input$confirmDelete) && input$confirmDelete > .self$lastConfirmDeleteValue && !.self$clearModal) {
                    print("delete confirmed")
                    lastConfirmDeleteValue <<- as.numeric(input$confirmDelete)
                    print(paste(
                        "CONFIRM PRESSED AFTER UPDATE:: input$confirmCancel", input$confirmCancel, "  ",    
                        "lastconfirmCancelVALue", .self$lastConfirmCancelValue, "  ",
                        "input$confirmDelete", input$confirmDelete, "  ",
                        "lastConfrimDeleteValue", .self$lastConfirmDeleteValue,
                        "input$deleteConfig", input$deleteConfig, "  ",
                        "lastDeleteConfigValue", .self$lastDeleteConfigValue
                    ))
                    tryCatch({
                        print(paste("deleting config", input$config))
                        resp <- .self$deleteConfig(session, input$config)
                        print(paste("deleted config", input$config))
                        updateSelectInput(session,"config", choices=c("None",configChoices(session)()))
                        ret <- div(class = 'alert alert-success', "Configuration successfully deleted.")
                    }, error = function(e) {
                        ret <- div(class = 'alert alert-danger', paste0("Error deleting configuration: ", e))
                    })
                } else if (!is.null(input$confirmCancel) && input$confirmCancel > .self$lastConfirmCancelValue) {
                    lastConfirmCancelValue <<- as.numeric(input$confirmCancel)
                    # Do nothing
                }    
            })
            ret
        }
    )
)
           
           
           
           