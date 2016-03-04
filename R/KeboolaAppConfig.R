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
        # the following fields are helper hacks for ui flow control
        lastLoadConfigValue = 'numeric',
        lastSaveConfigValue = 'numeric',
        lastSaveConfirmValue = 'numeric',
        lastConfirmDeleteValue = 'numeric',
        lastConfirmCancelValue = 'numeric',
        registeredInputs = 'list',
        component = 'character',
        configId = 'character',
        configLoading = 'numeric',
        validInputTypes = 'character'
    ),
    methods = list(
        initialize = function(sapiClient, component, configId, session = getDefaultReactiveDomain()) {
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
            lastSaveConfirmValue <<- 0
            lastLoadConfigValue <<- 0
            lastConfirmDeleteValue <<- 0
            lastConfirmCancelValue <<- 0
            
            configLoading <<- 0
            clearModal <<- FALSE
            client <<- sapiClient
            component <<- component
            configId <<- configId
            validInputTypes <<- c("text","numeric","date","select","slider","actionButton",
                                  "dateRange","checkbox","checkboxGroup","radioButtons",
                                  "dynamicRanges","dynamicDateRanges","dynamicFactors")
            registeredInputs <<- list()
            
        },
        
        configs = function() {
            "config fetcher
            \\subsection{Return Value}{list of app input configurations}"
            tryCatch({
                configs <- .self$client$listConfigurationRows(
                    .self$component,
                    .self$configId
                )
                return(configs)
            }, error = function(e) {
                # convert the error to a more descriptive message
                stop(paste0("Error loading app configs (", e, ')'))
            })
        },
        
        configChoices = function() {
            "This returns a list of 'config name -- date created' with key configId
             the returned list is used to populate the options for the config select input
            \\subsection{Return Value}{array of configId -> 'configname -- date' }"
            reactive({
                configs <- .self$configs()
                choices <- list()
                for (config in configs) {
                    #choices[[paste(config$id,config$dateCreated,sep=" -- ")]] <- config$id
                    choices[[config$id]] <- config$id
                }
                choices    
            })
        },
        
        saveConfig = function(comment) {
            "This method stores the entire session$input object as a row in the app configuration.
             Note that inputs with prefix kb_ will be omitted because they are system elements.
            \\subsection{Return Value}{TRUE, will throw an error if something goes wrong.}"
           if (is.null(.self$client)) {
               stop("Not connected to SAPI.")
           }
           tryCatch({
               configs <- list()
               for (name in names(session$input)) {
                   # only store non-system inputs
                   if (length(grep("^kb_", name)) == 0 && length(grep("^kb-", name)) == 0) {
                       configs[[name]] <- session$input[[name]]      
                   }
               }
               obj <- list()
               obj$comment <- comment
               obj$config <- configs
               resp <- .self$client$createConfigurationRow(
                   .self$component,
                   .self$configId,
                   gsub("-+","-", gsub("[^A-Za-z0-9_]", "-",obj$comment)), # replace non-alphanumerics with dashes
                   jsonlite::toJSON(obj, auto_unbox=TRUE))
               TRUE
           }, error = function(e) {
               # convert the error to a more descriptive message
               stop(paste0("Error saving config (", e, ')'))
           })
        },
        
        deleteConfig = function(rowId) {
            "Delete the configuration
            \\subsection{Parameters}{\\itemize{
            \\item{\\code{rowId} id of the configuration row}
            }}
            \\subsection{Return Value}{resp will be TRUE if successful. otherwise an error will be thrown.}"
            resp <- .self$client$deleteConfigurationRow(
                .self$component,
                .self$configId,
                rowId
            )
            resp
        },
        
        settingsModalButton = function() {
            "the toolbar button that brings up the configuration settings modal dialog
            \\subsection{Return Value}{list(button)}"
            list(
                keboolaModalButton(
                    "kb_configModal",
                    label = "",
                    icon = icon("gear"),
                    title = "Configuration Settings",
                    content = appConfigInput("kb")
                )
            )
        },
        
        deleteConfigResultUI = function() {
            "Actually performs the delete and returns a DOM element indicating operation status
            \\subsection{Return Value}{DOM}"
            ret <- list()
            session$input$kb_deleteConfig
            session$input$kb_confirmDelete
            session$input$kb_confirmCancel
            isolate({
                input <- session$input
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
                    print(paste0("Confirmed to delete: ", input$kb_config))
                    lastConfirmDeleteValue <<- as.numeric(input$kb_confirmDelete)
                    tryCatch({
                        resp <- .self$deleteConfig(input$kb_config)
                        print(paste("deleted config:", input$kb_config))
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
        },
        
        updateInputElement = function(elem, config = NULL) {
            if (is.null(config)) {
                newValue = ""
            } else {
                newValue <- config[[elem$id]]    
            }
            
            #print(paste("updating input element", elem$id, "with value", paste(newValue, collapse="' ")))
            switch(elem$type,
                   "select" = updateSelectInput(session,elem$id, selected=newValue),
                   "text" = updateTextInput(session, elem$id, value=newValue),
                   "slider" = updateSliderInput(session, elem$id, value=c(newValue[1],newValue[2])),
                   "date" = updateDateInput(session, elem$id, value=newValue),
                   "dateRange" = updateDateRangeInput(session, elem$id, start=newValue[1], end=newValue[2]),
                   "checkbox" = updateCheckboxInput(session, elem$id, value=newValue),
                   "checkboxGroup" = updateCheckboxGroupInput(session, elem$id, selected=newValue),
                   "numeric" = updateNumericInput(session, elem$id, value=newValue),
                   "radioButtons" = updateRadioButtons(session, elem$id, selected=newValue),
                   "dynamicRanges" = {
                        #updateSelectInput(session, elem$id, selected = newValue)
                        #if (length(newValue) > 1 || newValue !="") {
                        #   for (element in newValue) {
                        #       if (!is.null(session$input[[element]])) {
                        #           updateSliderInput(session, element, value=c(config[[element]][1],config[[element]][2]))    
                        #       }
                        #   }
                        #}
                       print("OLD update dynamicRanges")
                   },
                   "dynamicDateRanges" = {
                        #updateSelectInput(session, elem$id, selected=newValue)
                        #if (length(newValue) > 1 || newValue != "") {
                        #   for (element in newValue) {
                        #       updateDateRangeInput(session, element, start=config[[element]][1], end=config[[element]][2])
                        #   }
                        #}
                       print("OLD update dynamicDateRanges")
                   },
                   "dynamicFactors" = {
                       #updateSelectInput(session, elem$id, selected=newValue)
                       #if (length(newValue) > 1 || newValue != "") {
                    #       for (element in newValue) {
                     #          updateSelectInput(session, element, selected=config[[element]])
                      #    }        
                       #}
                       print("OLD update dynamicFactors")
                   },
                   stop(paste("Error loading configuration. Unknown input type given:", elem$type, ".  Valid types are:", paste(self$validInputTypes,collapse=", ")))
            )
        },
        
        defaultConfigCallback = function(config) {
            "This method is called when an input configuration is loaded
            \\subsection{Return Value}{void}"
         
            if (length(.self$registeredInputs) == 0) {
                stop("No inputs were registered so I have nothing to do.")
            }
            for (i in 1:length(.self$registeredInputs)) {
                elem <- .self$registeredInputs[[i]]
                if (elem$id %in% names(config)) {
                    .self$updateInputElement(elem, config)    
                } else {
                    # now, this element was set in the interface, but it does not appear in the config.
                    # we need to clear this element
                    .self$updateInputElement(elem)
                }
            }
            print(paste("DEFAULT ConfigCallback finished: "))
        },
        
        registerInputs = function(inputList) {
            validateInput <- function(input) {              
                if (!class(input) == "list" 
                    | !("id" %in% names(input))
                    | !("type" %in% names(input))
                    | !(input$type %in% .self$validInputTypes)
                ) {
                    stop(paste(
                        "The argument must be a list with members 'id' and 'type' and type must be one of: ", 
                        paste(validInputs, collapse=", ")
                    ))
                }    
            }
            
            lapply(inputList, function(input) {
                validateInput(input)
                .self$registeredInputs[[length(.self$registeredInputs) + 1]] <- input
            })
            print("Inputs are valid and registered")
        }
    )
)
