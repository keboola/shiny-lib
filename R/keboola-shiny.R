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
        token = 'character',
        runId = 'character',
        bucket = 'character',
        loginMsg = 'character'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then it
        #'  will be read from command line argument.
        #' @exportMethod
        initialize = function(session) {
            token <<- .self$getToken(session)()
            runId <<- .self$getRunId(session)()
            bucket <<- .self$getBucket(session)()
            loginMsg <<- .self$getLogin(session)()
        },
        
        #' Get the runId from the query string
        #' 
        #' @param Shiny server session object
        #' @return function closure to retrieve the runId
        getRunId = function(session) {
            reactive({
                val <- as.character(parseQueryString(session$clientData$url_search)$runId)
                if (length(val) == 0) {
                    val <- ''
                }
                val
            })
        },
        
        
        #' Get the bucket from the query string
        #' 
        #' @param Shiny server session object
        #' @return function closure to retrieve the runId
        getBucket = function(session) {
            reactive({
                val <- as.character(parseQueryString(session$clientData$url_search)$bucket)
                if (length(val) == 0) {
                    val <- ''
                }
                val
            })    
        },
        
        #' Get the token from the session headers or input element
        #' 
        #' @param Shiny server session object
        #' @return function closure to retrieve the token
        getToken = function(session) {
            reactive({
                val <- as.character(session$request$HTTP_X_STORAGEAPI_TOKENs)
                if (length(val) == 0) {
                    val <- as.character(session$input$token)
                    if (length(val) == 0) {
                        val <- ''
                    }
                }
                val
            })
        },
        
        #' Returns login status message or empty string if valid
        #' 
        #' @param the ShinySession object
        #' @return html string containing error message and field settings
        getLogin = function(session) {
            reactive({
                if (token != '' & session$input$login) {
                    tryCatch({
                        client <- SapiClient$new(token)
                        if (runId == "") {
                            stop("This application requires a valid runId in the query string.")
                        }
                        ""
                    }, error = function(e){
                        if (runId == "" & exists("client")){
                            message <- e
                        }else {
                            message <- "Please make sure the token and bucket values are correct"
                        }
                        paste(paste0(e,"<label>Error :</label><span class='error'>", message, "</span>"),
                              paste("<label>token</label>",token,sep=" : "),
                              paste("<label>bucket</label>",bucket,sep=" : "),
                              paste("<label>runId</label>",runId,sep=" : "), sep = "<br/>")
                    })
                } else "Please log in."
            })
        }
    )
)


