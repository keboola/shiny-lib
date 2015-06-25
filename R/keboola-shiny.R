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
        #' @param Optional name of data directory, if not supplied then
        #'  it will be read from command line argument.
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
                        } else {
                            message <- "Please make sure the token and bucket values are correct"
                        }
                        paste(paste0("<label>Error :</label><span class='text-danger'>", message, "</span>"),
                            paste("<label>token</label>",token,sep=" : "),
                            paste("<label>bucket</label>",bucket,sep=" : "),
                            paste("<label>runId</label>",runId,sep=" : "), sep = "<br/>")
                    })
                } else "Please log in."
            })
        },

        #' internal method that returns HTML content for the given section node
        #' NOTE: customElement method should be implemented by the application
        #' @param section - node of the descriptor
        #' @param level - heading level (1=h2, 2=h3)
        #' @return list of HTML elements
        processSection = function(section, level) {
            sectionRet <- list()
            if (level == 1) {
                sectionRet[[length(sectionRet) + 1]] <- h2(section$title)
            } else {
                sectionRet[[length(sectionRet) + 1]] <- h3(section$title)
            }
            for (statement in section$statements) {
                if (length(statement) > 0) {
                    statementRet <- list()
                    if (statement$type == 'text') {
                        statementRet[[length(statementRet) + 1]] <- 
                            HTML(
                                paste0(
                                    '<p>',
                                    gsub('\\[(.*?)\\[([a-zA-Z0-9_]+)\\]\\]', '<span class="kb-hint" title="" id="\\2">\\1</span>', statement$content),
                                    '</p>'
                                )
                            )
                        for (hint in statement$hints) {
                            statementRet[[length(statementRet) + 1]] <- 
                                HTML(
                                    paste0("<script>$('#", hint$id, "').attr('title', '", hint$hint, "');</script>")
                                )
                        }
                    }
                    if (statement$type == 'image') {
                        statementRet[[length(statementRet) + 1]] <- img(src = statement$content, width = 900)
                    }
                    if (statement$type == 'table') {
                        df <- data.frame()
                        for (row in statement$content) {
                            if (class(row) == 'list') {
                                df <- rbind(df, unlist(row))
                                names(df) <- names(row)
                            } # else, ignore the row
                        }
                       
                        if (nrow(df) > 0) {
                            tid <- stringi::stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z]")
                            statementRet[[length(statementRet) + 1]] <- shiny::renderDataTable(df)
                        }
                    } 
                    if ((statement$type == 'plot') || (statement$type == 'custom')) {
                        statementRet[[length(statementRet) + 1]] <- p(customElements(statement$id, statement$content), class = 'lg-plot')
                    }
                    if (nchar(statement$example) > 0) {
                        statementRet[[length(statementRet) + 1]] <- p(statement$example, class = 'lg-example')
                    }
                    if (nchar(statement$details) > 0) {
                        # skip details for table, they have been already used as caption
                        statementRet[[length(statementRet) + 1]] <- tag('details', statement$details)
                    }
                    htmlDiv <- div(statementRet, class='lg-statement')
                    sectionRet[[length(sectionRet) + 1]] <- htmlDiv
                }
            }
            sectionRet
        },

        #' Method returns HTML content for a descriptor 
        #' @param descriptor - descriptor data
        #'
        #' @exportMethod
        getDescription = function(descriptor) {
            contentRet <- list()

            for (section in descriptor$sections) {
                if (length(section$subsections) == 0) {
                    sectionRet <- list()
                    sectionRet[[length(sectionRet) + 1]] <- processSection(section, 1)
                    contentRet[[length(contentRet) + 1]] <- tag('section', sectionRet)
                } else {
                    sectionRet <- list()
                    groups <- c()
                    for (subsection in section$subsections) {
                        groups <- c(groups, subsection$title)
                    }
                    groups <- as.list(groups)
                   
                    for (subsection in section$subsections) {
                        ld <- list()
                        ld[[length(ld) + 1]] <- processSection(subsection, 2)
                        sectionRet[[length(sectionRet) + 1]] <- tag('li', ld)
                    }
                    sectionRet[['class']] <- 'kb-enumeration'
                    ul <- tag('ul', sectionRet)
                    sectionRet <- list()
                    sectionRet[[length(sectionRet) + 1]] <- processSection(section, 1)
                    sectionRet[[length(sectionRet) + 1]] <- ul
                    contentRet[[length(contentRet) + 1]] <- tag('section', sectionRet)
                }
            }
            contentRet
        }
    )
)


