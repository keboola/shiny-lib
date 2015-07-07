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
        errMsg = 'character',
        client = 'ANY', # keboola.sapi.r.client::SapiClient
        bucketId = 'character',
        runId = 'character'
    ),
    methods = list(
        #' Constructor.
        #'
        #' @param Optional name of data directory, if not supplied then
        #'  it will be read from command line argument.
        #' @exportMethod
        initialize = function() {
            errMsg <<- ''
            bucketId <<- ''
            runId <<- ''
            client <<- NULL
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
        #' @param Shiny server session object
        #' @return function closure to retrieve the token
        getToken = function(request, input) {
            val <- as.character(request$HTTP_X_STORAGEAPI_TOKENs)
            if (length(val) == 0) {
                val <- as.character(input$token)
                if (length(val) == 0) {
                    val <- ''
                }
            }
            return(val)
        },

        
        #' Returns login status message or empty string if valid
        #' 
        #' @param the ShinySession object
        #' @return html string containing error message and field settings
        #' @exportMethod
        getLogin = function(request, input, clientData) {
            print("getLogin")
            token <- .self$getToken(request, input)
            runId <<- .self$getRunId(clientData)
            bucketId <<- .self$getBucket(clientData)
            print("token")
            print(token)
            errMsg <<- ""
            if (token != '') {
                tryCatch({
                    client <<- keboola.sapi.r.client::SapiClient$new(token)
                    print("client successful")
                }, error = function(e){
                    errMsg <<- paste0("Please make sure that the token is valid (", e, ").")
                    print("client not successful")
                })
                if (.self$runId == "") {
                    errMsg <<- paste(.self$errMsg, "This application requires a valid runId in the query string.")
                }
                if (.self$bucketId == "") {
                    errMsg <<- paste(.self$errMsg, "This application requires a valid bucket in the query string.")
                }
                if (.self$errMsg != '') {
                    error <- paste(paste0("<label>Error :</label><span class='text-danger'>", .self$errMsg, "</span>"),
                            paste("<label>token</label>", token, sep = " : "),
                            paste("<label>bucket</label>", .self$bucketId, sep = " : "),
                            paste("<label>runId</label>", .self$runId, sep = " : "), sep = "<br/>")
                    # beware that loggedIn must be integer, otherwise it fails in conditionalPanel() !
                    print(paste0('error occured ', .self$errMsg))
                    loggedIn = 0
                } else {
                    error <- ''
                    print('success')
                    loggedIn = 1
                }
            } else {
                print('token empty')
                loggedIn = 0
                error <- 'Please log in.'
            }
            print('getLogin exiting')
            list(
                token = token, 
                runId = .self$runId, 
                bucket = .self$bucketId, 
                loggedIn = loggedIn, 
                errorMsg = error,
                cleint = .self$client
            )
        },

        
        #' Load tables from SAPI
        #' @param tableNames Vector or character table names (without bucket) to be loaded
        #' @param progressBar Optional Shiny progress bar, it is assumed to be in range 1,100
        #' @return list of data indexed by table name.
        loadTables = function(tableNames, progressBar = NULL) {
            if (is.null(.self$client)) {
                stop("Not connected to SAPI.")
            }
            tryCatch({
                ret <- list()
                cntr <- 0
                if (!is.null(progressBar)) {
                    progressBar$set(value = 2)
                }
                for (tableName in tableNames) {
                    data <- .self$client$importTable(
                        paste0(.self$bucketId, ".", tableName), 
                        options = list(whereColumn = "run_id", whereValues = .self$runId)
                    )
                    ret[[tableName]] <- data
                    if (!is.null(progressBar)) {
                        cntr <- cntr + 1
                        progressBar$set(value = ((100 / length(tableNames)) * cntr))
                    }
                }
                return(ret)
            }, error = function(e) {
                # convert the error to a more descriptive message
                stop(paste0("Error when loading data from SAPI (", e, ')'))
            })
        },
        
        
        getDescriptor = function() {
            print("getDescriptor")
            tryCatch({
                data <- .self$client$importTable(
                    paste0(.self$bucketId, ".finalResults"), 
                    options = list(
                        columns = c("item", "run_id", "sequence", 'value'),
                        whereColumn = "run_id",
                        whereValues = .self$runId
                    )
                )
                # grab only descriptors - needs to deal with split values from Redshift
                descriptors <- data[which(data$item == 'descriptor'), ]
                # get indexes of duplicated values (can happen if the job is run multiple times)
                dupes <- duplicated(descriptors[, "sequence"])
                # get only non duplicate items
                descriptors <- descriptors[!dupes, ]
                descriptors$sequence <- as.integer(descriptors$sequence)
                # sort by sequence and get only the value
                descriptors <- descriptors[order(descriptors$sequence), 'value']
                # paste into a single string
                descriptor <- paste0(descriptors, collapse = '')
                # process JSON
                #writeLines(descriptor, file("D://1/pokus.json"))
                #descriptor <- readLines(file("D://1/pokus.json"))
                descriptor <- jsonlite::fromJSON(descriptor, simplifyVector = FALSE)
                print("getDescriptor finsihed")
                return(descriptor)
            }, error = function(e) {
                stop(paste0("I cannot load descriptor (from finalResults table) from SAPI (", e, ")"))
            })
        },
        
        
        #' Internal method that returns HTML content for the given section node
        #' @param section - node of the descriptor
        #' @param level - heading level (1=h2, 2=h3)
        #' @param customElements - callback for processing custom elements
        #' @return list of HTML elements
        processSection = function(section, level, customElements) {
            print("processSection")
            sectionRet <- list()
            if (level == 1) {
                sectionRet[[length(sectionRet) + 1]] <- h2(section$title)
            } else {
                sectionRet[[length(sectionRet) + 1]] <- h3(section$title)
            }
            for (statement in section$statements) {
                if (length(statement) > 0) {
                    statementRet <- list()
                    print(paste0("Statement type: ", statement$type))
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
                       
                        print("Table")
                        print(df)
                        if (nrow(df) > 0) {
                            tid <- stringi::stri_rand_strings(n = 1, length = 8, pattern = "[A-Za-z]")
                         #   statementRet[[length(statementRet) + 1]] <- shiny::renderDataTable(df)
                            statementRet[[length(statementRet) + 1]] <- 
#                                print(xtable::xtable(df), type = 'html')
                                DT::datatable(
                                    df,
                                    class = 'table display',
                                    caption = statement$details
                               )
                            #library(DT)
                        }
                    } 
                    if ((statement$type == 'plot') || (statement$type == 'custom')) {
                        if (!is.null(customElements)) {
                            statementRet[[length(statementRet) + 1]] <- p(customElements(statement$id, statement$content), class = 'lg-plot')
                        }
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
            print("processSection exit")
            return(sectionRet)
        },

        #' Method returns HTML content for a descriptor 
        #' @param descriptor - descriptor data
        #' @param customElements - callback for printing custom elements, signature: function(elementId, content)
        #'  function should return a single HTML element. Pass NULL to ignore custom elements.
        #'
        #' @exportMethod
        getDescription = function(descriptor, customElements) {
            print("getDescription")
            oldOptions <- options(stringsAsFactors = FALSE)
            contentRet <- list()

            for (section in descriptor$sections) {
                if (length(section$subsections) == 0) {
                    sectionRet <- list()
                    sectionRet[[length(sectionRet) + 1]] <- processSection(section, 1, customElements)
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
                        ld[[length(ld) + 1]] <- processSection(subsection, 2, customElements)
                        sectionRet[[length(sectionRet) + 1]] <- tag('li', ld)
                    }
                    sectionRet[['class']] <- 'kb-enumeration'
                    ul <- tag('ul', sectionRet)
                    sectionRet <- list()
                    sectionRet[[length(sectionRet) + 1]] <- processSection(section, 1, customElements)
                    sectionRet[[length(sectionRet) + 1]] <- ul
                    contentRet[[length(contentRet) + 1]] <- tag('section', sectionRet)
                }
            }
            options(oldOptions)
            print("getDescription exit")
            return(contentRet)
        },

        commonPageHeader = function(appTitle) {
            ret <- list()
            ret[[length(ret) + 1]] <- tag('style', '
                .navbar .container-fluid {
                     padding-left: 30px;
                     padding-right: 30px;
                 }
                 
                 .navbar-brand {
                     padding: 0;
                 }
                 
                 .kb-shiny-app-title {
                     padding: 15px 15px;
                 }
                 
                 .kb-logo {
                     background: transparent url(https://connection.keboola.com/app/modules/admin/images/keboola-logo.png) 0 0 no-repeat;
                     display: inline-block;
                     width: 24px;
                     height: 32px;
                     position: relative;
                     top: 10px;
                 }
                 
                 .shiny-busy {
                    cursor: wait;
                 }
                 
                 .error {
                    color: #ff0033;
                 }
                 
                 .kb-example:before {
                     content: "Example: "
                 }
                 .kb-example {
                     color: #A4A4A4;
                 }
                 .kb-hint {
                     border-bottom: 1px dashed #333;
                 }
                 '
            )
            ret[[length(ret) + 1]] <- 
                div(class = "navbar navbar-default navbar-static-top kb-navbar-top",
                    div(class = "container-fluid",
                        div(class = "navbar-header",
                            a(class = "navbar-brand",
                              href = "https://connection.keboola.com",
                              span(class = "kb-logo"),
                              span("Keboola Connection")
                            )
                        ),
                        div(class = "collapse navbar-collapse",
                            div(class = "nav navbar-nav navbar-right navbar-brand kb-shiny-app-title",
                                appTitle
                            )
                        )
                    )
                )
            return(ret)
        }

    )
)