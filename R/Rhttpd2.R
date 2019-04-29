Rhttpd2 <- setRefClass(
    'Rhttpd2',
    fields = c('appList','listenAddr','httpdOrig','listenPort'),
    methods = list(
	initialize = function(...){
	    appList <<- list()
	    listenAddr <<- '127.0.0.1'
	    listenPort <<- 0L
	    callSuper(...)
	},
	finalize = function(){
		# This function is being called randomly, causing the apps to be removed and 
		# inaccessible, throwing errors and shutting down the server. All removal is shifted 
		# to whenever the R.server is started again
	    #if (length(appList) == 0) return()
	    #appList_names <- names(appList)
	    #cat(paste0('length(appList)=', length(appList), '\n'))
		#cat(paste0(appList_names, collapse=', '), '\n')
	    #for (i in rev(1:length(appList))){
			#cat(paste0('\tname=', appList_names[i], '\n'))
			#cat(is.null(appList[[appList_names[i]]]), '\n')
			#cat(appList_names[i] %in% appList_names, '\n')
			#remove(appList[[i]])
			#if(!is.null(appList[[i]]) && i %in% names(appList)) remove(appList[[i]])
			#cat(paste0('\n'))
	    #}
	},
	full_url = function(i){
		if(missing(i)) return(NULL)
	    paste('http://',listenAddr,':',listenPort,appList[[i]]$path,sep='')
	},
	launch = function(...){
	    .self$start(quiet=TRUE)		
	    # Try to create a new app from the supplied arguments
	    app <- RhttpdApp$new(...)
	    if (add(app)){
		appName <- app$name
		browseURL(full_url(which(appName == names(appList))))
		invisible()
	    } else {
		base::stop("No app to launch")
	    }
	},
	open = function(x){
	    if (missing(x)) return(print())
	    if (is.numeric(x) || is.integer(x)){
		x <- as.integer(x)
		if (!is.null(appList[[x]]))
		    return(invisible(browseURL(full_url(x))))
		else
		    base::stop("No app at index ",x)
	    } else if (is.character(x)){
		for (i in 1:length(appList)){
		    if (appList[[i]]$name==x){
			return(invisible(browseURL(full_url(x))))
		    }
		}
		base::stop("No app named",x)
	    }
	    base::stop("Argument must be an integer or character")
	},
	browse = function(x) open(x),
   start = function(listen='127.0.0.1',port=getOption('help.ports'),quiet=FALSE){

      if(nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
         warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
         utils::flush.console()
         return(invisible())
      }

      if(grepl('rstudio',base::.Platform$GUI,ignore.case=TRUE)){
         # RStudio has already set up host and port
         listenPort <<- tools:::httpdPort
         if (!missing(port))
            warning("RStudio has already started the web server on port ",tools:::httpdPort)
         return(invisible())
      }

      if (!missing(listen) && listen != '127.0.0.1'){
         listen <- '127.0.0.1'
         warning("This version of Rook can only listen on the loopback device.");
      }

      if (!missing(port)){
         oldPorts <- getOption('help.ports')
         on.exit(options(help.ports=oldPorts))
         options(help.ports=port)
      }

      if (length(appList) == 0)
         add(RhttpdApp$new(system.file('exampleApps/RookTestApp.R',package='Rook'),name='RookTest'))


      listenPort <<- suppressMessages(tools::startDynamicHelp(TRUE))

      if (listenPort == 0){
         base::stop("The internal web server could not be started!")
      }

      if (!quiet){
         cat('\nServer started on host',listen,'and port',listenPort,'. App urls are:\n\n')
         invisible(lapply(names(appList),function(i){
            cat('\thttp://',listen,':',listenPort,appList[[i]]$path,'\n',sep='')
               }))
      }
      invisible()
   },

	stop = function(){
	    listenPort <<- suppressMessages(tools::startDynamicHelp(FALSE))
	},
	add = function(app=NULL,name=NULL){

	    if (!inherits(app,'RhttpdApp'))
		app <- RhttpdApp$new(name=name,app=app)
	    if (!inherits(app,'RhttpdApp'))
		base::stop("Need an RhttpdApp object")

	    appList[[app$name]] <<- app
	    if(app$name=='httpd'){
		base::stop("Rook no longer supports assignment to tools:::httpd")
		#.self$httpdOrig <- tools:::httpd
		#assignInNamespace(
		#    app$name,
		#    function(path,query,postBody,headers) 
		#	.self$handler(app$name,path,query,postBody,headers), 
		#    'tools'
		#)
	    } else {
		assign(
		    app$name, 
		    function(path,query,postBody,headers) 
			.self$handler(app$name,path,query,postBody,headers), 
		    tools:::.httpd.handlers.env
		)
	    }

	    invisible(TRUE)
	},
	remove = function(app=NULL,all=FALSE){
	    if (all==TRUE){
		lapply(names(appList),remove)
		return(invisible(TRUE))
	    }
	    if (inherits(app,'RhttpdApp'))
		name <- app$name
	    else if (is.character(app))
		name <- app
	    else if (is.numeric(app) || is.integer(app))
		name <- appList[[app]]$name
	    else
		base::stop("Can only remove by object, app name, or index.")

	    if (is.null(appList[[name]])) return(FALSE)

	    appList[[name]] <<- NULL
	    ret <- FALSE
	    if(name=='httpd' && !is.null(httpdOrig)){
		tools:::httpd <- httpdOrig
		ret <- TRUE
	    } else if (exists(name,tools:::.httpd.handlers.env)){
		rm(list=name,pos=tools:::.httpd.handlers.env)
		ret <- TRUE
	    }
	    invisible(ret)
	},
	parse_headers = function(headers,env){

	    hlines <- strsplit(rawToChar(headers),'\n')[[1]]

	    lapply(
		strsplit(hlines,': '),
		function(x) {
		    assign(
			paste('HTTP_',gsub('-','_',gsub('(\\w+)','\\U\\1',x[1],perl=TRUE)),sep=''),
			x[2],
			env
			)
		}
	    )
	},
   build_env = function(appPath,path,query,postBody,headers){
      env <- new.env(hash=TRUE,parent=emptyenv())

      parse_headers(headers,env)

      # remove HTTP_ from content length and type
      if (exists('HTTP_CONTENT_LENGTH',env) && exists('HTTP_CONTENT_TYPE',env)){
         assign('CONTENT_LENGTH',env$`HTTP_CONTENT_LENGTH`,env)
         assign('CONTENT_TYPE',env$`HTTP_CONTENT_TYPE`,env)
         rm('HTTP_CONTENT_LENGTH','HTTP_CONTENT_TYPE',envir=env)
      }

      assign('SCRIPT_NAME',appPath,env)
      assign('PATH_INFO',sub(appPath,'',path,fixed=TRUE),env)

      # The R internal web server unescapes the query, so in order
      # abid the Rook spec, we have to do things in reverse:
      #
      # escape the query object so that subsequent URI building
      # methods will add the correct query string.
      assign('QUERY_STRING',
         ifelse(is.null(query),
            '',
            paste(names(query),Utils$escape(query),sep='=',collapse='&')
            ),
         env
         )
      
      if(exists("HTTP_REQUEST_METHOD", env)){
        assign('REQUEST_METHOD',get("HTTP_REQUEST_METHOD", env) ,env)
      } else {      
        assign('REQUEST_METHOD',ifelse(is.null(postBody),'GET','POST'),env)
      }

      hostport <- strsplit(get('HTTP_HOST',env),':',fixed=TRUE)[[1]]

      assign('SERVER_NAME',hostport[1],env)
      assign('SERVER_PORT',hostport[2],env)

      assign('rook.version',packageDescription('Rook',fields='Version'),env)
      assign('rook.url_scheme','http',env)
      assign('rook.input',RhttpdInputStream$new(postBody),env)
      assign('rook.errors',RhttpdErrorStream$new(),env)

      if (debug()>1)
         str(as.list(env))
      env
   },
   handler = function(appName,path,query,postBody,headers){
   
#cat('handler call start\n')

      if (debug()>0){
         cat('Request:',path,'\n')
      }
      app <- appList[[appName]]
      if (is.null(app)){
         base::stop("No app installed named ",appName)
         return()
      }
      if (!is.null(app$appEnv)){
         file_path = app$appEnv$.appFile
         mtime <- as.integer(file.info(file_path)$mtime)


         if (mtime > app$appEnv$.mtime){
            add(name=appName,app=file_path)
         }
         app <- appList[[appName]]
         if (is.null(app)){
            stop("No app installed named ",appName)
            return()
         }

         oldwd <- setwd(dirname(app$appEnv$.appFile))
         on.exit(setwd(oldwd))
      }
      env <- build_env(app$path,path,query,postBody,headers)
      if (is(app$app,'function')) {
#cat('handler was here\n')
         res <- try(app$app(env))
      } else {
         res <- try(app$app$call(env))
      }
      if (inherits(res,'try-error') || (is.character(res) && length(res) == 1)) {
         res
      } else {
         # Only need to handle the case where body is a vector of strings
         # We presume that if res$body is a location to a file then the
         # app has so named it. We also presume that res$body may be
         # a raw vector, but we let the internal web server deal with that.
         if (is.character(res$body) && length(res$body) > 1){
            res$body <- paste(res$body,collapse='')
         }
         contentType <- res$headers$`Content-Type`;
         res$headers$`Content-Type` <- NULL;

         # The internal web server expects a list like the below,
         # and the position of each element is important.
         ret <- list(
            payload = res$body,
            `content-type` = contentType,
            headers = NULL,
            `status code` = res$status
         )

         # Change the name of payload to file in the case that
         # payload *is* a filename
         if (!is.null(names(res$body)) && names(res$body)[1] == 'file'){
            names(ret) <- c('file',names(ret)[-1])
         }

         # Rhttpd doesn't allow Content-Length in the headers, so delete
         # it as well
         res$headers$`Content-Length` <- NULL;
         if (length(res$headers)>0){
            ret$headers <- paste(names(res$headers),': ',res$headers,sep='')
         }

         if (debug()>0){
            cat('Response:\n')
            str(ret)
         }
         ret
      }

#cat('handler call end\n')
   },
	print = function() {
	    if (listenPort > 0){
		cat("Server started on ",listenAddr,":",listenPort,"\n",sep='')
	    } else {
		cat("Server stopped\n")
	    }
	    if (length(appList) == 0){
		cat("No applications installed\n")
		return(invisible())
	    }
	    len <- max(nchar(names(appList)))
	    for (i in 1:length(appList)){
		appName <- sprintf(paste('%-',len,'s',sep=''),names(appList)[i])
		cat('[',i,'] ',appName,' ',full_url(i),'\n',sep='')
	    }
	    cat("\nCall browse() with an index number or name to run an application.\n")
	    invisible()
	},
	show = function() print(),
	debug = function(){
	    d <- getOption('Rhttpd_debug')
	    if (!is.null(d)) as.integer(d)
	    else 0
	}
    )
)