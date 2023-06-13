FunctionRecorder <- setRefClass(
  "FunctionRecorder",
  
  fields=list(runInfo="list",temporaryInfo="list", count="numeric",temporarycount="numeric"),
  
  methods = list(
    add = function(x) {
      count <<- count + 1
      temporarycount <<- temporarycount + 1
      if(length(temporaryInfo)<temporarycount) {
        ls <- list()
        v <- rep(0.0,length(x))
        for(i in 1:1000) {
          ls[[length(ls)+1]]<-v
        }
        runInfo <<-c(runInfo,temporaryInfo)
        temporarycount <<- 1
        temporaryInfo <<- ls
      }
      temporaryInfo[[temporarycount]]<<-x
    },
    get = function() {
      c(runInfo, temporaryInfo)[1:count]
    } ,
    get_as_df = function() tryCatch({
      df <- data.frame(do.call(rbind, get()),row.names = NULL)
      if(length(get())>0 && is.null(names(get()[[1]]))){
        names(df)<-c(paste0("P",1:(ncol(df)-1)),"Value")
      }
      df
    }),
    reset = function() {
      runInfo <<-list()
      temporaryInfo <<- list()
      temporarycount <<- 0
      count <<- 0
    },
    enhanceWithRecorder = function(fun,info_fun=\(p,r)c(p,r), ...) {
      reset()
      function(...) {
        r <- fun(...)
        add(info_fun(...,r))
        r
      }
    },
    initialize = function() {
      runInfo <<-list()
      temporaryInfo <<- list()
      temporarycount <<- 0
      count <<- 0
    }
  )
)

FunctionRecorderToDisk <- setRefClass("FunctionRecorderToDisk",
  fields = list(
    filename = "character",
    writeinterval = "numeric",
    lastwrittentime = "numeric",
    lastwrittencount = "numeric"
  ),
  
  contains = "FunctionRecorder",
  
  methods = list(
    
    write = function(force=TRUE) {
      if(count > lastwrittencount &
         (force || as.numeric(Sys.time()) > lastwrittentime + writeinterval*60)) {
        append <- lastwrittencount!=0
        write.table(
          get_as_df()[(lastwrittencount +1):count,],
          filename,
          col.names = !append,
          append = append,
          row.names = FALSE,
          sep=","
        )
        lastwrittencount <<- count
        lastwrittentime <<- as.numeric(Sys.time())
      }
    },
    
    add = function(...) {
      callSuper(...)
      write(force=FALSE)
    },
    
    reset = function(newfile = NULL) {
      if(count > lastwrittencount) {
        write()
      }
      if(!is.null(newfile) && newfile!="")
      {
        filename <<- newfile
      }
      callSuper()
      lastwrittentime <<- 0
      lastwrittencount <<- 0
      
    },
    
    initialize = function(file, interval=5) {
      callSuper()
      lastwrittentime <<- 0
      lastwrittencount <<- 0
      filename <<- file
      writeinterval <<- min(1,interval)
    },
    
    finalize = function() {
      write()
    }
  )
  
)

