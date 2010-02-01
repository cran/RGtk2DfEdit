#' A package for an editing data frames for RGtk2. Improves on base edit.data.frame function found in utils
#' @name RGtk2DfEdit-package
#' @docType package


# Bugs. Large data frames seem to act strangely.
# Column name change has 2px difference in Linux
# Can get stuck in multiple concurrent entries, need to lock this down
# Dragging out of cells causes a critical error message
# Updating to R.envir might get slow
# Crash unpredictably fixed by turning OnMotion off.
# Insert into model overwrites row
# Needs redo support
# Fix scrolling behavior
# Core dump on moving around?
# Need class coercion undo via types
# Pasting rows is buggy when they include factors... omit this for now
# Updating large data frames is slow... change EntryIntoModel?
# This causes the entry to give an error:
  # dfedit(data.frame(array(rnorm(1E7), c(1E5, 1E2))), dataset.name = "big.df")
# Deleting columns alters scroll

STACK_LIMIT <- 100000
VERSION_STRING <- "version 0.5.2.1"

# for pasting
if(.Platform$OS.type == "windows"){
  NEWLINE_CHAR <- "\r\n"
} else {
  NEWLINE_CHAR <- "\n"
}

data.types <- list(
    Character = list(class = "character", coerce = as.character, is.class = is.character, isText=T),
    Integer = list(class = "integer", coerce = as.integer, is.class = is.integer),
    Numeric = list(class = "numeric", coerce = as.numeric,
      is.class = function(x) is.numeric(x) && !is.integer(x)
    ),
    Logical = list(class = "logical", coerce = as.logical, is.class = is.logical),
    Factor = list(class = "factor", coerce = function(x) {
        x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)    
        x[nchar(x) == 0] <- NA
        return(as.factor(x))
      }, 
      is.class = is.factor, isFactor = T)    
  )   
   
  bgColor <- list(as.GdkColor(c(237,236,235)*256),as.GdkColor(c(235,234,219)*256))[[(.Platform$OS.type == "windows")+1]]
  selectedColor <- as.GdkColor(c(198, 213, 253)*256) # Linux
  selectedTextColor <- as.GdkColor("black")
  
  myValidInputKeys <- c(GDK_space:GDK_asciitilde, GDK_Delete, GDK_BackSpace)
  myMetaKeys <- c(GDK_Shift_L, GDK_Shift_R, GDK_Control_L, GDK_Control_R)
  myShiftKeys <- c(GDK_Shift_L, GDK_Shift_R)
  myValidNavigationKeys <- c(GDK_Down, GDK_Left, GDK_Up, GDK_Right,
  GDK_Page_Up, GDK_Page_Down, GDK_Return, GDK_ISO_Left_Tab, GDK_Tab, GDK_Home, GDK_End)
  exclude.factor <- NA

#' Convenience function to call data frame editor in its own window
#'
#' @param items A data frame (?) to display graphically
#' @param dataset.name Name for data set
#' @param size (height, width)
#' @return An object of class GtkDfEdit for which a few RGtk2-style methods are defined
#' @export
dfedit <- function(items, dataset.name = deparse(substitute(items)), size=c(500, 300)){
  obj <- gtkDfEdit(items, dataset.name, size.request=size)
  win <- gtkWindowNew()
  win$add(obj)
  win$setTitle(dataset.name)
  invisible(obj)
}

# Extract the external representation of the data frame
GetDataFrameFromModel <- function(model, drop=F){
  # 2-12-10
  return("[.RGtkDataFrame"(model,,-c(1, dim(model)[2]), drop=drop))
}    

 	# update the data frame  	
	# Now update the data frame from our model
	# Returns the new data frame
UpdateTheRDataFrame <- function(.local, new.df = NULL, do.factors = TRUE){  
  if(missing(new.df)) {
    new.df <- GetDataFrameFromModel(.local$model)    
  }
  if(do.factors){
  	df <- get(.local$dataset.name, envir=.GlobalEnv)
  	# we're going to coerce it to a data frame if it's a matrix
  	if(!is.data.frame(df)) df <- as.data.frame(df)	
  	# now we need to carry our original contrasts over...
  	for(nam in intersect(colnames(new.df), colnames(df))){
  	  n1 <- new.df[[nam]]
  	  n2 <- df[[nam]]
  	  if(is.factor(n1) && is.factor(n2) && 
          length(levels(n1)) > 1 && length(levels(n2)) > 1){
  		  contrasts(new.df[[nam]]) <- attr(n2, "contrast.name")
  		  attr(new.df[[nam]], "contrast.name") <- attr(n2, "contrast.name")
  	  }}}
  if(!exists(.local$dataset.name, envir=.GlobalEnv, inherits=F)) 
    print(paste("RGtk2DfEdit: Creating dataset", .local$dataset.name, "in global environment."))
	assign(.local$dataset.name, new.df, envir=.GlobalEnv) 
	return(new.df)
}  	

# call this for inserting stuff in and out of the model
# row.idx and col.idx are both vectors
# model[row.idx, col.idx] <- entry.frame
# model[,c(1:2)] <- entry.frame
# 2-14-10 updates. Do this from a data frame and use rows/columns as well.
# update col names
  #dat <- data.frame("hi" = integer(0), "there" = integer(0))
  #EntryIntoModel(.local, dat, integer(0), col.idx, add.factors=TRUE)    
# update row names
  #dat <- data.frame(row.names = c("hi", "there"))
  #EntryIntoModel(.local, dat, row.idx, integer(0), add.factors=TRUE)    
  
EntryIntoModel <- function(.local, entry.frame, row.idx=NULL, col.idx=NULL,  
  add.factors=FALSE, doing.undo=FALSE){

  model <- .local$model
  dim.model <- dim(model)
  dataset.name <- .local$dataset.name
  allColumns <- .local$allColumns
  
  #if(!length(entry.frame)) return(FALSE)
  if(is.null(col.idx)) stop("Can't handle null col.idx")
  if(is.null(row.idx)) row.idx <- 1:(dim.model[1])
  if(is.null(dim(entry.frame))) entry.frame <- data.frame(entry.frame, stringsAsFactors=FALSE)
  if(dim(entry.frame)[1] != length(row.idx)) stop("Row mismatch")
  if(dim(entry.frame)[2] != length(col.idx)) stop("Column mismatch")

  old.frame <- model[row.idx,col.idx,drop=F]
    # Don't bother editing if you aren't making a change...
    # If we're putting large frames in, this is inefficient, we read them twice
  #if(data.frame.contents.equal(old.frame, entry.frame)) return()
  if(identical(old.frame, entry.frame)) return()  
  if (!doing.undo) .local$theStack[[length(.local$theStack)+1]] <- list(
    old.frame = old.frame,
    new.frame = entry.frame,
    row.idx = row.idx,
    col.idx = col.idx)
  if(object.size(.local$theStack) > STACK_LIMIT){
    jj <- 0
    while(object.size(.local$theStack) > STACK_LIMIT && length(.local$theStack))
      .local$theStack[[jj <- jj + 1]] <- NULL
    if(object.size(.local$theStack) > STACK_LIMIT){ 
      warning("This edit is too large to support undo")
      .local$theStack <- list()
    }
  }

  dmn <- dimnames(model)  
  dimnames.changed <- FALSE            
   # update row and column names
  if(length(row.idx) && dmn[[1]][row.idx] != rownames(entry.frame)) {   
    dimnames.changed <- TRUE
    dmn[[1]][row.idx] <- rownames(entry.frame)
    dmn[[1]] <- make.unique(dmn[[1]])
    .local$model[,1] <- dmn[[1]]
  }
  if(length(col.idx) && dmn[[2]][col.idx] != colnames(entry.frame)) {
    dmn[[2]][col.idx] <- entry.frame.colnames <- colnames(entry.frame)
    dimnames.changed <- TRUE
    for(jj in 1:length(col.idx)){
      new.text <- entry.frame.colnames[jj]
      col.here <- col.idx[jj]-1
     .local$allColumns[[col.here]]$eventbox$getChildren()[[1]]$getChildren()[[1]]$setText(new.text)
    }
  }      
  if(dimnames.changed){
    "dimnames<-.RGtkDataFrame"(.local$model, dmn) 
  }

  theFrame <- as.data.frame(model)      
  theClasses <- sapply(theFrame, class)

  if(length(col.idx)) if(length(row.idx)) for(jj in 1:length(col.idx)){    
    # set the column names
    col.here <- col.idx[jj]        
      # Don't enter into the last column...
    if(col.here > dim(model)[2] - 1) next;
    theClass <- theClasses[col.here] 
    # Update frame      
    if(theClass == "factor") {
      xx <- theFrame[,col.here]
      lvls <- levels(xx)
      theLevelClass <- class(lvls)        
      to.model <- as(entry.frame[,jj], theLevelClass)
        # replace "" with NA
      to.model[!nchar(to.model)] <- NA
      if(add.factors && 
        (!to.model%in%lvls||!xx[row.idx]%in%xx[!row.idx])){ # code to add a new level
        x <- as.vector(xx)
        x[row.idx] <- to.model
        theFrame[,col.here] <- factor(x) # exclude NA's or not
        # Update frame          
        rGtkDataFrameSetFrame(model, theFrame)
      } else { # existing levels
        model[row.idx, col.here] <- to.model  # level in it already
      } # if(add.factors)        
      # full update for factors 
      UpdateTheRDataFrame(.local, new.df = theFrame[,-c(1, dim(theFrame)[2]), drop=F])
    } else { # theClass != factor

      # This assignment can be slow for large data frames...
      model[row.idx,col.here] <- as(entry.frame[,jj], theClass)            
            
    }                          
  }
  UpdateTheRDataFrame(.local, new.df = theFrame[,-c(1, dim(theFrame)[2]), drop=F], do.factors=FALSE)  
}

  # from a vector, get the class name
GetDataTypeName <- function(model, col.idx){
  x <- model[,col.idx]
  nn <- names(which(sapply(data.types, function(dt) dt$is.class(x))))
  if(length(nn) == 1) return(nn)
  stop("Data type name error")
}

SelectedRowIndices <- function(tv)
  tryCatch(
    sapply(gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(tv))$retval,
      gtkTreePathGetIndices)+1,
    error=function(e) integer(0)
  )
    
# Make a data frame to put into the model.
MakeInternalDataFrame <- function(dataset, round.numbers = TRUE){
  if(is.null(dim(dataset))) dataset <- cbind(dataset)
#    if(class(dataset) == "data.frame")
#      for(ii in which(sapply(dataset, class)=="factor")){
#        dd <- dataset[,ii]
#        dataset[,ii] <- as(levels(dd), class(levels(dd))[1])[as.integer(dd)]
#      }
  if(!length(row.names(dataset)))
      row.names(dataset) <- 1:dim(dataset)[1]
  data.frame(rows = row.names(dataset), dataset, " " = "", check.names = FALSE, stringsAsFactors = FALSE)
}

# From RGtk2 demo
quick_message <- function(message) {
  ## Create the widgets 
  suppressWarnings(     
  dialog <- gtkDialog("Message", NULL, c("modal","destroy-with-parent"), "gtk-ok", "none",
                      show = FALSE))
  label <- gtkLabel(message)     
  ## Ensure that the dialog box is destroyed when the user responds.     
  gSignalConnect(dialog, "response", gtkWidgetDestroy)  
  ## Add the label, and show everything we've added to the dialog.  
  dialog[["vbox"]]$add(label)
  dialog$showAll()
}

CtrlLetter <- function(keyval, stat, let)
  keyval == let && (as.flag(stat) & GdkModifierType['control-mask'])
ShiftLetter <- function(keyval, stat, let)
  keyval == let && (as.flag(stat) & GdkModifierType['shift-mask'])  

    # this is all horribly platform dependent
CopyToClipboard <- function(dat, do.rownames=F, do.colnames=F){
  write.function <- function(dat, p)
    write.table(dat, p, row.names=F, col.names=F, quote=F, sep="\t")

  if(do.rownames) {
    t.dat <- t(cbind(rownames(dat), dat))
  } else {
    t.dat <- t(dat)      
  }
  dat2 <- paste(lapply(as.data.frame(t.dat), 
    function(ll) paste(ll, collapse="\t")), collapse=NEWLINE_CHAR)
  if(do.colnames) {
    dat.cn <- ""
    if(do.rownames) dat.cn <- "\t" 
    dat.cn <- paste(dat.cn, paste(colnames(dat), collapse="\t"), sep="")
    dat2 <- paste(dat.cn, dat2, sep=NEWLINE_CHAR)
  }

  if(.Platform$OS.type == "windows") {
    #write.function(dat, "clipboard")
    get("writeClipboard", envir=.GlobalEnv)(dat2)
  } else if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin")
      a <- pipe("pbcopy", "w")
    else {
      if(!length(system('which xclip', intern=T))){
        print("xclip must be installed")
        return(FALSE)
      }
      a <- pipe("xclip -selection c", open="w")
    }
    write.function(dat2, a)
    close(a)
  }
}  

# return a table of characters from the clipboard
# need to do special from row names
ReadFromClipboard <- function(){
  read.function <- function(p) readLines(p)
  
  if(.Platform$OS.type == "windows") {
    dat <- read.function("clipboard")
  } else if (.Platform$OS.type == "unix"){
    if (Sys.info()["sysname"] == "Darwin")
      a <- pipe("pbpaste")
    else{ 
      if(!length(system('which xsel', intern=T))){
        print("xsel must be installed")
        return(FALSE)
      }
      a <- pipe("xsel -o -b", open="r")
    }
    dat <- read.function(a)
    close(a)
  }
          
  dat2 <- sapply(paste(dat, "\t", sep=""), 
    function(x) strsplit(x, "\t")[[1]], USE.NAMES=F)
  if(is.vector(dat2)) # 1 column selected
    dat2 <- as.matrix(dat2)
  else 
    dat2 <- t(dat2)
  return(dat2)
}
  
# Need to make sure this doesn't overwrite last column
InsertIntoModel <- function(.local, dat, do.rownames = F, do.colnames = F, 
    paste.rows=F){
  theRownames <- NULL
  theColnames <- NULL
  if(do.rownames && do.colnames) {
    theColnames <- theColnames[-1]
    theRownames <- theRownames[-1]
    dat <- dat[-1,-1,drop=F]
  } else if (do.rownames) {
    theRownames <- dat[,1]
    dat <- dat[,-1,drop=F]    
  } else if (do.colnames) {
    theColnames <- dat[1,]
    dat <- dat[-1,,drop=F] 
  }
  
  model <- .local$model
  view <- .local$view
  dd <- dim(dat)
  dm <- dim(model)
    # insertion points  
  ins <- c(SelectedRowIndices(view)[1], .local$selectedColumns[1])
  if(paste.rows) ins[1] <- SelectedRowIndices(.local$view.rn)[1]
  if(is.na(ins[1])) ins[1] <- 1
  if(is.na(ins[2])) ins[2] <- 1  
  if(paste.rows) ins <- c(SelectedRowIndices(.local$view.rn)[1],
    1:length(.local$allColumns))
  if(is.na(ins[2]) && !do.rownames) {
    quick_message("A column must be selected to paste to")
    return()
  }
  ins.end <- c(ins[1]+dd[1]-1, ins[2]+dd[2]-1) # end of insertion range    
  # change class of dat and copy in
  entry.frame <- data.frame(dat, stringsAsFactors = F)
  row.idx <- ins[1]:ins.end[1]
  #row.idx <- ins[1]+1:min(dd[1], dm[1]-ins[1]+1)-1
  
  col.idx <- ins[2] + 1:min(dd[2], dm[2]-ins[2])

    # We're falling off the frame. Add some rows.
  rr1 <- ins[1]+min(dd[1], dm[1]-ins[1]+1)-1    
  if(ins.end[1] > rr1){
    n.rows.to.add <- (ins.end[1] - rr1)      # no of rows to add
    m1 <- model[1,,drop=F]
    theClasses <- sapply(m1, class)    
    #theClasses[theClasses=="factor"] <- "character"
    # this is a nasty kludge to get a reasonable blank line
    for(jj in 1:length(m1)) {
      theClass <- theClasses[jj]
      if(theClass == "factor") {
        m1[,jj] <- as.factor(NA)
      } else {
        m1[,jj] <- as("", theClass)
      }
    }
    m1 <- m1[rep(1, n.rows.to.add),,drop=F]
    model$appendRows(m1)      
  }
  
  entry.frame <- entry.frame[1:length(row.idx), 1:length(col.idx),drop=F]  
  dmn <- dimnames(model)  
  if(is.null(theRownames)) {
    rownames(entry.frame) <- dmn[[1]][row.idx]
  } else {
    rownames(entry.frame) <- theRownames
  }
  if(is.null(theColnames)) {
    colnames(entry.frame) <- dmn[[2]][col.idx]
  } else {
    colnames(entry.frame) <- theColnames
  }      
  EntryIntoModel(.local, entry.frame, row.idx, col.idx, add.factors=TRUE)  
  # update
  .local$LAST_PATH <- gtkTreePathNewFromString(as.character(dim(model)[1]-1))
}

# from gWidgets
gtkMenuPopupHack <- function (object, parent.menu.shell = NULL, parent.menu.item = NULL, 
  func = NULL, data = NULL, button, activate.time) {
  checkPtrType(object, "GtkMenu")
  if (!is.null(parent.menu.shell)) 
      checkPtrType(parent.menu.shell, "GtkWidget")
  if (!is.null(parent.menu.item)) 
      checkPtrType(parent.menu.item, "GtkWidget")
  if (!is.null(func)) 
     func <- as.function(func)
  button <- as.numeric(button)
  activate.time <- as.numeric(activate.time)
  w <- .RGtkCall("S_gtk_menu_popup", object, parent.menu.shell, 
      parent.menu.item, func, data, button, activate.time, 
      PACKAGE = "RGtk2")
  return(invisible(w))
}


###############################################################################
# Factor editor
###############################################################################
##########################################
# Blocking editor

 # start.lvl = level name to start cycling at
DoBlockSize <- function(loc.window, .local, col.idx, selected.rows, start.lvl=NULL){

  UpdateColumn <- function(){
    block.size <- spinner$getValue()
    total.len <- length(selected.rows)
    # Update frame              
    #entry.frame <- gl(length(lvls), block.size, total.len, labels=lvls)

    entry.frame <- .local$model[selected.rows, col.idx, drop=F]      
    entry.frame[,1] <-  gl(length(lvls), block.size, total.len, labels=lvls)    
    EntryIntoModel(.local, entry.frame, selected.rows, col.idx, add.factors=FALSE)
    #model[selected.rows,col.idx] <- gl(length(lvls), block.size, total.len, labels=lvls)
  }

  model <- .local$model
  col.original <- model[,col.idx]
  if(!is.factor(col.original)) stop("Can't block over non-factors")  
  lvls <- unique(levels(col.original))
  if(!is.null(start.lvl)){
    if(!start.lvl%in%lvls) stop("start.level must be in levels")
    ww <- which(lvls == start.lvl)[1]
    if(ww > 1) lvls <- c(lvls[ww:length(lvls)], lvls[1:(ww-1)])
  }
  
  window2 <- gtkWindowNew(show=F)
  window2$setTitle("Blocking")
  window2$setTransientFor(loc.window)
  window2$setModal(TRUE)
  box0 <- gtkVBoxNew(FALSE, 5)
  
  window2$add(box0)
  window2$setResizable(FALSE)
  if(!is.null(loc.window)) window2$setTransientFor(loc.window)
  window2$setModal(TRUE)

  box1 <- gtkHBoxNew(FALSE, 5)
  box0$packStart(box1, FALSE, FALSE, 5)
  box1$add(gtkLabelNew("Select Block Size"))
  spinner_adj <- gtkAdjustment(1, 1, 100.0, 1.0, 5.0)
  spinner <- gtkSpinButton(spinner_adj, 1.0, 0)
  spinner$setValue(1)
  box1$add(spinner)
  spinner$grabFocus()

  box2 <- gtkHBoxNew(FALSE, 5)
  box0$packEnd(box2, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("    OK    ")
  gSignalConnect(spinner, "value-changed", function(...){
    UpdateColumn()
  })
  gSignalConnect(spinner, "key-press-event", function(obj, evt){
    if(evt[["keyval"]] == GDK_Return) window2$destroy()
    FALSE
  })
  gSignalConnect(button, "clicked", function(...){
    window2$destroy()
  })
  
  box2$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Cancel")
  gSignalConnect(button, "clicked", function(...){
    # Update frame
    model[,col.idx] <- col.original    
    window2$destroy()
    })
  box2$packEnd(button, FALSE, FALSE, 5)
  window2$show()
  UpdateColumn()   
}

DoFactorEditor <- function(data.model, .local, col.idx=integer(0)){

  # Update levels of model at col.idx, new levels are xx
  UpdateModel <- function(data.model, levls, col.idx){
    df <- as.data.frame(data.model)
    levls <- unique(levls)
      # we're changing the levels (ordering), not the same as editing level names
    df[,col.idx] <- factor(df[,col.idx], levels=levls, exclude = exclude.factor)
    # Update frame    
    data.model$setFrame(df)
  }

  cell.edited <- function(cell, path.string, new.text, data){
    xx <- .local$xx      
    if(!nchar(new.text))# || new.text%in%xx) 
      stop("New name must exist")# and be unique")
    checkPtrType(data, "GtkListStore")
    model <- data
    path <- gtkTreePathNewFromString(path.string)
    iter <- model$getIter(path)$iter

    i <- path$getIndices()[[1]]+1

      # editing the level names
    df <- as.data.frame(data.model)
    zz <- df[,col.idx]
    lzz <- levels(zz)    
    ww <- which(xx[i]==lzz)
    
    if(length(ww)){
      lzz[ww] <- new.text
      levels(zz) <- lzz
      df[,col.idx] <- zz
      # Update frame
      data.model$setFrame(df)
    }
    xx[i] <- new.text
    model$set(iter, 0, new.text)
    .local$xx <- xx
    UpdateLabel()    
  }
    
  add.item <- function(button, data) {
    xx <- .local$xx      
     for(k in 1:(length(xx)[1]+1)){
       nl <- paste("Level", k, sep="_")
       if(!nl%in%xx) break
     }
     xx <- c(xx, nl)
     .local$xx <- xx     
     iter <- model$append()$iter
     model$set(iter, 0, xx[length(xx)])

     UpdateModel(data.model, xx, col.idx)
     UpdateLabel()
   }


  remove.item <- function(widget, data)
  {
     xx <- .local$xx   
     checkPtrType(data, "GtkTreeView")
     treeview <- data
     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]]){
        iter <- selected$iter
         
        path <- model$getPath(iter)
        i <- path$getIndices()[[1]]+1
        model$remove(iter)
        
        xx <- xx[-i]
        .local$xx <- xx           
        path$prev()
        selection$selectPath(path)
        
        UpdateModel(data.model, xx, col.idx)
        UpdateLabel()        
      }
  }

  move.item.up <- function(widget, data)
  {
     xx <- .local$xx
     checkPtrType(data, "GtkTreeView")
     treeview <- data

     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]])
       {
         iter <- selected$iter
           
         path <- model$getPath(iter)
         i <- path$getIndices()[[1]]+1
         if(i == 1) return()
         model$set(iter, 0, xx[i-1])
         path$prev()
         selection$selectPath(path)
         selected <- selection$getSelected()
         iter <- selected$iter
         model$set(iter, 0, xx[i])
         tmp <- xx[i-1]
         xx[i-1] <- xx[i]
         xx[i] <- tmp
         .local$xx <- xx
         UpdateModel(data.model, xx, col.idx) 
         UpdateLabel()                 
       }
  }

  move.item.down <- function(widget, data)
  {
     xx <- .local$xx
     checkPtrType(data, "GtkTreeView")
     treeview <- data

     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]])
       {
         iter <- selected$iter
           
         path <- model$getPath(iter)
         i <- path$getIndices()[[1]]+1
         if(i == length(xx)) return()
         model$set(iter, 0, xx[i+1])
         gtkTreePathNext(path)
         selection$selectPath(path)
         selected <- selection$getSelected()
         iter <- selected$iter
         model$set(iter, 0, xx[i])
         tmp <- xx[i+1]
         xx[i+1] <- xx[i]
         xx[i] <- tmp
         .local$xx <- xx         
         UpdateModel(data.model, xx, col.idx)    
         UpdateLabel()              
       }
  }

  edit.item <- function(widget, data) {
     checkPtrType(data, "GtkTreeView")
     treeview <- data

     model <- treeview$getModel()
     selection <- treeview$getSelection()
   
     selected <- selection$getSelected()
     if (selected[[1]])
       {
         iter <- selected$iter         
         path <- model$getPath(iter)         
         treeview$setCursorOnCell(path,
           treeview$getColumns()[[1]],
           treeview$getColumns()[[1]]$getCellRenderers()[[1]],
           TRUE)
         UpdateLabel()           
       }
  }
  
  UpdateLabel <- function(){
    xx <- .local$xx  
    lab1$setMarkup(paste("Control (first level) is: <b>", xx[1], "</b>", sep=""))
  }  

  # called with no selection
  data.factors <- sapply(data.model[,], is.factor)
  if(!length(colnames(data.model)[data.factors])) stop("No columns are factors")
  if(!length(col.idx)) col.idx=which(data.factors)[1]
    
  window <- gtkWindowNew(show=F)
  if(!is.null(.local$toplevel)) {
    window$setTransientFor(.local$toplevel)  
    window$setModal(TRUE)
  }  
  window$setTitle("Factor Editor")
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)

  fr0 <- gtkFrameNew(label="Column Selection")

  cb1 <- gtkComboBoxNewText()  
  cb1$show()
  cb1["width-request"] <- 100
  
  for (item in colnames(data.model)[data.factors]) # omit "rows" heading
    cb1$appendText(item)  
  ##
  #cb1$setActive(col.idx)
  cb1$setActive(which(data.factors[col.idx])-1)
  fr0$add(cb1)
    # switch the active column
  

  box0$packStart(fr0, FALSE, FALSE, 5)
    
  fr1 <- gtkFrameNew(label="Factor Level Order")
  box0$packStart(fr1, TRUE, TRUE, 5)

  box1 <- gtkHBoxNew(FALSE, 5)
  fr1$add(box1)

  box1.5 <- gtkVBoxNew(FALSE, 0)  
  box1$packStart(box1.5, TRUE, TRUE, 5)

  box1.6 <- gtkHBoxNew(FALSE, 0)  
  box1.5$packStart(box1.6, FALSE, TRUE, 5)  
  lab1 <- gtkLabelNew("")  
  box1.6$packStart(lab1, FALSE, FALSE, 5)
                                            
  MakeModel <- function(data.model, col.idx)    {
    col.original <- data.model[,col.idx]
    if(!is.factor(col.original)) {
      col.original <- as.factor(integer(0))
      #stop("Can't edit non-factors")
      print("Can't edit non-factors")    
      }
    .local$xx <- na.omit(cbind(levels(col.original)))
    model <- gtkListStoreNew("gchararray")
    xx <- .local$xx
    sapply(xx, function(x) model$set(model$append()$iter, 0, x))
    UpdateLabel()  
    return(model)
  }
    
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setPolicy("automatic", "automatic")
  box1.5$packStart(sw, TRUE, TRUE, 0)
  treeview <- gtkTreeViewNew()  
  col.original <- data.model[,col.idx]
  model <- MakeModel(data.model, col.idx)  
  treeview$setModel(model)
  
  gSignalConnect(cb1, "changed", function(widget){
      new.idx <- which(widget$getActiveText()==colnames(data.model))
      model <- MakeModel(data.model, new.idx)
      treeview$setModel(model)
      assign("col.idx", new.idx, envir=parent.env(environment()))
      assign("col.original", data.model[,new.idx], envir=parent.env(environment()))
  })   
  
  treeview$setRulesHint(TRUE)
  treeview$setHeadersVisible(FALSE)
  treeview$getSelection()$setMode("single")
  renderer <- gtkCellRendererTextNew()
  renderer$setData("column", 0)

  renderer['editable-set'] <- TRUE
  renderer['editable'] <- TRUE
  treeview$insertColumnWithAttributes(-1, "Name", renderer, text = 0)

  sw$setShadowType(as.integer(1))
  sw$add(treeview)
  sw$setSizeRequest(300, -1)
  window$setResizable(FALSE)

  gSignalConnect(renderer, "edited", cell.edited, model)

  box2 <- gtkVBoxNew(FALSE, 5)
  box1$packStart(box2, FALSE, FALSE, 5)

  button1 <- gtkButtonNewWithLabel("Move Up")
  box2$packStart(button1, FALSE, FALSE, 0)

  button2 <- gtkButtonNewWithLabel("Move Down")
  box2$packStart(button2, FALSE, FALSE, 0)

  button3 <- gtkButtonNewWithLabel("Edit Name")
  box2$packStart(button3, FALSE, FALSE, 0)

  button4 <- gtkButtonNewWithLabel("Add Level")
  box2$packStart(button4, FALSE, FALSE, 0)

  button5 <- gtkButtonNewWithLabel("Remove Level")
  box2$packStart(button5, FALSE, FALSE, 0)
  
  gSignalConnect(button1, "clicked", move.item.up, data=treeview)
  gSignalConnect(button2, "clicked", move.item.down, data=treeview)
  gSignalConnect(button3, "clicked", edit.item, data=treeview)
  gSignalConnect(button4, "clicked", add.item, model)
  gSignalConnect(button5, "clicked", remove.item, treeview)    

  expander <- gtkExpanderNew(label="Factor Contrasts")
  fr2 <- gtkFrameNew(label="Contrast Coding")
  #box0$packStart(fr2, FALSE, FALSE, 5)
  box0$packStart(expander, FALSE, FALSE, 5)
  expander$add(fr2)
  box4 <- gtkVBoxNew(FALSE, 5)
  fr2$add(box4)

  contrast.coding <- list(
  "Treatment (default) - contrasts each level with the first.\nThe first level is omitted." = "contr.treatment", 
  "Helmert - contrast the second level with the first, the \nthird with the average of the first two, and so on." = "contr.helmert",  
  "Polynomial - contrasts based on orthogonal polynomials." = "contr.poly", 
  "Sum - sum to zero contrasts." = "contr.sum", 
  "SAS - treatment contrasts with base level set to be the\nlast level of the factor." = "contr.SAS")

  df <- get(.local$dataset.name, envir=.GlobalEnv)
  contr <- attr(df[,col.idx-1], "contrast.name")
    
  grb <- gtkRadioButtonNewWithLabel(NULL, label=names(contrast.coding)[1])
  grb$setActive(TRUE)
  box4$packStart(grb, FALSE, FALSE, 0)
  for(ii in 2:length(names(contrast.coding))){
    grb <- gtkRadioButtonNewWithLabel(group=grb$getGroup(), 
      label=names(contrast.coding)[ii])
    if(!is.null(contr) && contr==unlist(contrast.coding)[ii]) grb$setActive(TRUE)
    box4$packStart(grb, FALSE, FALSE, 0)
  }

  fr3 <- gtkFrameNew(label="Fill Column")
  box0$packStart(fr3, FALSE, FALSE, 5)
  box5 <- gtkHBoxNew(TRUE, 5)
  fr3$add(box5)
  button <- gtkButtonNewWithLabel("  Fill With Replicates...  ")
  gSignalConnect(button, "clicked", function(...){
    DoBlockSize(window, .local, col.idx, 1:dim(data.model)[1])
  })
  box5$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("  Random Fill  ")
  box5$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("  Selected  ")
  box5$packEnd(button, FALSE, FALSE, 0)

  box6 <- gtkHBoxNew(FALSE, 5)
  box0$packEnd(box6, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("    OK    ")
  gSignalConnect(button, "clicked", function(...){
    contr <- contrast.coding[[which(sapply(rev(grb$getGroup()), gtkToggleButtonGetActive))]]
	
  	# update contrasts
  	# we smuggle the contrast name in attr("contrast.name")
    df <- UpdateTheRDataFrame(.local)
  	if(length(levels(df[,col.idx-1])) > 1) {
      contrasts(df[,col.idx-1]) <- contr
    	attr(df[,col.idx-1], "contrast.name") <- contr
    	}
  	assign(.local$dataset.name, df, envir=.GlobalEnv)
	
    window$destroy()
  })

  box6$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Cancel")
  gSignalConnect(button, "clicked", function(...) {
    df <- as.data.frame(data.model)
    df[,col.idx] <- col.original
    # Update frame
    data.model$setFrame(df)    
    window$destroy()
    })

  box6$packEnd(button, FALSE, FALSE, 5)
  UpdateLabel()
  window$show()
}

###############################################################################
# End factor editor
###############################################################################

###############################################################################
# Sort dialog
###############################################################################

# Returns the ordering of the table
DoSortDialog <- function(data.model, handler, .localenv){
  window <- gtkWindowNew(show=F) 
  if(!is.null(.localenv$toplevel)) {
    window$setTransientFor(.localenv$toplevel)  
    window$setModal(TRUE)
  }
  window$setTitle("Sort Options")
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)
  items <- colnames(data.model)
  fr0 <- gtkFrameNew(label="Sort Key Selection")
  box1 <- gtkVBoxNew(FALSE, 5)
  box0$add(fr0)
  fr0$add(box1)
  MakeRadiobuttonGroup <- function(labels, theChoice=1){
    box3 <- gtkVBoxNew()
    grb1 <- gtkRadioButtonNewWithLabel(NULL, label=labels[1])
    if(theChoice == 1) grb1$setActive(TRUE)
    box3$add(grb1)
    for(jj in 2:length(labels)){
      grb1 <- gtkRadioButtonNewWithLabel(group=grb1$getGroup(),label=labels[jj])
      if(jj == theChoice) grb1$setActive(TRUE)
      box3$add(grb1)
    }
    return(list(box=box3, grb=grb1))
  }
  MakeComboEntry <- function(items, box1){
    box2 <- gtkHBoxNew()
		cb1 <- gtkComboBoxNewText()  
		cb1$show()
		cb1["width-request"] <- 100		
		for (item in items) # omit "rows" heading
		  cb1$appendText(item)  
		cb1$setActive(0)
    box5 <- gtkVBoxNew()		
    box5$packStart(cb1, TRUE, FALSE, 0)    
    box2$packStart(box5, TRUE, TRUE, 10)
    xx1 <- MakeRadiobuttonGroup(c("Ascending", "Descending"))
    box3 <- xx1$box
    grb1 <- xx1$grb
    xx2 <- MakeRadiobuttonGroup(c("Default", "Character", "Numeric"))
    box4 <- xx2$box
    grb2 <- xx2$grb
		box2$packStart(box4, FALSE, FALSE, 0)    
		box2$packStart(box3, FALSE, FALSE, 0)
    box1$packStart(box2, FALSE, FALSE, 0)
    return(list(col=cb1, ord=grb1, typ=grb2, box=box2))
  }
  .sl <- new.env()
  .sl$theList <- list()
  .sl$theList[[length(.sl$theList)+1]] <- MakeComboEntry(items, box1)
  fr1 <- gtkFrameNew(label="Add/Remove Keys")
  box9000 <- gtkHBoxNew(FALSE, 5)
  fr1$add(box9000)
  button <- gtkButtonNewWithLabel("Remove A Key")
  gSignalConnect(button, "clicked", function(obj, ...){
    if(length(.sl$theList)<2) return(FALSE)
    .sl$theList[[length(.sl$theList)]]$box$destroy()
    .sl$theList[[length(.sl$theList)]] <- NULL
    window$resize(window$getSize()$width, 1)
  })
  box9000$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Add A Key")
  gSignalConnect(button, "clicked", function(obj, data=.sl){
    .sl$theList[[length(.sl$theList)+1]] <- MakeComboEntry(items, box1)
  })
  box9000$packEnd(button, FALSE, FALSE, 0)
  box0$packStart(fr1, FALSE, FALSE, 0)

  fr2 <- gtkVBoxNew()
  box18000 <- gtkHBoxNew(FALSE, 5)
  fr2$add(box18000)
  button <- gtkButtonNewWithLabel(" Cancel ")
  gSignalConnect(button, "clicked", function(obj, data=.sl){
    window$destroy()
  })
  box18000$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("   OK   ")
  box18000$packEnd(button, FALSE, FALSE, 0)
  gSignalConnect(button, "clicked", function(obj, data=.sl){
		opts <- lapply(.sl$theList, function(item){
				list(col = item$col$getActiveText(),
  				ord = which(rev(sapply(item$ord$getGroup(), gtkToggleButtonGetActive))),
  				typ = which(rev(sapply(item$typ$getGroup(), gtkToggleButtonGetActive))))
		})
		dataset.order <- do.call("order", lapply(opts, function(item){
		  xx <- data.model[[item$col]]
		  if(item$typ == 1) {
        xrank <- xtfrm(xx)
      } else if (item$typ == 2) {
		    xrank <- xtfrm(as.character(xx))
      } else if (item$typ == 3) {
		    xrank <- xtfrm(as.numeric(xx))
      } else {
        stop("Sort error")
      }
			(c(-1, 1)[(item$ord==1)+1])*xrank
		}))
    handler(dataset.order, .localenv)
    window$destroy()    
  })
  box0$packStart(fr2, FALSE, FALSE, 0)  
  window$setResizable(FALSE)
  window$show()
}
###############################################################################
# End sort dialog
###############################################################################

#' Data frame editor for RGtk2
#'
#' @param items A data frame (?) to display graphically
#' @param dataset.name Name for data set
#' @param container An RGtk2 container object to place widget within. (Can this be missing?)
#' @return An object of class GtkDfEdit for which a few RGtk2-style methods are defined
#' @export
gtkDfEdit <- function(items, dataset.name = deparse(substitute(items)), size.request=c(500, 300)){
   # our local environment
    .local <- new.env()
  ############################################################
  # Turns out that scroll_row_timeout has no horizontal autoscroll.
  # I based this on gtktreeview::gtk_tree_view_vertical_autoscroll, no offset
  HScroll <- function(data){ 
      # which side of the middle are we?
    view <- .local$view
    sw.ha <- .local$sw.ha
    ptr <- view$getBinWindow()$getPointer()
    vr <- view$getVisibleRect()$visible.rect
    sw.ha.value <- sw.ha$value
    direction <- ifelse (ptr$x - sw.ha.value <= vr$width/2, -1, 1) 
  
    val <- sw.ha.value + direction*sw.ha$step_increment
    if(0 <= val && val <= sw.ha$upper - sw.ha$page_size) {
      sw.ha$setValue(val)
    } else if (val < 0) {
      sw.ha$setValue(0)
    } else {
      sw.ha$setValue(sw.ha$upper - sw.ha$page_size)
    }
    TRUE
  }
  
  # Bind this to button-release, focus-out and enter-notify
  RemoveHScrollTimeout <- function(obj, event){
    try({
      gSourceRemove(.local$hScrollTimeout) 
      .local$doingHScroll <- FALSE
    }, silent=T) 
    FALSE
  }
  
  AddHScrollTimeout <- function(obj, event){
    sw.ha <- .local$sw.ha
    view <- .local$view
    if (!.local$doingHScroll){
      ptr <- obj$getBinWindow()$getPointer()
      if (as.flag(ptr$mask) & GdkEventMask['button-press-mask']){
      x <- ptr$x - sw.ha$getValue()
      y <- ptr$y
      vr <- view$getVisibleRect()$visible.rect
      h <- vr$height
      w <- vr$width
      z1 <- y > h/w*x 
      z2 <- y > h - h/w*x
      if((z1 && !z2) || (!z1 && z2)){ 
        .local$hScrollTimeout <- gTimeoutAdd(.local$SCROLL_ROW_TIMEOUT, HScroll)
        .local$doingHScroll <- TRUE
      }
      }} #if, if
      TRUE
  }
  ############################################################
  # End autoscroll

###############################################################################
# Keyboard handling
###############################################################################

  PaintSelectionOnTimeout <- function(){
    # New code, 2-12-10.
    # We want to add a timeout to the LAST key pressed.
    try({
      gSourceRemove(.local$do.paint.timeout) 
      .local$do.paint.timeout <- NULL
    }, silent=T)

    .local$do.paint.timeout <- gTimeoutAdd(150, function(){
			.local$do.paint <- TRUE
       UpdateSelectionRectangle()
			return(FALSE)
		})
  }
  ####
  ############################################################
  MoveCursor <- function(widget, direction, stat=as.integer(0)){
#  print(direction)
    cursor.info <- gtkTreeViewGetCursor(widget)
    path <- cursor.info$path
    allColumns <- .local$allColumns

    new.idx <- GetColIdx(cursor.info$focus.column)
    ori.idx <- new.idx


    if (direction == "right" && new.idx < length(allColumns)-1)
      new.idx <- new.idx+1
    else if (direction == "left" && 1 < new.idx)
      new.idx <- new.idx-1
    else if (direction == "down" && gtkTreePathCompare(path, .local$LAST_PATH)!=0)
      gtkTreePathNext(path)
    else if (direction == "up")
      gtkTreePathPrev(path)
    else {
      #if(.local$do.paint){
      #.local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      #}
      #gtkTreeViewSetCursorOnCell(widget, path, allColumns[[new.idx]]$column, allColumns[[new.idx]]$renderer, FALSE)
      return(TRUE)
    }      
      #column indicators
    if(direction%in%c("up", "down") && stat == as.integer(0)){
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      }
      UpdateColumnSelection(allColumns, new.idx)
    }
      
    if(direction%in%c("left", "right")){
      selectedColumns <- GetSelectedColumns()
      col.idx <- new.idx
      if (stat == as.integer(0)) { # select only this column
        if(.local$do.paint){
          .local$do.paint <- FALSE
          .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
        }
        .local$start.column.select <- NULL
         selectedColumns.new <- col.idx
         UpdateColumnSelection(allColumns, selectedColumns.new)
      } else if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
        if(.local$do.paint){
          .local$do.paint <- FALSE
          .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
        }
          # this had better exist!
        selectedColumns.new <- .local$start.column.select:col.idx
        UpdateColumnSelection(allColumns, selectedColumns.new)
        PaintSelectionOnTimeout()
      } else {
        .local$start.column.select <- NULL
      }
    }

    new.col <- allColumns[[new.idx]]$column
    renderer <- allColumns[[new.idx]]$renderer

      # ScrollToCell seems to stop working after we add a new column.
      # This routine is from gtktreeview::gtk_tree_view_scroll_to_cell
    if(direction%in%c("left", "right")){
      cell_rect <- gtkTreeViewGetBackgroundArea(widget, path, new.col)$rect
      vis_rect <- gtkTreeViewGetVisibleRect(widget)$visible.rect
      dest_x <- vis_rect$x
      dest_y <- vis_rect$y
      if (cell_rect$x < vis_rect$x)
        dest_x <- cell_rect$x
      if (cell_rect$x+cell_rect$width > vis_rect$x + vis_rect$width)
        dest_x <- cell_rect$x + cell_rect$width - vis_rect$width
      gtkTreeViewScrollToPoint(widget, dest_x, dest_y)
    }
    if(.local$flash.cursor){    
      .local$do.rendererEditingStarted <- FALSE    
      gtkTreeViewSetCursorOnCell(widget, path, new.col, renderer, TRUE)    
      .local$do.rendererEditingStarted <- TRUE 
      .local$flash.cursor <- FALSE
    }          
      # This has the side effect of deselecting all but 1 row     
    gtkTreeViewSetCursorOnCell(widget, path, new.col, renderer, FALSE)
    return(TRUE)
  }
  
  RowNamesKeyPress <- function(widget, event,...) {
    keyval <- event[["keyval"]] 
    stat <- as.flag(event[["state"]])    
    if (CtrlLetter(keyval, stat, GDK_z)){
      #print("Ctrl-z")
      if(!length(.local$theStack)) return(TRUE)
      last.action <- .local$theStack[[length(.local$theStack)]]
      if(!is.null(last.action)){
        EntryIntoModel(.local, last.action$old.frame, last.action$row.idx, last.action$col.idx, doing.undo=TRUE)
        .local$theStack[[length(.local$theStack)]] <- NULL
      }
      return(TRUE)
    }
    return(FALSE)
  }    
      
  #MoveCursor(widget, direction, stat, allColumns, flash.cursor){
  ViewKeyPress <- function(widget,event,...) {
    keyval <- event[["keyval"]] 
    if(keyval%in%c(GDK_Page_Up, GDK_Page_Down, GDK_Home, GDK_End)) {
      #if(.local$do.paint){
      #  .local$do.paint <- FALSE
      #  .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      #}
      PaintSelectionOnTimeout()
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      }      
      return(FALSE)    
    }
    allColumns <- .local$allColumns
    view <- .local$view
    model <- .local$model
    stat <- as.flag(event[["state"]])
     # give control back to treeview on going shift- or ctrl-up or down
    if(CtrlLetter(keyval, stat, GDK_Up) || CtrlLetter(keyval, stat, GDK_Down) 
      || CtrlLetter(keyval, stat, GDK_Left) || CtrlLetter(keyval, stat, GDK_Right)) 
      return(FALSE)
    if (ShiftLetter(keyval, stat, GDK_Up) || ShiftLetter(keyval, stat, GDK_Down)){
      PaintSelectionOnTimeout()
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
      }
      return(FALSE)            
    }
    if (CtrlLetter(keyval, stat, GDK_c)) {
      #print("Ctrl-c")
      CopyToClipboard(model[SelectedRowIndices(.local$view), GetSelectedColumns()+.local$COLUMN_OFFSET, drop=F])
      return(TRUE)
    }
    if (CtrlLetter(keyval, stat, GDK_v)){
      #print("Ctrl-v")
      dat <- ReadFromClipboard() # character vector
      InsertIntoModel(.local, dat)
      return(TRUE)
    }
    if (CtrlLetter(keyval, stat, GDK_z)){
      #print("Ctrl-z")
      if(!length(.local$theStack)) return(TRUE)
      last.action <- .local$theStack[[length(.local$theStack)]]
      if(!is.null(last.action)){
        EntryIntoModel(.local, last.action$old.frame, last.action$row.idx, last.action$col.idx, doing.undo=TRUE)
        .local$theStack[[length(.local$theStack)]] <- NULL
      }
      return(TRUE)
    }
    if (CtrlLetter(keyval, stat, GDK_a)){
      #print("Ctrl-a")
      SelectAll(allColumns)
      return(TRUE)
    }
    # this makes us drop out of selection mode...
    if(keyval == GDK_Delete) {
      row.idx <- SelectedRowIndices(view)
      col.idx <- GetSelectedColumns() + COLUMN_OFFSET
      entry.frame <- .local$model[row.idx, col.idx, drop=F]      
      entry.frame[,] <- ""
      EntryIntoModel(.local, entry.frame, row.idx, col.idx)
      return(TRUE)
    }
      # don't leave select mode for control-letters
    if(keyval%in%myMetaKeys ||
      (stat == GdkModifierType['control-mask'])){
      if(keyval%in%myShiftKeys)
        .local$start.column.select <- GetColIdx(widget$getCursor()$focus.column)
      return(TRUE)
    }
    # stop painting selection rectangle
    .local$rectangles <- list()
    
    if(keyval%in%myValidNavigationKeys){
      if (keyval == GDK_Up || ShiftLetter(keyval, stat, GDK_Return))
        return(MoveCursor(widget, "up", stat))
      if(keyval == GDK_Down || keyval == GDK_Return)
        return(MoveCursor(widget, "down", stat))
      if(keyval == GDK_Right || keyval == GDK_Tab)
        return(MoveCursor(widget, "right", stat))
      if(keyval == GDK_Left || keyval == GDK_ISO_Left_Tab)
        return(MoveCursor(widget, "left", stat))
      return(FALSE)
    }

    cursor.info <- widget$getCursor()
    col.idx <- GetColIdx(cursor.info$focus.column)
    renderer <- allColumns[[col.idx]]$renderer
      # probably don't need this, cursor's there already?
     gtkTreeViewSetCursorOnCell(widget, cursor.info$path,
      cursor.info$focus.column, renderer, TRUE)
    if(event$time != .local$last.time){
      .local$last.time <- event$time
      gtkPropagateEvent(view, event) # or gdkEventPut(event)
    } else {
      warning("Key handling loop")
      gtkCellRendererStopEditing(renderer, FALSE)
    }
    return(TRUE)
  }

  RendererEditingStarted <- function(renderer, entry, path, data) {
    checkPtrType( entry, "GtkEntry")
    if(!.local$do.rendererEditingStarted) return(FALSE)
    model <- .local$model
    view <- .local$view
      # don't paint rectangles
    .local$do.paint <- FALSE
	
  	entry$setData("renderer", renderer)
  	entry$setData("index", data)
  	entry$setData("path.str", path)	   
  	entry$setData("path", gtkTreePathNewFromString(path))
     
      # We've trapped the temporary GtkEntry the renderer creates.
    gSignalConnect(entry, "key-press-event", RendererEntryKeyPress)
      # you can leave the entry in 2 ways, focusing out or pressing a key from within
    gSignalConnect(entry, "focus-out-event", after=T, function(entry, event){
      model <- .local$model
      view <- .local$view
  	  col.idx <- entry$getData("index")  
  	  row.idx <- as.integer(entry$getData("path.str"))+1      
      keyval <- entry$getData("keyval")
      # don't paint rectangles
      # .local$do.paint <- TRUE

      if(.local$doingEntryIntoCompletion){ #we're entering data into a factor 
        entry.frame <- .local$model[row.idx, col.idx, drop=F] 
        #entry.frame[1,1] <- entry$getText()
        zz <- list()
        zz[[colnames(entry.frame)[1]]] <- entry$getText()
        entry.frame <- data.frame(zz, row.names=rownames(entry.frame))      
        EntryIntoModel(.local, entry.frame, row.idx, col.idx, add.factors=TRUE)        
          # Get out of selection in reasonable way
        if(!is.null(keyval)){
	        stat <- entry$getData("stat")			  
          if (ShiftLetter(keyval, stat, GDK_Return))
             return(FALSE)
          if (keyval == GDK_Return)
             MoveCursor(view, "down")        
        }
      } else {  # not a factor, just do it
        entry.frame <- .local$model[row.idx, col.idx, drop=F]      
        entry.frame[1,1] <- entry$getText()      
        EntryIntoModel(.local, entry.frame, row.idx, col.idx)
      }
      #.local$doing.entry <- FALSE # unlock
      return(FALSE)
    })
   col.idx <- data
   .local$doingEntryIntoCompletion <- FALSE  
   if(!is.null(data.types[[GetDataTypeName(model, col.idx)]]$isFactor)){
     create.completion.model <- function(factor.levels) {
       store <- gtkListStoreNew("gchararray")
       sapply(factor.levels, function(x) store$set(store$append()$iter, 0, x))
       return(store)
     }
     factor.levels <- levels(model[,col.idx])
     completion <- gtkEntryCompletionNew()
       # Assign the completion to the entry
     entry$setCompletion(completion)
       # Create a tree model and use it as the completion model
     completion.model <- create.completion.model(factor.levels)
     completion$setModel(completion.model)
     completion$setTextColumn(0)
     completion$setMinimumKeyLength(0)
     gtkEntryCompletionSetInlineCompletion(completion, TRUE)
     .local$doingEntryIntoCompletion <- TRUE
       # move cursor down if you hit return after you select a match
     gSignalConnect(completion, "match-selected", function(widget, completion.model, iter){
       entry$setText(gtkTreeModelGetValue(completion.model, iter, 0)$value)
       MoveCursor(view, "down")
       return(TRUE)
     })
	  entry$setData("completion", completion)
    }
    return(FALSE)
  }

  RendererEntryKeyPress <- function(entry, event, ...) {
    keyval <- event[["keyval"]]                        
    stat <- as.flag(event[["state"]])
    entry$setData("keyval", keyval)
  	entry$setData("stat", stat)   
    view <- .local$view
      # control keys for popup selection
    if(.local$doingEntryIntoCompletion && stat == as.integer(0) && keyval%in%c(GDK_Up, GDK_Down, GDK_Return))
      return(FALSE)
    if(!keyval%in%myValidInputKeys && !keyval%in%myMetaKeys){
       # paint rectangles again
       # .local$do.paint <- TRUE
        # we're done editing, put data into the model
      if (keyval == GDK_Up || ShiftLetter(keyval, stat, GDK_Return))
        return(MoveCursor(view, "up"))
      if (keyval == GDK_Down || keyval == GDK_Return)
        return(MoveCursor(view, "down"))
      if (keyval == GDK_Left || keyval == GDK_ISO_Left_Tab)
        return(MoveCursor(view, "left"))
      if (keyval == GDK_Right || keyval == GDK_Tab)
        return(MoveCursor(view, "right")) 
      return(TRUE)
    }
    if(keyval%in%myMetaKeys)
      return(TRUE)
    return(FALSE)
  }

  
###############################################################################
# End keyboard handling
###############################################################################
    
  .local$selectedColumns <- integer(0)
	##############################################################################
	# Miscellaneous functions
	###############################################################################
  GetColIdx <- function(column){
    tryCatch(
      as.integer(column["title"]),
      error = function(e) integer(0))
  }

  # Don't use this for potentially large numbers of rows  
  GetSelectedColumns <- function() .local$selectedColumns
   # lazy state setting to save init time
   # remember to return allColumns
  ColumnSetState <- function(allColumns, ii, state){    
    if(length(ii) != 1 || ii < 1 || length(allColumns) < ii) 
	  stop(paste("trying to set column index outside bounds:", ii))	
    if(is.null(allColumns[[ii]]$state.set))
      gtkWidgetModifyBg(allColumns[[ii]]$eventbox, as.integer(1), selectedColor)
    gtkWidgetSetState(allColumns[[ii]]$button,
      ifelse(state, as.integer(1), as.integer(0)))
    allColumns[[ii]]$state.set <- TRUE
    .local$allColumns <- allColumns
    if(state) {
      .local$selectedColumns <- sort(union(ii, .local$selectedColumns)) 
    } else {
      .local$selectedColumns <- sort(setdiff(.local$selectedColumns, ii)) 
    }
  }
  
  UpdateColumnSelection <- function(allColumns, selectedColumns.new) {
      selectedColumns <- GetSelectedColumns()
      selectedColumns.new[selectedColumns.new == length(allColumns)] <- length(allColumns)-1
      for (ii in setdiff(selectedColumns.new, selectedColumns))
        ColumnSetState(allColumns, ii, TRUE)
      for (ii in setdiff(selectedColumns, selectedColumns.new))
        ColumnSetState(allColumns, ii, FALSE)
  }
  
  SelectAll <- function(allColumns){
    UpdateColumnSelection(allColumns, 1:length(allColumns))
    if(length(.local$rectangles)){
      .local$rectangles <- list()
      .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
    }
    gtkTreeSelectionSelectAll(gtkTreeViewGetSelection(.local$view))
#    gtkTreeSelectionSelectAll(gtkTreeViewGetSelection(.local$view.rn))    
  }

  RowNamesButtonPress <- function(widget, event, data) {
    UpdateColumnSelection(.local$allColumns, integer(0))
    if(length(.local$rectangles)){
      .local$rectangles <- list()
      .local$viewGetBinWindow$invalidateRect(NULL, FALSE)             
    }
    if (event[["button"]] == as.integer(3)){ # our popup menu
      info <- widget$getPathAtPos(event[["x"]], event[["y"]])
      m <- Row3rdButtonMenu(.local$model, info$path$getIndices()+1)
      gtkMenuPopupHack(m, button = event$GetButton(), 
        activate.time = gdkEventGetTime(event))
      return(TRUE)
    }    
    return(FALSE)
  }

  ViewButtonPress <- function(widget, event, data) {
    model <- .local$model
    allColumns <- .local$allColumns
    if (event[["button"]] == as.integer(3)){ # our popup menu
      info <- widget$getPathAtPos(event[["x"]], event[["y"]])
      col.idx <- GetColIdx(info$column)
      row.idx <- info$path$getIndices()+1
	  	m <-Cell3rdButtonMenu(model, row.idx, col.idx)
      gtkMenuPopupHack(m, button = event$GetButton(),
        activate.time = gdkEventGetTime(event))
      return(TRUE)
    }
    if (event[["button"]] == as.integer(1)){
      info <- widget$getPathAtPos(event[["x"]], event[["y"]])
      if(is.null(info$path)) return(FALSE)
      if(event[["state"]] != as.integer(0)) return(FALSE)
      #widget$setCursorOnCell(info$path, allColumns[[col.idx]]$column, allColumns[[col.idx]]$renderer, FALSE)
      UpdateColumnSelection(allColumns, GetColIdx(info$column))
    }   
    .local$flash.cursor <- TRUE
      #  Flash the rectangles off when you click
    .local$do.paint <- TRUE        # 2-12-10
    if(length(.local$rectangles)){
      .local$rectangles <- list()
  	  widget$getBinWindow()$invalidateRect(NULL, FALSE)
    }
    return(FALSE)
  }
  
    # Sync column selection with mouse-down
  ViewMotionNotify <- function(widget, event){
    allColumns <- .local$allColumns
    pointer <- gdkWindowGetPointer(event[["window"]])
    if (as.flag(pointer$mask) & GdkModifierType["button1-mask"]){
      info <- gtkTreeViewGetPathAtPos(widget, pointer[["x"]], pointer[["y"]])
      if (info$retval){
        col.idx <- GetColIdx(info$column)
    		if(!length(GetSelectedColumns())) {
    		  new.sel <- col.idx
    		} else {
    		  new.sel <- GetSelectedColumns()[1]:col.idx
    		  UpdateColumnSelection(allColumns, new.sel)
    		}
      }
    }
    return(FALSE)
  }
  
  ViewButtonRelease <- function(widget, event){
    allColumns <- .local$allColumns
    sw.va <- .local$sw.view.va
        
    info <- widget$getPathAtPos(event$x, event$y)
    selectedColumns <- GetSelectedColumns()
    col.idx <- GetColIdx(info$column)
    
      # Guess which way the user's gone outside the columns
    if(!length(col.idx)){      
      view <- .local$view
      sw.ha <- .local$sw.ha
      ptr <- view$getBinWindow()$getPointer()
      vr <- view$getVisibleRect()$visible.rect
      sw.ha.value <- sw.ha$value
      direction <- ifelse (ptr$x - sw.ha.value <= vr$width/2, -1, 1)
      if(direction == 1) col.idx <- length(allColumns)
      if(direction == -1) col.idx <- 1
    }
    
    if(length(selectedColumns) && length(col.idx)) {
      selectedColumns <- selectedColumns[1]:col.idx
    } else if (length(col.idx)) {
      selectedColumns <- col.idx
    }# otherwise col.idx is NA    
    .local$rectangles <- list() # reset our rectangle-drawing

   # 2-12-2010
  	# gsr <- widget$getSelection()$getSelectedRows()$retval
  	#if(!length(gsr)){ # no rows selected, deselect all column
    #	UpdateColumnSelection(allColumns, integer(0))
  	#  return(FALSE) # return if we didn't select any rows this time
  	#} else { # rows selected
   UpdateColumnSelection(allColumns, selectedColumns)
  	#}
   UpdateSelectionRectangle()
  }

  UpdateSelectionRectangle <- function(){
    widget <- .local$view
    allColumns <- .local$allColumns
    sw.va <- .local$sw.view.va

    gsr <- widget$getSelection()$getSelectedRows()$retval
    if(length(gsr)){
    	selectedColumns <- GetSelectedColumns()
    	path1 <- gsr[[1]]
    	path2 <- gsr[[length(gsr)]]
    	path1.index <- path1$GetIndices()+1
    	path2.index <- path2$GetIndices()+1
    	
    	currentVadj <- sw.va$getValue() # this gets called *before* value-changed
    	if(length(selectedColumns)){
    	  rect1 <- widget$getCellArea(path1, allColumns[[selectedColumns[1]]]$column)$rect
    	  rect1$y <- rect1$y + currentVadj
    	  rect2 <- widget$getCellArea(path2, 
          allColumns[[selectedColumns[length(selectedColumns)]]]$column)$rect
    	  rect2$y <- rect2$y + currentVadj
    	  .local$rectangles <- list(gdkRectangleUnion(rect1, rect2)$dest)
    	}
    	  # contiguous
    	  # we need this in windows but not osx or linux    
 	  } else {
  	  .local$rectangles <- list()
 	  }
  	widget$getBinWindow()$invalidateRect(NULL, FALSE)
    return(FALSE)   
  }
   

  Cell3rdButtonMenu <- function(model, row.idx, col.idx){
	  typ <- GetDataTypeName(model, col.idx+COLUMN_OFFSET)
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  copyWithNamesItem <- gtkMenuItem("Copy With Names")
	  pasteItem <- gtkMenuItem("Paste")
	  m$append(cutItem)
	  m$append(copyItem)
	  m$append(copyWithNamesItem)	  
	  m$append(pasteItem)
	  lapply(c(cutItem), gtkWidgetSetSensitive, FALSE)
	  gSignalConnect(copyItem, "activate", 
      function(...) CopyToClipboard(model[SelectedRowIndices(.local$view), 
      GetSelectedColumns()+.local$COLUMN_OFFSET, drop=F]))
	  gSignalConnect(copyWithNamesItem, "activate", 
      function(...) CopyToClipboard(model[SelectedRowIndices(.local$view), 
      GetSelectedColumns()+.local$COLUMN_OFFSET, drop=F], do.rownames=T, do.colnames=T))      
	  gSignalConnect(pasteItem, "activate", function(...) {
      dat <- ReadFromClipboard() # character vector
      InsertIntoModel(.local, dat)
     })      
	  
	  m$append(gtkSeparatorMenuItem())
	
      model <- .local$model
      view <- .local$view	
	  editFactorsItem <- gtkMenuItem("Edit Factors...")
	  randomizeItem <- gtkMenuItem("Randomize Selected")
	  fillItem <- gtkMenuItem("Fill Selected Down")	  
	  fillCyclicItem <- gtkMenuItem("Fill In Blocks")
      m$append(editFactorsItem)
	  m$append(randomizeItem)
	  m$append(fillItem)
	  m$append(fillCyclicItem)
	  if(is.null(data.types[[typ]]$isFactor))
	    lapply(c(editFactorsItem, fillCyclicItem), gtkWidgetSetSensitive, FALSE)
	  gSignalConnect(editFactorsItem, "activate", function(...) DoFactorEditor(model, .local, col.idx + COLUMN_OFFSET))
	  gSignalConnect(randomizeItem, "activate", function(...){
         sr <- SelectedRowIndices(view)
         if(length(sr)){
           entry.frame <- .local$model[sr, GetSelectedColumns()+1, drop=F]
           rn <- row.names(entry.frame)
           entry.frame <- entry.frame[sample(1:(dim(entry.frame)[1])),,drop=F]
           rownames(entry.frame) <- rn
           EntryIntoModel(.local, entry.frame, sr, GetSelectedColumns()+1)
         }
       })  
	  gSignalConnect(fillItem, "activate", function(...){
         sr <- SelectedRowIndices(view)
         sc <- GetSelectedColumns()+1         
         if(length(sr) && length(sc)){
           entry.frame <- .local$model[sr, sc, drop=F]
           rn <- row.names(entry.frame)
           for(jj in 1:length(sc))
             entry.frame[,jj] <- entry.frame[1,jj]
           rownames(entry.frame) <- rn
           EntryIntoModel(.local, entry.frame, sr, sc)
         }
       })  
	  gSignalConnect(fillCyclicItem, "activate", function(...) {
         sr <- SelectedRowIndices(view)
         if(length(sr)) DoBlockSize(.local$toplevel, .local, col.idx+COLUMN_OFFSET, sr, start.lvl=model[sr[1], col.idx+COLUMN_OFFSET])
       })
    return(m)
  }
  
  # Function to call when the data is sorted
  SortHandler <- function(new.order, .local){
    model <- .local$model
    .local$model$setFrame(model[new.order,])
    UpdateTheRDataFrame(.local)    
    quick_message("  Undo for this operation not yet implemented  ")
    # turn undo off  
    .local$theStack <- list()
  }

	Corner3rdButtonMenu <- function(model){	
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  pasteItem <- gtkMenuItem("Paste")
	  m$append(cutItem)
	  m$append(copyItem)
	  m$append(pasteItem)
	  m$append(gtkSeparatorMenuItem())
	  gSignalConnect(copyItem, "activate", function(...) 
      CopyToClipboard(model[,,drop=F], do.rownames=T, do.colnames=T))

	  lapply(c(cutItem, pasteItem), 
      gtkWidgetSetSensitive, FALSE)
      	  
	  sortItem <- gtkMenuItem("Sort...")	  
	  gSignalConnect(sortItem, "activate", function(...) 
      DoSortDialog(.local$model[,-dim(.local$model)[2]], SortHandler, .local))
	  m$append(sortItem)
	  m$append(gtkSeparatorMenuItem())

    # move dataset 1 column along
 	  #ordinalItem <- gtkMenuItem("Default Row Names")
	  #gSignalConnect(ordinalItem, "activate", function(...) 
    #  SetModelRowNames(.local, 1:dim(.local$model)[1]))

 	  setColumnItem <- gtkMenuItem("Row Names To Column")
	  gSignalConnect(setColumnItem, "activate", function(...) 
      InsertColumn(1, .local$model[,1,drop=F]))
	  #m$append(ordinalItem)      
	  m$append(setColumnItem)
	  m$append(gtkSeparatorMenuItem())
 	  setNameItem <- gtkMenuItem("Edit Dataset Name")
	  m$append(setNameItem)	  
	  
	  lapply(c(setNameItem), gtkWidgetSetSensitive, FALSE)
    
    m$append(gtkSeparatorMenuItem())
	  aboutItem <- gtkMenuItem("About...")
	  m$append(aboutItem)	  
	  gSignalConnect(aboutItem, "activate", function(...){
  	  dlg <- gtkAboutDialogNew(show=F)
      dlg["authors"] <- c("Tom Taverner <Thomas.Taverner@pnl.gov>", 
        "Contributions from John Verzani", 
        "R bindings to Gtk by Michael Lawrence and Duncan Temple Lang")
      dlg["program-name"] <- "RGtk2DfEdit"
      dlg["comments"] <- "A spreadsheet data frame editor for the R environment"
      dlg["copyright"] <- "GPLv2"
      dlg["version"] <- VERSION_STRING
      gtkDialogRun(dlg)
      gtkWidgetDestroy(dlg)
	  })
    	  
	  return(m)
	}
	
  InsertRow <- function(.local, row.idx){
    model <- .local$model
    new.row <- rep(NA, dim(model)[2])
    the.idx <- append(1:dim(model)[1], dim(model)[1]+1, after=row.idx)    
    new.df <- rbind(as.data.frame(model), new.row)
    new.df[dim(new.df)[1], dim(new.df)[2]] <- ""
    new.df <- new.df[the.idx,]
    new.df[,1] <- rownames(new.df)    
    .local$model$setFrame(new.df)
    UpdateTheRDataFrame(.local, do.factors=FALSE)
   .local$LAST_PATH <- gtkTreePathNewFromString(as.character(dim(.local$model)[1]-1))              
  }

	Row3rdButtonMenu <- function(model, row.idx){
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  pasteItem <- gtkMenuItem("Paste")
	  m$append(cutItem)
	  m$append(copyItem)
	  m$append(pasteItem)
	  m$append(gtkSeparatorMenuItem())
	  gSignalConnect(copyItem, "activate", function(...) 
       CopyToClipboard(model[SelectedRowIndices(.local$view.rn),-c(1, dim(model)[2]), drop=F], do.rownames=T))
	  gSignalConnect(pasteItem, "activate", function(...) {
      dat <- ReadFromClipboard() # character vector
      InsertIntoModel(.local, dat, do.rownames = T, paste.rows=T)
    })                     

	  insertItem <- gtkMenuItem("Insert")
	  gSignalConnect(insertItem, "activate", function(...) InsertRow(.local, row.idx-1))
	  insertAfterItem <- gtkMenuItem("Insert After")    
	  gSignalConnect(insertAfterItem, "activate", function(...) InsertRow(.local, row.idx))
    deleteItem <- gtkMenuItem("Delete")
	  gSignalConnect(deleteItem, "activate", function(...){	  
	    sr <- SelectedRowIndices(.local$view.rn)
	    #sr <- setdiff(sr, dim(model)[1])
	    if(length(sr)){
  	    new.df <- as.data.frame(model)[-sr,]
        .local$model$setFrame(new.df)
        UpdateTheRDataFrame(.local, do.factors=FALSE)
        .local$LAST_PATH <- gtkTreePathNewFromString(as.character(dim(.local$model)[1]-1))            
      }
    })
	  clearItem <- gtkMenuItem("Clear Contents")
	  m$append(insertItem)
	  m$append(insertAfterItem)	  
	  m$append(deleteItem)
	  m$append(clearItem)

	  lapply(c(cutItem, clearItem, pasteItem), gtkWidgetSetSensitive, FALSE)
    if(!length(SelectedRowIndices(.local$view.rn))){	  
  	  lapply(c(deleteItem), gtkWidgetSetSensitive, FALSE)    
 	  }
	  
	  return(m)
	}
	
	Column3rdButtonMenu <- function(model, col.idx){	
	  typ <- GetDataTypeName(model, col.idx+COLUMN_OFFSET)
	  theColumn <- .local$model[,col.idx+COLUMN_OFFSET,drop=F]
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  pasteItem <- gtkMenuItem("Paste")
	  m$append(cutItem)
	  m$append(copyItem)
	  m$append(pasteItem)
	  m$append(gtkSeparatorMenuItem())
	  gSignalConnect(copyItem, "activate", 
      function(...) CopyToClipboard(.local$model[,GetSelectedColumns()+1,drop=F],
         do.colnames=T))
	  gSignalConnect(pasteItem, "activate", function(...) {
      dat <- ReadFromClipboard() # character vector
      InsertIntoModel(.local, dat, do.colnames = T)
     })              

	  insertItem <- gtkMenuItem("Insert")
	  deleteItem <- gtkMenuItem("Delete")
	  clearItem <- gtkMenuItem("Clear Contents")
	  m$append(insertItem)
	  m$append(deleteItem)
	  m$append(clearItem)
	  m$append(gtkSeparatorMenuItem())  
	  gSignalConnect(insertItem, "activate", function(...) InsertColumn(col.idx))
	  gSignalConnect(deleteItem, "activate", function(...) DeleteColumn(GetSelectedColumns()))  
	  
	  sortItem <- gtkMenuItem("Sort...")	  
	  gSignalConnect(sortItem, "activate", function(...) 
      DoSortDialog(.local$model[,-dim(.local$model)[2]], SortHandler, .local))
	  m$append(sortItem)
	  m$append(gtkSeparatorMenuItem())
	  
    	  
	  lapply(c(cutItem, clearItem), gtkWidgetSetSensitive, FALSE)
	  if(col.idx == length(.local$allColumns)) 
      lapply(c(deleteItem), gtkWidgetSetSensitive, FALSE)

	  dataTypeItems <- list()
	  for(theNewTypeName in names(data.types)){
  		item <- gtkCheckMenuItem(theNewTypeName)
  		item$setDrawAsRadio(TRUE)
  		dataTypeItems[[length(dataTypeItems)+1]] <- item
  		gSignalConnect(item, "button-release-event", function(item, evt, theNewTypeName) {
  			CoerceDataTypeTo(col.idx, theNewTypeName)
  			m$popdown()
  			return(TRUE)
  		  }, theNewTypeName)
  		if (theNewTypeName==typ) item$setActive(TRUE)  
  		m$append(item)
	  }
	  m$append(gtkSeparatorMenuItem())
	  editFactorsItem <- gtkMenuItem("Edit Factors...")  
	  m$append(editFactorsItem)  
	  m$append(gtkSeparatorMenuItem())
	  abbreviateItem <- gtkMenuItem("Shorten")
	  m$append(abbreviateItem)
	  setAsRowNamesItem <- gtkMenuItem("Column To Row Names")
	  m$append(setAsRowNamesItem)

	  gSignalConnect(editFactorsItem, "activate", function(...) DoFactorEditor(model, .local, col.idx + COLUMN_OFFSET))  
	  if(typ!="Factor") editFactorsItem$setSensitive(FALSE)
	  
	  gSignalConnect(abbreviateItem, "activate", function(...){
	     abcol <- cbind(abbreviate(as.character(theColumn[[1]]), minlength=10))
	     colnames(abcol) <- colnames(theColumn)
	     rownames(abcol) <- NULL
	     InsertColumn(col.idx, abcol)
    })	  
	  gSignalConnect(setAsRowNamesItem, "activate", function(...){
	    dat <- data.frame(row.names = make.unique(as.character(theColumn[[1]])))
      EntryIntoModel(.local, dat, 1:dim(model)[1], integer(0), add.factors=TRUE)
    })	  
    	      	                                         
	  return(m)
	}

  # corner clicked
  # just update the dataset name when we change the text
  CornerBoxButtonPress <- function (obj, event, data){
      # get out of editing or selection
    button <- event[['button']]
    typ <- event[['type']]
    stat <- event[['state']]
    col.idx <- data$col.idx
    if (button == as.integer(1)){
     if(typ == as.integer(4)){ # single clicked
       gtkTreeViewGetSelection(.local$view.rn)$unselectAll()
       SelectAll(.local$allColumns)     
     } else if(typ == as.integer(5)){ # double clicked
      box = obj$getChildren()[[1]]
      label = box$getChildren()[[1]]
      height = label$allocation$height
      box$remove(label)
      entry <- gtkEntryNew()
      entry$setText(label$getText())
      entry$setHasFrame(FALSE)
      entry$modifyBase(as.integer(0), bgColor)
      entry$setAlignment(1)
      entry$setSizeRequest(-1, height-2) # 1 pixel margins I guess?
      box$packEnd(entry, FALSE, FALSE, 0)
      entry$grabFocus()
      gSignalConnect(entry, "key-press-event", function(obj, event, data=col.idx){
        if (event[["keyval"]] == GDK_Return) .local$view$grabFocus()
      })
      gSignalConnect(entry, "focus-out-event", function(obj, event, data=col.idx){
        getText <- obj$getText()
    	  if(nchar(getText) < 1) return(FALSE)
    	  
        label$setText(getText)
    	  assign(getText, GetDataFrameFromModel(.local$model), envir=.GlobalEnv)
    	  .local$dataset.name <- getText	  
        print(paste("RGtk2DfEdit: Creating dataset", .local$dataset.name, "in global environment."))
        obj$destroy()
        box$packEnd(label, FALSE, FALSE, 0)
        FALSE
      })
      return(TRUE)
    	} # end double clicked
    	} # end clicked
      if (button == as.integer(3)){ # our popup menu  
    	m <- Corner3rdButtonMenu(.local$model)
      gtkMenuPopupHack(m, button = event$GetButton(),
          activate.time = gdkEventGetTime(event))
      return(FALSE)
    }
    return(TRUE)
  }

	# column header clicked
  EventBoxButtonPress <- function (obj, event, data){
    # get out of editing or selection
    
    view <- .local$view
    view.rn <- .local$view.rn
    model <- .local$model
    allColumns <- .local$allColumns  
    
    # unselect the main view
    if(length(.local$rectangles)){
      .local$rectangles <- list()
      .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
    }      
    for(tv in c(view, view.rn)){
      pfc <- gtkTreeViewGetCursor(tv)
      #if(!is.null(pfc$path))
      #  gtkTreeViewSetCursorOnCell(tv, .local$FIRST_PATH, pfc$focus.column) 
      gtkTreeViewGetSelection(tv)$unselectAll()
    }      
    
    button <- event[['button']]
    typ <- event[['type']]
    stat <- event[['state']]
    col.idx <- data$col.idx
      # ignore d-click on last column
    # ignore if it's the end column  
    lastColumn <- col.idx == length(allColumns)
      
    if (!lastColumn && button == as.integer(1) && typ == as.integer(5)){ # double clicked
      box = obj$getChildren()[[1]]
      label = box$getChildren()[[1]]
      height = label$allocation$height
      box$remove(label)
      entry <- gtkEntryNew()
      entry$setText(label$getText())
      entry$setHasFrame(FALSE)
      entry$modifyBase(as.integer(0), bgColor)
      entry$setAlignment(1)
      entry$setSizeRequest(-1, height-2) # 1 pixel margins I guess?
      box$packEnd(entry, FALSE, FALSE, 0)
      entry$grabFocus()
      gSignalConnect(entry, "key-press-event", function(obj, event, data=col.idx){
        if (event[["keyval"]] == GDK_Return) .local$view$grabFocus()
      })    
      gSignalConnect(entry, "focus-out-event", function(obj, event, data=col.idx){
        #label$setText(getText)
        #colnames(model)[data+COLUMN_OFFSET] <- getText
        #UpdateTheRDataFrame(.local)
   	    ll <- list()
   	    ll[[obj$getText()]] <- integer(0)
   	    dat <- data.frame(ll)
        obj$destroy()
        box$packEnd(label, FALSE, FALSE, 0)
        EntryIntoModel(.local, dat, integer(0), data+COLUMN_OFFSET, add.factors=TRUE)        
        FALSE
      })
      return(TRUE)
    } else if (button == as.integer(1) && typ == as.integer(4)){ # column clicked  	

    	handler <- .local$columnClickedHandler
      if(!is.null(handler) && is.function(handler)) {
        df <- GetDataFrameFromModel(.local$model)
        col <- data$col.idx
        handler(df, col)
      }        

      selectedColumns <- GetSelectedColumns()
      if (stat == as.integer(0)) { # select only this column
         selectedColumns.new <- col.idx
        # extend shift-selection
        .local$start.column.select <- col.idx
      } else if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
        if(is.null(.local$start.column.select))
          .local$start.column.select <- col.idx
        selectedColumns.new <- .local$start.column.select:col.idx  
      } else if (as.flag(stat) & GdkModifierType['control-mask']) { # range
         selectedColumns.new <-
          c(union, setdiff)[[col.idx%in%selectedColumns+1]](selectedColumns, col.idx)
      } else {
        .local$start.column.select <- NULL
      }
      UpdateColumnSelection(allColumns, selectedColumns.new)          
    } # clicked
    if (button == as.integer(3)){ # our popup menu
  	  m <- Column3rdButtonMenu(model, col.idx)
      gtkMenuPopupHack(m, button = event$GetButton(),
          activate.time = gdkEventGetTime(event))
      return(FALSE)
     }
    return(TRUE)
  }
  
  # new version. coerce all selected.
  CoerceDataTypeTo <- function(col.idx, theNewTypeName){
    model <- .local$model
    df <- as.data.frame(model)

    selectedColumns <- GetSelectedColumns()
    if(!length(selectedColumns)) selectedColumns <- col.idx
    for(col.idx in selectedColumns){
      if(theNewTypeName%in%names(data.types)){
        df[,col.idx+COLUMN_OFFSET] <- data.types[[theNewTypeName]]$coerce(df[,col.idx+COLUMN_OFFSET])
      } else {
        stop("Unknown data type name")
      }
        # if class is text then ellipsize it...
      renderer <- .local$allColumns[[col.idx]]$renderer
      if(!is.null(data.types[[theNewTypeName]]$isText)) {
        renderer['ellipsize-set'] <- TRUE
        renderer['ellipsize'] <- PangoEllipsizeMode['end']
      } else if (renderer['ellipsize-set'] == TRUE){
        renderer['ellipsize'] <- PangoEllipsizeMode['none']
        renderer['ellipsize-set'] <- FALSE
      }
    }
    # update frame
  	model$setFrame(df)	
  } # end function

  
  # replace the entire gtktab
  ReplaceEditWindow <- function(new.df){
    new.df.to.insert <- MakeInternalDataFrame(new.df)        
    .local$gtktab$destroy()
    .local$gtktab <- MakeDFEditWindow(new.df.to.insert, .local$dataset.name)   
    .local$group.main$packStart(.local$gtktab, TRUE, TRUE, 0)
  }
    
  # Delete column at position in backing D.F.
  DeleteColumn <- function(col.idx){
    backing.df <- GetDataFrameFromModel(.local$model)  
    new.df <- backing.df[,-col.idx,drop=F]
    
    #path <- gtkTreeViewGetCursor(.local$view)$path
    
    ReplaceEditWindow(new.df)
    UpdateColumnSelection(.local$allColumns, integer(0)) 
    UpdateTheRDataFrame(.local)    
    
    #  #set the cursor
    #col.obj <- .local$allColumns[[col.idx]]
    #gtkTreeViewSetCursorOnCell(.local$view, path, col.obj$column, 
    #  col.obj$renderer, FALSE)    
  }
      
  # Append column at a position, null defaults to blank
  InsertColumn <- function(insert.idx, new.column=NULL, default.name = "New.Column"){
    backing.df <- GetDataFrameFromModel(.local$model)
    ncols <- dim(backing.df)[2]
    idx.ord <- append(1:ncols, ncols+1, after=insert.idx-1)    
    if(is.null(new.column)){    
      new.colname <- make.names(append(colnames(backing.df), default.name), 
        unique=T)[ncols+1]
      new.column <- cbind(rep("", dim(backing.df)[1]))    
      colnames(new.column) <- new.colname
    }
    new.df <- data.frame(backing.df, new.column, stringsAsFactors=F, 
      check.names=F)[,idx.ord]  
      
    #path <- gtkTreeViewGetCursor(.local$view)$path
              
    ReplaceEditWindow(new.df)
    UpdateTheRDataFrame(.local)
    
    #  # set the cursor
    #col.obj <- .local$allColumns[[insert.idx]]
    #gtkTreeViewSetCursorOnCell(.local$view, path, 
    #  col.obj$column, col.obj$renderer, FALSE)    
  }    


	###############################################################################
	# End of misc functions
	###############################################################################

	###############################################################################
	# GUI setup
	###############################################################################
	# create a column with the title as the index
	NewColumn <- function(model, j, width=100, is.row = F, is.editable=T){
		renderer <- gtkCellRendererTextNew()
		renderer['xalign'] <- 1
		renderer['editable-set'] <- TRUE
		renderer['editable'] <- is.editable
		column <- gtkTreeViewColumnNewWithAttributes(
      ifelse(is.row, "", as.character(j-1)), renderer, text = j-1)
		gtkTreeViewColumnSetFixedWidth(column, width)
		gtkTreeViewColumnSetSizing(column, GtkTreeViewColumnSizing['fixed'])
		gtkTreeViewColumnSetResizable(column, TRUE)
		return(list(column=column, renderer=renderer, col.idx=j))
	}

	# Must be called after widget is mapped
	MakeButtonAndEventBox <- function(col.obj, label.str, handler){
		label <- gtkLabelNew(label.str)
		box <- gtkHBoxNew()
		gtkBoxPackEnd(box, label, FALSE, FALSE, 0)
		eventbox <- gtkEventBoxNew()
		gtkContainerAdd(eventbox, box)
		view.col <- col.obj$column
		gtkTreeViewColumnSetWidget(view.col, eventbox)
		gtkAlignmentSet(gtkWidgetGetParent(eventbox), 0, 1, 1, 1)
		col.idx <- GetColIdx(view.col)
		col.obj$eventbox <- eventbox
		col.obj$button <- gtkWidgetGetParent(gtkWidgetGetParent(gtkWidgetGetParent(eventbox)))
		gSignalConnect(eventbox, "button-press-event", handler, data=list(col.idx=col.idx))
		return(col.obj)
	}
	  # function that takes a NewColumn returned object and adds 
    # eventboxes, buttons, etc
    # MUST BE DONE AFTER VIEW IS MAPPED
  InitColumn <- function(col.obj, nam){
    col.obj <- MakeButtonAndEventBox(col.obj, label.str=nam, handler=EventBoxButtonPress)
    gSignalConnect(col.obj$renderer, "editing-started", after=T, RendererEditingStarted, data=col.obj$col.idx)
    col.obj$state.set <- NULL    
    return(col.obj)
  }  
  InitAllColumns <- function(model, allColumns){
    ds.colnames <- colnames(model)
    for(j in 1:length(allColumns)){
      allColumns[[j]] <- InitColumn(allColumns[[j]], ds.colnames[j+1])
      allColumns[[j]]$col.idx <- j+.local$COLUMN_OFFSET
    }
    # The last column is not editable
    allColumns[[j]]$renderer['editable'] <- FALSE 
    return(allColumns)
  }
  
    # don't paint when you're scrolling
  MakeAVerticalScrollbar <- function(gtktab, va){
	  vbar <- gtkVScrollbarNew(va)
	  vbar$setEvents(GdkEventMask["button-press-mask"] | GdkEventMask["button-release-mask"])
	  gtktab$attach(vbar, 1, 2, 0, 1, 0, 5)
	 
	  gSignalConnect(vbar,"button-press-event", function(obj, evt) {
		  .local$do.paint <- FALSE
		  .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
		  return(FALSE)
	  })  
	  gSignalConnect(vbar,"button-release-event", function(obj, evt) {
		  .local$do.paint <- TRUE
		  .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
		  return(FALSE)
	  })
	  return(vbar)
  }
	
   ViewExpose <- function(widget, event=NULL, data=NULL){
    if(.local$do.paint){
      sw.va <- .local$sw.view.va
      currentVadj <- sw.va$getValue() # this gets called *before* value-changed
      for(r in .local$rectangles){
        r$y <- r$y - currentVadj
        r1 <- r
        if(r1$height > 0 && r1$width > 0) 
          gdkDrawRectangle(.local$viewGetBinWindow, .local$gc.invert, filled = F, r1$x, r1$y, r1$width, r1$height)          
      }
    } # display mode
    return(FALSE)
  }

	###############################################################################
	# End of GUI setup
	###############################################################################
		   
MakeDFEditWindow <- function(ds, dataset.name, ...){  
  sw.view <- gtkScrolledWindowNew()
  sw.view$setPolicy(GtkPolicyType['never'], GtkPolicyType['never'])
  sw.view$setSizeRequest(size.request[1], size.request[2])
  
  model <- rGtkDataFrame(ds)
  .local$model <- model
  view <- gtkTreeViewNewWithModel(model)
  gtkTreeViewSetFixedHeightMode(view, TRUE)
  allColumns <- vector("list", (dim(model)[2]-1))
  for(j in 2:(dim(model)[2])){
    tmp <- NewColumn(model, j)
    gtkTreeViewAppendColumn(view, tmp$column)
    allColumns[[j-1]] <- tmp
  }

  ss <- view$getSelection()
  ss$setMode(as.integer(3)) # multiple
  view$setRubberBanding(TRUE)

  sw.view$add(view)
  sw.view.va <- sw.view$getVadjustment()
  sw.view.ha <- sw.view$getHadjustment()

  view.rn <- gtkTreeViewNewWithModel(model)
  view.rn$SetFixedHeightMode(TRUE)
  rowname.column.obj <- NewColumn(model, 1, width=10, is.row=T)
  .local$rowname.column.obj <- rowname.column.obj
  gtkTreeViewAppendColumn(view.rn, rowname.column.obj$column)
  ss.rn <- view.rn$getSelection()
  ss.rn$setMode(as.integer(3)) # multiple
  
  sw.rn <- gtkScrolledWindowNew()
  sw.rn$add(view.rn)
  view.rn$setSizeRequest(-1, 10)
  sw.rn$setPolicy(GtkPolicyType['never'], GtkPolicyType['never'])
  sw.rn.va <- sw.rn$getVadjustment()
  
  gtkWidgetModifyBase(view, GtkStateType['selected'],  selectedColor)
  gtkWidgetModifyBase(view, GtkStateType['active'], selectedColor)
  gtkWidgetModifyText(view, GtkStateType['selected'], as.GdkColor("black"))
  gtkWidgetModifyText(view, GtkStateType['active'], as.GdkColor("black"))
  gtkWidgetModifyBase(view.rn, GtkStateType['normal'], bgColor)
  gtkWidgetModifyBase(view.rn, GtkStateType['selected'], selectedColor)
  gtkWidgetModifyBase(view.rn, GtkStateType['active'], selectedColor)
  gtkWidgetModifyText(view.rn, GtkStateType['selected'], as.GdkColor("black"))
  gtkWidgetModifyText(view.rn, GtkStateType['active'], as.GdkColor("black"))

  paned <- gtkHPanedNew()
  paned$add1(sw.rn)
  paned$add2(sw.view)
  paned$setPosition(100)
  
  gtktab <- gtkTableNew(2,2, FALSE)
  gtktab$attach(paned, 0, 1, 0, 1, 5, 5)
  hbar <- gtkHScrollbarNew(sw.view.ha)
  gtktab$attach(hbar, 0, 1, 1, 2, 5, 0)
  
  vbar <- MakeAVerticalScrollbar(gtktab, sw.view.va)
    
  ## we're doing all this after we map
  gSignalConnect(view, "map", after=T, data=.local, function(view, .local){ 
    .local$rowname.column.obj <- MakeButtonAndEventBox(.local$rowname.column.obj, 
      label.str=.local$dataset.name, handler=CornerBoxButtonPress)    
    allColumns <- InitAllColumns(.local$model, allColumns)
  
    gSignalConnect(view,"key-press-event", ViewKeyPress)  
    gSignalConnect(view,"button-press-event", ViewButtonPress)  
    gSignalConnect(view,"button-release-event", after=T, ViewButtonRelease)    
    #gSignalConnect(view,"motion-notify-event", ViewMotionNotify)  
    gSignalConnect(view.rn,"key-press-event", RowNamesKeyPress)      
    gSignalConnect(view.rn,"button-press-event", RowNamesButtonPress)
        
    rows.renderer <- view.rn$getColumns()[[1]]$getCellRenderers()[[1]]
    gSignalConnect(rows.renderer,"edited", function(renderer, path, new.text){        
      entry.frame <- data.frame(row.names = new.text)
      EntryIntoModel(.local, entry.frame, as.integer(path)+1, integer(0))  
      return(FALSE)
    })
    gSignalConnect(view, "expose-event", after=T, ViewExpose)
    
  #  gSignalConnect(view, "scroll-event", after=T, ViewRefresh)
    gSignalConnect(view, "leave-notify-event", AddHScrollTimeout)
    lapply(c("enter-notify-event", "button-release-event", "focus-out-event"), 
      function(evt) gSignalConnect(view, evt, RemoveHScrollTimeout))  
  
    .local$viewGetBinWindow <- view$getBinWindow()		      
    .local$gc.invert <- gdkGCNew(.local$viewGetBinWindow) 
    .local$allColumns <- allColumns
    .local$vbar <- vbar
    .local$view <- view
    .local$view.rn <- view.rn
    .local$ss <- ss
    .local$ss.rn <- ss.rn  
    .local$sw.view.va <- sw.view.va 
    .local$sw.ha <- sw.view.ha    
    toplevel <- .local$group.main$getToplevel()
    if (toplevel$flags() & GtkWidgetFlags["toplevel"]){
      .local$toplevel <- toplevel
    }  
   	view$grabFocus()  	
    return(TRUE)  
   }) # end of map event callback
 
	gSignalConnect(view, "focus-in-event", function(obj, event){
	  if(!.local$disconnected.scrollID) {
		#stop("Other handler still connected")
		return(FALSE)
	  }
	  vbar$unrealize()
	  vbar <- MakeAVerticalScrollbar(gtktab, sw.view.va)
	  gtkTreeSelectionUnselectAll(ss.rn)    
	  .local$scrollID <- 0
	  .local$scrollID <- gSignalConnect(sw.view.va, "value-changed",
		function(obj, data=sw.rn.va){
		  gtkAdjustmentSetValue(data, gtkAdjustmentGetValue(obj))
		})
	  .local$disconnected.scrollID <- FALSE
	  FALSE
	})
	gSignalConnect(view, "focus-out-event", function(obj, event){
	 try({gSignalHandlerDisconnect(sw.view.va, .local$scrollID)
	  .local$disconnected.scrollID <- TRUE
	  }, silent=T)
	  FALSE
	})
	
	## end 	
	gSignalConnect(view.rn, "focus-in-event", function(obj, event){
	  if(!.local$disconnected.scrollID) {
		#stop("Other handler still connected")
		return(FALSE)
	  }
	  vbar$unrealize()
	  #vbar <- gtkVScrollbarNew(sw.rn.va)
	  #gtktab$attach(vbar, 1, 2, 0, 1, 0, 5)
	  vbar <- MakeAVerticalScrollbar(gtktab, sw.rn.va)
	  gtkTreeSelectionUnselectAll(ss)    
	  .local$scrollID <- 0
	  .local$scrollID <- gSignalConnect(sw.rn.va, "value-changed",
		function(obj, data=sw.view.va){
		  gtkAdjustmentSetValue(data, gtkAdjustmentGetValue(obj))
		})
	  .local$disconnected.scrollID <- FALSE
	  FALSE
	})
	gSignalConnect(view.rn, "focus-out-event", function(obj, event){
	 try({gSignalHandlerDisconnect(sw.rn.va, .local$scrollID)
	  .local$disconnected.scrollID <- TRUE
	  }, silent=T)
	  FALSE
	})
	## end 

  return(gtktab)
} # end MakeDFEditWindow

  last.time <- 0
  COLUMN_OFFSET <- 1
  .local$COLUMN_OFFSET <- COLUMN_OFFSET
  gtktab <- NULL

  .local$selectedColumns <- integer(0)
  .local$do.paint <- TRUE
  .local$do.rendererEditingStarted <- TRUE
  .local$doingEntryIntoFactor <- FALSE
  .local$flash.cursor <- FALSE        
  .local$model <- NULL
  .local$allColumns <- NULL
  .local$scrollID <- 0
  .local$disconnected.scrollID <- TRUE
  .local$FIRST_PATH <- NULL
  .local$LAST_PATH <- NULL  
  .local$doingHScroll <- FALSE
  .local$SCROLL_ROW_TIMEOUT <- 150  
  .local$theStack <- list()
  .local$group.main <- gtkVBoxNew()
  .local$FIRST_PATH <- gtkTreePathNewFromString(0)    
  .local$scrollID <- 0
  .local$last.time <- 0  
  .local$disconnected.scrollID <- TRUE  

  ## Finally do the setup
  .local$dataset.name <- dataset.name
  ds <- MakeInternalDataFrame(items)
  .local$LAST_PATH <- gtkTreePathNewFromString(dim(ds)[1]-1)  
  .local$gtktab <- MakeDFEditWindow(ds, dataset.name, size.request) 
  .local$group.main$packStart(.local$gtktab, TRUE, TRUE, 0)
  .local$group.main$setData(".local", .local) # use getData
  class(.local$group.main) <- c("GtkDfEdit", class(.local$group.main))

  return(.local$group.main)
} # get out of Rgtk2DfEdit scope


#' get selected row and column indices
#'
#' @param object The RGtk2DfEdit object
#' @return the 1-indexed selected rows
#' @export
gtkDfEditGetSelection <- function(object){
    l <- object$getData(".local")
	tv <- l$view
	return(list(
	  rows=tryCatch(
        sapply(gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(tv))$retval,
          gtkTreePathGetIndices)+1,
        error=function(e) integer(0)
      ),
	  columns=l$selectedColumns
	  ))
}


## JV ADD METHODS
## Should be able to call these via RGtk2 dispatch: obj$getModel() instead of gtkDfEditGetModel(obj)
#' get Model from object
#'
#' @param object The RGtk2DfEdit object
#' @return the RGtk2DataFrame that is the backend model for the widget
#' @export
gtkDfEditGetModel <- function(object) {
  object$getData(".local")$model
}

#' Return the dimensions (nrow, ncol) of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the number of rows and columns -- not counting row names
#' @export
gtkDfEditGetDimension <- function(object) {
  dim(object$getModel()) - c(0,2)       # rownames
}

#' Return the columns of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the column names for the current object
#' @export
gtkDfEditGetColumnNames <- function(object) {
  model <- object$getModel()
  nms <- colnames(model)[-1]
  return(nms)
}

#' Set the columns of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the column names for the current object
#' @export
gtkDfEditSetColumnName <- function(object, idx, new.name) {
  model <- object$getModel()
  l <- object$getData(".local")
  colnames(model)[idx+l$COLUMN_OFFSET] <- new.name
  l$allColumns[[idx]]$eventbox$getChildren()[[1]]$getChildren()[[1]]$setText(new.name)
  invisible()
}

#' Return the row names of the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the row names for the current object
#' @export
gtkDfEditGetRowNames <- function(object) {
  model <- object$getModel()
  nms <- model[,1, drop=TRUE]
 return(nms)
}

#' Return a data frame from the RGtk2DfEdit object
#'
#' @param object The RGtk2DfEdit object
#' @return Returns the data frame with row names and column names
#' @export
gtkDfEditGetDataFrame <- function(object) {
  #dimnames(model) <- list(object$getRowNames(), object$getColumnNames())
  return(GetDataFrameFromModel(object$getModel()))
}


#' Function to call when column is clicked
#'
#' IF set to NULL, no handler is called.
#' @param object The RGtk2DfEdit object
#' @param handler Function to call when column clicked. Signature is (dataframe, column number). If NULL (default)
#'        no handler is called.
#' @export
gtkDfEditSetColumnClickHandler <- function(object, columnClickedHandler=NULL) {
 ## XXX must set up handler to make this work
  l <- object$getData(".local")
  l$columnClickedHandler <- columnClickedHandler
  object$setData(".local", l)
  invisible()
}


#' S3 data extraction method
#'
#' Grabs data frame then passes onto [.data.frame method
#' @method [ RGtkDfEdit
#' @param x The RGtk2DfEdit object
#' @param i Row index
#' @param j Column indext
#' @param drop passed to extraction for data frame
#' @return The extracted entries
#' @export
"[.GtkDfEdit" <- function(x, i, j, drop = TRUE) {
   if(missing(drop))
     if (missing(i)) {
       drop <- TRUE
     } else {
       drop <- length(x$getDimension()[2]) == 1
     }
   df <- x$getDataFrame()
   df[i,j, drop=drop]
}
