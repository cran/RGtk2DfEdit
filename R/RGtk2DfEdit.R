#' A package for an editing data frames for RGtk2. Improves on base edit.data.frame function found in utils
#' @name RGtk2DfEdit-package
#' @docType package


# Bugs. Large data frames seem to act strangely.
# Column name change has 2px difference in Linux
# Updating to R.envir might get slow
# Crash unpredictably fixed by turning OnMotion off.
# Needs redo support
# Core dump on moving around?
# Deleting columns alters scroll
# Should be able to specify the environment to update dataframe in
# Problems with big frames: Updates are slow
# cairoDevice fails on Mac. Need to have a DO_CAIRO flag.
# Need redo stack
# Space doesn't start editing
# Double-click doesn't open row editor
# Default row and column names are annoying
# Page-down/End runs onto the last rows
# Last row can edit
# Undoing "Default row names" fails if DoUndo has set.levels TRUE
# Needs a SetSize dialog
# Needs Load and Save methods
# Completely revamped insertion and deletion mechanism
# Focus out needs to kill rowname entry
# Paste inserts NA in characters
# Scrolling in rownames gives event queue problems
# Can't get out of corner editor
# Copying also copies last row
################

STACK_LIMIT <- 1E8
VERSION_STRING <- "version 0.5.4"

MAKE_PRETTY <- FALSE
SPRINTF_FORMAT <-  "%.10G"

# for pasting
if(.Platform$OS.type == "windows"){
  NEWLINE_CHAR <- "\r\n"
} else {
  NEWLINE_CHAR <- "\n"
}

DO_CAIRO <- TRUE
if (Sys.info()["sysname"] == "Darwin")
  DO_CAIRO <- FALSE
  
DEFAULT_COLNAMES <-  c(LETTERS, sort(as.vector(outer(LETTERS, LETTERS, FUN=paste, sep=""))))


myDataTypeCoercions <- list(
  character = function(x){
    x <- as.character(x) 
    x[is.na(x)] <- ""   
    x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)  
    x[nchar(x) == 0] <- ""
    attr(x, "repr.class") <- "character"
    return(x)
  }, 
  integer = function(x) {
    x <- as.integer(x)
    attr(x, "repr.class") <- "integer"
    return(x)    
  },
  logical = function(x) {
    x <- as.logical(x)
    attr(x, "repr.class") <- "logical"
    return(x)    
  },
  numeric = function(x) {
    x <- as.numeric(x)
    if(MAKE_PRETTY){
      xx <- sprintf(SPRINTF_FORMAT, x)
      xx[is.na(x)] <- ""
      attr(xx, "repr.class") <- "numeric"          
      return(xx)    
    }
    attr(x, "repr.class") <- "numeric"
    return(x)
  }, 
  factor = function(x){
    x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)  
    x[x=="NA"|nchar(x) == 0] <- NA         
    x <- as.factor(x)
    attr(x, "repr.class") <- "factor"
    return(x)
  }
)

GetClasses <- function(theFrame){
#  print(colnames(theFrame))
  if(MAKE_PRETTY){
    rr <- as.vector(sapply(theFrame, function(xx) {
      r <- attr(xx, "repr.class")
      if(is.null(r)) r <- NA
      return(r)
      }))
    names(rr) <- NULL
    return(rr)
  }
 sapply(theFrame, class)
}
  
SetClasses <- function(theFrame, classes){
  for(jj in 1:ncol(theFrame)) attr(theFrame[,jj], "repr.class") <- classes[jj]
  theFrame
}

# Coerce frame2 to theClasses1 column classes 
CoerceDataTypes <- function(frame2, theClasses1){ 
  theClasses2 <- GetClasses(frame2)
  if(NA%in%theClasses1) stop("Trying to coerce to NA class") 
  stopifnot(length(theClasses1) == length(theClasses2))  
  xx <- theClasses1 != theClasses2  
  xx[is.na(xx)] <- TRUE 
  for(jj in which(xx)){ 
    do.coerce <- myDataTypeCoercions[[theClasses1[jj]]]
    frame2[,jj] <- do.coerce(frame2[,jj])
  } # for jj  
  return(frame2) 
}

ChangeCells <- function(df, nf, row.idx, col.idx){ 

  dmm <- dim(df) 
  if(missing(row.idx)) row.idx <- 1:dmm[1] 
  if(missing(col.idx)) col.idx <- 1:dmm[2] 
  stopifnot(ncol(nf) == length(col.idx) && nrow(nf) == length(row.idx)) 

  oldf = df[row.idx, col.idx, drop=F]
  theClasses <- GetClasses(df) 

  nf <- CoerceDataTypes(nf, theClasses[col.idx])

  idxf <- which(theClasses == "factor")
  idxf <- idxf[idxf%in%col.idx]
  
  if(length(idxf)){
    for(jj in idxf){
      nf.col = which(col.idx==jj)
      xx <- df[[jj]] 
      lvls <- levels(xx) 
      lcl <- class(lvls) 
      to.model <- as(nf[,nf.col], lcl) 
      to.model[!nchar(to.model)] <- NA 
      #if(!to.model%in%lvls||!xx[row.idx]%in%xx[!row.idx]){  
      x <- as.vector(xx) 
      x[row.idx] <- to.model 
      df[,jj] <- myDataTypeCoercions[["factor"]](x)            
      #}     
    } 
    cc <- !col.idx%in%idxf 
    df[row.idx, col.idx[cc] ] <- nf[,cc,drop=F]
  } else {
    df[row.idx, col.idx] <- nf
  }

  return(list(df = df,  
    undo = list( 
      func = "ChangeCells", 
      args = list(nf=oldf, row.idx=row.idx, col.idx=col.idx) 
    ) 
  )) 
}


SetFactorAttributes <- function(df, idx, info){ 
  theCol <- df[[idx]]
  stopifnot(class(theCol) == "factor")
  lvls <- levels(theCol)
  old <- list(levels=lvls)
  if(length(lvls) > 1){
    old$contrasts <- contrasts(theCol)
    old$contrast.name <- attr(theCol, "contrast.name")
  }

  if(!is.null(info$levels))
    theCol <- factor(theCol, levels=info$levels)
  if(!is.null(info$contrasts))
    contrasts(theCol) <- info$contrasts
  if(!is.null(info$contrast.name)) 
    attr(theCol, "contrast.name") <- info$contrast.name
  
  df[[idx]] <- theCol

  return(list(df = df,  
    undo = list( 
      func = "SetFactorAttributes", 
      args = list(idx=idx, info=old) 
    ) 
  )) 
}

CoerceColumns <- function(df, theClasses, idx){
  if(length(theClasses) == 1 && length(idx) > 1) 
    theClasses <- rep(theClasses, length(idx))
  stopifnot(length(idx) == length(theClasses)) 
  stopifnot(max(idx) <= ncol(df))

  old.c <- GetClasses(df)[idx] 
  df[,idx] <- CoerceDataTypes(df[,idx,drop=F], theClasses) 
 
  return(list(df = df,
    undo=list(  
      func = "CoerceColumns", 
      args = list(theClasses = old.c, idx=idx)
    ) 
  )) 
} 

ChangeColumnNames <- function(df, theNames, idx){
  stopifnot(length(idx) == length(theNames)) 
  stopifnot(max(idx) <= ncol(df))

  oldNames <- colnames(df)[idx] 
  colnames(df)[idx] <- theNames 
 
  return(list(df = df,
    undo=list(  
      func = "ChangeColumnNames", 
      args = list(theNames = oldNames, idx=idx)
    ) 
  )) 
} 
 
ChangeRowNames <- function(df, theNames, idx){
  stopifnot(length(idx) == length(theNames))   
  stopifnot(max(idx) <= nrow(df))
  rdf <- rownames(df)
  oldNames <- rdf[idx] 
  theNames <- make.unique(c(rdf[-idx], theNames))[length(rdf)+1:length(idx)-length(idx)] 
  rownames(df)[idx] <- theNames
  df[idx, 1] <- theNames
  return(list(df = df,
    undo=list(  
      func = "ChangeRowNames", 
      args = list(theNames = oldNames, idx=idx)
    ) 
  )) 
} 

# Makes indexes  
InsertIndex <- function(orig, insertions){ 
  all.idx <- 1:orig 
  li <- 1:length(insertions) 
  del.idx <- insertions + li - 1 
  ins.idx <- orig + li 
  for(jj in li) 
    all.idx <- append(all.idx, ins.idx[jj], after=del.idx[jj]-1) 
  return(all.idx) 
} 
# deleting indexed rows and columns 
DeleteRows <- function(df, idx) 
  list(df = df[-idx,,drop=F],  
    undo = list( 
      func = "InsertRows", 
      args = list(
        nf = df[idx,,drop=F],
        idx=idx-1:length(idx)+1)))

InsertRows <- function(df, nf, idx){
  ddf <- dim(df)
  stopifnot(ddf[2] == dim(nf)[2] && dim(nf)[1] == length(idx))
  colnames(nf) <- colnames(df)
  xx <- InsertIndex(ddf[1], idx)  
  list(df = rbind(df, nf)[xx,,drop=F],  
    undo = list( 
      func = "DeleteRows", 
      args = list(
        idx=idx+1:length(idx)-1)))
}

InsertNARows <- function(df, idx){
assign("df", df, envir=.GlobalEnv)
  ddf <- dim(df)
  xx <- InsertIndex(ddf[1], idx)  
  lidx <- length(idx)
  nf <- data.frame(rbind(rep(NA, ddf[2])))[rep(1, lidx),]
  colnames(nf) <- colnames(df)
  rownames(nf) <- make.unique(c(rownames(df), idx+1:lidx-1))[ddf[1]+1:lidx]
  nf[,ddf[2]] <- ""
  nf[,1] <- rownames(nf)
  
  list(df = rbind(df, nf)[xx,,drop=F],  
    undo = list( 
      func = "DeleteRows", 
      args = list(
        idx=idx+1:length(idx)-1)))
}


DeleteColumns <- function(df, idx) 
  list(df = df[,-idx,drop=F],  
    undo = list( 
      func = "InsertColumns", 
      args = list(
        nf = df[,idx,drop=F],
        idx=idx-1:length(idx)+1))) 
        
InsertColumns <- function(df, nf, idx){
  ddf <- dim(df)
  stopifnot(ddf[1] == dim(nf)[1] && dim(nf)[2] == length(idx))
  xx <- InsertIndex(ddf[2], idx) 
  list(df = cbind(df, nf)[,xx,drop=F],  
    undo = list( 
      func = "DeleteColumns", 
      args = list(
        idx=idx+1:length(idx)-1)))
}

InsertNAColumns <- function(df, idx){
  ddf <- dim(df)
  xx <- InsertIndex(ddf[2], idx) 
  lidx <- length(idx)
  nf <- data.frame(X=cbind(rep("", ddf[1])),stringsAsFactors=FALSE)[,rep(1, length(idx)),drop=F]
  colnames(nf) <- make.unique(c(colnames(df), DEFAULT_COLNAMES[idx+1:lidx-2]))[ddf[2]+1:lidx]

  list(df = cbind(df, nf)[,xx,drop=F],  
    undo = list( 
      func = "DeleteColumns", 
      args = list(
        idx=idx+1:length(idx)-1)))
}

# Need to make sure this doesn't overwrite last column 
GetTaskPasteIn <- function(theFrame, dat, insert.row, insert.col, 
  do.rownames = F, do.colnames = F){ 

  dat <- data.frame(dat, stringsAsFactors=F)
         
  theRownames <- NULL 
  theColnames <- NULL 
  if(do.rownames && do.colnames) { # Trim
    theRownames <- dat[,1] 
    theColnames <- dat[1,]
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
   
  dd <- dim(dat) 
  dm <- dim(theFrame)
  if (is.null(insert.row) ) insert.row <- 1
  if (is.null(insert.col)) insert.col <- 1
  ins <- c(insert.row, insert.col)
  
  ins.end <- c(ins[1]+dd[1]-1, ins[2]+dd[2]-1) # end of insertion range  

  tasks <- list()
  
  if(dm[1] < ins[1]+dd[1]){ # Rows
    idx <- (dm[1]+1):(ins[1]+dd[1])
    idx <- rep(dm[1], length(idx))
    tasks[[length(tasks)+1]] <- list(func="InsertNARows", args=list(idx=idx)) 
  }
  if(dm[2] < ins[2]+dd[2]){ # Columns
    idx <- (dm[2]+1):(ins[2]+dd[2])
    idx <- rep(dm[2], length(idx))
    tasks[[length(tasks)+1]] <- list(func="InsertNAColumns", args=list(idx=idx))
  }

  row.idx <- ins[1]:ins.end[1]
  col.idx <- ins[2]:ins.end[2]
  
  if(do.rownames && !is.null(theRownames))
    tasks[[length(tasks)+1]] <- list(func="ChangeRowNames", 
        arg = list(theNames = theRownames, idx=row.idx))

  if(do.colnames && !is.null(theColnames))
    tasks[[length(tasks)+1]] <- list(func="ChangeColumnNames", 
        arg = list(theNames = theColnames, idx=col.idx))  
  
  tasks[[length(tasks)+1]] <- list(func="ChangeCells", 
    args=list(nf=dat, row.idx=row.idx, col.idx=col.idx))
    
  return(tasks)
} 

# Do a task list
DoTask <- function(df, task){
  undo <- list()
  for(taskItem in task){
    arg <- taskItem$arg 
    arg$df <- df 
    rv <- do.call(taskItem$func, arg) 
    df <- rv$df 
    undo[[length(undo)+1]] <- rv$undo
  }
  return(list(df=df, undo=undo))
}
 

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
#' @param col.width (width)
#' @return An object of class GtkDfEdit for which a few RGtk2-style methods are defined
#' @export
dfedit <- function(items, dataset.name = deparse(substitute(items)), size=c(500, 300), col.width = 64){
  obj <- gtkDfEdit(items, dataset.name, size.request=size, col.width = col.width)
  win <- gtkWindowNew()
  win$add(obj)
  win$setTitle(dataset.name)
  invisible(obj)
}
   
# Our handler:
#      isText <- theClass == "factor" || theClass == "character"
#	    renderer <- .local$allColumns[[kk-1]]$renderer
#      renderer.set <- renderer['ellipsize-set']
#      if( isText && !renderer.set) {
#	        renderer['ellipsize'] <- PangoEllipsizeMode['end']
#	        renderer['ellipsize-set'] <- TRUE
#      } else if (!isText && renderer.set) {
#	        renderer['ellipsize'] <- PangoEllipsizeMode['none']
#	        renderer['ellipsize-set'] <- FALSE
#      }

DoUndo <- function(.local){
  if(!length(.local$undoStack)) return(TRUE)
  undo <- .local$undoStack[[length(.local$undoStack)]]
  rv <- DoTask(.local$theFrame, rev(undo))
  .local$UpdateDfEditor(rv$df)
  #print(.local$undoStack)
  .local$undoStack[[length(.local$undoStack)]] <- NULL
}


SelectedRowIndices <- function(tv, .local){
  sr <- integer(0)
  tryCatch({    
    sr <- sapply(gtkTreeSelectionGetSelectedRows(gtkTreeViewGetSelection(tv))$retval,
      gtkTreePathGetIndices)+1
    lsr <- length(sr)
    ddf1 <- dim(.local$theFrame)[1]
    if(sr[lsr] == ddf1){
       sr[lsr] <- ddf1-1
       sr <- unique(sr)
    }
  },
  error=function(e) integer(0))
  return(sr)
}
    
# Make a data frame to put into the model.
# We have an extra row for names and a blank at the end.
MakeInternalDataFrame <- function(dataset){
  if(is.null(dim(dataset))) dataset <- cbind(dataset)
  if(!length(rownames(dataset)) && dim(dataset)[1])
      rownames(dataset) <- 1:dim(dataset)[1]
  if(!length(colnames(dataset)) && dim(dataset)[2])
      colnames(dataset) <- DEFAULT_COLNAMES[1:dim(dataset)[2]]
  theFrame <- data.frame(rows = row.names(dataset), dataset, 
     " " = vector("character", nrow(dataset)), 
     check.names = FALSE, stringsAsFactors = FALSE)
  
  blankRow <- data.frame(rows=" ", rbind(rep(NA, dim(dataset)[2])), " " = "", row.names = " ", stringsAsFactors=F)
  theClasses <- GetClasses(theFrame) 
  blankRow <- CoerceDataTypes(blankRow, GetClasses(theFrame))
  for(jj in which(theClasses == "factor"))  
    blankRow <- SetFactorAttributes(df=blankRow, idx=jj, info=list(levels=levels(theFrame[,jj])))$df
  
  names(blankRow) <- names(theFrame)

  theFrame <- rbind(theFrame, blankRow)

  # Pretty printing option
  if(MAKE_PRETTY){
    theClasses <- sapply(theFrame, class)  
    for(jj in 1:ncol(theFrame))
      theFrame[,jj] <- myDataTypeCoercions[[theClasses[jj]]](theFrame[,jj])
  }
  theFrame
}

# Make a data frame to extract from the model.
MakeExternalDataFrame <- function(theFrame){
  theFrame <- theFrame[-dim(theFrame)[1],-c(1, dim(theFrame)[2]), drop=F]
  if(MAKE_PRETTY)
    for(jj in 1:ncol(theFrame)) {
      theFrame[,jj] <- as(theFrame[,jj], attr(theFrame[,jj], "repr.class"))
    }
  theFrame
}

MakeLastPath <- function(df){
    if(dim(df)[1] == 1) return(gtkTreePathNewFromString("0"))
    return(gtkTreePathNewFromString(as.character(dim(df)[1]-2)))
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
  read.function <- function(p) readLines(p, warn=FALSE)
  
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

BlockSizeHandler <- function(the.column, data){
  .local <- data$.local
  row.idx = data$row.idx
  col.idx = data$col.idx
  entry.frame <- data.frame(X=the.column)
  task <- list(
   list(func="ChangeCells", 
    arg = list(nf=entry.frame, row.idx=row.idx, col.idx=col.idx))
   )
  DoTaskWrapper(.local, task)
}

 # start.lvl = level name to start cycling at
DoBlockSize <- function(the.column, loc.window, handler, data, start.lvl=NULL){

  .BlockEnv <- new.env()
  .BlockEnv$the.column <- the.column  
  UpdateColumn <- function(){
    block.size <- spinner$getValue()
    total.len <- length(.BlockEnv$the.column)
    .BlockEnv$the.column <-  gl(length(lvls), block.size, total.len, labels=lvls)
    #handler(the.column, data)
  }

  if(!is.factor(the.column)) stop("Can't block over non-factors")  
  lvls <- unique(levels(the.column))
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
    if(evt[["keyval"]] == GDK_Return) {
      handler(.BlockEnv$the.column, data) 
      window2$destroy()
    }
    if(evt[["keyval"]] == GDK_Escape) {
      window2$destroy()
    }    
    FALSE
  })
  gSignalConnect(button, "clicked", function(...){
    handler(.BlockEnv$the.column, data)  
    window2$destroy()
  })
  
  box2$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("   Cancel   ")
  gSignalConnect(button, "clicked", function(...){
    # Update frame  
    window2$destroy()
    })
  box2$packEnd(button, FALSE, FALSE, 5)
  window2$show()
  UpdateColumn()
}

FactorEditorHandler <- function(the.column, col.idx, data){
  .local <- data
  #print(.FactorEnv$the.column)
  entry.frame <- data.frame(X=the.column)    
  task <- list(list(func="ChangeCells", 
     arg = list(nf=entry.frame, col.idx=col.idx)),
    list(func="SetFactorAttributes", 
    arg = list(idx=col.idx, info=list(levels=levels(the.column), 
     contrasts=contrasts(the.column), contrast.name=attr(the.column, "contrast.name"))))         
     )
  DoTaskWrapper(.local, task)
}

DoFactorEditor <- function(theFrame, toplevel, col.idx=integer(0), 
  handler=NULL, data=NULL){

  .FactorEnv <- new.env()
  .FactorEnv$col.idx <- col.idx

  UpdateView <- function(){
    contr <- contrast.coding[[which(sapply(rev(grb$getGroup()), gtkToggleButtonGetActive))]]
    if(length(levels(.FactorEnv$the.column)) > 1) {
      contrasts(.FactorEnv$the.column) <- contr
      attr(.FactorEnv$the.column, "contrast.name") <- contr
    }
    handler(.FactorEnv$the.column, .FactorEnv$col.idx , data)
  }

  cell.edited <- function(cell, path.string, new.text, data){
    xx <- .FactorEnv$xx      
    if(!nchar(new.text))# || new.text%in%xx) 
      stop("New name must exist")# and be unique")
    checkPtrType(data, "GtkListStore")
    model <- data
    path <- gtkTreePathNewFromString(path.string)
    iter <- model$getIter(path)$iter

    i <- path$getIndices()[[1]]+1

      # editing the level names
    zz <- theFrame[,.FactorEnv$col.idx]
    lzz <- levels(zz)    
    ww <- which(xx[i]==lzz)
    
    if(length(ww)){
      lzz[ww] <- new.text
      levels(zz) <- lzz
      .FactorEnv$the.column <- zz
      UpdateView()            
    }
    xx[i] <- new.text
    model$set(iter, 0, new.text)
    .FactorEnv$xx <- xx
    UpdateLabel()    
  }
    
  add.item <- function(button, data) {
    xx <- .FactorEnv$xx      
     for(k in 1:(length(xx)[1]+1)){
       nl <- paste("Level", k, sep="_")
       if(!nl%in%xx) break
     }
     xx <- c(xx, nl)
     .FactorEnv$xx <- xx     
     iter <- model$append()$iter
     model$set(iter, 0, xx[length(xx)])
            
    .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
     UpdateView()
     UpdateLabel()
   }


  remove.item <- function(widget, data)
  {
     xx <- .FactorEnv$xx   
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
        .FactorEnv$xx <- xx           
        path$prev()
        selection$selectPath(path)        
        .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
        UpdateView()            
        UpdateLabel()        
      }
  }

  move.item.up <- function(widget, data)
  {
     xx <- .FactorEnv$xx
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
         .FactorEnv$xx <- xx
        .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
         UpdateView()
         UpdateLabel()                 
       }
  }

  move.item.down <- function(widget, data)
  {
     xx <- .FactorEnv$xx
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
         .FactorEnv$xx <- xx         
        .FactorEnv$the.column <- factor(.FactorEnv$the.column, levels=unique(xx))
         UpdateView()                
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
    xx <- .FactorEnv$xx  
    lab1$setMarkup(paste("Control (first level) is: <b>", xx[1], "</b>", sep=""))
  }  

  # called with no selection
  data.factors <- sapply(theFrame[,], is.factor)
  if(!length(colnames(theFrame)[data.factors])) stop("No columns are factors")
  if(!length(col.idx)) col.idx=which(data.factors)[1]
  .FactorEnv$col.idx <- col.idx
    
  window <- gtkWindowNew(show=F)
  if(!is.null(toplevel)) {
    window$setTransientFor(toplevel)  
    window$setModal(TRUE)
  }  
  window$setTitle("Factor Editor")
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)

  fr0 <- gtkFrameNew(label="Column Selection")

  cb1 <- gtkComboBoxNewText()  
  cb1$show()
  cb1["width-request"] <- 75
  
  for (item in colnames(theFrame)[data.factors]) # omit "rows" heading
    cb1$appendText(item)  

  theIdx <- which(names(data.factors[data.factors==1]) == names(data.factors[col.idx]))-1
  cb1$setActive(theIdx)
  fr0$add(cb1)  
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
                                            
  MakeModel <- function(){
    col.original <- .FactorEnv$col.original
    if(!is.factor(col.original)) {
      col.original <- as.factor(integer(0))
      #stop("Can't edit non-factors")
      print("Can't edit non-factors")    
      }
    .FactorEnv$xx <- na.omit(cbind(levels(col.original)))
    model <- gtkListStoreNew("gchararray")
    xx <- .FactorEnv$xx
    sapply(xx, function(x) model$set(model$append()$iter, 0, x))
    UpdateLabel()  
    return(model)
  }
    
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setPolicy("automatic", "automatic")
  box1.5$packStart(sw, TRUE, TRUE, 0)
  treeview <- gtkTreeViewNew()  
  
  .FactorEnv$the.column <- theFrame[,.FactorEnv$col.idx]
  .FactorEnv$col.original <- .FactorEnv$the.column
  model <- MakeModel()  
  treeview$setModel(model)
  
  gSignalConnect(cb1, "changed", function(widget){
      new.idx <- which(widget$getActiveText()==colnames(theFrame))
      .FactorEnv$col.idx <- new.idx
      .FactorEnv$the.column <- theFrame[,new.idx]
      .FactorEnv$col.original <- .FactorEnv$the.column
      model <- MakeModel()
      treeview$setModel(model)
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

  expander <- gtkExpanderNew(label="Factor Contrasts (For Experts)")
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

  contr <- attr(.FactorEnv$the.column, "contrast.name")
    
  grb <- gtkRadioButtonNewWithLabel(NULL, label=names(contrast.coding)[1])
  grb$setActive(TRUE)
  box4$packStart(grb, FALSE, FALSE, 0)
  for(ii in 2:length(names(contrast.coding))){
    grb <- gtkRadioButtonNewWithLabel(group=grb$getGroup(), 
      label=names(contrast.coding)[ii])
    if(!is.null(contr) && contr==unlist(contrast.coding)[ii]) grb$setActive(TRUE)
    box4$packStart(grb, FALSE, FALSE, 0)
  }

  if(0){
  fr3 <- gtkFrameNew(label="Fill Column")
  box0$packStart(fr3, FALSE, FALSE, 5)
  box5 <- gtkHBoxNew(TRUE, 5)
  fr3$add(box5)
  button <- gtkButtonNewWithLabel("  Fill With Replicates...  ")
  gSignalConnect(button, "clicked", function(...){
  	    .local <- data
         DoBlockSize(
          .FactorEnv$the.column,
          window,
          BlockSizeHandler,
          data =  list(.local=.local, row.idx=1:length(.FactorEnv$the.column), col.idx=col.idx))
  })
  box5$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("  Random Fill  ")
  box5$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("  Selected  ")
  box5$packEnd(button, FALSE, FALSE, 0)
  }

  box6 <- gtkHBoxNew(FALSE, 5)
  box0$packEnd(box6, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("    OK    ")
  gSignalConnect(button, "clicked", function(...){    	
    UpdateView()
    window$destroy()
  })

  box6$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Cancel")
  gSignalConnect(button, "clicked", function(...) {
   .FactorEnv$the.column <- .FactorEnv$col.original
    UpdateView()
    window$destroy()
    })

  box6$packEnd(button, FALSE, FALSE, 5)
  UpdateLabel()
  window$show()
}

###############################################################################
# End factor editor
###############################################################################

DoPasteDialog <- function(.localenv){

  window <- gtkWindowNew(show=F) 
  if(!is.null(.localenv$toplevel)) {
    window$setTransientFor(.localenv$toplevel)  
    window$setModal(TRUE)
  }  
  window$setTitle("Paste In Data")
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)
  window$setResizable(FALSE)
  window$resize(window$getSize()$width, 1)

  rb = rev(c("No Row Names Or Column Names", "Row Names Only", "Column Names Only", "Row Names And Column Names"))
  fr1 <- gtkFrameNew(label="Name Options")
  xx1 <- MakeRadiobuttonGroup(rb)
  fr1$add(xx1$box)
  box0$add(fr1)

  fr2 <- gtkVBoxNew()
  box18000 <- gtkHBoxNew(FALSE, 5)
  fr2$add(box18000)
  button <- gtkButtonNewWithLabel(" Cancel ")
  gSignalConnect(button, "clicked", function(...){
    window$destroy()
  })
  box18000$packEnd(button, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("   OK   ")
  box18000$packEnd(button, FALSE, FALSE, 0)
  gSignalConnect(button, "clicked", function(...){
    ww <- which(rev(sapply(xx1$grb$getGroup(), gtkToggleButtonGetActive)))
    do.colnames <- F; do.rownames <- F
    if(ww == 1 || ww == 2) do.colnames <- T
    if(ww == 1 || ww == 3) do.rownames <- T
    dat <- ReadFromClipboard()
    task <- GetTaskPasteIn(.localenv$theFrame, dat, 
      1, 2, do.rownames=do.rownames, do.colnames=do.colnames)
    DoTaskWrapper(.localenv, task)
    window$destroy()    
  })
  box0$packStart(fr2, FALSE, FALSE, 0)  
  window$show()
}


###############################################################################
# Sort dialog
###############################################################################

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
  
# Returns the ordering of the table
DoSortDialog <- function(theFrame, handler, .localenv){
  window <- gtkWindowNew(show=F) 
  if(!is.null(.localenv$toplevel)) {
    window$setTransientFor(.localenv$toplevel)  
    window$setModal(TRUE)
  }
  window$setTitle("Sort Options")
  window$setBorderWidth(5)
  box0 <- gtkVBoxNew(FALSE, 5)
  window$add(box0)
  items <- colnames(theFrame)
  fr0 <- gtkFrameNew(label="Sort Key Selection")
  box1 <- gtkVBoxNew(FALSE, 5)
  box0$add(fr0)
  fr0$add(box1)

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
		  xx <- theFrame[[item$col]]
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

  DoTaskWrapper <- function(.local, task){
    rv <- DoTask(.local$theFrame, task)
    .local$UpdateDfEditor(rv$df)
    .local$undoStack[[length(.local$undoStack)+1]] <- rv$undo         
    
    if(object.size(.local$undoStack) > STACK_LIMIT){
      warning("Stack full")    
      jj <- 0
      while(object.size(.local$undoStack) > STACK_LIMIT && length(.local$undoStack))
        .local$undoStack[[jj <- jj + 1]] <- NULL
      if(object.size(.local$undoStack) > STACK_LIMIT){ 
        warning("This edit is too large to support undo")
        .local$undoStack <- list()
      }
    }
  }


gtkDfEdit <- function(items, dataset.name = deparse(substitute(items)), size.request=c(500, 300), col.width = 64){
   # our local environment
    .local <- new.env()    
    
    last.time <- 0
    COLUMN_OFFSET <- 1
    .local$COLUMN_OFFSET <- COLUMN_OFFSET
    gtktab <- NULL
  
    .local$selectedColumns <- integer(0)
    .local$col.width <- col.width
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
  
    val <- sw.ha.value + direction*sw.ha[["step_increment"]]
    if(0 <= val && val <= sw.ha[["upper"]] - sw.ha[["page_size"]]) {
      sw.ha$setValue(val)
    } else if (val < 0) {
      sw.ha$setValue(0)
    } else {
      sw.ha$setValue(sw.ha[["upper"]] - sw.ha[["page_size"]])
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

    .local$do.paint.timeout <- gTimeoutAdd(100, function(){
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
    row.idx <- as.integer(gtkTreePathGetIndices(path))+1

    new.idx <- GetColIdx(cursor.info$focus.column)
    ori.idx <- new.idx

    if (direction == "right" && new.idx < length(allColumns)-1)
      new.idx <- new.idx+1
    else if (direction == "left" && 1 < new.idx)
      new.idx <- new.idx-1
    else if (direction == "down" && row.idx < dim(.local$theFrame)[1])
      gtkTreePathNext(path)
    else if (direction == "up")
      gtkTreePathPrev(path)
    else {
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
        .local$start.key.column.select <- NULL
         selectedColumns.new <- col.idx
         UpdateColumnSelection(allColumns, selectedColumns.new)
      } else if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
        if(.local$do.paint){
          .local$do.paint <- FALSE
          .local$viewGetBinWindow$invalidateRect(NULL, FALSE)    
        }
          # this had better exist!
        selectedColumns.new <- .local$start.key.column.select:col.idx
        UpdateColumnSelection(allColumns, selectedColumns.new)
        PaintSelectionOnTimeout()
      } else {
        .local$start.key.column.select <- NULL
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
  
  RowNamesClearContents <- function(row.idx){
    if(length(row.idx)==0) return(FALSE)
    nf <- .local$theFrame[row.idx,, drop=F]
    nf[,-1] <- ""
    task <- list(list(func="ChangeCells", 
      arg = list(nf=nf, row.idx=row.idx)))
    DoTaskWrapper(.local, task)
  }
  
  RowNamesKeyPress <- function(widget, event, ...) {
    keyval <- event[["keyval"]] 
    stat <- as.flag(event[["state"]])    
    if (CtrlLetter(keyval, stat, GDK_z)){
      #print("Ctrl-z")
      DoUndo(.local)
      return(TRUE)
    }
    if(keyval == GDK_Delete) {
      row.idx <- SelectedRowIndices(.local$view.rn, .local)
      RowNamesClearContents(row.idx)
      return(TRUE)
    }

      # ignore last row
    if(!keyval%in%myValidNavigationKeys){
      cursor.info <- gtkTreeViewGetCursor(widget)
      col.idx <- GetColIdx(cursor.info$focus.column)
      row.idx <- as.integer(gtkTreePathGetIndices(cursor.info$path))+1
      if(row.idx > dim(.local$theFrame)[1]-1) return(TRUE)
    }
    return(FALSE)
  }    
  .local$allow.key.flag <- TRUE
  
  #MoveCursor(widget, direction, stat, allColumns, flash.cursor){  
  ViewKeyPress <- function(widget,event,...) {
    keyval <- event[["keyval"]]
    stat <- as.flag(event[["state"]])   
    event.time <- event[["time"]]    
      # Paging events
    if(keyval%in%c(GDK_Page_Up, GDK_Page_Down, GDK_Home, GDK_End)) {
        # Update the paint rectangle, and set a timeout if we're select-paging
      if(stat==0)
        .local$do.paint <- FALSE
      else 
        PaintSelectionOnTimeout()
      if(.local$do.paint){
        .local$do.paint <- FALSE
        .local$viewGetBinWindow$invalidateRect(NULL, FALSE)
      }      
      if( .local$allow.key.flag && event.time > .local$last.time ){
        if( !(gtkRangeGetValue(.local$vbar) == 0) && # these are conditions under which we don't scroll
            !(gtkRangeGetValue(.local$vbar) == .local$va[["upper"]] - .local$va[["page_size"]]))
          .local$allow.key.flag <- FALSE
        
        .local$last.time <- event.time

        if(stat==0 && length(GetSelectedColumns()) > 1){
					cursor.info <- gtkTreeViewGetCursor(widget)
					new.idx <- GetColIdx(cursor.info$focus.column)
          UpdateColumnSelection(.local$allColumns, new.idx)          
        }
        return(FALSE)
      } else {     # Blocked
        return(TRUE)
      }
    }
    allColumns <- .local$allColumns
    view <- .local$view
    model <- .local$model
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
      CopyToClipboard(.local$theFrame[SelectedRowIndices(.local$view, .local), GetSelectedColumns()+.local$COLUMN_OFFSET, drop=F])
      return(TRUE)
    }
    if (CtrlLetter(keyval, stat, GDK_v)){
      #print("Ctrl-v")
      dat <- ReadFromClipboard() # character vector
      row.idx <- SelectedRowIndices(.local$view, .local)
      col.idx <- min(GetSelectedColumns())
      if(length(row.idx) && length(col.idx)){
        row.idx <- row.idx[1]
        task <- GetTaskPasteIn(.local$theFrame, dat, row.idx, col.idx+.local$COLUMN_OFFSET)
        DoTaskWrapper(.local, task)
      }
      return(TRUE)
    }
    if (CtrlLetter(keyval, stat, GDK_z)){
      #print("Ctrl-z")
      DoUndo(.local)
      return(TRUE)
    }
    if (CtrlLetter(keyval, stat, GDK_a)){
      #print("Ctrl-a")
      SelectAll(allColumns)
      return(TRUE)
    }
    # this makes us drop out of selection mode...
    if(keyval == GDK_Delete) {
      row.idx <- SelectedRowIndices(.local$view, .local)
      col.idx <- GetSelectedColumns() + COLUMN_OFFSET      
      if(length(row.idx)==0) { # delete a column
        ColumnClearContents(col.idx)  
      } else {
        nf <- .local$theFrame[row.idx, col.idx, drop=F]      
        stopifnot(ncol(nf) > 0)  
        nf[,] <- ""
        task <- list(list(func="ChangeCells", 
          arg = list(nf=nf, row.idx=row.idx, col.idx=col.idx)))
        DoTaskWrapper(.local, task)
      }
      return(TRUE)
    }
      # don't leave select mode for control-letters
    if(keyval%in%myMetaKeys ||
      (stat == GdkModifierType['control-mask'])){
      if(keyval%in%myShiftKeys)
        .local$start.key.column.select <- GetColIdx(widget$getCursor()$focus.column)
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

      # Cell editing start from keyboard
    if(.local$allow.key.flag && event.time > .local$last.time){
      cursor.info <- gtkTreeViewGetCursor(widget)
      col.idx <- GetColIdx(cursor.info$focus.column)
      renderer <- allColumns[[col.idx]]$renderer        
      path <- cursor.info$path
      row.idx <- as.integer(gtkTreePathGetIndices(path))+1
        # ignore last row
      if(row.idx > dim(.local$theFrame)[1]-1) return(TRUE)
      
      tryCatch({        
        .local$allow.key.flag <- FALSE                        
        gtkTreeViewSetCursorOnCell(widget, cursor.info$path,
          cursor.info$focus.column, renderer, TRUE)              
        gtkPropagateEvent(view, event)
        }, 
        error = function(e){
          warning(e)
          .local$allow.key.flag <- TRUE
      })
      .local$last.time <- event.time
      return(FALSE)
    } else { # either key flag is false or time is out of order
      return(TRUE)
    }
    return(TRUE)
  }

  RendererEditingStarted <- function(renderer, entry, path, data) {
    checkPtrType( entry, "GtkEntry")
    if(!.local$do.rendererEditingStarted) return(FALSE)
        
    theFrame <- .local$theFrame
    view <- .local$view
      # don't paint rectangles
    .local$do.paint <- FALSE
		  
    gObjectSetData(entry, "index", data)
    gObjectSetData(entry, "renderer", renderer)
    gObjectSetData(entry, "path.str", path)
    gObjectSetData(entry, "path", gtkTreePathNewFromString(path))
    gObjectSetData(entry, "text", gtkEntryGetText(entry))    
    .local$entry <- entry
        
      # We've trapped the temporary GtkEntry the renderer creates.
    gSignalConnect(entry, "key-press-event", RendererEntryKeyPress)
      # you can leave the entry in 2 ways, focusing out or pressing a key from within
    gSignalConnect(entry, "focus-out-event", after=T, function(entry, event){
      if(gObjectGetData(entry, "text") != gtkEntryGetText(entry)){
        tryCatch({
      	  col.idx <- entry$getData("index")  
      	  row.idx <- as.integer(entry$getData("path.str"))+1      
          keyval <- entry$getData("keyval")
          theText <- entry$getText()          
          if(substr(theText, 1, 1) == "="){
            theCmd <- sub("^=(.*?)$", "\\1", theText, perl=TRUE)  
            eval(parse(text=theCmd), envir=.GlobalEnv)
          }          
          nf <- data.frame(theText, stringsAsFactors=F)
          task <- list(list(func="ChangeCells", 
            arg = list(nf=nf, row.idx=row.idx, col.idx=col.idx)))
          DoTaskWrapper(.local, task)
        }, error = function(e) print(e) )
        if(.local$doingEntryIntoCompletion){ #we're entering data into a factor 
            if (keyval == GDK_Return)
               MoveCursor(view, "down")
        }        
      }
      .local$allow.key.flag <- TRUE # unlock
      .local$entry <- NULL         
      return(FALSE)
    }) # focus out event
    
   col.idx <- data
   .local$doingEntryIntoCompletion <- FALSE  
   typ <- GetClasses(.local$theFrame)[col.idx]
   if(typ == "factor"){
     create.completion.model <- function(factor.levels) {
       store <- gtkListStoreNew("gchararray")
       sapply(factor.levels, function(x) store$set(store$append()$iter, 0, x))
       return(store)
     }
     factor.levels <- levels(theFrame[,col.idx])
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
       MoveCursor(.local$view, "down")
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
    if(.local$doingEntryIntoCompletion) {
      if(stat == as.integer(0) && keyval%in%c(GDK_Up, GDK_Down, GDK_Return))
        return(FALSE)
      if(keyval == GDK_Escape){
        entry$setText(entry$getData("text"))
        return(MoveCursor(view, "down"))        
      }
    }
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
      ll <- length(allColumns)
      if(ll > 1){ # for zero columns, don't select anything
        selectedColumns.new[selectedColumns.new == ll] <- ll-1  # end column
        selectedColumns.new <- unique(selectedColumns.new)
        for (ii in setdiff(selectedColumns.new, selectedColumns))
          ColumnSetState(allColumns, ii, TRUE)
        for (ii in setdiff(selectedColumns, selectedColumns.new))
          ColumnSetState(allColumns, ii, FALSE)
      }
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
     # kill any open entries
    #if(!is.null(.local$entry))
      #.local$entry$unrealize()  

    typ <- event[['type']]
    stat <- event[['state']]
    info <- widget$getPathAtPos(event[["x"]], event[["y"]])
    if(is.null(info$path)) return(TRUE)
         

    row.idx <- info$path$getIndices()+1
    if(row.idx > dim(.local$theFrame)[1]-1) return(TRUE)
              
    UpdateColumnSelection(.local$allColumns, integer(0))
    if(length(.local$rectangles)){
      .local$rectangles <- list()
      .local$viewGetBinWindow$invalidateRect(NULL, FALSE)             
    }
    if (event[["button"]] == as.integer(1)){ 
     if(typ == as.integer(4)){ # single clicked     
        handler <- .local$rowClickHandler
        if(!is.null(handler) && is.function(handler)) {
          row.idx <- info$path$getIndices()+1
          df <- MakeExternalDataFrame(.local$theFrame)
          handler(df, row.idx)
        }
     } else if(typ == as.integer(5)){ # ignore double click
       return(TRUE)
     }      
    } else if (event[["button"]] == as.integer(3)){ # our popup menu
      m <- Row3rdButtonMenu(.local$theFrame, row.idx)
      gtkMenuPopupHack(m, button = event$GetButton(), 
        activate.time = gdkEventGetTime(event))
      return(TRUE)
    }    
    return(FALSE)
  }

  ViewButtonPress <- function(widget, event, data) {
    model <- .local$model
    allColumns <- .local$allColumns
    info <- widget$getPathAtPos(event[["x"]], event[["y"]])
    if(is.null(info$path)) return(TRUE)
    row.idx <- info$path$getIndices()+1
    col.idx <- GetColIdx(info$column)
    typ <- event[['type']]
    stat <- event[['state']]    

    if (event[["button"]] == as.integer(3)){ # our popup menu
	  	m <- Cell3rdButtonMenu(model, row.idx, col.idx)
      gtkMenuPopupHack(m, button = event$GetButton(),
        activate.time = gdkEventGetTime(event))
      return(TRUE)
    } else if (event[["button"]] == as.integer(1)){    
      if(typ == as.integer(4)){ # single clicked      
        if(is.null(info$path)) return(FALSE)    
        selectedColumns <- GetSelectedColumns()
        if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
          if(is.null(.local$start.column.select)) .local$start.column.select <- col.idx
          selectedColumns <- .local$start.column.select:col.idx          
        } else {      
             selectedColumns <- col.idx
            .local$start.column.select <- col.idx
        }
        UpdateColumnSelection(allColumns, selectedColumns) 
      }  else if (typ == as.integer(5)) { # ignore dclick
        return(TRUE)
      }
     }
    
    .local$flash.cursor <- TRUE
      #  Flash the rectangles off when you click
    .local$do.paint <- TRUE        # 2-12-10
    if(length(.local$rectangles)){
      .local$rectangles <- list()
  	  widget$getBinWindow()$invalidateRect(NULL, FALSE)
    }

    if(row.idx > dim(.local$theFrame)[1]-1) return(TRUE)
    return(FALSE)
  }
  
    # Sync column selection with mouse-down
  ViewMotionNotify <- function(widget, event){
    while(gtkEventsPending())
      gtkMainIteration()  
    allColumns <- .local$allColumns
    pointer <- gdkWindowGetPointer(event[["window"]])
    if (as.flag(pointer$mask) & GdkModifierType["button1-mask"]){
      info <- gtkTreeViewGetPathAtPos(widget, pointer[["x"]], pointer[["y"]])
      if (info$retval){
        col.idx <- GetColIdx(info$column)
        if(is.null(.local$start.select.column)) .local$start.select.column <- col.idx
  		  new.sel <- sort(.local$start.select.column:col.idx)
  		  UpdateColumnSelection(allColumns, new.sel)
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
    
    if(!is.null(.local$start.column.select) && length(col.idx)) {
      selectedColumns <- .local$start.column.select:col.idx
    } else if (length(col.idx)) {
      selectedColumns <- col.idx
    }# otherwise col.idx is NA 
   UpdateColumnSelection(allColumns, selectedColumns)
   .local$rectangles <- list() # reset our rectangle-drawing
   UpdateSelectionRectangle()
  }

  # Don't draw if only 1 selected square
  UpdateSelectionRectangle <- function(){
    widget <- .local$view
    allColumns <- .local$allColumns
    sw.va <- .local$sw.view.va

    gsr <- widget$getSelection()$getSelectedRows()$retval
  	selectedColumns <- GetSelectedColumns() 
    if(length(gsr)){
    	path1 <- gsr[[1]]
    	path2 <- gsr[[length(gsr)]]
    	path1.index <- path1$GetIndices()+1
    	path2.index <- path2$GetIndices()+1

      if(path2.index == dim(.local$theFrame)[1]) path2$prev()
    	
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
	  typ <-  GetClasses(.local$theFrame)[col.idx+COLUMN_OFFSET]	  
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
      function(...) CopyToClipboard(model[SelectedRowIndices(.local$view, .local), 
      GetSelectedColumns()+.local$COLUMN_OFFSET, drop=F]))
	  gSignalConnect(copyWithNamesItem, "activate", 
      function(...) CopyToClipboard(model[SelectedRowIndices(.local$view, .local), 
        GetSelectedColumns()+.local$COLUMN_OFFSET, drop=F], do.rownames=T, do.colnames=T))      
	  gSignalConnect(pasteItem, "activate", function(...) {
	  
    dat <- ReadFromClipboard() # character vector
    task <- GetTaskPasteIn(.local$theFrame, dat, row.idx, col.idx+.local$COLUMN_OFFSET)
    DoTaskWrapper(.local, task)
      
  })      
	  
	  m$append(gtkSeparatorMenuItem())
	
    model <- .local$model
    view <- .local$view	
    theFrame <- .local$theFrame
	  editFactorsItem <- gtkMenuItem("Edit Factors...")
	  randomizeItem <- gtkMenuItem("Randomize Selected")
	  fillItem <- gtkMenuItem("Fill Selected Down")	  
	  fillCyclicItem <- gtkMenuItem("Fill In Blocks")
      m$append(editFactorsItem)
	  m$append(randomizeItem)
	  m$append(fillItem)
	  m$append(fillCyclicItem)
	  if(typ != "factor")
	    lapply(c(editFactorsItem, fillCyclicItem), gtkWidgetSetSensitive, FALSE)
	  gSignalConnect(editFactorsItem, "activate", function(...) {
     # DoFactorEditor(theFrame, .local, col.idx + COLUMN_OFFSET))
      DoFactorEditor(theFrame, .local$toplevel, col.idx + COLUMN_OFFSET, 
       FactorEditorHandler, data=.local)
    })
	  gSignalConnect(randomizeItem, "activate", function(...){
         sr <- SelectedRowIndices(.local$view, .local)                            
         if(length(sr)){
           entry.frame <- .local$model[sr, GetSelectedColumns()+1, drop=F]
           entry.frame <- entry.frame[sample(1:(dim(entry.frame)[1])),,drop=F]
           task <- list(list(func="ChangeCells", 
              arg = list(nf=entry.frame, row.idx=sr, col.idx=GetSelectedColumns()+1)))
           DoTaskWrapper(.local, task)
         }
       })  
	  gSignalConnect(fillItem, "activate", function(...){
         sr <- SelectedRowIndices(.local$view, .local)
         sc <- GetSelectedColumns()+1         
         if(length(sr) && length(sc)){
           entry.frame <- .local$model[sr, sc, drop=F]
           for(jj in 1:length(sc))
             entry.frame[,jj] <- entry.frame[1,jj]
           task <- list(list(func="ChangeCells", 
              arg = list(nf=entry.frame, row.idx=sr, col.idx=GetSelectedColumns()+1)))
           DoTaskWrapper(.local, task)
         }
       })  
	  gSignalConnect(fillCyclicItem, "activate", function(...) {
         sr <- SelectedRowIndices(.local$view, .local)
         cc <- col.idx+COLUMN_OFFSET         
         df1 <- dim(.local$theFrame)[1]
         if(length(sr) < 2 && df1 > 1) sr <- 1:(df1-1)
                  
         if(length(sr)) {
           DoBlockSize(
            theFrame[sr, cc],
            .local$toplevel,
            BlockSizeHandler,
            data =  list(.local=.local, row.idx=sr, col.idx=cc), 
            start.lvl=theFrame[sr[1], cc])
           }
       })
    return(m)
  }
  
  # Function to call when the data is sorted
  SortHandler <- function(new.order, .local){
    dd <- dim(.local$theFrame)
    if(dd[1] > 1){
      theFrame <- .local$theFrame[new.order, ,drop=F]
      task <- list(
        list(func="ChangeRowNames", 
          arg = list(theNames = rownames(theFrame), idx=1:(dd[1]-1))),
        list(func="ChangeCells", 
          arg = list(nf=theFrame, row.idx=1:(dd[1]-1))))
      DoTaskWrapper(.local, task)
    }
  }

	Corner3rdButtonMenu <- function(model){	
	  theFrame <- .local$theFrame
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  pasteItem <- gtkMenuItem("Paste...")
	  m$append(cutItem)
	  m$append(copyItem)	  
	  m$append(pasteItem)
	  m$append(gtkSeparatorMenuItem())
	  gSignalConnect(copyItem, "activate", function(...) 
      CopyToClipboard(theFrame[,,drop=F], do.rownames=T, do.colnames=T))
	  gSignalConnect(pasteItem, "activate", function(...){
      DoPasteDialog(.local)
    })      
	  lapply(c(cutItem), 
      gtkWidgetSetSensitive, FALSE)
      	  
	  sortItem <- gtkMenuItem("Sort...")	  
	  gSignalConnect(sortItem, "activate", function(...){
	    dd <- dim(.local$theFrame)
      DoSortDialog(.local$theFrame[-dd[1], -dd[2],drop=F], SortHandler, .local)
    })
	  m$append(sortItem)
	  m$append(gtkSeparatorMenuItem())
    # move dataset 1 column along
 	  ordinalItem <- gtkMenuItem("Default Rows")
	  gSignalConnect(ordinalItem, "activate", function(...) {
	    dd1 <- dim(.local$theFrame)[1]
	    if(dd1 > 1){
        task <- list(
          list(func="ChangeRowNames", 
            arg = list(theNames = 1:(dd1-1), idx=1:(dd1-1))))
        DoTaskWrapper(.local, task)
      }
	  })
	  m$append(ordinalItem)      
	  m$append(gtkSeparatorMenuItem())
	  ordinalItem <- gtkMenuItem("Default Columns")
	  gSignalConnect(ordinalItem, "activate", function(...) {
	    dd2 <- dim(.local$theFrame)[2]
	    if(dd2 > 2){
        task <- list(
          list(func="ChangeColumnNames", 
            arg = list(theNames = DEFAULT_COLNAMES[1:(dd2-2)], idx=2:(dd2-1))))
        DoTaskWrapper(.local, task)
      }
	  })

	  m$append(ordinalItem)      
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
        "RGtk2 by Michael Lawrence and Duncan Temple Lang",
        "cairoDevice by Michael Lawrence")
      dlg["program-name"] <- "RGtk2DfEdit"
      dlg["comments"] <- "A spreadsheet data frame editor for the R environment"
      dlg["copyright"] <- "GPLv2"
      dlg["version"] <- VERSION_STRING
      gtkDialogRun(dlg)
      gtkWidgetDestroy(dlg)
	  })
    	  
	  return(m)
	}

	Row3rdButtonMenu <- function(theFrame, row.idx){	  
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  pasteItem <- gtkMenuItem("Paste")
	  m$append(cutItem)
	  m$append(copyItem)
	  m$append(pasteItem)
	  m$append(gtkSeparatorMenuItem())
	  gSignalConnect(copyItem, "activate", function(...) 
       CopyToClipboard(theFrame[SelectedRowIndices(.local$view.rn, .local),-c(1, dim(theFrame)[2]), drop=F], do.rownames=T))
	  gSignalConnect(pasteItem, "activate", function(...) {
      dat <- ReadFromClipboard() # character vector
      task <- GetTaskPasteIn(.local$theFrame, dat, row.idx, 2, do.rownames= T)
      DoTaskWrapper(.local, task)
    })                     

	  insertItem <- gtkMenuItem("Insert")
	  gSignalConnect(insertItem, "activate", function(...) {
	     task <- list(list(func="InsertNARows", 
                   arg = list(idx=row.idx)))
       DoTaskWrapper(.local, task)
    })
    deleteItem <- gtkMenuItem("Delete")
	  gSignalConnect(deleteItem, "activate", function(...){	  
	    sr <- SelectedRowIndices(.local$view.rn, .local)
	    if(length(sr)){
  	    task <- list(list(func="DeleteRows", arg = list(idx=sr)))
        DoTaskWrapper(.local, task)
	    }
    })
	  clearItem <- gtkMenuItem("Clear Contents")
	  m$append(insertItem)
	  m$append(deleteItem)
	  m$append(clearItem)
	  gSignalConnect(clearItem, "activate", function(...){	  	  
      row.idx <- SelectedRowIndices(.local$view.rn, .local)
      RowNamesClearContents(row.idx)
    })

	  lapply(c(cutItem), gtkWidgetSetSensitive, FALSE)
    if(!length(SelectedRowIndices(.local$view.rn, .local)))	  
  	  lapply(c(deleteItem, pasteItem), gtkWidgetSetSensitive, FALSE)
    if(row.idx == dim(theFrame)[1])	  
  	  lapply(c(cutItem, copyItem, deleteItem, pasteItem, clearItem, cutItem), 
        gtkWidgetSetSensitive, FALSE)  
        
	  m$append(gtkSeparatorMenuItem())
	  setAsNamesItem <- gtkMenuItem("To Column Names")
	  m$append(setAsNamesItem)	  

	  gSignalConnect(setAsNamesItem, "activate", function(...){
	    theNames <- as.character(theFrame[row.idx,-1])
	    theNames[is.na(theNames)] <- ""	    
	    theNames <- make.unique(theNames)	    
	    dd2 <- dim(.local$theFrame)[2]
      task <- list(
        list(func="ChangeColumnNames", 
          arg = list(theNames = theNames, idx=2:dd2)),
        list(func="DeleteRows", 
          arg = list(idx=row.idx))
       )
      DoTaskWrapper(.local, task)      
    })    	      	                	  
 	  
	  return(m)
	}

  ColumnClearContents <- function(col.idx){
    if(length(col.idx)==0) return(FALSE)
    nf <- .local$model[,col.idx, drop=F]        
    stopifnot(ncol(nf) > 0)  
    nf[,] <- ""
    task <- list(list(func="ChangeCells", 
      arg = list(nf=nf, col.idx=col.idx)))
    DoTaskWrapper(.local, task)    
  }
  	
	Column3rdButtonMenu <- function(model, col.idx){	
	  theFrame <- .local$theFrame
    typ <- GetClasses(theFrame)[col.idx+COLUMN_OFFSET]	 
     
	  theColumn <- theFrame[,col.idx+COLUMN_OFFSET,drop=F]
	  m <- gtkMenu()
	  cutItem <- gtkMenuItem("Cut")
	  copyItem <- gtkMenuItem("Copy")
	  pasteItem <- gtkMenuItem("Paste")
	  m$append(cutItem)
	  m$append(copyItem)
	  m$append(pasteItem)
	  m$append(gtkSeparatorMenuItem())
	  gSignalConnect(copyItem, "activate", 
      function(...) CopyToClipboard(.local$theFrame[,GetSelectedColumns()+1,drop=F],
         do.colnames=T))
	  gSignalConnect(pasteItem, "activate", function(...) {
      dat <- ReadFromClipboard() # character vector
      task <- GetTaskPasteIn(.local$theFrame, dat, 
        1, col.idx+.local$COLUMN_OFFSET, do.colnames= T)
      DoTaskWrapper(.local, task)
     })

	  insertItem <- gtkMenuItem("Insert")
	  deleteItem <- gtkMenuItem("Delete")
	  clearItem <- gtkMenuItem("Clear Contents") 
	  m$append(insertItem)
	  m$append(deleteItem)
	  m$append(clearItem)
	  m$append(gtkSeparatorMenuItem())  
	  gSignalConnect(clearItem, "activate", function(...) ColumnClearContents(GetSelectedColumns()+COLUMN_OFFSET))
	  gSignalConnect(insertItem, "activate", function(...) {
       task <- list(list(func="InsertNAColumns", 
                   arg = list(idx=col.idx+COLUMN_OFFSET)))
       DoTaskWrapper(.local, task)
                         
    })
	  gSignalConnect(deleteItem, "activate", function(...) {
       sc <- GetSelectedColumns()+COLUMN_OFFSET
       if(length(sc)){	    
         task <- list(list(func="DeleteColumns", 
                     arg = list(idx=GetSelectedColumns()+COLUMN_OFFSET)))
         DoTaskWrapper(.local, task)	  
       }
   })  
	  
	  sortItem <- gtkMenuItem("Sort...")	  
	  gSignalConnect(sortItem, "activate", function(...) {
	    dd <- dim(.local$theFrame)
      DoSortDialog(.local$theFrame[-dd[1], -dd[2],drop=F], SortHandler, .local)
	  })
	  m$append(sortItem)
	  m$append(gtkSeparatorMenuItem())
	  
    	  
	  lapply(c(cutItem), gtkWidgetSetSensitive, FALSE)
	  if(col.idx == length(.local$allColumns)) 
      lapply(c(deleteItem), gtkWidgetSetSensitive, FALSE)

    dataTypeNames <- list(Character="character", Integer="integer", Factor="factor", Logical="logical", Numeric="numeric")
	  dataTypeItems <- list()
	  for(theNewTypeName in names(dataTypeNames)){
  		item <- gtkCheckMenuItem(theNewTypeName)
  		item$setDrawAsRadio(TRUE)
  		dataTypeItems[[length(dataTypeItems)+1]] <- item
  		gSignalConnect(item, "button-release-event", function(item, evt, theNewTypeName){  		
  	    task <- list(list(func="CoerceColumns", 
          arg = list(theClasses = dataTypeNames[[theNewTypeName]], idx=GetSelectedColumns()+COLUMN_OFFSET)))
        DoTaskWrapper(.local, task)  		  
  			m$popdown()
  			return(TRUE)
  		  }, theNewTypeName)
  		if (dataTypeNames[[theNewTypeName]]==typ) item$setActive(TRUE)  
  		m$append(item)
	  }
	  m$append(gtkSeparatorMenuItem())
	  editFactorsItem <- gtkMenuItem("Edit Factors...")  
	  m$append(editFactorsItem)  
	  m$append(gtkSeparatorMenuItem())
	  abbreviateItem <- gtkMenuItem("Shorten")
	  m$append(abbreviateItem)
	  setAsNamesItem <- gtkMenuItem("To Row Names")
	  m$append(setAsNamesItem)

	  gSignalConnect(editFactorsItem, "activate", function(...) 
      DoFactorEditor(theFrame, .local$toplevel, col.idx + COLUMN_OFFSET, 
        FactorEditorHandler, data=.local))  
	  if(typ != "factor") editFactorsItem$setSensitive(FALSE)
	  
	  gSignalConnect(abbreviateItem, "activate", function(...){
	     abcol <- data.frame(X=cbind(abbreviate(as.character(theColumn[[1]]), minlength=10)))	     
	     task <- list(
          list(func="CoerceColumns", 
            arg = list(theClasses = "character", idx=col.idx+COLUMN_OFFSET)),
           list(func="ChangeCells", 
           arg = list(nf=abcol, col.idx=col.idx+COLUMN_OFFSET))
      )
       DoTaskWrapper(.local, task)               
    })	  
	  gSignalConnect(setAsNamesItem, "activate", function(...){
	    theNames <- as.character(theColumn[[1]])
	    theNames[is.na(theNames)] <- ""	    
	    theNames <- make.unique(theNames)
	    dd1 <- dim(.local$theFrame)[1]
      task <- list(
        list(func="ChangeRowNames", 
          arg = list(theNames = theNames, idx=1:dd1)),
        list(func="DeleteColumns", 
          arg = list(idx=col.idx+COLUMN_OFFSET))
      )
      DoTaskWrapper(.local, task)      
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
      gSignalConnect(entry, "key-press-event", function(obj, event, ...){ 
        if (event[["keyval"]] == GDK_Return || event[["keyval"]] == GDK_Escape ) .local$view$grabFocus()
        return(FALSE)
      })
      gSignalConnect(entry, "focus-out-event", function(obj, event, data=col.idx){
        getText <- obj$getText()
    	  if(nchar(getText) > 0 && getText != .local$dataset.name) {
          label$setText(getText)
      	  assign(getText, MakeExternalDataFrame(.local$theFrame), envir=.GlobalEnv)
      	  .local$dataset.name <- getText	  
          print(paste("RGtk2DfEdit: Creating dataset", .local$dataset.name, "in global environment."))
        }
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
  ColumnHeaderButtonPress <- function (obj, event, data){
    # get out of editing or selection    
    view <- .local$view
    view.rn <- .local$view.rn
    model <- .local$model
    allColumns <- .local$allColumns
    
      # kill any active entries
    if(!is.null(.local$entry)) {
      tryCatch({
      .local$entry$unrealize()  
      }, silent=T)
    }
        
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
        task <- list(list(func="ChangeColumnNames", 
         arg = list(theNames = obj$getText(),  idx=col.idx+1)))
        obj$destroy()
        box$packEnd(label, FALSE, FALSE, 0)                        
        DoTaskWrapper(.local, task)        
        FALSE
      })
      return(TRUE)
    } else if (button == as.integer(1) && typ == as.integer(4)){ # column clicked  	

    	handler <- .local$columnClickHandler
      if(!is.null(handler) && is.function(handler)) {
        df <- MakeExternalDataFrame(.local$theFrame)
        col <- data$col.idx
        handler(df, col)
      }        

      selectedColumns <- GetSelectedColumns()
      if (stat == as.integer(0)) { # select only this column
         selectedColumns.new <- col.idx                       
        # extend shift-selection
        .local$start.column.select <- col.idx
      } else if (as.flag(stat) & GdkModifierType['shift-mask']) { # range
        if(is.null(.local$start.column.select)){
          .local$start.column.select <- col.idx
        }
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
  
  # replace the entire gtktab
  # new.df is the internal DF representation
  ReplaceEditWindow <- function(theFrame){
    UpdateColumnSelection(.local$allColumns, integer(0))       
    .local$gtktab$destroy()
    .local$theFrame <- theFrame
    .local$gtktab <- MakeDFEditWindow(.local)
    .local$group.main$packStart(.local$gtktab, TRUE, TRUE, 0)
    .local$start.column.select <- NULL
  }
  
  UpdateDfEditor <- function(theFrame){  
    if(ncol(theFrame) != ncol(.local$model)){
      ReplaceEditWindow(theFrame)
    } else {
      cn <- colnames(theFrame)
      for(jj in which(cn != colnames(.local$theFrame)))
        ModelChangeColumnName(jj, cn[jj])
      .local$theFrame <- theFrame
      .local$model$setFrame(theFrame)
    }    
   	assign(.local$dataset.name, MakeExternalDataFrame(.local$theFrame), envir=.GlobalEnv)     
    .local$LAST_PATH <- MakeLastPath(.local$theFrame)
  }
    
  #.local$ReplaceEditWindow <- ReplaceEditWindow
  .local$UpdateDfEditor <- UpdateDfEditor
  
  ModelChangeColumnName <- function(idx, new.name)
    .local$allColumns[[idx-1]]$eventbox$getChildren()[[1]]$getChildren()[[1]]$setText(new.name)
  .local$ModelChangeColumnName <- ModelChangeColumnName
  
	###############################################################################
	# End of misc functions
	###############################################################################

	###############################################################################
	# GUI setup
	###############################################################################
	# create a column with the title as the index
	NewColumn <- function(model, j, width=.local$col.width, is.row = F, is.editable=T){
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
    col.obj <- MakeButtonAndEventBox(col.obj, label.str=nam, handler=ColumnHeaderButtonPress)
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
  
  MakeAVerticalScrollbar <- function(gtktab, va){
	  vbar <- gtkVScrollbarNew(va)
	  gtktab$attach(vbar, 1, 2, 0, 1, 0, 5)
	    # scroll on this doesn't repaint, kill it
	  #vbar$setEvents(GdkEventMask["scroll-mask"])	  
    gSignalConnect(vbar, "scroll-event", function(...) TRUE)
	  return(vbar)
  }
	
   ViewExpose <- function(widget, event=NULL, data=NULL){
    if(.local$do.paint){
      sw.va <- .local$sw.view.va
      currentVadj <- gtkAdjustmentGetValue(sw.va) # this gets called *before* value-changed
      for(r in .local$rectangles){
        r$y <- r$y - currentVadj
        r1 <- r
        if(r1$height > 0 && r1$width > 0){
          if(DO_CAIRO){
            selectedColorRgb <- c(49, 106, 197)
            cr <- gdkCairoCreate(.local$viewGetBinWindow)
            cr$setSourceRgba(selectedColorRgb[1]/256, 
              selectedColorRgb[2]/256, selectedColorRgb[3]/256, 0.2)      
            cr$rectangle(r1$x, r1$y, r1$width, r1$height)
            cr$clip()
            cr$paint()
          }
          gdkDrawRectangle(.local$viewGetBinWindow, .local$gc.invert, filled = F, r1$x, r1$y, r1$width, r1$height)          
        }
      }
    } # display mode
    return(FALSE)
  }
  
  DoScrollUpdate <- function(obj, evt){
    while(gtkEventsPending())
      gtkMainIteration()
    dir <- evt[["direction"]]
    va <- .local$vbar$getAdjustment()
    if (dir == GdkScrollDirection["down"]) {
      new.val <- min(va$getValue() + va[["step_increment"]], va[["upper"]] - va[["page_size"]])    
      va$setValue(new.val)
    } else if (dir == GdkScrollDirection["up"]) {
      va$setValue(va$getValue() - va[["step_increment"]])
    }
    FALSE
  }      
  
  ViewOnScrollChanged <- function(obj, data){
    # Take over the event loop! 
    # See http://wiki.laptop.org/go/PyGTK/Smooth_Animation_with_PyGTK
    while(gtkEventsPending())
      gtkMainIteration()
    gtkAdjustmentSetValue(data, gtkAdjustmentGetValue(obj))
    .local$allow.key.flag <- TRUE  # For paging, however, this has a problem...
  }
  
  .local$scrollID <- 0    	  
        
  
  ConnectViewsTogether <- function(gtktab, view1, sw1, sw2, ss2){  
  	gSignalConnect(view1, "focus-in-event", function(obj, event, data=list(gtktab = gtktab, sw1=sw1, sw2=sw2, ss2=ss2)){
  	#print("Focus in")
    	gtktab <- data$gtktab
    	sw1 <- data$sw1
    	sw2 <- data$sw2
    	ss2 <- data$ss2
 	    if(is.null(gObjectGetData(sw1, "scrollID"))){
 	      if(!is.null(gObjectGetData(sw2, "scrollID"))) {
          gSignalHandlerDisconnect(sw2, gObjectGetData(sw2, "scrollID"))
 	        gObjectSetData(sw2, "scrollID", NULL)                                           
 	      }
 	      #print(.local$unrealized.flag)
 	      if(is.null(.local$unrealized.flag)) tryCatch({
          .local$vbar$setAdjustment(sw1)
          .local$va <- sw1 #.local$vbar$getAdjustment()          
          gtkTreeSelectionUnselectAll(ss2)    	  
          gObjectSetData(sw1, "scrollID", gSignalConnect(sw1, "value-changed", ViewOnScrollChanged, data=sw2))
        }, error = function(e) {
          warning("Error disposing")
        })
   	  }
     	#print("End focus in")	   	  
  	  return(FALSE)  	  
	  }) #gSignalConnect focus-in
	  
	gSignalConnect(view1, "focus-out-event", function(obj, event, data=sw1){
	#print("Focus out")
	sw1 <- data
	if(!is.null(gObjectGetData(sw1, "scrollID"))){
    gSignalHandlerDisconnect(sw1, gObjectGetData(sw1, "scrollID"))
  	gObjectSetData(sw1, "scrollID", NULL)
	} else {
	}
	#print("End focus out")	
  return(FALSE)
  })

}   

	###############################################################################
	# End of GUI setup
	###############################################################################
		   
MakeDFEditWindow <- function(.local, ...){  
  sw.view <- gtkScrolledWindowNew()
  sw.view$setPolicy(GtkPolicyType['never'], GtkPolicyType['never'])
  sw.view$setSizeRequest(size.request[1], size.request[2])
  
  theFrame <- .local$theFrame
  dataset.name <- .local$dataset.name
  model <- rGtkDataFrame(theFrame)
  .local$model <- model
  .local$mapped <- FALSE
  view <- gtkTreeViewNewWithModel(model)
  gtkTreeViewSetFixedHeightMode(view, TRUE)
  allColumns <- vector("list", (dim(theFrame)[2]-1))
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
  view.rn$setEnableSearch(FALSE)
  view$setEnableSearch(FALSE)  

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
  ## Notebooks map too!
  gSignalConnect(view, "map", after=T, data=.local, function(view, .local){   
    if(.local$mapped) return(TRUE)
    .local$mapped <- TRUE    
    .local$do.paint <- FALSE        
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
      idx <- as.integer(path)+1
      if(new.text != row.names(.local$theFrame)[idx]){      
        task <- list(list(func="ChangeRowNames", 
          arg = list(theNames = new.text, idx=idx)))
        DoTaskWrapper(.local, task)
      }
      return(FALSE)
    })

    gSignalConnect(view, "expose-event", after=T, ViewExpose)    
    gSignalConnect(view, "scroll-event", after=T, DoScrollUpdate)
    gSignalConnect(view.rn, "scroll-event", after=T, DoScrollUpdate)    
    
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
    .local$rows.renderer <- rows.renderer  
    toplevel <- .local$group.main$getToplevel()
    if (toplevel$flags() & GtkWidgetFlags["toplevel"]){
      .local$toplevel <- toplevel
    }  

      # Kill drag and drop, it causes a crash    
      # http://www.mail-archive.com/gtk-app-devel-list@gnome.org/msg11623.html      
    screen <- .local$group.main$getScreen()
    settings <- gtkSettingsGetForScreen(screen)
    settings[["gtk-dnd-drag-threshold"]] <- 10000
    
   	view$grabFocus()  	
   	
    return(TRUE)  
   }) # end of map event callback
       
  ConnectViewsTogether(gtktab, view, sw.view.va, sw.rn.va, ss.rn)
  ConnectViewsTogether(gtktab, view.rn, sw.rn.va, sw.view.va, ss)  

  return(gtktab)
} # end MakeDFEditWindow


  .local$doing.scroll <- FALSE
  ## Finally do the setup
  .local$dataset.name <- dataset.name
  .local$theFrame <- MakeInternalDataFrame(items)
  .local$LAST_PATH <- MakeLastPath(.local$theFrame)
  .local$undoStack <- list()
  .local$gtktab <- MakeDFEditWindow(.local, size.request) 
  .local$group.main$packStart(.local$gtktab, TRUE, TRUE, 0)
  .local$group.main$setData(".local", .local) # use getData
    
  gSignalConnect(.local$group.main, "unrealize", function(view, ...){      
    .local$unrealized.flag <- TRUE
    return(FALSE)
  })
  gSignalConnect(.local$group.main, "unmap", function(view, ...){      
    .local$unrealized.flag <- TRUE
    return(FALSE)
  })  
   
   
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
  return(MakeExternalDataFrame(object$getModel()))
}


#' Function to call when column is clicked
#'
#' IF set to NULL, no handler is called.
#' @param object The RGtk2DfEdit object
#' @param handler Function to call when column clicked. Signature is (dataframe, column number). If NULL (default)
#'        no handler is called.
#' @export
gtkDfEditSetColumnClickHandler <- function(object, columnClickHandler=NULL) {
 ## XXX must set up handler to make this work
  l <- object$getData(".local")
  l$columnClickHandler <- columnClickHandler
  object$setData(".local", l)
  invisible()
}

#' Function to call when row is clicked
#'
#' IF set to NULL, no handler is called.
#' @param object The RGtk2DfEdit object
#' @param handler Function to call when row clicked. Signature is (dataframe, row number). If NULL (default)
#'        no handler is called.
#' @export
gtkDfEditSetRowClickHandler <- function(object, rowClickHandler=NULL) {
  l <- object$getData(".local")
  l$rowClickHandler <- rowClickHandler
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
