#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param force PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname vscjob_CreateConfig
#' @export

vscjob_CreateConfig <- function(force = FALSE){
  if(file.exists("~/.vscjob.config") & !force) {
    stop("Config file already exists.")
  }
  file.copy(from = system.file("extdata", ".vscjob.config", package = "vscjob"),
            to = "~/", overwrite = TRUE)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param global PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname vscjob_LoadConfig
#' @export

vscjob_LoadConfig <- function(global = F){
  if(!file.exists("~/.vscjob.config")) {
    vscjob_CreateConfig()
  }
  vscjob_options <- read.table("~/.vscjob.config", header = F, sep = "=", row.names = 1)
  vscjob_options <-
    setNames(
      split(vscjob_options$V2, f = seq(nrow(vscjob_options))),
      nm = rownames(vscjob_options)
    )
  if(global) assign(x = "vscjob_options", value = vscjob_options, envir = globalenv())
  return(vscjob_options)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname vscjob_SetConfig
#' @export

vscjob_SetConfig <- function(...){
  library(rlang)
  if(!file.exists("~/.vscjob.config")) {
    vscjob_CreateConfig()
  }
  opt.default <- vscjob_LoadConfig(global = F)
  opt.new <- list(...)
  if(is_empty(opt.new)){
    message("Current config:")
    return(opt.default)
  }
  opt.nm <- names(opt.default)
  opt.nm.new <- names(opt.new)
  if(any(!opt.nm.new %in% opt.nm)) {
    stop("Invalid name(s): ",
         paste0(setdiff(opt.nm.new, opt.nm), collapse = ", "))
  }
  for (i in opt.nm.new) {
    if(!grepl("^pbs",i)){
      check.sys.dir.exist(opt.new[[i]], nm = i)
    }
    opt.default[[i]] <- opt.new[[i]]
    message("Set '",i,"' to '",opt.new[[i]],"'")
  }
  opt.towrite <- t(data.frame(opt.default))
  write.table(opt.towrite, file = "~/.vscjob.config",
              row.names = T, sep = "=", quote = F, col.names = F)
  return(opt.towrite)
}
