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
#' @param global PARAM_DESCRIPTION, Default: T
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

vscjob_LoadConfig <- function(global = T){
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
