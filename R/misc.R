#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param walltime PARAM_DESCRIPTION, Default: NULL
#' @param project PARAM_DESCRIPTION, Default: NULL
#' @param nodes PARAM_DESCRIPTION, Default: NULL
#' @param ppn PARAM_DESCRIPTION, Default: NULL
#' @param email PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pbs.string
#' @export

pbs.string <- function(
  walltime = NULL,
  project = NULL,
  nodes = NULL,
  ppn = NULL,
  email = NULL
) {
  library(rlang)
  opt <- vscjob_LoadConfig(global = F)
  walltime <- walltime %||% opt$pbs.walltime
  project <- project %||% opt$pbs.project
  nodes <- nodes %||% opt$pbs.nodes
  ppn <- ppn %||% opt$pbs.ppn
  email <- email %||% opt$pbs.email
  pbs <- paste0(
    "#!/bin/bash -l\n",
    "#PBS -l walltime=", walltime, "\n",
    "#PBS -A ", project, "\n",
    "#PBS -l nodes=", nodes, ":ppn=", ppn, "\n",
    "#PBS -m ae\n",
    "#PBS -M ", email, "\n"
  )
  return(pbs)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param nm PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname check.sys.dir.exist
#' @export

check.sys.dir.exist <- function(x, nm = NULL){
  path <- x
  for (i in c("VSC_DATA","VSC_SCRATCH","VSC_HOME")) {
    v1 <- paste0("${",i,"}")
    if(grepl(v1, path, fixed = T)){
      path <- sub(pattern = v1, replacement = Sys.getenv(i), x = path, fixed = T)
    }
    v2 <- paste0("$",i)
    if(grepl(v2, path, fixed = T)){
      path <- sub(v2, Sys.getenv(i), x = path, fixed = T)
    }
  }
  if(!file.exists(path)) {
    if(is.null(nm)){
      stop("Directory '",x,"' does not exist.")
    } else {
      stop(nm, " directory '",x,"' does not exist.")
    }
  }
  return(path)
}

