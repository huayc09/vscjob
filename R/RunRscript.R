#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file PARAM_DESCRIPTION
#' @param nCore PARAM_DESCRIPTION, Default: 36
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname RunRscript
#' @export

RunRscript <- function(file, nCore = 36) {
  if(!file.exists(file)) stop("R script file path not found")
  r.file <- gsub(" ", "\\ ", normalizePath(file), fixed = T)
  wd <- gsub(" ", "\\ ", getwd(), fixed = T)
  d <- Sys.time()
  sh.name <- paste0(d, "_", file, ".sh")
  fileConn <- file(sh.name)
  writeLines(c(
    "#!/usr/bin/bash",
    "#PBS -l walltime=1:00:00",
    "#PBS -l pmem=5gb",
    "#PBS -A lp_vsc32982",
    paste0("#PBS -l nodes=1:ppn=", nCore),
    "#PBS -m ae  # notify on aborted, end",
    "#PBS -M yichao.hua@kuleuven.vib.be",

    paste0("cd ", wd),
    'export PATH="${VSC_DATA}/miniconda3/bin:${PATH}"',
    "source activate general",

    paste0("Rscript ", r.file)
  ), fileConn)
  close(fileConn)

  command <- paste0("qsub ", gsub(" ", "\\ ", sh.name, fixed = T))
  system(command)
}
