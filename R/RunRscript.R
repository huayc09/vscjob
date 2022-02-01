#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param file PARAM_DESCRIPTION
#' @param env PARAM_DESCRIPTION, Default: 'general'
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
#' @rdname RunRscript
#' @export

RunRscript <- function(
  file,
  env = "general",
  walltime = NULL,
  project = NULL,
  nodes = NULL,
  ppn = NULL,
  email = NULL
) {
  check.sys.dir.exist(file, "R script file")
  r.file <- gsub(" ", "\\ ", normalizePath(file), fixed = T)
  wd <- gsub(" ", "\\ ", getwd(), fixed = T)
  sh.name <- "rscript.sh"
  fileConn <- file(sh.name)
  writeLines(c(
    pbs.string(
      walltime = walltime,
      project = project,
      nodes = nodes,
      ppn = ppn,
      email = email
    ),

    paste0("cd ", wd),
    'export PATH="${VSC_DATA}/miniconda3/bin:${PATH}"',
    paste0("conda activate ", env),

    paste0("Rscript ", r.file)
  ), fileConn)
  close(fileConn)

  system("qsub rscript.sh")
}

# RunRscript <- function(file, nCore = 36) {
#   if(!file.exists(file)) stop("R script file path not found")
#   r.file <- gsub(" ", "\\ ", normalizePath(file), fixed = T)
#   wd <- gsub(" ", "\\ ", getwd(), fixed = T)
#   sh.name <- "rscript.sh"
#   fileConn <- file(sh.name)
#   writeLines(c(
#     "#!/usr/bin/bash",
#     "#PBS -l walltime=1:00:00",
#     "#PBS -l pmem=5gb",
#     "#PBS -A lp_vsc32982",
#     paste0("#PBS -l nodes=1:ppn=", nCore),
#     "#PBS -m ae  # notify on aborted, end",
#     "#PBS -M yichao.hua@kuleuven.vib.be",
#
#     paste0("cd ", wd),
#     'export PATH="${VSC_DATA}/miniconda3/bin:${PATH}"',
#     "source activate general",
#
#     paste0("Rscript ", r.file)
#   ), fileConn)
#   close(fileConn)
#
#   system("qsub rscript.sh")
# }
