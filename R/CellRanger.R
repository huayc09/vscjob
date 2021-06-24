#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param fastqs PARAM_DESCRIPTION
#' @param IDs PARAM_DESCRIPTION
#' @param spe PARAM_DESCRIPTION
#' @param program.path PARAM_DESCRIPTION, Default: NULL
#' @param ref PARAM_DESCRIPTION, Default: NULL
#' @param walltime PARAM_DESCRIPTION, Default: NULL
#' @param project PARAM_DESCRIPTION, Default: NULL
#' @param nodes PARAM_DESCRIPTION, Default: NULL
#' @param ppn PARAM_DESCRIPTION, Default: NULL
#' @param email PARAM_DESCRIPTION, Default: NULL
#' @param sequential PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname RunCellranger
#' @export

RunCellranger <- function(
  fastqs,
  IDs,
  spe,
  program.path = NULL,
  ref = NULL,
  walltime = NULL,
  project = NULL,
  nodes = NULL,
  ppn = NULL,
  email = NULL,
  sequential = F
) {
  library(stringi)
  if(!spe %in% c("mouse","human")) {
    stop("'spe' must be 'mouse' or 'human'")
  }
  if(length(fastqs) != length(IDs)) {
    stop("'fastqs' and 'IDs' must be the same length.")
  }
  pbs <- pbs.string(
    walltime = walltime,
    project = project,
    nodes = nodes,
    ppn = ppn,
    email = email
  )
  message("PBS arguments:\n",pbs)
  library(rlang)
  opt <- vscjob_LoadConfig(global = F)
  program.path <- program.path %||% opt$cellranger.program.path
  if(is.null(ref)) {
    ref <- switch(
      spe,
      mouse = opt$cellranger.ref.mouse,
      human = opt$cellranger.ref.human)
  }
  message("CellRanger path: ", program.path, "\n",
          "Reference genome path: ", ref, "\n")
  check.sys.dir.exist(program.path)
  check.sys.dir.exist(ref)

  code.cellranger <- vector()
  for (i in seq_along(IDs)) {
    check.sys.dir.exist(fastqs[i])
    code.cellranger[i] <- paste0(
      "cellranger count --id=",IDs[i],
      " --transcriptome=",ref,
      " --fastqs=",fastqs[i],
      " --expect-cells=5000 --jobmode=local --localcores=36 --localmem=180"
    )
  }
  if(sequential) {
    sh.name <- paste0(
      Sys.Date(),
      "_cellranger_",
      stri_rand_strings(1,6),
      ".sh"
    )
    file.create(sh.name)
    cat(pbs, file = sh.name)
    cat(
      "export PATH=$PATH:",
      program.path, "\n",
      file = sh.name, append = T
    )
    cat(
      "cd", getwd(), "\n",
      file = sh.name, append = T
    )
    cat(
      code.cellranger, sep = "",
      file = sh.name, append = T
    )
    command <- paste0(
      "cd ", getwd(),
      "\nqsub ",
      sh.name
    )
    system(command)
    message("Job submitted: ",sh.name)
  } else {
    for (i in seq_along(IDs)) {
      sh.name <- paste0(
        Sys.Date(),
        "_cellranger_",
        IDs[i],"_",
        stri_rand_strings(1,6),
        ".sh"
      )
      file.create(sh.name)
      cat(pbs, file = sh.name)
      cat(
        "export PATH=$PATH:",
        program.path, "\n", sep = "",
        file = sh.name, append = T
      )
      cat(
        "cd", getwd(), "\n",
        file = sh.name, append = T
      )
      cat(
        code.cellranger[i], sep = "",
        file = sh.name, append = T
      )
      command <- paste0(
        "cd ", getwd(),
        "\nqsub ",
        sh.name
      )
      system(command)
      message("Job submitted: ",sh.name)
    }
  }
}

# RunCellranger(fastqs = "external/cellranger_tiny_fastq",
#               IDs = "tiny",
#               spe = "human",
#               walltime = "00:10:00",
#               ref = "external/cellranger_tiny_ref/3.0.0"
#               )
