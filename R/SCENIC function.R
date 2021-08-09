#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param seu PARAM_DESCRIPTION
#' @param dir PARAM_DESCRIPTION, Default: getwd()
#' @param file.name PARAM_DESCRIPTION, Default: 'seu.loom'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ExportBasicLoom
#' @export

ExportBasicLoom <- function(seu, dir = getwd(), file.name = "seu.loom") {
  library(Seurat)
  library(dplyr)
  library(SeuratDisk)
  DefaultAssay(seu) <- 'RNA'
  seu_loom <-
    CreateSeuratObject(
      counts = GetAssayData(seu, slot = "counts"),
      min.cells = 1, min.features = 1) %>%
    NormalizeData() %>%
    FindVariableFeatures()
  lfile <- as.loom(seu_loom,  filename = file.path(dir, file.name))
  lfile$close_all()
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param loom.path PARAM_DESCRIPTION
#' @param spe PARAM_DESCRIPTION
#' @param project.name PARAM_DESCRIPTION, Default: 'Scenic_project'
#' @param dir PARAM_DESCRIPTION, Default: getwd()
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
#' @rdname RunScenic
#' @export

RunScenic <- function(
  loom.path,
  spe,
  project.name = "Scenic_project",
  dir = getwd(),
  walltime = NULL,
  project = NULL,
  nodes = NULL,
  ppn = NULL,
  email = NULL)
{
  library(rlang)
  check.sys.dir.exist(loom.path, nm = "Loom path")
  input <- gsub(" ", "\\ ", normalizePath(loom.path), fixed = T)

  check.sys.dir.exist(dir, nm = "SCENIC working dir")
  sce_wd <- file.path(dir, project.name)
  dir.create(sce_wd)
  wd <- gsub(" ", "\\ ", sce_wd, fixed = T)

  opt <- vscjob_LoadConfig(global = F)
  nodes <- nodes %||% opt$pbs.nodes
  ppn <- ppn %||% opt$pbs.ppn
  nCore <- as.numeric(nodes) * as.numeric(ppn)
  if(!spe %in% c("mouse","human")) stop("'spe' should be 'mouse' or 'human'")
  ref <- switch(
    spe,
    mouse = list(
      TFs = opt$sce.database.path.mouse.TFs,
      motifs = opt$sce.database.path.mouse.motifs,
      db = opt$sce.database.path.mouse.db
    ),
    human = list(
      TFs = opt$sce.database.path.human.TFs,
      motifs = opt$sce.database.path.human.motifs,
      db = opt$sce.database.path.human.db
    )
  )
  check.sys.dir.exist(ref$TFs, nm = "TFs")
  check.sys.dir.exist(ref$motifs, nm = "motifs")
  check.sys.dir.exist(ref$db, nm = "SCENIC database")

  fileConn <- file(file.path(project.name, "SCENIC_template.sh"))
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
    "export NXF_SINGULARITY_CACHEDIR=${VSC_DATA}/singularity_cache/",
    "source activate science",

    "nextflow run aertslab/SCENICprotocol \\",
    "--thr_min_genes 0 \\",
    "--thr_min_cells 0 \\",
    "--thr_n_genes 999999 \\",
    "--thr_pct_mito 1 \\",
    paste0("--num_workers ",nCore," \\"),
    paste0("--threads ",nCore," \\"),
    "-profile singularity \\",
    paste0("--loom_input ", input, " \\"),
    paste0("--TFs ", ref$TFs, " \\"),
    paste0("--motifs ", ref$motifs, " \\"),
    paste0("--db ", ref$db)
  ), fileConn)
  close(fileConn)

  command <- paste0("cd ", wd, "\nqsub SCENIC_template.sh")
  system(command)
}

# RunScenic(loom.path = "seu.loom", spe = "human", walltime = "0:30:00")
