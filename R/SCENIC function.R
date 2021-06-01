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
# ExportBasicLoom(seu)
# ExportBasicLoom(EC_HNSCC, dir = "2021-2-3 Amelie HNSCC EC/", file.name = "EC_HNSCC.loom")

# loom.path = "seu.loom"
sce.database.path <- list(
  human = list(
    TFs = "${VSC_DATA}/SCENIC/databases/hs_hgnc_tfs.txt",
    motifs = "${VSC_DATA}/SCENIC/databases/motifs-v9-nr.hgnc-m0.001-o0.0.tbl",
    db = "${VSC_DATA}/SCENIC/databases/hg38__refseq-r80__10kb_up_and_down_tss.mc9nr.feather"
  ),
  mouse = list(
    TFs = "${VSC_DATA}/SCENIC/databases/mm_mgi_tfs.txt",
    motifs = "${VSC_DATA}/SCENIC/databases/motifs-v9-nr.mgi-m0.001-o0.0.tbl",
    db = "${VSC_DATA}/SCENIC/databases/mm9-tss-centered-10kb-7species.mc9nr.feather"
  )
)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param loom.path PARAM_DESCRIPTION
#' @param spe PARAM_DESCRIPTION, Default: 'human'
#' @param project.name PARAM_DESCRIPTION, Default: 'Scenic_project'
#' @param dir PARAM_DESCRIPTION, Default: getwd()
#' @param nCore PARAM_DESCRIPTION, Default: 36
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
  spe = "human",
  project.name = "Scenic_project",
  dir = getwd(),
  nCore = 36,
  walltime = "8:00:00") {
  input <- gsub(" ", "\\ ", normalizePath(loom.path), fixed = T)
  sce_wd <- file.path(dir, project.name)
  wd <- gsub(" ", "\\ ", sce_wd, fixed = T)
  dir.create(sce_wd)
  if(!file.exists(loom.path)) stop("loom.path not found")
  fileConn <- file(file.path(project.name, "SCENIC_template.sh"))
  writeLines(c(
    "#!/usr/bin/bash",
    paste0("#PBS -l walltime=", walltime),
    "#PBS -l pmem=5gb",
    "#PBS -A lp_vsc32982",
    paste0("#PBS -l nodes=1:ppn=", nCore),
    "#PBS -m ae  # notify on aborted, end",
    "#PBS -M yichao.hua@kuleuven.be",

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
    paste0("--TFs ", sce.database.path[[spe]][["TFs"]], " \\"),
    paste0("--motifs ", sce.database.path[[spe]][["motifs"]], " \\"),
    paste0("--db ", sce.database.path[[spe]][["db"]])
  ), fileConn)
  close(fileConn)

  command <- paste0("cd ", wd, "\nqsub SCENIC_template.sh")
  system(command)
}

# spe = "human"
# RunScenic(loom.path = "Lung.loom", spe = "human", nCore = 24, project.name = "SCENIC_Lung_Lee")
# RunScenic(loom.path = "EC_HNSCC.loom", spe = "human", nCore = 24, project.name = "SCENIC_HNSCC")

