library(vscjob)
library(Seurat)
library(dplyr)
library(mosaic)
library(rlist)
library(purrr)
library(roxygen2)
library(sinew)
library(SeuratExtend)
options(max.print = 50, spe = "mouse", nCores = 12)

usethis::use_data(mouse_human_genesymbols, overwrite = TRUE)
makeOxygen(RunRscript)
roxygenize()


# value <- list(seu = pbmc, features = "CD3D", group.by = "seurat_clusters", split.by = NULL, cell = NULL,
#               ncol = 1, scales = "free_y", type = c("violin","boxplot"), outlier.size = 1)
# for (i in names(value)) {
#   assign(i, value = value[[i]])
# }
