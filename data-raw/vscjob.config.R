# config template ---------------------------------------------------------

vsc.config <- list(
  sce.database.path.human.TFs = "${VSC_DATA}/SCENIC/databases/hs_hgnc_tfs.txt",
  sce.database.path.human.motifs = "${VSC_DATA}/SCENIC/databases/motifs-v9-nr.hgnc-m0.001-o0.0.tbl",
  sce.database.path.human.db = "${VSC_DATA}/SCENIC/databases/hg38__refseq-r80__10kb_up_and_down_tss.mc9nr.feather",
  sce.database.path.mouse.TFs = "${VSC_DATA}/SCENIC/databases/mm_mgi_tfs.txt",
  sce.database.path.mouse.motifs = "${VSC_DATA}/SCENIC/databases/motifs-v9-nr.mgi-m0.001-o0.0.tbl",
  sce.database.path.mouse.db = "${VSC_DATA}/SCENIC/databases/mm9-tss-centered-10kb-7species.mc9nr.feather"
)

vsc.config <- t(data.frame(vsc.config))
write.table(vsc.config, file = "inst/extdata/.vscjob.config",
            row.names = T, sep = "=", quote = F, col.names = F)
