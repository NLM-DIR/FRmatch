
########################################################################################################
## filter_cluster()
########################################################################################################

filter_cluster <- function(sce.object, filter.size, filter.fscore=NULL, filter.nomarker=FALSE){

  ## cluster size
  tab <- table(colData(sce.object)$cluster_membership)

  ## filter by cluster size
  cluster.keep <- names(which(tab>=filter.size))

  ## filter by fscore
  if(!is.null(filter.fscore)){
    cluster.keep <- sce.object@metadata$f_score %>%
      filter(clusterName %in% cluster.keep & score>=filter.fscore) %>%
      pull(clusterName)
  }

  sce.object.filt <- subset_by_cluster(sce.object, cluster.keep)

  ## output
  return(sce.object.filt)
}


########################################################################################################
## subset_by_cluster()
########################################################################################################

subset_by_cluster <- function(sce.object, cluster.keep){

  ## colData
  ind.keep <- colData(sce.object)$cluster_membership %in% cluster.keep
  sce.object.filt <- sce.object[,ind.keep]
  ## metaData
  if(!is.null(sce.object@metadata$cluster_marker_info)){
    sce.object.filt@metadata$cluster_marker_info <- sce.object@metadata$cluster_marker_info %>%
      filter(clusterName %in% cluster.keep)
  }
  if(!is.null(sce.object@metadata$f_score)){
    sce.object.filt@metadata$f_score <- sce.object@metadata$f_score %>%
      filter(clusterName %in% cluster.keep)
  }
  if(!is.null(sce.object@metadata$cluster_order)){
    sce.object.filt@metadata$cluster_order <- base::intersect(sce.object@metadata$cluster_order, cluster.keep)
  }
  ## rowData
  if(!is.null(sce.object@metadata$cluster_marker_info)){
    rowData(sce.object.filt)$NSF_markers <- rownames(sce.object.filt) %in% sce.object.filt@metadata$cluster_marker_info$markerGene
  }

  ## output
  return(sce.object.filt)
}


