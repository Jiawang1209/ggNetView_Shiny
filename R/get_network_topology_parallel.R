#' Create network topology in parallel
#'
#' Generates a data frame containing network topology information from either a
#' pre-built graph object or directly from an adjacency matrix.
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#'   The network object to be visualized.
#' @param mat Numeric Matrix (default = NULL)
#' The matrix to build graph_obj
#' @param transfrom.method Character.
#'Data transformation methods applied before correlation analysis.
#' Options include:
#' "none" (raw data),
#' "scale" (z-score standardization),
#' "center" (mean centering only),
#' "log2" (log2 transfrom),
#' "log10" (log10 transfrom),
#' "ln" (natural transfrom ),
#' "rrarefy" (random rarefaction using \code{vegan::rrarefy}),
#' "rrarefy_relative" (rarefy then convert to relative abundance).
#' @param r.threshold Numeric.
#' Correlation coefficient threshold; edges are kept only if |r| >= r.threshold.
#' @param p.threshold Numeric.
#' Significance threshold for correlations; edges are kept only if p < p.threshold.
#' @param method Character.
#' Relationship analysis methods.
#' Options include: "WGCNA", "SpiecEasi", "SPARCC" and "cor".
#' @param cor.method Character.
#' Correlation analysis method.
#' Options include "pearson", "kendall", and "spearman".
#' @param proc Character.
#' Correlation p-value adjustment methods.
#' Options include:
#' "Bonferroni", "Holm", "Hochberg",
#' "SidakSS", "SidakSD","BH",
#' "BY", "ABH", and "TSBH".
#' @param bootstrap Numeric  (default = 100).
#' Number of bootstrap iterations for stability analysis
#' @param parallel Logical (default = FALSE).
#' Whether to enable parallel computation.
#' @param n_workers Integer (default = NULL)
#' Number of parallel workers to use when \code{parallel = TRUE}
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#'
#' @returns data frame of network topolog
#' @export
#'
#' @examples NULL
get_network_topology_parallel <- function(graph_obj,
                                          mat = NULL,
                                          transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                          r.threshold = 0.7,
                                          p.threshold = 0.05,
                                          method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                          cor.method = c("pearson", "kendall", "spearman"),
                                          proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD","BH", "BY","ABH","TSBH"),
                                          bootstrap = 100,
                                          parallel = FALSE,
                                          n_workers = NULL,
                                          seed = 1115
                                          ){


  set.seed(seed)

  progressr::handlers("txtprogressbar")
  progressr::handlers(global = TRUE)

  # self network topology attributes
  # create igraph object
  ig <- tidygraph::as.igraph(graph_obj)

  # argument check
  # transfrom.method <-  match.arg(transfrom.method)
  # cor.method <- match.arg(cor.method)
  # proc <- match.arg(proc)

  if (is.null(mat)) {
    graph_obj = graph_obj

    network.raw = NULL

    cohesion_out <- data.frame(
      cohension_position = NA,
      cohension_negative = NA
    )

    Weighted.simu <- data.frame(
      remain.mean = NA,
      remain.sd = NA,
      remain.se = NA
    )

    Unweighted.simu <- data.frame(
      remain.mean = NA,
      remain.sd = NA,
      remain.se = NA
    )



  }else{
    # data transfrom
    mat <- switch (
      transfrom.method,
      none = mat,
      scale = t(scale(t(mat), scale = T, center = T)),
      center = t(scale(t(mat), scale = F, center = T)),
      log2 = log2(mat + 1),
      log10 = log10(mat + 1),
      ln = log(mat + 1),
      rrarefy = t(vegan::rrarefy(t(mat), min(colSums(mat)))),
      rrarefy_relative = t(vegan::rrarefy(t(mat), min(colSums(mat)))) / colSums(t(vegan::rrarefy(t(mat), min(colSums(mat)))))
    )

    # calculate correlation

    # WGCNA
    if (method == "WGCNA") {
      sp.ra <- colMeans(t(mat))

      # WGCNA for correlation
      occor <- WGCNA::corAndPvalue(t(mat), method = cor.method)
      mtadj <- multtest::mt.rawp2adjp(unlist(occor$p),proc=proc)
      adpcor <- mtadj$adjp[order(mtadj$index),2]
      occor.p <- matrix(adpcor, dim(t(mat))[2])

      # R and pvalue
      occor.r <- occor$cor
      diag(occor.r) <- 0
      occor.r[occor.p > p.threshold | abs(occor.r) < r.threshold] = 0
      occor.r[is.na(occor.r)]=0

      network.raw <- occor.r[colSums(abs(occor.r)) > 0, colSums(abs(occor.r)) > 0]
      sp.ra2 <- sp.ra[colSums(abs(occor.r))>0]

    }

    # SpiecEasi
    if (method == "SpiecEasi") {
      sp.ra <- colMeans(t(mat))
      # SpiecEasi for correlation
      SpiecEasi_obj <- SpiecEasi::spiec.easi(as.matrix(t(mat)),
                                             method = SpiecEasi.method,
                                             lambda.min.ratio=1e-2,
                                             nlambda=20,
                                             pulsar.params=list(rep.num=50)
      )

      # return adjacency matrix
      am <- SpiecEasi::getRefit(SpiecEasi_obj)

      rownames(am) <- rownames(mat)
      colnames(am) <- rownames(mat)

      am2 <- am*(abs(am) >= r.threshold)

      am2[is.na(am2)] <- 0
      diag(am2) <- 0
      sum(abs(am2) > 0) / 2
      sum(colSums(abs(am2)) > 0)

      network.raw <- am2[colSums(abs(am2)) > 0,colSums(abs(am2)) > 0]
      sp.ra2<-sp.ra[colSums(abs(am2)) > 0]
      sum(row.names(network.raw) == names(sp.ra2))

    }

    # SparCC
    if (method == "SparCC") {
      sp.ra <- colMeans(t(mat))
      # Sparcc for correlation
      SparCC_obj <- SpiecEasi::sparcc(as.matrix(t(mat)))

      SparCC_graph <- abs(SparCC_obj$Cor) >= r.threshold

      diag(SparCC_graph) <- 0

      rownames(SparCC_graph) <- rownames(mat)
      colnames(SparCC_graph) <- rownames(mat)

      SparCC_graph <- Matrix::Matrix(SparCC_graph, sparse=TRUE)

      SparCC_graph2 <- SparCC_graph

      SparCC_graph2[is.na(SparCC_graph2)] <- 0
      diag(SparCC_graph2) <- 0
      sum(abs(SparCC_graph2) > 0) / 2
      sum(colSums(abs(SparCC_graph2)) > 0)

      network.raw <- SparCC_graph2[colSums(abs(SparCC_graph2)) > 0,colSums(abs(SparCC_graph2)) > 0]
      sp.ra2<-sp.ra[colSums(abs(SparCC_graph2)) > 0]
    }

    # cor
    if (method == "cor") {
      sp.ra <- colMeans(t(mat))
      # Cor for correlation
      occor <- psych::corr.test(t(mat), method = cor.method)
      mtadj <- multtest::mt.rawp2adjp(unlist(occor$p),proc=proc)
      adpcor <- mtadj$adjp[order(mtadj$index),2]
      occor.p <- matrix(adpcor, dim(t(mat))[2])

      # R and pvalue
      occor.r <- occor$r
      diag(occor.r) <- 0
      occor.r[occor.p > p.threshold | abs(occor.r) < r.threshold] = 0
      occor.r[is.na(occor.r)]=0

      network.raw <- occor.r[colSums(abs(occor.r)) > 0, colSums(abs(occor.r)) > 0]
      sp.ra2 <- sp.ra[colSums(abs(occor.r))>0]

    }
  }



  .rand.remov.once<-function(netRaw, rm.percent, sp.ra, abundance.weighted=T){
    id.rm<-sample(1:nrow(netRaw), round(nrow(netRaw)*rm.percent))
    net.Raw=netRaw
    net.Raw[id.rm,]=0;  net.Raw[,id.rm]=0;
    if (abundance.weighted){
      net.stength= net.Raw*sp.ra
    } else {
      net.stength= net.Raw
    }

    sp.meanInteration<-colMeans(net.stength)

    id.rm2<- which(sp.meanInteration<=0)
    remain.percent<-(nrow(netRaw)-length(id.rm2))/nrow(netRaw)

    remain.percent
  }


  .rmsimu<-function(netRaw, rm.p.list, sp.ra, abundance.weighted=T,bootstrap=bootstrap){
    t(sapply(rm.p.list,function(x){
      remains=sapply(1:bootstrap,function(i){
        .rand.remov.once(netRaw=netRaw, rm.percent=x, sp.ra=sp.ra, abundance.weighted=abundance.weighted)
      })
      remain.mean=mean(remains)
      remain.sd=sd(remains)
      remain.se=sd(remains)/(bootstrap^0.5)
      result<-c(remain.mean,remain.sd,remain.se)
      names(result)<-c("remain.mean","remain.sd","remain.se")
      result
    }))
  }


  .cohension_compute <- function(network.raw){

    ASV_Correlation <- network.raw %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "ASV") %>%
      tidyr::pivot_longer(cols = -ASV, values_to = "Correlation", names_to = "ASV_to") %>%
      dplyr::filter(ASV != ASV_to) %>%
      dplyr::filter(Correlation != 0) %>%
      dplyr::group_by(ASV) %>%
      dplyr::mutate(r_pos_mean = mean(dplyr::if_else(Correlation > 0, Correlation, 0), na.rm = T),
                    r_neg_mean = mean(dplyr::if_else(Correlation < 0, Correlation, 0), na.rm = T),
                    t_total = mean(dplyr::if_else(Correlation < 0, abs(Correlation), abs(Correlation)), na.rm = T)) %>%
      dplyr::ungroup()  %>%
      dplyr::distinct(ASV, .keep_all = T) %>%
      dplyr::select(1,4,5,6)



    relative_abundance_df <- mat %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "ASV") %>%
      tidyr::pivot_longer(cols = -ASV, values_to = "relative_abundance", names_to = "Groups") %>%
      dplyr::group_by(ASV) %>%
      dplyr::summarise(mean_relative_abundance = mean(relative_abundance))


    ASV_cohensition <- ASV_Correlation %>%
      dplyr::left_join(relative_abundance_df, by = "ASV") %>%
      dplyr::mutate(positive_cohension = r_pos_mean * mean_relative_abundance,
                    negative_cohension = r_neg_mean * mean_relative_abundance,
                    total_cohension = t_total * mean_relative_abundance
      )

    cohension_list <- list(
      cohension_position = mean(ASV_cohensition$positive_cohension[ASV_cohensition$positive_cohension != 0]),
      cohension_negative = mean(ASV_cohensition$negative_cohension[ASV_cohensition$negative_cohension != 0])
    )


    return(cohension_list)
  }


  # function
  .network.efficiency <- function(ig){
    dd <- 1/igraph::distances(ig)
    diag(dd) <- NA
    efficiency <- mean(dd, na.rm=T)
    return(efficiency)
  }

  .info.centrality.vertex <- function(ig, net=NULL, verbose=F){
    if(is.null(net)) {
      net <- .network.efficiency(ig)
    }
    if(is.numeric(net)==F){
      net <- .network.efficiency(ig)
    }
    count <- c()
    for(i in 1:length(igraph::V(ig))){
      count <- c(count, (net - .network.efficiency(igraph::delete_vertices(ig, i)))/net)
      if(verbose){
        print(paste("node",i,"current\ info\ score", count[i], collapse="\t"))
      }
    }
    return(count)
  }

  # get node and edge number
  .get_topology <- function(ig){

    # compute network topology
    # node number
    node_number <- igraph::vcount(ig)

    # edge number
    edge_number <- igraph::ecount(ig)

    # degree
    degree_average <- mean(igraph::degree(ig, mode = "all"))
    degree_average_out <- mean(igraph::degree(ig, mode = "out"))
    degree_average_in <- mean(igraph::degree(ig, mode = "in"))
    degree_average_total <- mean(igraph::degree(ig, mode = "total"))
    # same output

    # mean_distance
    distance_mean <- igraph::mean_distance(ig, directed = FALSE)

    # diameter
    dia <- igraph::diameter(ig, directed = FALSE)

    # transitivity (global/local)
    clust_global <- igraph::transitivity(ig, type = "global")
    clust_local  <- mean(igraph::transitivity(ig, type = "local"), na.rm = T)

    # density
    density <- igraph::edge_density(ig)

    # shortest path between vertices
    network_efficiency <- .network.efficiency(ig)

    # Information Centrality
    network_info.centrality <- sum(.info.centrality.vertex(ig))

    # betweenness
    betweenness_vals <- mean(igraph::betweenness(ig), na.rm = T)
    betweenness_edge_vals <- mean(igraph::edge_betweenness(ig), na.rm = T)

    # closeness
    closeness_vals <- mean(igraph::closeness(ig), na.rm = T)
    closeness_vals_out <- mean(igraph::closeness(ig, mode = "out"), na.rm = T)
    closeness_vals_in <- mean(igraph::closeness(ig, mode = "in"), na.rm = T)
    closeness_vals_all <- mean(igraph::closeness(ig, mode = "all"), na.rm = T)
    closeness_vals_total <- mean(igraph::closeness(ig, mode = "total"), na.rm = T)
    # same output


    # eigen_centrality
    eigen_vals <- mean(igraph::eigen_centrality(ig)$vector, na.rm = T)

    # modularity
    fc <- igraph::cluster_fast_greedy(ig)
    modularity <- igraph::modularity(ig, igraph::membership(fc))

    # K-core decomposition of graphs
    kcore_vals  <- igraph::coreness(ig)
    kcore_mean  <- mean(kcore_vals, na.rm = T)
    kcore_max   <- max(kcore_vals, na.rm = T)
    kcore_min   <- min(kcore_vals, na.rm = T)

    # Cohension from igraph
    cohesion <- igraph::cohesion(ig)
    vertex_cohesion <- igraph::vertex_connectivity(ig)
    edge_cohesion   <- igraph::edge_connectivity(ig)

    # Cohension
    ## Positive Cohension
    position_cohension <- cohesion_out$cohension_position

    ## Negative Cohension
    negative_cohension <- cohesion_out$cohension_negative

    # Robustness
    ## Robustness_weight
    robustness_weight = mean(as.data.frame(Weighted.simu)[["remain.mean"]])

    ## Robustness_unweight
    robustness_unweight = mean(as.data.frame(Unweighted.simu)[["remain.mean"]])

    # Vulenrability
    vulenrability = max(.info.centrality.vertex(ig))


    # Stability

    out <- data.frame(
      Node = node_number,
      Edge = edge_number,
      Degree = degree_average,
      Distance = distance_mean,
      Diameter = dia,
      Density = density,
      Transitivity_global = clust_global,
      Transitivity_local = clust_local,
      Betweenness = betweenness_vals,
      Betweenness_edge = betweenness_edge_vals,
      Closeness = closeness_vals,
      Eigen_centrality = eigen_vals,
      Modularity = modularity,
      K_core_mean = kcore_mean,
      K_core_max = kcore_max,
      K_core_min = kcore_min,
      Network_efficiency = network_efficiency,
      Network_info.centrality = network_info.centrality,
      Cohension_Positive = position_cohension,
      Cohension_Negative = negative_cohension,
      Robustness_weight = robustness_weight,
      Robustness_unweight = robustness_unweight,
      Vulenrability = vulenrability,
      Stability = mean(c(robustness_weight, robustness_unweight))
    )

    return(out)


  }

  # compute
  if (is.null(mat)) {
    Weighted.simu <- data.frame(
      remain.mean = NA,
      remain.sd = NA,
      remain.se = NA
    )

    Unweighted.simu <- data.frame(
      remain.mean = NA,
      remain.sd = NA,
      remain.se = NA
    )

  }else{
    Weighted.simu <- .rmsimu(netRaw=network.raw,
                             rm.p.list=seq(0.05,1,by=0.05),
                             sp.ra=sp.ra2,
                             abundance.weighted=T,
                             bootstrap=bootstrap)

    Unweighted.simu <- .rmsimu(netRaw=network.raw,
                               rm.p.list=seq(0.05,1,by=0.05),
                               sp.ra=sp.ra2,
                               abundance.weighted=F,
                               bootstrap=bootstrap)
  }

  robustness <- data.frame(Proportion.removed = rep(seq(0.05,1,by=0.05),2),
                           rbind(Weighted.simu, Unweighted.simu),
                           weighted=rep(c("weighted", "unweighted"), each=length(seq(0.05,1,by=0.05)))
  )


  if (is.null(network.raw)) {

    cohesion_out <- data.frame(
      cohension_position = NA,
      cohension_negative = NA
    )

  }else{
    cohesion_out <- .cohension_compute(network.raw = network.raw)
  }


  # node topology
  network_topology <- .get_topology(ig = ig) %>%
    tibble::rownames_to_column(var = "ID") %>%
    tidyr::pivot_longer(cols = -ID, names_to = "Kind", values_to = "Value") %>%
    dplyr::select(-ID) %>%
    purrr::set_names(c("Topology", "Target_network"))

  network_topology



  # single thread
  if (isFALSE(parallel)) {

    future::plan(future::sequential)

    # random topology
    random_topology <- list()

    progressr::with_progress({
      p <- progressr::progressor(steps = bootstrap)

      # random network and topology
      for (i in 1:bootstrap) {

        random_graph <- igraph::sample_gnm(n = igraph::vcount(ig),
                                           m = igraph::ecount(ig),
                                           directed = F,
                                           loops = F)
        # 获取属性
        random_topology[[i]] <- .get_topology(ig = random_graph) %>%
          dplyr::mutate(Robustness_weight = NA,
                        Robustness_unweight = NA,
                        Cohension_Positive = NA,
                        Cohension_Negative = NA,
                        Stability = NA
          )

        p()

      }
    })


  }

  # multi-threading
  if (isTRUE(parallel)) {

    # ---- restore global states on exit (CRAN-friendly) ----
    old_size <- getOption("future.globals.maxSize")
    old_plan <- future::plan()

    on.exit({
      options(future.globals.maxSize = old_size)
      future::plan(old_plan)
    }, add = TRUE)

    options(future.globals.maxSize = 6 * 1024^3)


    future::plan(future::multisession, workers = n_workers)
    message("Number of parallel workers: ", n_workers)


    progressr::with_progress({
      p <- progressr::progressor(steps = bootstrap)

      # random topology
      random_topology <- future.apply::future_lapply(1:bootstrap, function(x){

        random_graph <- igraph::sample_gnm(n = igraph::vcount(ig),
                                           m = igraph::ecount(ig),
                                           directed = F,
                                           loops = F)

        # 获取属性
        r_topology <- .get_topology(ig = random_graph) %>%
          dplyr::mutate(Robustness_weight = NA,
                        Robustness_unweight = NA,
                        Cohension_Positive = NA,
                        Cohension_Negative = NA,
                        Stability = NA
          )

        p()
        r_topology

      },
      future.seed = T)

    })


  }

  random_topology_df <- do.call(rbind, random_topology)

  # output
  random_topology_df_mean <- colMeans(random_topology_df) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Kind") %>%
    purrr::set_names(c("Topology", "Random_nerwork"))

  # output topology
  topology_df <- dplyr::left_join(network_topology,
                                  random_topology_df_mean,
                                  by = "Topology"
  )

  out <-  list(topology = topology_df,
               Robustness = robustness
  )

  return(out)

}
