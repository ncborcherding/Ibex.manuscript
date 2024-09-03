#Adapted from https://bioconductor.org/books/3.14/OSCA.advanced/clustering-redux.html
library(ggplot2)
library(patchwork)
library(viridis)
plotSweep <- function(out, matrix) {
  df <- as.data.frame(out$parameters)
  df$num.clusters <- vapply(as.list(out$clusters), function(cluster) { 
  length(unique(cluster))
}, 0L)

  all.sil <- lapply(as.list(out$clusters), function(cluster) {
    if(length(levels(cluster)) > 1) {
      sil <- approxSilhouette(matrix, cluster)
      val <- mean(sil$width)
    } else {
      val <- NA
    }
    val
  })
  df$silhouette <- unlist(all.sil)

  all.wcss <- lapply(as.list(out$clusters), function(cluster) {
    sum(clusterRMSD(matrix, cluster, sum=TRUE), na.rm=TRUE)
  })
  df$wcss <- unlist(all.wcss)
  
 

    plot1 <- ggplot(df, aes(x=k, y=num.clusters, group=cluster.fun, color=cluster.fun)) + 
                  geom_line(lwd=2) + 
                  scale_y_log10() + 
                  scale_color_viridis(discrete = TRUE) + 
                  theme_clean()
    plot2 <- ggplot(df, aes(x=k, y=silhouette, group=cluster.fun, color=cluster.fun)) + 
                  geom_line(lwd=2) + 
                  scale_color_viridis(discrete = TRUE) + 
                  theme_clean()
    plot3 <- ggplot(df, aes(x=k, y=wcss, group=cluster.fun, color=cluster.fun)) + 
                  geom_line(lwd=2) + 
                  scale_color_viridis(discrete = TRUE) + 
                  theme_clean()
    
    plot <- plot1 + plot2 + plot3 + plot_layout(ncol = 3, guides = "collect")

  return(plot)
}
