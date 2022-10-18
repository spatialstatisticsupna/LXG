proc.conf_mat <- function(conf_mat,metrics,pcts,conf_mats) {
  a<-conf_mat$table[2,2]
  b<-conf_mat$table[1,2]
  c<-conf_mat$table[2,1]
  d<-conf_mat$table[1,1]
  
  m <- metric(a,b,c,d)
  seed.metrics <- m[["metrics"]]
  metric.names <- c("DR","OE","CE","P","R","DC","Kappa","OA")
  metrics[seed,metric.names] <- c(seed.metrics[c("dr","oe","ce","p","r","dc")],
                                  conf_mat$overall["Kappa"],
                                  seed.metrics["oa"])
  seed.pcts <- m[["porcentajes"]]
  pcts.names <- c("pTP","pFN","pFP","pTN")
  pcts[seed,pcts.names] <- seed.pcts
  conf_mat.names <- c("TP","FN","FP","TN")
  conf_mats[seed,conf_mat.names] <- c(a,b,c,d)
  
  return(list(metrics,pcts,conf_mats))
}