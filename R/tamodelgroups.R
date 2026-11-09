tamodels.gp <- function(x, times=1000, t=100) {
  out <- lapply(x, tamodels, times=times, t=t)
  sam.est <- sapply(out, `[[`, 'estimate')
  models <- do.call(rbind, lapply(out, `[[`, 'models'))
  models.AIC <- data.frame(samples=names(x),
                      models[rownames(models)=='AIC',],
                      row.names=NULL)
  n.models <- ncol(models.AIC) - 1
  sel.model.AIC <- apply(models.AIC, 1, function(x) which(x == min(x)))
  best.AIC <- colnames(models.AIC)[sel.model.AIC]
  names(best.AIC) <- names(x)
  models.BIC <- data.frame(samples=names(out),
                         models[rownames(models)=='BIC',],
                         row.names=NULL)
  sel.model.BIC <- apply(models.BIC, 1, function(x) which(x == min(x)))
  best.BIC <- colnames(models.BIC)[sel.model.BIC]
  names(best.BIC) <- names(x)
  mc.out <- do.call(rbind, lapply(out, `[[`, 'monte.carlo'))
  mc.estimates <- data.frame(sample = rep(names(out), each=n.models),
                             model = rownames(out[[1]]$monte.carlo),
                             mc.out,
                             BICn=rep(best.BIC, each=n.models),
                             AICn=rep(best.AIC, each=n.models),
                             row.names=NULL)
  mc.estimates$AIC <- as.numeric(mc.estimates$model == mc.estimates$AICn)
  mc.estimates$BIC <- as.numeric(mc.estimates$model == mc.estimates$BICn)
  mc.prefinal <- mc.estimates[ , !(colnames(mc.estimates) %in% c('AICn', 'BICn'))]
  mc.final <- mc.prefinal[which(mc.prefinal$AIC == 1 | mc.prefinal$BIC == 1), ]
  return(list(AIC = models.AIC,
              BIC = models.BIC,
              completness=sam.est,
              MC = mc.final,
              iterations = out[[1]]$iterations,
              time.bin = out[[1]]$time.bin))
}
res1 <- tagroups(group=bahamas$spec$taxon, id=bahamas$spec$sample,
                 mcid=bahamas$post$Specimen, mcprob=bahamas$post$Probability,
                 mcage=bahamas$post$Age)
out1 <- lapply(res1$mcdist, function(x) apply(x, 1, median))
tamodels.gp(out1, t=50)

a <- NULL
a <- rnorm(50, 2000, 30)
tamodelgroups(out1)
