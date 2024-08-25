library(lmom)
out1 <- postdist(age=florida$post$cal.BP, prob=florida$post$probability,
         id=florida$post$UniqueID, idname='id', size=10000, outdata = TRUE)
round(out1$sample,2)

leodia <- bahamas$spec$sample[which(bahamas$spec$taxon == 'Leodia')]
tucetona <- bahamas$spec$sample[which(bahamas$spec$taxon == 'Tucetona')]
bah <- bahamas$post
group <- vector(length=nrow(bah))
group[which(bah$Specimen %in% leodia)] <- 'echinoid'
group[which(bah$Specimen %in% tucetona)] <- 'mollusk'
out3 <- data.frame(rep(NA,40))
for (i in unique(group)) {
  x <- bah[which(group == i),]
  out1 <- postdist(age=x$Age, prob=x$Probability,
                   id=x$Specimen, idname=i, size=10000, outdata = TRUE)
  out2 <- timeavestats(out1, times=100)
  out3 <- data.frame(out3, out2)
}
round(out3[,-1],2)

bahamas$spec$taxon
c <- timeavestats(out1, 100)
round(c,2)
str(out1$sample)
quantile(b[[2]], probs=c(0.025, 0.975))
out1$sample[5]
head(b[[1]])
cor(b[[1]][3,], b[[1]][4,])
b[[2]]
