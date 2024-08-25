library(timeaveraging)
?timeavestats

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

?timeavestats
