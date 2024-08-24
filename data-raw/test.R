library(lmom)
out1 <- postdist(age=florida$post$cal.BP, prob=florida$post$probability,
         id=florida$post$UniqueID, idname='id', size=10000, outdata = TRUE)
round(out1$sample,2)

a <- c(0,7,5)
b <- timeavestats(out1, 1000)
mean(2*sqrt(apply(b, 1, function(x) var(x))-out1$sample[4]))
hist(apply(b, 1, function(x) 2*sd(x)))

citation('lmom')
data.frame(specid=colnames(out1$outdata), mean=apply(out1$outdata, 2, mean),
           out1$spec[,1:2])
frag <- florida$spec$UniqueID[which(florida$spec$Fragmentation=="fragment")]
comp <- florida$spec$UniqueID[which(florida$spec$Fragmentation=="complete")]
ech <- florida$spec$UniqueID[which(florida$spec$Taxon1 =="e")]
moll <- florida$spec$UniqueID[which(florida$spec$Taxon1 =="m")]
echf <- intersect(frag, ech)
echc <- intersect(comp, ech)
molf <- intersect(frag, moll)
molc <- intersect(comp, moll)
echfrag <- florida$post[which(florida$post$UniqueID %in% echf),]
echcomp <- florida$post[which(florida$post$UniqueID %in% echc),]
molfrag <- florida$post[which(florida$post$UniqueID %in% molf),]
molcomp <- florida$post[which(florida$post$UniqueID %in% molc),]
samplelist <- list(echfrag=echfrag, echcomp=echcomp,
                   molfrag=molfrag, molcomp=molcomp)
str(samplelist)
str(out2)
out2 <- lapply(samplelist, function(x) postdist(x$cal.BP, x$probability,
               x$UniqueID, 'id', 10000, TRUE))
out3 <- sapply(out2, '[[', 3)
str(out3)
round(out3, 2)
IQR(out3$echfrag$mean)

plot(out1$var, out1$ex.var, xlab='Monte Carlo variance (i=10000)',
     ylab='exact variance')
r2 <- cor(out1$var, out1$ex.var)^2
r3 <- round(r2,abs(floor(log10(1-r2))))
mtext(side=3, line=-1.5, bquote(r^2==.(r3)))
abline(a=0, b=1)
hist(sqrt(out1$var)-sqrt(out1$ex.var),main='', breaks=seq(-5, 5, 0.1))
