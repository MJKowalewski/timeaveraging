library(timeaveraging)
master <- read.csv("C:/Users/kowalewski/UFL Dropbox/Michal Kowalewski/R PROJECTS/timeaveraging/data-raw/ppd2024-04-21.csv")
exter <- read.csv("C:/Users/kowalewski/UFL Dropbox/Michal Kowalewski/R PROJECTS/timeaveraging/data-raw/spcm2024-04-21.csv")
colnames(exter)
master2 <- master[1000,]
size <- 100
multiply <- function(x) sample(x[,1], size=size, replace=T, prob=x[,2])
x <- split(data.frame(master$age, master$probability),master$udid)
y <- unlist(lapply(x, multiply))
z <- data.frame(id=rep(unique(master$udid), each=size), age=y)
master[which(tapply(master$age, master$udid, max) < 0),]
out1 <- postdist(age=master$age, prob=master$probability,
                 id=master$udid, idname='all', size=10000, outdata = TRUE)
medianF <- function(x) {
  c <- cumsum(x[,2])/sum(x[,2]) - 0.5
  (x[,1][which(c == min(c[c>=0]))] + x[,1][which(-c == min(abs(c[c<=0])))])/2
}
median(cbind(c(-1,-5,-12), c(1/3,1/3,1/3)))
for(i in unique(sid)) {
  which()
}

head(exter)
View(out1$outdata)
is.numeric(master$age)
head(master)
warnings()
sort(tapply(master$age, master$udid, max))
out1 <- postdist(age=master$age, prob=master$probability,
                 id=master$udid, idname='all', size=100, outdata = TRUE)
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
  out2 <- timeavestats(out1, times=1000)
  out3 <- data.frame(out3, out2)
}
round(out3[,-1],2)

