a1new.15len <- get.15LenAu(a1clu, pubList)
a2new.15len <- get.15LenAu(a2clu, pubList)
a3new.15len <- get.15LenAu(a3clu, pubList)
a4new.15len <- get.15LenAu(a4clu, pubList)
a5new.15len <- get.15LenAu(a5clu, pubList)
a6new.15len <- get.15LenAu(a6clu, pubList)
a7new.15len <- get.15LenAu(a7clu, pubList)
a8new.15len <- get.15LenAu(a8clu, pubList)
a9new.15len <- get.15LenAu(a9clu, pubList)
a10new.15len <- get.15LenAu(a10clu, pubList)
a11new.15len <- get.15LenAu(a11clu, pubList)

fulau.list <- lapply(pubList, getAu)
areaClu <- list()
for(i in 1:length(clusters)){
        areaClu[[i]]  <- sapply(clusters[[i]], countAu, totList = fulau.list)
}
area.15len <- get.15LenAu(areaClu, pubList)

area.ts <- getAreaNormTS2(area.15len, pubList)

role <- read.table(file.choose())
net1 <- readLines(file.choose())
net1 <- net1[2 : 60458]
net1 <- strsplit(net1, '"')

role.code <- as.character()
aus <- as.character()
for(i in 1:length(net1)){
        aus[i] <- net1[[i]][2]
        if(as.numeric(net1[[i]][1]) %in% role$V1){
                role.code[i] <- as.character(role[which(role$V1 == as.numeric(net1[[i]][1])), 7])
        }
        else{
                role.code[i] <- 'NA'
        }
}
au.role <- data.frame(aus, role.code)

area.names <- as.character()
for(i in 1:length(area.15len)){
        if(area.15len[i] %in% au.role[, 1]){
                role.code <- au.role[which(area.15len[i] == au.role[, 1]), 2]
        }
        area.names[i] <- paste(role.code, '-', area.15len[i], sep = '')
}
names(area.ts) <- area.names

library(cluster)
library(dtw)
areaD <-  dist(area.ts, method = 'DTW')
agn.area <- agnes(areaD)
jpeg("agn_tree.jpeg", width = 6000, height = 3000)
plot(as.dendrogram(agn.area))
dev.off()
labs.area <- gsub('-[A-Z].*', '', agn.area$order.lab)

autoPlot3(area.ts[agn.area$order], 'area')

par(mfrow = c(3, 1))
plot(TTSnew[[322]], type = 'o', xaxt = 'n', xlab = 'Year', ylab = '', ylim = c(-2.3, 3.95), cex.lab = 2.5,
        cex.axis = 2.5, cex.sub = 2.5)
abline(h = 0, col = 2, lty = 2)
title(main = paste(names(TTSnew[322]), sep = ''), cex.main = 2.5)
axis(1, 1:length(TTSnew[[322]]), cex.axis = 2.5)
plot(TTSnew[[204]], type = 'o', xaxt = 'n', xlab = 'Year', ylab = '', ylim = c(-2.3, 3.95), cex.lab = 2.5,
        cex.axis = 2.5, cex.sub = 2.5)
abline(h = 0, col = 2, lty = 2)
title(main = paste(names(TTSnew[204]), sep = ''), cex.main = 2.5)
axis(1, 1:length(TTSnew[[204]]), cex.axis = 2.5)
plot(TTSnew[[210]], type = 'o', xaxt = 'n', xlab = 'Year', ylab = '', ylim = c(-2.3, 3.95), cex.lab = 2.5,
        cex.axis = 2.5, cex.sub = 2.5)
abline(h = 0, col = 2, lty = 2)
title(main = paste(names(TTSnew[210]), sep = ''), cex.main = 2.5)
axis(1, 1:length(TTSnew[[210]]), cex.axis = 2.5)

dtw(TTSnew[[322]], TTSnew[[210]])$distance
dtw(TTSnew[[204]], TTSnew[[322]])$distance
dtw(TTSnew[[204]], TTSnew[[322]], window.type = 'slanted', window.size = 3)$distance
dtw(TTSnew[[322]], TTSnew[[210]], window.type = 'slanted', window.size = 3)$distance

Diss.DTW <- matrix(0, nrow = length(TTSnew), ncol = length(TTSnew))
for(i in 1:nrow(Diss.DTW )){
        for(j in 1:ncol(Diss.DTW )){
                Diss.DTW [i, j] <- dtw(TTSnew[[i]], TTSnew[[j]], window.type = 'slanted', window.size = 3)$distance
        }
}
names(TTSnew) -> rownames(Diss.DTW ) -> colnames(Diss.DTW) 
Diss.DTW <- as.dist(Diss.DTW )
agn.dtw.w <- agnes(Diss.DTW)

jpeg("agn_tree.window.jpeg", width = 6000, height = 3000)
plot(agn.dtw.w)
dev.off()
autoPlot3(TTSnew[agn.dtw.w$order], 'New Cut')


field.D <- matrix(0, nrow = length(field.ts), ncol = length(field.ts))
for(i in 1:nrow(field.D )){
        for(j in 1:ncol(field.D )){
                field.D [i, j] <- dtw(field.ts[[i]], field.ts[[j]], window.type = 'slanted', window.size = 3)$distance
        }
}
names(field.ts) -> rownames(field.D ) -> colnames(field.D) 
field.D <- as.dist(field.D )
agn.field <- agnes(field.D)


jpeg("agn_tree.window.jpeg", width = 8000, height = 3500)
plot(agn.field)
abline(h = 16.388, lty = 2, col = 2)
dev.off()

autoPlot3(field.ts[agn.field$order], 'Windowed')

cut(as.dendrogram(agn.field), h = 16.388)

field.gn40 <- getOrders(agn.field, 16.388)
for(i in 1:40){
        tsList <- field.ts[agn.field$order[field.gn40[[i]]]]
        autoPlot3(tsList, paste('group', as.character(i), sep = ''))
}

#test samples
field.sample <- field.ts[sample(1:length(field.ts), 50)]
field.sample.D <- matrix(0, nrow = length(field.sample), ncol = length(field.sample))
for(i in 1:nrow(field.sample.D)){
        for(j in 1:ncol(field.sample.D)){
                field.sample.D [i, j] <- dtw(field.sample[[i]], field.sample[[j]], window.type = 'slanted', window.size = 3)$distance
        }
}
names(field.sample) -> rownames(field.sample.D ) -> colnames(field.sample.D) 
field.D <- as.dist(field.sample.D )
agn.field.sample <- agnes(field.sample.D)

autoPlot3(field.sample[agn.field.sample$order], 'WinSize = 3,')

cut(as.dendrogram(agn.dtw.w), h = 16)
area.gn40 <- getOrders(agn.dtw.w, 16)
for(i in 1:40){
        tsList <- TTSnew[agn.dtw.w$order[area.gn40[[i]]]]
        autoPlot3(tsList, paste('group', as.character(i), sep = ''))
}

jpeg("agn_tree.window.jpeg", width = 6500, height = 3300)
plot(agn.dtw.w)
abline(h = 16, lty = 2, col = 2)
dev.off()

area.sample <- TTSnew[sample(1:length(TTSnew), 50)]
area.sample.D <- matrix(0, nrow = length(area.sample), ncol = length(area.sample))
for(i in 1:nrow(area.sample.D)){
        for(j in 1:ncol(area.sample.D)){
                area.sample.D [i, j] <- dtw(area.sample[[i]], area.sample[[j]], window.type = 'slanted', window.size = 3)$distance
        }
}
names(area.sample) -> rownames(area.sample.D ) -> colnames(area.sample.D) 
area.D <- as.dist(area.sample.D )
agn.area.sample <- agnes(area.sample.D)

autoPlot3(area.sample[agn.area.sample$order], 'WinSize = 3,')

#relationship between role and clusters under field:
as.dendrogram(agn.field)[[1]][[1]][[1]][[1]][[1]][[1]][[1]]

c8.order1 <- c(1:121)
c8.order2 <- c(122:319)
c8.order3 <- c(320:376)
c8.order4 <- c(377:380)
c8.order5 <- c(381:392)
c8.order6 <- c(393:610)
c8.order7 <- c(611:705)
c8.order8 <- c(706:749)

c8.ts1 <- field.ts[agn.field$order[c8.order1]]
c8.ts2 <- field.ts[agn.field$order[c8.order2]]
c8.ts3 <- field.ts[agn.field$order[c8.order3]]
c8.ts4 <- field.ts[agn.field$order[c8.order4]]
c8.ts5 <- field.ts[agn.field$order[c8.order5]]
c8.ts6 <- field.ts[agn.field$order[c8.order6]]
c8.ts7 <- field.ts[agn.field$order[c8.order7]]
c8.ts8 <- field.ts[agn.field$order[c8.order8]]

autoPlot3(c8.ts1, 'cluster1 ')
autoPlot3(c8.ts2, 'cluster2 ')
autoPlot3(c8.ts3, 'cluster3 ')
autoPlot3(c8.ts4, 'cluster4 ')
autoPlot3(c8.ts5, 'cluster5 ')
autoPlot3(c8.ts6, 'cluster6 ')
autoPlot3(c8.ts7, 'cluster7 ')
autoPlot3(c8.ts8, 'cluster8 ')

labs.field <- gsub('-[A-Z].*', '', agn.field$order.lab)
c8.1 <- table(labs.field[c8.order1])
c8.2 <- table(labs.field[c8.order2])
c8.3 <- table(labs.field[c8.order3])
c8.4 <- table(labs.field[c8.order4])
c8.5 <- table(labs.field[c8.order5])
c8.6 <- table(labs.field[c8.order6])
c8.7 <- table(labs.field[c8.order7])
c8.8 <- table(labs.field[c8.order8])

sortTB.role <- function(tb){
        #library(gtools) #mixedorder()
        lab <- as.character()
        #for(i in 1:10) lab[i] <- paste('a', i, sep = '') #here we have 10 areas to be labbelled
        lab <- c('R7', 'R6', 'R5', 'R4', 'R3', 'R2', 'R1', 'NA')
        arr <- rep(0, length(lab))
        for(i in 1:length(lab)){
                if(lab[i] %in% names(tb)){
                        arr[i] <- tb[which(lab[i] == names(tb))]
                }
        }
        names(arr) <- lab
return(arr)
}

c8 <- cbind(sortTB.role(c8.1), sortTB.role(c8.2), sortTB.role(c8.3), sortTB.role(c8.4), sortTB.role(c8.5), 
                sortTB.role(c8.6), sortTB.role(c8.7), sortTB.role(c8.8))

clu8 <- as.character()
for(i in 1:8) clu8[i] <- paste('cluster', as.character(i), sep = '')
colnames(c8) <- clu8

c8 <- c8 / rowSums(c8)

barplot(t(c8), col = rainbow(nrow(t(c8))), names.arg = rownames(c8), space = 0.1)
legend('topleft', rownames(t(c8)), cex = 0.8, fill = rainbow(nrow(t(c8))))
title(main = 'Barplot with Eight Clusters from DTW Slanted Window Metric (Proportion)')

#test relationship
hub.node <- apply(c8[1:3, ], 2, sum)
none.hub.node <- apply(c8[4:7, ], 2, sum)
chisq.test(cbind(hub.node, none.hub.node), simulate.p.value = T, B = 5000)

procfreq <- function(x, digits=4) {
        total <- sum(x)
        rowsum <- apply(x,1,sum)
        colsum <- apply(x,2,sum)
        prop <- x/total
        rowprop <- sweep(x,1,rowsum,"/")
        colprop <- sweep(x,2,colsum,"/")
        expected <- (matrix(rowsum) %*% t(matrix(colsum))) / total
        dimnames(expected) <- dimnames(x)
        resid <- (x-expected)/sqrt(expected)
        adj.resid <- resid / sqrt((1-matrix(rowsum)/total) %*% t(1-matrix(colsum)/total))
        df <- prod(dim(x)-1)
        X2 <- sum(resid^2)
        attr(X2,"P-value") <- 1-pchisq(X2,df)
        ## Must be careful about zero freqencies. Want 0*log(0) = 0.
        tmp <- x*log(x/expected)
        tmp[x==0] <- 0
        G2 <- 2 * sum(tmp)
        attr(G2,"P-value") <- 1-pchisq(G2,df)
        ls <- list(sample.size=total,
                row.totals=rowsum,
                col.totals=colsum,
                overall.proportions=prop,
                row.proportions=rowprop,
                col.proportions=colprop,
                expected.freqs=expected,
                residuals=resid,
                adjusted.residuals=adj.resid,
                chi.square=X2,
                likelihood.ratio.stat=G2,
                df=df)
return(ls)
}

procfreq(cbind(hub.node, none.hub.node))
p.table <- matrix(0, nrow = 3, ncol = 3)
for(i in 1:3){
        for(j in 1:3){
                p.table[i, j] <- chisq.test(cbind(c8[i, ], c8[j, ]))$p.value
        }
}

#analysis on c8.ts2
clu2.D <- matrix(0, nrow = length(c8.ts2), ncol = length(c8.ts2))
for(i in 1:nrow(clu2.D)){
        for(j in 1:ncol(clu2.D)){
                clu2.D [i, j] <- dtw(c8.ts2[[i]], c8.ts2[[j]], window.type = 'slanted', window.size = 3)$distance
        }
}
names(c8.ts2) -> rownames(clu2.D ) -> colnames(clu2.D) 
clu2.D <- as.dist(clu2.D)
agn.clu2 <- agnes(clu2.D)

plot(agn.clu2)
plot(as.dendrogram(agn.field)[[1]][[1]][[1]][[1]][[1]][[1]][[2]])

cut(as.dendrogram(agn.field)[[1]][[1]][[1]][[1]][[1]][[1]][[2]], h = 16)
jpeg('cluster2.jpeg', width = 3000, height = 1200)
plot(as.dendrogram(agn.field)[[1]][[1]][[1]][[1]][[1]][[1]][[2]])
abline(h = 16, col = 2, lty = 2)
dev.off()

jpeg('cluster2.2.jpeg', width = 3000, height = 1200)
plot(as.dendrogram(agn.clu2))
abline(h = 16, col = 2, lty = 2)
dev.off()

#save cluster2 data to a json file 
library(rjson)
sink('json.txt')
cat(toJSON('c8.ts2'))
sink()


c9.order1 <- c(1:121)
c9.order2 <- c(122:273)
c9.order3 <- c(274:319)
c9.order4 <- c(320:376)
c9.order5 <- c(377:380)
c9.order6 <- c(381:392)
c9.order7 <- c(393:610)
c9.order8 <- c(611:705)
c9.order9 <- c(706:749)

c9.ts1 <- field.ts[agn.field$order[c9.order1]]
c9.ts2 <- field.ts[agn.field$order[c9.order2]]
c9.ts3 <- field.ts[agn.field$order[c9.order3]]
c9.ts4 <- field.ts[agn.field$order[c9.order4]]
c9.ts5 <- field.ts[agn.field$order[c9.order5]]
c9.ts6 <- field.ts[agn.field$order[c9.order6]]
c9.ts7 <- field.ts[agn.field$order[c9.order7]]
c9.ts8 <- field.ts[agn.field$order[c9.order8]]
c9.ts9 <- field.ts[agn.field$order[c9.order9]]

autoPlot3(c9.ts1, 'cluster1 ')
autoPlot3(c9.ts2, 'cluster2 ')
autoPlot3(c9.ts3, 'cluster3 ')
autoPlot3(c9.ts4, 'cluster4 ')
autoPlot3(c9.ts5, 'cluster5 ')
autoPlot3(c9.ts6, 'cluster6 ')
autoPlot3(c9.ts7, 'cluster7 ')
autoPlot3(c9.ts8, 'cluster8 ')
autoPlot3(c9.ts9, 'cluster9 ')

c9.1 <- table(labs.field[c9.order1])
c9.2 <- table(labs.field[c9.order2])
c9.3 <- table(labs.field[c9.order3])
c9.4 <- table(labs.field[c9.order4])
c9.5 <- table(labs.field[c9.order5])
c9.6 <- table(labs.field[c9.order6])
c9.7 <- table(labs.field[c9.order7])
c9.8 <- table(labs.field[c9.order8])
c9.9 <- table(labs.field[c9.order9])

c9 <- cbind(sortTB.role(c9.1), sortTB.role(c9.2), sortTB.role(c9.3), sortTB.role(c9.4), sortTB.role(c9.5), 
                sortTB.role(c9.6), sortTB.role(c9.7), sortTB.role(c9.8), sortTB.role(c9.9))

clu9 <- as.character()
for(i in 1:9) clu9[i] <- paste('cluster', as.character(i), sep = '')
colnames(c9) <- clu9

procfreq(c9)
#c9 <- c9 / rowSums(c9)
barplot(t(c9), col = rainbow(nrow(t(c9))), names.arg = rownames(c9), space = 0.1)
legend('topleft', rownames(t(c9)), cex = 0.8, fill = rainbow(nrow(t(c9))))
title(main = 'Barplot with Eight Clusters from DTW Slanted Window Metric (Proportion)')

getLabs <- function(TSlist, gpName = 'group'){
        labs <- as.character()
        for(i in 1:length(TSlist)){
                labs[i] <- paste(gpName, 'P.', as.character(i), ' -- ', names(TSlist[i]), sep = '')
        }
return(labs)
}

c9.1.labs <- getLabs(c9.ts1, 'C.1')
c9.2.labs <- getLabs(c9.ts2, 'C.2')
c9.3.labs <- getLabs(c9.ts3, 'C.3')
c9.4.labs <- getLabs(c9.ts4, 'C.4')
c9.5.labs <- getLabs(c9.ts5, 'C.5')
c9.6.labs <- getLabs(c9.ts6, 'C.6')
c9.7.labs <- getLabs(c9.ts7, 'C.7')
c9.8.labs <- getLabs(c9.ts8, 'C.8')
c9.9.labs <- getLabs(c9.ts9, 'C.9')

c9.labs <- c(c9.1.labs, c9.2.labs, c9.3.labs, c9.4.labs, c9.5.labs, c9.6.labs, 
                c9.7.labs, c9.8.labs, c9.9.labs)
agn.field.c9 <- agn.field
agn.field.c9$order.lab <- c9.labs
jpeg("agn_tree.window.c9.jpeg", width = 9000, height = 4000)
plot(agn.field.c9)
dev.off()
