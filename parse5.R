#to make changes on the plots
#assign names to the areas

a1names <- as.character()
a2names <- as.character()
a3names <- as.character()
a4names <- as.character()
a5names <- as.character()
a6names <- as.character()
a7names <- as.character()
a8names <- as.character()
a9names <- as.character()
a10names <- as.character()

for(i in 1:length(a1fulen)) a1names[i] <- paste('(a1)', a1fulen[i], sep = '')
for(i in 1:length(a2fulen)) a2names[i] <- paste('(a2)', a2fulen[i], sep = '')
for(i in 1:length(a3fulen)) a3names[i] <- paste('(a3)', a3fulen[i], sep = '')
for(i in 1:length(a4fulen)) a4names[i] <- paste('(a4)', a4fulen[i], sep = '')
for(i in 1:length(a5fulen)) a5names[i] <- paste('(a5)', a5fulen[i], sep = '')
for(i in 1:length(a6fulen)) a6names[i] <- paste('(a6)', a6fulen[i], sep = '')
for(i in 1:length(a7fulen)) a7names[i] <- paste('(a7)', a7fulen[i], sep = '')
for(i in 1:length(a8fulen)) a8names[i] <- paste('(a8)', a8fulen[i], sep = '')
for(i in 1:length(a9fulen)) a9names[i] <- paste('(a9)', a9fulen[i], sep = '')
for(i in 1:length(a10fulen)) a10names[i] <- paste('(a10)', a10fulen[i], sep = '')

rownames(a1NTS) <- a1names
rownames(a2NTS) <- a2names
rownames(a3NTS) <- a3names
rownames(a4NTS) <- a4names
rownames(a5NTS) <- a5names
rownames(a6NTS) <- a6names
rownames(a7NTS) <- a7names
rownames(a8NTS) <- a8names
rownames(a9NTS) <- a9names
rownames(a10NTS) <- a10names

TNTS <- rbind(a1NTS, a2NTS, a3NTS, a4NTS, a5NTS, a6NTS, a7NTS, a8NTS, a9NTS, a10NTS)

library(cluster)
agn <- agnes(TNTS)
#plot(agn)

#this part is for grouping clusters
g4.order1 <- c(1:29)
g4.order2 <- c(30:45)
g4.order3 <- c(46:78)
g4.order4 <- c(79:92)

g4.ts1 <- TNTS[agn$order[g4.order1], ]
g4.ts2 <- TNTS[agn$order[g4.order2], ]
g4.ts3 <- TNTS[agn$order[g4.order3], ]
g4.ts4 <- TNTS[agn$order[g4.order4], ]

autoPlot2 <- function(TSs, gpName = 'group'){
        for(i in 1:nrow(TSs)){
                ts <- TSs[i, ]
                fileName <- paste(gpName, 'plot ', as.character(i), '.jpeg', sep = '')
                jpeg(fileName, width = 1600, height = 800)
                plot(ts, type = 'o', xaxt = 'n', xlab = '', ylim = c(-2.2, 3.65))
                abline(h = 0, col = 2, lty = 2)
                title(main = paste(gpName, 'plot ', as.character(i), ' / ', rownames(TSs)[i], sep = ''))
                axis(1, 1:length(ts), as.character(1991:2010))
                dev.off()
        }
}

autoPlot2(g4.ts1, 'group1')
autoPlot2(g4.ts2, 'group2')
autoPlot2(g4.ts3, 'group3')
autoPlot2(g4.ts4, 'group4')

#for seven clusters
g7.order1 <- c(1:29)
g7.order2 <- c(30:45)
g7.order3 <- c(46:57)
g7.order4 <- c(58:74)
g7.order5 <- c(75:78)
g7.order6 <- c(79:84)
g7.order7 <- c(85:92)

g7.ts1 <- TNTS[agn$order[g7.order1], ]
g7.ts2 <- TNTS[agn$order[g7.order2], ]
g7.ts3 <- TNTS[agn$order[g7.order3], ]
g7.ts4 <- TNTS[agn$order[g7.order4], ]
g7.ts5 <- TNTS[agn$order[g7.order5], ]
g7.ts6 <- TNTS[agn$order[g7.order6], ]
g7.ts7 <- TNTS[agn$order[g7.order7], ]

autoPlot2(g7.ts1, 'group1')
autoPlot2(g7.ts2, 'group2')
autoPlot2(g7.ts3, 'group3')
autoPlot2(g7.ts4, 'group4')
autoPlot2(g7.ts5, 'group5')
autoPlot2(g7.ts6, 'group6')
autoPlot2(g7.ts7, 'group7')

#A1--KLEIN4, ML
(ts1 <- addZero(table(sapply(getAuPub(a1fulen[25], area1List), getYear))))
(ts1 - mean(ts1))/sd(ts1)

#get authors with 15 or more activity years
get.15LenAu <- function(ls, area){
        mp <- as.character()
        for(i in 1:length(ls)){
                if(max(ls[[i]] != 0)){
                        if(sum(ls[[i]] == max(ls[[i]])) > 1){
                                maxActRange <- 0
                                author <- as.character()
                                for(j in 1:sum(ls[[i]] == max(ls[[i]]))){
                                        if(diff(range(sapply(getAuPub(names(ls[[i]][ls[[i]] == max(ls[[i]])][j]), area), getYear))) > maxActRange){
                                                maxActRange <- diff(range(sapply(getAuPub(names(ls[[i]][ls[[i]] == max(ls[[i]])][j]), area), getYear)))
                                                author <- names(ls[[i]][ls[[i]] == max(ls[[i]])][j])
                                        }
                                }
                                if(maxActRange > 14){
                                        mp[length(mp) + 1] <- author
                                }
                        }else{
                                if(diff(range(sapply(getAuPub(names(ls[[i]][which(ls[[i]] == max(ls[[i]]))]), area), getYear))) > 14){
                                        mp[length(mp) + 1] <- names(ls[[i]][which(ls[[i]] == max(ls[[i]]))])
                                }
                        }
                }
        }
return(mp)
}

a1.15len <- get.15LenAu(a1clu, area1List)
a2.15len <- get.15LenAu(a2clu, area2List)
a3.15len <- get.15LenAu(a3clu, area3List)
a4.15len <- get.15LenAu(a4clu, area4List)
a5.15len <- get.15LenAu(a5clu, area5List)
a6.15len <- get.15LenAu(a6clu, area6List)
a7.15len <- get.15LenAu(a7clu, area7List)
a8.15len <- get.15LenAu(a8clu, area8List)
a9.15len <- get.15LenAu(a9clu, area9List)
a10.15len <- get.15LenAu(a10clu, area10List)
a11.15len <- get.15LenAu(a11clu, area11List)

#this function gets normalized ts in list
getAreaNormTS2 <- function(l15, area){
        ts <-  list()
        for(i in 1:length(l15)){
                t <- addZero(table(sapply(getAuPub(l15[i], area), getYear)))
                t <- (t - mean(t)) / sd(t)
                ts[[length(ts)+1]] <- t
        }
return(ts)
}

#set list names first
a1names <- as.character()
a2names <- as.character()
a3names <- as.character()
a4names <- as.character()
a5names <- as.character()
a6names <- as.character()
a7names <- as.character()
a8names <- as.character()
a9names <- as.character()
a10names <- as.character()
a11names <- as.character()

for(i in 1:length(a1.15len)) a1names[i] <- paste('(a1)', a1.15len[i], sep = '')
for(i in 1:length(a2.15len)) a2names[i] <- paste('(a2)', a2.15len[i], sep = '')
for(i in 1:length(a3.15len)) a3names[i] <- paste('(a3)', a3.15len[i], sep = '')
for(i in 1:length(a4.15len)) a4names[i] <- paste('(a4)', a4.15len[i], sep = '')
for(i in 1:length(a5.15len)) a5names[i] <- paste('(a5)', a5.15len[i], sep = '')
for(i in 1:length(a6.15len)) a6names[i] <- paste('(a6)', a6.15len[i], sep = '')
for(i in 1:length(a7.15len)) a7names[i] <- paste('(a7)', a7.15len[i], sep = '')
for(i in 1:length(a8.15len)) a8names[i] <- paste('(a8)', a8.15len[i], sep = '')
for(i in 1:length(a9.15len)) a9names[i] <- paste('(a9)', a9.15len[i], sep = '')
for(i in 1:length(a10.15len)) a10names[i] <- paste('(a10)', a10.15len[i], sep = '')
for(i in 1:length(a11.15len)) a11names[i] <- paste('(a11)', a11.15len[i], sep = '')



a1TSnew <- getAreaNormTS2(a1.15len, area1List)
names(a1TSnew) <- a1names
a2TSnew <- getAreaNormTS2(a2.15len, area2List)
names(a2TSnew) <- a2names
a3TSnew <- getAreaNormTS2(a3.15len, area3List)
names(a3TSnew) <- a3names
a4TSnew <- getAreaNormTS2(a4.15len, area4List)
names(a4TSnew) <- a4names
a5TSnew <- getAreaNormTS2(a5.15len, area5List)
names(a5TSnew) <- a5names
a6TSnew <- getAreaNormTS2(a6.15len, area6List)
names(a6TSnew) <- a6names
a7TSnew <- getAreaNormTS2(a7.15len, area7List)
names(a7TSnew) <- a7names
a8TSnew <- getAreaNormTS2(a8.15len, area8List)
names(a8TSnew) <- a8names
a9TSnew <- getAreaNormTS2(a9.15len, area9List)
names(a9TSnew) <- a9names
a10TSnew <- getAreaNormTS2(a10.15len, area10List)
names(a10TSnew) <- a10names
a11TSnew <- getAreaNormTS2(a11.15len, area11List)
names(a11TSnew) <- a11names

TTSnew <- c(a1TSnew, a2TSnew, a3TSnew, a4TSnew, a5TSnew, a6TSnew, a7TSnew, a8TSnew, a9TSnew, a10TSnew, a11TSnew)

#new cluster using DTW as distance 
library(cluster)
library(dtw)
distD <-  dist(TTSnew, method = 'DTW')
agn <- agnes(distD)
plot(agn)
jpeg("agn_tree.jpeg", width = 6000, height = 3000)
plot(as.dendrogram(agn))
dev.off()
labs <- gsub('[A-Z].*', '', agn$order.lab)
as.dendrogram(agn)
gn7.order1 <- c(1:96)
gn7.order2 <- c(97:175)
gn7.order3 <- c(176:197)
gn7.order4 <- c(198:446)
gn7.order5 <- c(447:453)
gn7.order6 <- c(454:517)
gn7.order7 <- c(518:524)


gn7.1 <- table(labs[gn7.order1])
gn7.2 <- table(labs[gn7.order2])
gn7.3 <- table(labs[gn7.order3])
gn7.4 <- table(labs[gn7.order4])
gn7.5 <- table(labs[gn7.order5])
gn7.6 <- table(labs[gn7.order6])
gn7.7 <- table(labs[gn7.order7])

sortTB <- function(tb){
        #library(gtools) #mixedorder()
        lab <- as.character()
        #for(i in 1:10) lab[i] <- paste('a', i, sep = '') #here we have 10 areas to be labbelled
        lab <- c('(a10)', '(a8)', '(a5)', '(a9)', '(a2)', '(a6)', '(a3)', '(a4)', '(a7)', '(a1)')
        arr <- rep(0, length(lab))
        for(i in 1:length(lab)){
                if(lab[i] %in% names(tb)){
                        arr[i] <- tb[which(lab[i] == names(tb))]
                }
        }
        names(arr) <- lab
return(arr)
}

gn7 <- cbind(sortTB(gn7.1), sortTB(gn7.2), sortTB(gn7.3), sortTB(gn7.4), sortTB(gn7.5), 
                sortTB(gn7.6), sortTB(gn7.7))
gn7num <- as.character(c(sum(gn7.1), sum(gn7.2), sum(gn7.3), sum(gn7.4), sum(gn7.5), 
                sum(gn7.6), sum(gn7.7)))
grp7 <- as.character()
for(i in 1:7) grp7[i] <- paste('group', as.character(i), sep = '')
colnames(gn7) <- grp7

gn7 <- gn7 / rowSums(gn7)

barplot(t(gn7), col = rainbow(nrow(t(gn7))), names.arg = rownames(gn7), space = 0.1)
legend('topleft', rownames(t(gn7)), cex = 0.8, fill = rainbow(nrow(t(gn7))))
title(main = 'Barplot with Seven Clusters from DTW Distance Metric')

gn7.ts1 <- TTSnew[agn$order[gn7.order1] ]
gn7.ts2 <- TTSnew[agn$order[gn7.order2] ]
gn7.ts3 <- TTSnew[agn$order[gn7.order3] ]
gn7.ts4 <- TTSnew[agn$order[gn7.order4] ]
gn7.ts5 <- TTSnew[agn$order[gn7.order5] ]
gn7.ts6 <- TTSnew[agn$order[gn7.order6] ]
gn7.ts7 <- TTSnew[agn$order[gn7.order7] ]

#this auto plot function takes list instead of matrix
autoPlot3 <- function(TSlist, gpName = 'group'){
        for(i in 1:length(TSlist)){
                ts <- TSlist[[i]]
                fileName <- paste(gpName, 'plot ', as.character(i), '--', names(TSlist[i]), '.jpeg', sep = '')
                jpeg(fileName, width = 1600, height = 800)
                plot(ts, type = 'o', xaxt = 'n', xlab = 'Year', ylab = '', ylim = c(-2.3, 3.95), cex.lab = 2.5,
                        cex.axis = 2.5, cex.sub = 2.5)
                abline(h = 0, col = 2, lty = 2)
                title(main = paste(gpName, 'plot ', as.character(i), ' / ', names(TSlist[i]), sep = ''), cex.main = 2.5)
                axis(1, 1:length(ts), cex.axis = 2.5)
                dev.off()
        }
}

autoPlot3(gn7.ts1, 'group1')
autoPlot3(gn7.ts2, 'group2')
autoPlot3(gn7.ts3, 'group3')
autoPlot3(gn7.ts4, 'group4')
autoPlot3(gn7.ts5, 'group5')
autoPlot3(gn7.ts6, 'group6')
autoPlot3(gn7.ts7, 'group7')
