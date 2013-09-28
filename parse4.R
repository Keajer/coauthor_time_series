#parse work 4
getAvgPubTeam <- function(publs){
        years <- 1991:2010
        auCounter <- rep(0, 20)
        pubCounter <- rep(0, 20)
        auList <- list()
        for(i in 1:20) auList[[i]] <- as.character()
        for(i in 1:length(publs)){
                pubCounter[which(getYear(publs[[i]]) == years)] = pubCounter[which(getYear(publs[[i]]) == years)] + 1
                auCounter[which(getYear(publs[[i]]) == years)] = auCounter[which(getYear(publs[[i]]) == years)] + length(getAu(publs[[i]]))
                auList[[which(getYear(publs[[i]]) == years)]] = c(auList[[which(getYear(publs[[i]]) == years)]], getAu(publs[[i]]))
        }
        uniqAuLength <- unlist(lapply(lapply(auList, unique), length))
        avgPubTeam <- pubCounter / uniqAuLength / (auCounter / pubCounter)
return(avgPubTeam)
}

a1PT <- getAvgPubTeam(area1List)
a2PT <- getAvgPubTeam(area2List)
a3PT <- getAvgPubTeam(area3List)
a4PT <- getAvgPubTeam(area4List)
a5PT <- getAvgPubTeam(area5List)
a6PT <- getAvgPubTeam(area6List)
a7PT <- getAvgPubTeam(area7List)
a8PT <- getAvgPubTeam(area8List)
a9PT <- getAvgPubTeam(area9List)
a10PT <- getAvgPubTeam(area10List)
a11PT <- getAvgPubTeam(area11List)

par(mfrow = c(6, 2))
plot(a1PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a2PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a3PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a4PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a5PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a6PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a7PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a8PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a9PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a10PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')
plot(a11PT, type = 'o', ylim = c(0, 0.2), xaxt = 'n', xlab = '')
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')

#this function gets the distribution of team size for an area over year
getDists2 <- function(area){
        years <- 1991:2010
        teamSize <- list()
        names <- as.character()
         for(i in 1:length(years)){
                tz <- as.numeric()
                for(j in 1:length(area)){
                        if(getYear(area[[j]]) == years[i]){
                                tz[length(tz) + 1] <- length(getAu(area[[j]]))
                        }
                }
                teamSize[[i]] <- tz
                names[i] <- as.character(length(tz))
         }
         names(teamSize) <- names
return(teamSize)
}

a1TeamSize <- getDists2(area1List)
a2TeamSize <- getDists2(area2List)
a3TeamSize <- getDists2(area3List)
a4TeamSize <- getDists2(area4List)
a5TeamSize <- getDists2(area5List)
a6TeamSize <- getDists2(area6List)
a7TeamSize <- getDists2(area7List)
a8TeamSize <- getDists2(area8List)
a9TeamSize <- getDists2(area9List)
a10TeamSize <- getDists2(area10List)
a11TeamSize <- getDists2(area11List)

boxplot(a1TeamSize, main = 'Area1 Team Size Distribution over Years')
boxplot(a2TeamSize, main = 'Area2 Team Size Distribution over Years')
boxplot(a3TeamSize, main = 'Area3 Team Size Distribution over Years')
boxplot(a4TeamSize, main = 'Area4 Team Size Distribution over Years')
boxplot(a5TeamSize, main = 'Area5 Team Size Distribution over Years')
boxplot(a6TeamSize, main = 'Area6 Team Size Distribution over Years')
boxplot(a7TeamSize, main = 'Area7 Team Size Distribution over Years')
boxplot(a8TeamSize, main = 'Area8 Team Size Distribution over Years')
boxplot(a9TeamSize, main = 'Area9 Team Size Distribution over Years')
boxplot(a10TeamSize, main = 'Area10 Team Size Distribution over Years')
boxplot(a11TeamSize, main = 'Area11 Team Size Distribution over Years')

hist(sapply(a1au.list, length), breaks = 9)
hist(sapply(a2au.list, length), breaks = 9)
hist(sapply(a3au.list, length), breaks = 9)
hist(sapply(a4au.list, length), breaks = 9)
hist(sapply(a5au.list, length), breaks = 9)
hist(sapply(a6au.list, length), breaks = 9)
hist(sapply(a7au.list, length), breaks = 9)
hist(sapply(a8au.list, length), breaks = 9)
hist(sapply(a9au.list, length), breaks = 9)
hist(sapply(a10au.list, length), breaks = 9)
hist(sapply(a11au.list, length), breaks = 9)

#this function gets dataframe of normalized ts of full length authors within an area
getAreaNormTS <- function(fl, area){
        ts <-  as.numeric()
        for(i in 1:length(fl)){
                t <- addZero(table(sapply(getAuPub(fl[i], area), getYear)))
                t <- (t - mean(t)) / sd(t)
                ts <- rbind(ts, t)
        }
return(ts)
}
a1NTS <- getAreaNormTS(a1fulen, area1List)
rownames(a1NTS) <- rep('a1', nrow(a1NTS))
a2NTS <- getAreaNormTS(a2fulen, area2List)
rownames(a2NTS) <- rep('a2', nrow(a2NTS))
a3NTS <- getAreaNormTS(a3fulen, area3List)
rownames(a3NTS) <- rep('a3', nrow(a3NTS))
a4NTS <- getAreaNormTS(a4fulen, area4List)
rownames(a4NTS) <- rep('a4', nrow(a4NTS))
a5NTS <- getAreaNormTS(a5fulen, area5List)
rownames(a5NTS) <- rep('a5', nrow(a5NTS))
a6NTS <- getAreaNormTS(a6fulen, area6List)
rownames(a6NTS) <- rep('a6', nrow(a6NTS))
a7NTS <- getAreaNormTS(a7fulen, area7List)
rownames(a7NTS) <- rep('a7', nrow(a7NTS))
a8NTS <- getAreaNormTS(a8fulen, area8List)
rownames(a8NTS) <- rep('a8', nrow(a8NTS))
a9NTS <- getAreaNormTS(a9fulen, area9List)
rownames(a9NTS) <- rep('a9', nrow(a9NTS))
a10NTS <- getAreaNormTS(a10fulen, area10List)
rownames(a10NTS) <- rep('a10', nrow(a10NTS))

TNTS <- rbind(a1NTS, a2NTS, a3NTS, a4NTS, a5NTS, a6NTS, a7NTS, a8NTS, a9NTS, a10NTS)

library(cluster)
agn <- agnes(TNTS)
plot(agn)

#this part is for grouping clusters
g4.order1 <- c(1:29)
g4.order2 <- c(30:45)
g4.order3 <- c(46:78)
g4.order4 <- c(79:92)

g4.1 <- table(agn$order.lab[g4.order1])
g4.2 <- table(agn$order.lab[g4.order2])
g4.3 <- table(agn$order.lab[g4.order3])
g4.4 <- table(agn$order.lab[g4.order4])

g4.ts1 <- TNTS[agn$order[g4.order1], ]
g4.ts2 <- TNTS[agn$order[g4.order2], ]
g4.ts3 <- TNTS[agn$order[g4.order3], ]
g4.ts4 <- TNTS[agn$order[g4.order4], ]

autoPlot2 <- function(TSs, gpName = 'group'){
        for(i in 1:nrow(TSs)){
                ts <- TSs[i, ]
                fileName <- paste(gpName, 'plot ', as.character(i), '.jpeg', sep = '')
                jpeg(fileName, width = 1600, height = 800)
                plot(ts, type = 'o', xaxt = 'n', xlab = '')
                title(main = paste(gpName, 'plot ', as.character(i), sep = ''))
                axis(1, 1:length(ts), as.character(1991:2010))
                dev.off()
        }
}

autoPlot2(g4.ts1, 'group1')
autoPlot2(g4.ts2, 'group2')
autoPlot2(g4.ts3, 'group3')
autoPlot2(g4.ts4, 'group4')

#function to sort tables with labels
sortTB <- function(tb){
        #library(gtools) #mixedorder()
        lab <- as.character()
        #for(i in 1:10) lab[i] <- paste('a', i, sep = '') #here we have 10 areas to be labbelled
        lab <- c('a10', 'a8', 'a5', 'a9', 'a2', 'a6', 'a3', 'a4', 'a7', 'a1')
        arr <- rep(0, length(lab))
        for(i in 1:length(lab)){
                if(lab[i] %in% names(tb)){
                        arr[i] <- tb[which(lab[i] == names(tb))]
                }
        }
        names(arr) <- lab
return(arr)
}

g4 <- cbind(sortTB(g4.1), sortTB(g4.2), sortTB(g4.3), sortTB(g4.4))
g4num <- as.character(c(sum(g4.1), sum(g4.2), sum(g4.3), sum(g4.4)))
grp4 <- as.character()
for(i in 1:4) grp4[i] <- paste('group', as.character(i), sep = '')
colnames(g4) <- grp4

barplot(t(g4), col = rainbow(nrow(t(g4))), names.arg = colnames(t(g4)), space = 0.1)
legend('topleft', rownames(t(g4)), cex = 0.8, fill = rainbow(nrow(t(g4))))

cut(as.dendrogram(agn), h = 5.65)#to get 10 groups
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

g7.1 <- table(agn$order.lab[g7.order1])
g7.2 <- table(agn$order.lab[g7.order2])
g7.3 <- table(agn$order.lab[g7.order3])
g7.4 <- table(agn$order.lab[g7.order4])
g7.5 <- table(agn$order.lab[g7.order5])
g7.6 <- table(agn$order.lab[g7.order6])
g7.7 <- table(agn$order.lab[g7.order7])

g7 <- cbind(sortTB(g7.1), sortTB(g7.2), sortTB(g7.3), sortTB(g7.4), sortTB(g7.5), 
                sortTB(g7.6), sortTB(g7.7))
g7num <- as.character(c(sum(g7.1), sum(g7.2), sum(g7.3), sum(g7.4), sum(g7.5), 
                sum(g7.6), sum(g7.7)))
grp7 <- as.character()
for(i in 1:7) grp7[i] <- paste('group', as.character(i), sep = '')
colnames(g7) <- grp7

barplot(t(g7), col = rainbow(nrow(t(g7))), names.arg = rownames(g7), space = 0.1)
legend('topleft', rownames(t(g7)), cex = 0.8, fill = rainbow(nrow(t(g7))))

