#parse3.R

#function to get average #Author per Publication over year
#this will use functions getYear, getAu in parse.R file
getAuPerPub <- function(publs){
        years <- 1991:2010
        auCounter <- rep(0, 20)
        pubCounter <- rep(0, 20)
        for(i in 1:length(publs)){
                pubCounter[which(getYear(publs[[i]]) == years)] = pubCounter[which(getYear(publs[[i]]) == years)] + 1
                auCounter[which(getYear(publs[[i]]) == years)] = auCounter[which(getYear(publs[[i]]) == years)] + length(getAu(publs[[i]]))
        }
        avgAuPub <- auCounter / pubCounter
return(avgAuPub)
}

avgAuPerPub <- getAuPerPub(pubList)

plot(avgAuPerPub, type = 'o', ylim = c(3, 4), xaxt = 'n', xlab = '')
title(main = ("Average Author Per Publication over Years"))
axis(1, 1:20, as.character(1991:2010))
title(xlab = 'Year')

#this function gets distribution of total active period and average productivty for an area
getDists <- function(mp, area){
        ap <- as.numeric()
        avgp <- as.numeric()
        for(i in 1:length(mp)){
                s <- addZero(table(sapply(getAuPub(mp[i], area), getYear)))
                ap[i] <- length(s)
                avgp[i] <- mean(s)
        }
return(list(ap, avgp))
}

#get distribtions for all the ares:
a1dist <- getDists(a1mp, area1List)
a2dist <- getDists(a2mp, area2List)
a3dist <- getDists(a3mp, area3List)
a4dist <- getDists(a4mp, area4List)
a5dist <- getDists(a5mp, area5List)
a6dist <- getDists(a6mp, area6List)
a7dist <- getDists(a7mp, area7List)
a8dist <- getDists(a8mp, area8List)
a9dist <- getDists(a9mp, area9List)
a10dist <- getDists(a10mp, area10List)
a11dist <- getDists(a11mp, area11List)

#Active Period & Average productivity by areas
par(mfrow = c(2, 1))
hist(a1dist[[1]], main = 'Active Period Distribution, Area 1', xlab = '')
hist(a1dist[[2]], main = 'Average Productivity Distribution, Area 1', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a2dist[[1]], main = 'Active Period Distribution, Area 2', xlab = '')
hist(a2dist[[2]], main = 'Average Productivity Distribution, Area 2', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a3dist[[1]], main = 'Active Period Distribution, Area 3', xlab = '')
hist(a3dist[[2]], main = 'Average Productivity Distribution, Area 3', xlab = '', breaks = 80)

par(mfrow = c(2, 1))
hist(a4dist[[1]], main = 'Active Period Distribution, Area 4', xlab = '')
hist(a4dist[[2]], main = 'Average Productivity Distribution, Area 4', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a5dist[[1]], main = 'Active Period Distribution, Area 5', xlab = '')
hist(a5dist[[2]], main = 'Average Productivity Distribution, Area 5', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a6dist[[1]], main = 'Active Period Distribution, Area 6', xlab = '')
hist(a6dist[[2]], main = 'Average Productivity Distribution, Area 6', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a7dist[[1]], main = 'Active Period Distribution, Area 7', xlab = '')
hist(a7dist[[2]], main = 'Average Productivity Distribution, Area 7', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a8dist[[1]], main = 'Active Period Distribution, Area 8', xlab = '')
hist(a8dist[[2]], main = 'Average Productivity Distribution, Area 8', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a9dist[[1]], main = 'Active Period Distribution, Area 9', xlab = '')
hist(a9dist[[2]], main = 'Average Productivity Distribution, Area 9', xlab = '', breaks = 50)

par(mfrow = c(2, 1))
hist(a10dist[[1]], main = 'Active Period Distribution, Area 10', xlab = '')
hist(a10dist[[2]], main = 'Average Productivity Distribution, Area 10', xlab = '', breaks = 30)

par(mfrow = c(2, 1))
hist(a11dist[[1]], main = 'Active Period Distribution, Area 11', xlab = '')
hist(a11dist[[2]], main = 'Average Productivity Distribution, Area 11', xlab = '', breaks = 50)

#a function gets full length authors
getFullLenAu <- function(ls, area){
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
                                if(maxActRange == 19){
                                        mp[length(mp) + 1] <- author
                                }
                        }else{
                                if(diff(range(sapply(getAuPub(names(ls[[i]][which(ls[[i]] == max(ls[[i]]))]), area), getYear))) == 19){
                                        mp[length(mp) + 1] <- names(ls[[i]][which(ls[[i]] == max(ls[[i]]))])
                                }
                        }
                }
        }
return(mp)
}

a1fulen <- getFullLenAu(a1clu, area1List)
a2fulen <- getFullLenAu(a2clu, area2List)
a3fulen <- getFullLenAu(a3clu, area3List)
a4fulen <- getFullLenAu(a4clu, area4List)
a5fulen <- getFullLenAu(a5clu, area5List)
a6fulen <- getFullLenAu(a6clu, area6List)
a7fulen <- getFullLenAu(a7clu, area7List)
a8fulen <- getFullLenAu(a8clu, area8List)
a9fulen <- getFullLenAu(a9clu, area9List)
a10fulen <- getFullLenAu(a10clu, area10List)
a11fulen <- getFullLenAu(a11clu, area11List)

#this function takes an array of authors and auto saves plots for each series in an area:
autoPlot <- function(auls, area, areaName){
        for(i in 1:length(auls)){
                fileName <- paste(areaName, 'plot ', as.character(i), '.jpeg', sep = '')
                ts <- addZero(table(sapply(getAuPub(auls[i], area), getYear)))
                jpeg(fileName, width = 1600, height = 800)
                plot(ts, type = 'o', xaxt = 'n', xlab = '')
                title(main = paste(areaName, 'plot ', as.character(i), sep = ''))
                axis(1, 1:length(ts), as.character(1:20))
                dev.off()
        }
}

#make plots for each author in each area:
autoPlot(a1fulen, area1List, 'a1')
autoPlot(a2fulen, area2List, 'a2')
autoPlot(a3fulen, area3List, 'a3')
autoPlot(a4fulen, area4List, 'a4')
autoPlot(a5fulen, area5List, 'a5')
autoPlot(a6fulen, area6List, 'a6')
autoPlot(a7fulen, area7List, 'a7')
autoPlot(a8fulen, area8List, 'a8')
autoPlot(a9fulen, area9List, 'a9')
autoPlot(a10fulen, area10List, 'a10')
autoPlot(a11fulen, area11List, 'a11')

#this function gets all the 
getFuLenTS<- function(fl, area){
        acculs <- list()
        for(i in 1:length(fl)){
                acculs[[i]] <- addZero(table(sapply(getAuPub(fl[i], area), getYear)))
                acculs[[i]] <- (acculs[[i]] - mean(acculs[[i]])) / sd(acculs[[i]])
        }
        accu <- as.numeric()
        con <- as.numeric()
        for(i in 1:20){
                container <- 0
                for(j in 1:length(acculs)){
                        container <- container + acculs[[j]][i]
                }
                accu[i] <- container / 20
        }
return(accu)
}

#these are the full len 
a1futs<- getFuLenTS(a1fulen, area1List)
a2futs<- getFuLenTS(a2fulen, area2List)
a3futs<- getFuLenTS(a3fulen, area3List)
a4futs<- getFuLenTS(a4fulen, area4List)
a5futs<- getFuLenTS(a5fulen, area5List)
a6futs<- getFuLenTS(a6fulen, area6List)
a7futs<- getFuLenTS(a7fulen, area7List)
a8futs<- getFuLenTS(a8fulen, area8List)
a9futs<- getFuLenTS(a9fulen, area9List)
a10futs<- getFuLenTS(a10fulen, area10List)
a11futs<- getFuLenTS(a11fulen, area11List)

plot(a1futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a1futs), names(a1futs))
title(main = paste("Area 1, Number of Full Len Authors:", as.character(length(a1fulen))))

plot(a2futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a2futs), names(a2futs))
title(main = paste("Area 2, Number of Full Len Authors:", as.character(length(a2fulen))))

plot(a3futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a3futs), names(a3futs))
title(main = paste("Area 3, Number of Full Len Authors:", as.character(length(a3fulen))))

plot(a4futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a4futs), names(a4futs))
title(main = paste("Area 4, Number of Full Len Authors:", as.character(length(a4fulen))))

plot(a5futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a5futs), names(a5futs))
title(main = paste("Area 5, Number of Full Len Authors:", as.character(length(a5fulen))))

plot(a6futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a6futs), names(a6futs))
title(main = paste("Area 6, Number of Full Len Authors:", as.character(length(a6fulen))))

plot(a7futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a7futs), names(a7futs))
title(main = paste("Area 7, Number of Full Len Authors:", as.character(length(a7fulen))))

plot(a8futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a8futs), names(a8futs))
title(main = paste("Area 8, Number of Full Len Authors:", as.character(length(a8fulen))))

plot(a9futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a9futs), names(a9futs))
title(main = paste("Area 9, Number of Full Len Authors:", as.character(length(a9fulen))))

plot(a10futs, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a10futs), names(a10futs))
title(main = paste("Area 10, Number of Full Len Authors:", as.character(length(a10fulen))))

