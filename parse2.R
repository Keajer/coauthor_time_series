#parse2.R

clu <- readLines(file.choose())
net <- readLines(file.choose())

clu <- clu[2 : 60458]
net <- net[2 : 60458]

net <- gsub('[0-9]+ \\\"', '', net)
net <- gsub('\\\"', '', net)

#this function gets the list of authors for each cluster
getCluster <- function(ls1 = clu, ls2 = net){
        cluster <- list()
        for(i in unique(ls1)){
                cluster[[i]] <- ls2[which(ls1 == i)]
        }
        names(cluster) <- unique(ls1)
return(cluster)
}

clusters <- getCluster()

#source some functions we used in previous parse, e.g. countAu(), getAuPub(), getYear(), addZero()
#now lets get publication counts each author for each area in each cluster
#try area 11 first
a1clu <- list()
for(i in 1:length(clusters)){
        a1clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a1au.list)
}
a2clu <- list()
for(i in 1:length(clusters)){
        a2clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a2au.list)
}
a3clu <- list()
for(i in 1:length(clusters)){
        a3clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a3au.list)
}
a4clu <- list()
for(i in 1:length(clusters)){
        a4clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a4au.list)
}
a5clu <- list()
for(i in 1:length(clusters)){
        a5clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a5au.list)
}
a6clu <- list()
for(i in 1:length(clusters)){
        a6clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a6au.list)
}
a7clu <- list()
for(i in 1:length(clusters)){
        a7clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a7au.list)
}
a8clu <- list()
for(i in 1:length(clusters)){
        a8clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a8au.list)
}
a9clu <- list()
for(i in 1:length(clusters)){
        a9clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a9au.list)
}
a10clu <- list()
for(i in 1:length(clusters)){
        a10clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a10au.list)
}
a11clu <- list()
for(i in 1:length(clusters)){
        a11clu[[i]]  <- sapply(clusters[[i]], countAu, totList = a11au.list)
}

#this function gets the most productive author in a cluster for one area 
#it also gets rid of the 0 cluster and authors and 
getMostProd <- function(ls, area){
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
                                if(maxActRange >= 11){
                                        mp[length(mp) + 1] <- author
                                }
                        }else{
                                if(diff(range(sapply(getAuPub(names(ls[[i]][which(ls[[i]] == max(ls[[i]]))]), area), getYear))) >= 11){
                                        mp[length(mp) + 1] <- names(ls[[i]][which(ls[[i]] == max(ls[[i]]))])
                                }
                        }
                }
        }
return(mp)
}

a1mp <- getMostProd(a1clu, area1List)
a2mp <- getMostProd(a2clu, area2List)
a3mp <- getMostProd(a3clu, area3List)
a4mp <- getMostProd(a4clu, area4List)
a5mp <- getMostProd(a5clu, area5List)
a6mp <- getMostProd(a6clu, area6List)
a7mp <- getMostProd(a7clu, area7List)
a8mp <- getMostProd(a8clu, area8List)
a9mp <- getMostProd(a9clu, area9List)
a10mp <- getMostProd(a10clu, area10List)
a11mp <- getMostProd(a11clu, area11List)

#this function gets total active period and average productivity with in an area
getAvgProd <- function(mp, area){
        container <- 0
        maxLength <- 0
        for(i in 1:length(mp)){
                tb <- table(sapply(getAuPub(mp[i], area), getYear))
                container <- container + sum(tb)
                if(length(addZero(tb))> maxLength){
                        maxLength <- length(addZero(tb))
                }
        }
return(container / maxLength)
}

#this function is to get list of normalized time series for one area
getAccuTS<- function(mp, area){
        acculs <- list()
        for(i in 1:length(mp)){
                acculs[[i]] <- addZero(table(sapply(getAuPub(mp[i], area), getYear)))
                acculs[[i]] <- (acculs[[i]] - mean(acculs[[i]])) / sd(acculs[[i]])
        }

        for(i in 1:length(acculs)){
                acculs[[i]] <- acculs[[i]][-length(acculs[[i]])]
        }
        maxLength <- max(sapply(acculs, length))
        accu <- as.numeric()
        con <- as.numeric()
        for(i in 1:maxLength){
                contributer <- 0
                container <- 0
                for(j in 1:length(acculs)){
                        if(is.na(acculs[[j]][i]) != 1){
                                contributer <- contributer + 1
                                container <- container + acculs[[j]][i]
                        }
                }
                accu[i] <- container / contributer
                con[i] <- contributer
        }
        names(accu) <- con
return(accu)
}

a1accu <- getAccuTS(a1mp, area1List)[-1]
a2accu <- getAccuTS(a2mp, area2List)[-1]
a3accu <- getAccuTS(a3mp, area3List)[-1]
a4accu <- getAccuTS(a4mp, area4List)[-1]
a5accu <- getAccuTS(a5mp, area5List)[-1]
a6accu <- getAccuTS(a6mp, area6List)[-1]
a7accu <- getAccuTS(a7mp, area7List)[-1]
a8accu <- getAccuTS(a8mp, area8List)[-1]
a9accu <- getAccuTS(a9mp, area9List)[-1]
a10accu <- getAccuTS(a10mp, area10List)[-1]
a11accu <- getAccuTS(a11mp, area11List)[-1]


plot(a1accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a1accu), names(a1accu))
title(main = paste("Area 1, Number of Clusters:", as.character(length(a1mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a1mp, area1List))))

plot(a2accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a2accu), names(a2accu))
title(main = paste("Area 2, Number of Clusters:", as.character(length(a2mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a2mp, area2List))))

plot(a3accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a3accu), names(a3accu))
title(main = paste("Area 3, Number of Clusters:", as.character(length(a3mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a3mp, area3List))))

plot(a4accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a4accu), names(a4accu))
title(main = paste("Area 4, Number of Clusters:", as.character(length(a4mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a4mp, area4List))))

plot(a5accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a5accu), names(a5accu))
title(main = paste("Area 5, Number of Clusters:", as.character(length(a5mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a5mp, area5List))))

plot(a6accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a6accu), names(a6accu))
title(main = paste("Area 6, Number of Clusters:", as.character(length(a6mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a6mp, area6List))))

plot(a7accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a7accu), names(a7accu))
title(main = paste("Area 7, Number of Clusters:", as.character(length(a7mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a7mp, area7List))))

plot(a8accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a8accu), names(a8accu))
title(main = paste("Area 8, Number of Clusters:", as.character(length(a8mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a8mp, area8List))))

plot(a9accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a9accu), names(a9accu))
title(main = paste("Area 9, Number of Clusters:", as.character(length(a9mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a9mp, area9List))))

plot(a10accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a10accu), names(a10accu))
title(main = paste("Area 10, Number of Clusters:", as.character(length(a10mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a10mp, area10List))))

plot(a11accu, type = 'o', xaxt = 'n', xlab = '')
axis(1, 1:length(a11accu), names(a11accu))
title(main = paste("Area 11, Number of Clusters:", as.character(length(a11mp))))
title(xlab =paste('Average Productivity:', as.character(getAvgProd(a11mp, area11List))))
