#this function will implement LCSS dissimilarity function in R
LCSS <- function(a, b, delta, epsilon){
        m <- length(a)
        n <- length(b)
        if(n < m){
                temp <- a
                a <- b
                b <- temp
                m <- length(a)
                n <- length(b)
        }
        lcsTable <- matrix(0, nrow = m+1, ncol = n+1)
        for(i in 1:m){
                for(j in (i - delta):(i + delta)){
                        if(j > 0 & j < n){
                                if((b[j] + epsilon) >= a[i] & (b[j] - epsilon) <= a[i]){
                                        lcsTable[i+1, j+1] <- lcsTable[i, j] + 1
                                }else if(lcsTable[i, j+1] > lcsTable[i+1, j]){
                                        lcsTable[i+1, j+1] <- lcsTable[i, j+1]
                                }else{
                                        lcsTable[i+1, j+1] <- lcsTable[i+1, j]
                                }
                        }
                }
        }
        lcsTable <- lcsTable[-1, -1]
        lcs <- max(lcsTable[m, ])
        diss <- 1 / (lcs / max(n, m) + 0.00001)
return(diss)
}


LCSS(TTSnew[[1]], TTSnew[[2]], 3, 0.025)

#apply LCSS to get diss matrix
DissM <- matrix(0, nrow = length(TTSnew), ncol = length(TTSnew))
for(i in 1:nrow(DissM)){
        for(j in 1:ncol(DissM)){
                DissM[i, j] <- LCSS(TTSnew[[i]], TTSnew[[j]], 1, 0.25)
        }
}
names(TTSnew) -> rownames(DissM) -> colnames(DissM) 
Diss <- as.dist(DissM)
agn2 <- agnes(Diss)

plot(as.dendrogram(agn2))
cut(as.dendrogram(agn2), h = 0.26)
abline(h = 0.27, lty = 2,  col = 2)

#function for getting orders from dendrogram object
getOrders <- function(agn, cut){
        cuts <- cut(as.dendrogram(agn), h = cut)
        orders <- as.numeric()
        od <- list()
        for(i in 1:length(cuts$lower)){
                orders[i] <- length(unlist(cuts$lower[[i]]))
                if(i ==1){
                        od[[i]] <- i:orders[i]
                }else{
                        od[[i]] <- (max(unlist(od)) + 1):sum(orders[1:i])
                }
        }
return(od)
}

gn9 <- getOrders(agn2, 0.26)

for(i in 1:length(gn9)){
        tsList <- TTSnew[agn2$order[gn9[[i]]]]
        autoPlot3(tsList, paste('group', as.character(i), sep = ''))
}

#get barplot
labs2 <- gsub('[A-Z].*', '', agn2$order.lab)

gn9.1 <- table(labs2[gn9[[1]]])
gn9.2 <- table(labs2[gn9[[2]]])
gn9.3 <- table(labs2[gn9[[3]]])
gn9.4 <- table(labs2[gn9[[4]]])
gn9.5 <- table(labs2[gn9[[5]]])
gn9.6 <- table(labs2[gn9[[6]]])
gn9.7 <- table(labs2[gn9[[7]]])
gn9.8 <- table(labs2[gn9[[8]]])
gn9.9 <- table(labs2[gn9[[9]]])

gn9_bar <- cbind(sortTB(gn9.1), sortTB(gn9.2), sortTB(gn9.3), sortTB(gn9.4), sortTB(gn9.5), 
                sortTB(gn9.6), sortTB(gn9.7), sortTB(gn9.8), sortTB(gn9.9))
gn9num <- as.character(c(sum(gn9.1), sum(gn9.2), sum(gn9.3), sum(gn9.4), sum(gn9.5), 
                sum(gn9.6), sum(gn9.7), sum(gn9.8), sum(gn9.9)))
grp9 <- as.character()
for(i in 1:9) grp9[i] <- paste('group', as.character(i), sep = '')
colnames(gn9_bar) <- grp9

gn9_bar <- gn9_bar / rowSums(gn9_bar)

barplot(t(gn9_bar), col = rainbow(nrow(t(gn9_bar))), names.arg = rownames(gn9_bar), space = 0.1)
legend('topleft', rownames(t(gn9_bar)), cex = 0.8, fill = rainbow(nrow(t(gn9_bar))))
title(main = 'Barplot with Nine Clusters from LCSS Distance Metric')

#56clusters under LCSS
cut(as.dendrogram(agn2), h = 0.21)
gn56 <- getOrders(agn2, 0.21)
for(i in 1:56){
        tsList <- TTSnew[agn2$order[gn56[[i]]]]
        autoPlot3(tsList, paste('group', as.character(i), sep = ''))
}

#40 clusters under DTW
cut(as.dendrogram(agn), h = 14.5)
gn40 <- getOrders(agn, 14.5)
for(i in 1:40){
        tsList <- TTSnew[agn$order[gn40[[i]]]]
        autoPlot3(tsList, paste('group', as.character(i), sep = ''))
}

#ramdon generate test set for examing
library(dtw)
library(cluster)
smpTS <- TTSnew[sample(1:524, 50)]
#for DTW
DM_DTW <- dist(smpTS, method = 'DTW')
agn_dtw <- agnes(DM_DTW)
plot(as.dendrogram(agn_dtw), main = 'Sample Testing for DTW')
autoPlot3(smpTS[agn_dtw$order], 'DTW')

#for LCSS
DM_LCSS <- matrix(0, nrow = length(smpTS), ncol = length(smpTS))
for(i in 1:nrow(DM_LCSS)){
        for(j in 1:ncol(DM_LCSS)){
                DM_LCSS[i, j] <- LCSS(smpTS[[i]], smpTS[[j]], 1, 0.015)
        }
}
names(smpTS) -> rownames(DM_LCSS) -> colnames(DM_LCSS) 
DM_LCSS <- as.dist(DM_LCSS)
agn_lcss <- agnes(DM_LCSS)
plot(as.dendrogram(agn_lcss), main = 'Sample Testing for LCSS')

autoPlot3(smpTS[agn_lcss$order], 'LCSS')
