#read in the output file
huge <- readLines(file.choose())
#find pattern and grep
pattern <- 'ID [0-9]+$'
ok <- grep(pattern, huge)

#get tha a string vector of publication IDs
getID <- function(str){
        ID <- unlist(strsplit(str, ' '))[2]
}
IDs <- sapply(huge[ok], getID, USE.NAMES = FALSE)

#get the list of all publications
pubList = list()
for(i in 1:(length(ok)-1)){
        pubList[[i]] <- huge[(ok[i]) : (ok[i+1]-2)]
}
pubList[[length(ok)]] <- huge[ok[length(ok)] : (length(huge) -1)]
names(pubList) <- IDs

getAu <- function(list){
        pattern <- '^([ ]|AU).*$'
        ok <- grep(pattern, list)
        Aus <- list()
        Aus <- list[ok]
        for(i in 1:length(Aus)){
                Aus[i] <- sub('^AU ', '', Aus[i])
                Aus[i] <- sub('^ ', '', Aus[i])
        }
return(Aus)
}

#all the authors in array this is just for checking
# au.list <- unlist(lapply(pubList, getAu))

countAu <- function(au, totList){
        counts = 0
        for(i in 1:length(totList)){
                if(au %in% getAu(totList[[i]])){
                        counts = counts + 1
                }
        }
return(counts)
}

getAuPub <- function(au, aList){
        auPub <- list()
        for(i in 1:length(aList)){
                if(au %in% getAu(aList[[i]])){
                        auPub[length(auPub) + 1] <- aList[i]
                }
        }
return(auPub)
}

getYear <- function(pub){
        str <- pub[5]
        year <- as.numeric(substr(str, nchar(str) - 3, nchar(str)))
return(year)
}

addZero <- function(p){
        i = 2
        while(i <= diff(range(as.numeric(names(p)))) + 1){
                if(as.numeric(names(p)[i]) - as.numeric(names(p)[i - 1]) > 1){
                        puppy <- setNames(0, as.character(as.numeric(names(p)[i - 1]) + 1))
                        p <- append(p, puppy, after = i-1)
                }else{
                        i = i + 1
                }
        }
return(as.vector(unname(p)))
}

clu <- readLines(file.choose())
net <- readLines(file.choose())

clu <- clu[2 : 752]
net <- net[2 : 752]

#sometimes formats of net are different
# net <- gsub('^.*?"', '', net)
# net <- gsub('\\\"', '', net)
net <- gsub('^.*?]', '', net)
net <- gsub('\\\".*$', '', net)

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
pubclu <- list()
for(i in 1:length(clusters)){
        pubclu[[i]]  <- sapply(clusters[[i]], countAu, totList = pubList)
}

# clusters[[1]][2] %in% net
# clusters[[1]][2] %in% au.list
# getAuPub(clusters[[1]][2], pubList)
# countAu(clusters[[1]][2], pubList)

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

#then generate authors and their time series with at least 15 year activity in a list
au15 <- get.15LenAu(pubclu, pubList)
tsls <- getAreaNormTS2(au15, pubList)

#start to processing the node role
role <- read.table(file.choose())
net1 <- readLines(file.choose())
# need to manually change the index to get the data for processing
net1 <- net1[2 : 752]
net1 <- strsplit(net1, '"')

node.role <- as.character()
aus <- as.character()
for(i in 1:length(net1)){
        aus[i] <- gsub('^.*?]', '', net1[[i]][2])
        # aus[i] <- gsub('', '', net1[[i]][2])
        if(as.numeric(net1[[i]][1]) %in% role$V1){
                node.role[i] <- as.character(role[which(role$V1 == as.numeric(net1[[i]][1])), 7])
        }
        else{
                node.role[i] <- 'NA'
        }
}
au.role <- data.frame(aus, node.role)

# #function to take out headings before names:
# new.clu <- list()
# for(i in 1:length(clusters)){
#         new.ls <- as.character()
#         for(j in 1:length(clusters[[i]])){
#                 new.ls[j] <- gsub('^.*?]', '', clusters[[i]][j])
#         }
#         new.clu[[i]] <- new.ls
# }
# names(new.clu) <- names(clusters)

# attach node roles to our time series list
labs <- as.character()
for(i in 1:length(au15)){
        if(au15[i] %in% au.role[, 1]){
                nodeRole <- au.role[which(au15[i] == au.role[, 1]), 2]
        }
        labs[i] <- paste(nodeRole, '-', au15[i], sep = '')
}
names(tsls) <- labs

library(cluster)
library(dtw)
# for DTW metric, use slanted window and size 3
Diss.DTW <- matrix(0, nrow = length(tsls), ncol = length(tsls))
for(i in 1:nrow(Diss.DTW )){
        for(j in 1:ncol(Diss.DTW )){
                Diss.DTW [i, j] <- dtw(tsls[[i]], tsls[[j]], window.type = 'slanted', window.size = 3)$distance
        }
}
names(tsls) -> rownames(Diss.DTW ) -> colnames(Diss.DTW) 
Diss.DTW.IR <- as.dist(Diss.DTW)
agn.dtw.w <- agnes(Diss.DTW.IR)
plot(agn.dtw.w)

as.dendrogram(agn.dtw.w)[[1]][[1]][[1]][[1]]

c5.1 <- c(1:4)
c5.2 <- c(5:7)
c5.3 <- c(8:12)
c5.4 <- c(13:13)
c5.5 <- c(14:)

c5.ts1 <- tsls[agn.dtw.w$order[c5.1]]
c5.ts2 <- tsls[agn.dtw.w$order[c5.2]]
c5.ts3 <- tsls[agn.dtw.w$order[c5.3]]
c5.ts4 <- tsls[agn.dtw.w$order[c5.4]]
c5.ts5 <- tsls[agn.dtw.w$order[c5.5]]

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

labs.agn <- gsub('-[A-Z].*', '', agn.dtw.w$order.lab)
lab5.1 <- table(labs.agn[c5.1])
lab5.2 <- table(labs.agn[c5.2])
lab5.3 <- table(labs.agn[c5.3])
lab5.4 <- table(labs.agn[c5.4])
lab5.5 <- table(labs.agn[c5.5])

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

lab5 <- cbind(sortTB.role(lab5.1), sortTB.role(lab5.2), sortTB.role(lab5.3), sortTB.role(lab5.4), sortTB.role(lab5.5))

clu5 <- as.character()
for(i in 1:5) clu5[i] <- paste('cluster', as.character(i), sep = '')
colnames(lab5) <- clu5

# test relationship between hub nodes and none-hub nodes
hub.node <- apply(lab5[1:3, ], 2, sum)
none.hub.node <- apply(lab5[4:7, ], 2, sum)
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

procfreq(lab5)

procfreq(rbind(hub.node, none.hub.node))


autoPlot3(c5.ts1, 'cluster1 ')
autoPlot3(c5.ts2, 'cluster2 ')
autoPlot3(c5.ts3, 'cluster3 ')
autoPlot3(c5.ts4, 'cluster4 ')
autoPlot3(c5.ts5, 'cluster5 ')
