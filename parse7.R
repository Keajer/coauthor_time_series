#forcus on cluster2:
library(cluster)
cluster2 <- as.dendrogram(agn.field)[[1]][[1]][[1]][[1]][[1]][[1]][[2]]
cluster1 <- as.dendrogram(agn.field)[[1]][[1]][[1]][[1]][[1]][[1]][[1]]
c2.labs <- labs.field[c8.order2]
c1.labs <- labs.field[c8.order1]
c2.1 <- cluster2[[1]]
c2.2 <- cluster2[[2]]
c2.role <- names(table(c2.labs))

c2.1.tb <- table(c2.labs[1:length(unlist(c2.1))])
c2.2.tb <- table(c2.labs[-(1:length(unlist(c2.1)))])

pup <- setNames(0, 'R7')
c2.1.tb <- append(c2.1.tb, pup, after = 6)

pup <- setNames(0, 'R3')
c2.2.tb <- append(c2.2.tb, pup, after = 2)
pfc21 <- procfreq(cbind(c2.1.tb, c2.2.tb))

c2.1.1 <- cluster2[[1]][[1]]
c2.1.2 <- cluster2[[1]][[2]]

c2.1.1.tb <- table(c2.labs[1:length(unlist(c2.1.1))])
c2.1.2.tb <- table(c2.labs[(length(unlist(c2.1.1)) + 1 ): length(unlist(c2.1))])
# pup <- setNames(0, 'R7')
#c2.1.1.tb <- append(c2.1.1.tb, pup, after = 6)
#c2.1.2.tb <- append(c2.1.2.tb, pup, after = 6)
pfc221 <- procfreq(cbind(c2.1.1.tb, c2.1.2.tb))

c2.2.1 <- cluster2[[2]][[1]]
c2.2.2 <- cluster2[[2]][[2]]

c2.2.1.tb <- table(c2.labs[(length(unlist(c2.1)) + 1) : (length(unlist(c2.1)) + length(unlist(c2.2.1)))])
c2.2.2.tb <- table(c2.labs[(length(unlist(c2.1)) + length(unlist(c2.2.1)) + 1) : length(unlist(cluster2))])
pup <- setNames(0, 'R7')
c2.2.1.tb <- append(c2.2.1.tb , pup, after = 6)
pup <- setNames(0, 'R1')
c2.2.2.tb <- append(c2.2.2.tb, pup, after = 0)
pup <- setNames(0, 'R2')
c2.2.2.tb <- append(c2.2.2.tb, pup, after = 1)
pup <- setNames(0, 'R4')
c2.2.2.tb <- append(c2.2.2.tb, pup, after = 3)

pfc222 <- procfreq(cbind(c2.2.1.tb, c2.2.2.tb))

c1.1 <- cluster1[[1]]
c1.2 <- cluster1[[2]]
c1.1.1 <- c1.1[[1]]
c1.1.2 <- c1.1[[2]]
c1.1.1.1 <- c1.1.1[[1]]
c1.1.1.2 <- c1.1.1[[2]]

c1.1.1.1.tb <- table(c1.labs[1:length(unlist(c1.1.1.1))])
c1.1.1.2.tb <- table(c1.labs[(length(unlist(c1.1.1.1)) +1 ) : length(unlist(c1.1.1))])

pfc131 <- procfreq(cbind(c1.1.1.1.tb, c1.1.1.2.tb))

c1.1.1.1.1 <- c1.1.1.1[[1]]
c1.1.1.1.2 <- c1.1.1.1[[2]]

c1.1.1.1.1.tb <- table(c1.labs[1:length(unlist(c1.1.1.1.1))])
c1.1.1.1.2.tb <- table(c1.labs[(length(unlist(c1.1.1.1.1)) +1) : length(unlist(c1.1.1.1))])
pup <- setNames(0, 'R3')
c1.1.1.1.2.tb <- append(c1.1.1.1.2.tb, pup, after = 2)

pfc141 <- procfreq(cbind(c1.1.1.1.1.tb, c1.1.1.1.2.tb))

c1.1.1.2.1 <- c1.1.1.2[[1]]
c1.1.1.2.2 <- c1.1.1.2[[2]]

c1.1.1.2.1.tb <- table(c1.labs[(length(unlist(c1.1.1.1)) + 1) : (length(unlist(c1.1.1.1)) + length(unlist(c1.1.1.2.1)))])
c1.1.1.2.2.tb <- table(c1.labs[(length(unlist(c1.1.1.1)) + length(unlist(c1.1.1.2.1)) + 1) : length(unlist(c1.1.1))])

pup <- setNames(0, 'R7')
c1.1.1.2.1.tb <- append(c1.1.1.2.1.tb, pup, after = 6)

pfc142 <- procfreq(cbind(c1.1.1.2.1.tb, c1.1.1.2.2.tb))

library(dtw) # make merge plots for dtw with slanted window
plot(dtw(TTSnew[[322]], TTSnew[[210]], k = T), type = 'two', 
        off = 1, match.lty = 2, match.indices = 20)
title(main = 'DTW without Slanted Window')
plot(dtw(TTSnew[[322]], TTSnew[[210]], k = T, window.type = 'slanted', window.size = 3), 
        type = 'two', off = 1, match.lty = 2, match.indices = 20)
title(main = 'DTW with Slanted Window')

#finding some pairs for dtw window type and step analysis
#window types
#sakoeChiba window
dtwWindow.plot(sakoeChibaWindow, window.size = 3, reference = 20, query = 15)
title(main = 'Sakoe Chiba Window')
#slanted window
dtwWindow.plot(slantedBandWindow, window.size = 3, reference = 20, query = 15)
title(main = 'Slanted Window')


ts5.1 <- field.ts[agn.field$order[377]]
ts5.2 <- field.ts[agn.field$order[378]]
ts.alignment <- dtw(ts5.1[[1]], ts5.2[[1]], k = T, window.type = 'slanted', window.size = 1)
plot(ts.alignment, type = 'two', off = 1, match.lty = 2, match.indices = length(ts.alignment$index1))
title(main = 'Two Lately Joined Time Series with Window Size 3')
ts.lcm <- ts.alignment$costMatrix
image(x = 1:nrow(ts.lcm), y = 1:ncol(ts.lcm), ts.lcm)
text(row(ts.lcm), col(ts.lcm), label = round(ts.lcm))
lines(ts.alignment$index1, ts.alignment$index2)
title(main = 'Cost Matrix and Wraping Alignment with Window Size 1')

dtwPlotDensity(ts.alignment)

ts1.70 <- field.ts[agn.field$order[70]]
ts1.71 <- field.ts[agn.field$order[71]]
ts.alignment <- dtw(ts1.70[[1]], ts1.71[[1]], k = T, window.type = 'slanted', window.size = 1)
plot(ts.alignment, type = 'two', off = 1, match.lty = 2, match.indices = length(ts.alignment$index1))
title(main = 'Slanted Window and Window Size 1')


field1.D <- matrix(0, nrow = length(field.ts), ncol = length(field.ts))
for(i in 1:nrow(field1.D )){
        for(j in 1:ncol(field1.D )){
                field1.D [i, j] <- dtw(field.ts[[i]], field.ts[[j]], window.type = 'slanted', window.size = 1)$distance
        }
}
names(field.ts) -> rownames(field1.D ) -> colnames(field1.D) 
field1.D <- as.dist(field1.D )
agn1.field <- agnes(field1.D)

jpeg("agn_tree.size1.jpeg", width = 9000, height = 4000)
plot(agn1.field)
#abline(h = 16.388, lty = 2, col = 2)
dev.off()


autoPlot3(field.ts[agn1.field$order], 'Window Size 1')

ts3.4 <- field.ts[agn.field$order[277]]
ts3.5 <- field.ts[agn.field$order[278]]
ts.alignment <- dtw(ts3.4[[1]], ts3.5[[1]], k = T, window.type = 'slanted', window.size = 3)
plot(ts.alignment, type = 'two', off = 1, match.lty = 2, match.indices = length(ts.alignment$index1))
title(main = 'Slanted Window and Window Size 3')

