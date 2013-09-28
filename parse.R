#this script parses multiple files for field2
#get publication IDs for top 11 areas from py generated csv file
area <- readLines(file.choose())
areals <- list()
for(i in 1:length(area)){
        areals[i] <- strsplit(area[i], ',')
}

#load publication file
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

#function to get a list of publicaitons in an area from whole set of publication
getAreaPub <- function(area, list){
        areaList <- list()
        for(i in 1:length(list)){
                if(names(list)[i] %in% area){
                        areaList[length(areaList) + 1] <- list[i]
                }
        }
return(areaList)
}
#lists of publications in each area
area1List <- getAreaPub(areals[[1]], pubList)
area2List <- getAreaPub(areals[[2]], pubList)
area3List <- getAreaPub(areals[[3]], pubList)
area4List <- getAreaPub(areals[[4]], pubList)
area5List <- getAreaPub(areals[[5]], pubList)
area6List <- getAreaPub(areals[[6]], pubList)
area7List <- getAreaPub(areals[[7]], pubList)
area8List <- getAreaPub(areals[[8]], pubList)
area9List <- getAreaPub(areals[[9]], pubList)
area10List <- getAreaPub(areals[[10]], pubList)
area11List <- getAreaPub(areals[[11]], pubList)

#this funciong find authors in a publication
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

#get lists of authors for each publication in each area, with the unique authors in each area
a1au.list <- lapply(area1List, getAu)
a2au.list <- lapply(area2List, getAu)
a3au.list <- lapply(area3List, getAu)
a4au.list <- lapply(area4List, getAu)
a5au.list <- lapply(area5List, getAu)
a6au.list <- lapply(area6List, getAu)
a7au.list <- lapply(area7List, getAu)
a8au.list <- lapply(area8List, getAu)
a9au.list <- lapply(area9List, getAu)
a10au.list <- lapply(area10List, getAu)
a11au.list <- lapply(area11List, getAu)

uniqa1au.list <- unique(unlist(a1au.list))
uniqa2au.list <- unique(unlist(a2au.list))
uniqa3au.list <- unique(unlist(a3au.list))
uniqa4au.list <- unique(unlist(a4au.list))
uniqa5au.list <- unique(unlist(a5au.list))
uniqa6au.list <- unique(unlist(a6au.list))
uniqa7au.list <- unique(unlist(a7au.list))
uniqa8au.list <- unique(unlist(a8au.list))
uniqa9au.list <- unique(unlist(a9au.list))
uniqa10au.list <- unique(unlist(a10au.list))
uniqa11au.list <- unique(unlist(a11au.list))

#this function is to count # of publications from one author
countAu <- function(au, totList){
        counts = 0
        for(i in 1:length(totList)){
                if(au %in% totList[[i]]){
                        counts = counts + 1
                }
        }
return(counts)
}

#this finds top 5 productive authors in each area
a1Top5 <- names(sort(sapply(uniqa1au.list, countAu, totList = a1au.list), decreasing = TRUE)[1:5])
a2Top5 <- names(sort(sapply(uniqa2au.list, countAu, totList = a2au.list), decreasing = TRUE)[1:5])
a3Top5 <- names(sort(sapply(uniqa3au.list, countAu, totList = a3au.list), decreasing = TRUE)[1:5])
a4Top5 <- names(sort(sapply(uniqa4au.list, countAu, totList = a4au.list), decreasing = TRUE)[1:5])
a5Top5 <- names(sort(sapply(uniqa5au.list, countAu, totList = a5au.list), decreasing = TRUE)[1:5])
a6Top5 <- names(sort(sapply(uniqa6au.list, countAu, totList = a6au.list), decreasing = TRUE)[1:5])
a7Top5 <- names(sort(sapply(uniqa7au.list, countAu, totList = a7au.list), decreasing = TRUE)[1:5])
a8Top5 <- names(sort(sapply(uniqa8au.list, countAu, totList = a8au.list), decreasing = TRUE)[1:5])
a9Top5 <- names(sort(sapply(uniqa9au.list, countAu, totList = a9au.list), decreasing = TRUE)[1:5])
a10Top5 <- names(sort(sapply(uniqa10au.list, countAu, totList = a10au.list), decreasing = TRUE)[1:5])
a11Top5 <- names(sort(sapply(uniqa11au.list, countAu, totList = a11au.list), decreasing = TRUE)[1:5])

#this function finds all the pulications for a particular author
getAuPub <- function(au, aList){
        auPub <- list()
        for(i in 1:length(aList)){
                if(au %in% getAu(aList[[i]])){
                        auPub[length(auPub) + 1] <- aList[i]
                }
        }
return(auPub)
}

#list of all publications for one particular author
area1.1 <- getAuPub(a1Top5[1], area1List)
area1.2 <- getAuPub(a1Top5[2], area1List)
area1.3 <- getAuPub(a1Top5[3], area1List)
area1.4 <- getAuPub(a1Top5[4], area1List)
area1.5 <- getAuPub(a1Top5[5], area1List)
area2.1 <- getAuPub(a2Top5[1], area2List)
area2.2 <- getAuPub(a2Top5[2], area2List)
area2.3 <- getAuPub(a2Top5[3], area2List)
area2.4 <- getAuPub(a2Top5[4], area2List)
area2.5 <- getAuPub(a2Top5[5], area2List)
area3.1 <- getAuPub(a3Top5[1], area3List)
area3.2 <- getAuPub(a3Top5[2], area3List)
area3.3 <- getAuPub(a3Top5[3], area3List)
area3.4 <- getAuPub(a3Top5[4], area3List)
area3.5 <- getAuPub(a3Top5[5], area3List)
area4.1 <- getAuPub(a4Top5[1], area4List)
area4.2 <- getAuPub(a4Top5[2], area4List)
area4.3 <- getAuPub(a4Top5[3], area4List)
area4.4 <- getAuPub(a4Top5[4], area4List)
area4.5 <- getAuPub(a4Top5[5], area4List)
area5.1 <- getAuPub(a5Top5[1], area5List)
area5.2 <- getAuPub(a5Top5[2], area5List)
area5.3 <- getAuPub(a5Top5[3], area5List)
area5.4 <- getAuPub(a5Top5[4], area5List)
area5.5 <- getAuPub(a5Top5[5], area5List)
area6.1 <- getAuPub(a6Top5[1], area6List)
area6.2 <- getAuPub(a6Top5[2], area6List)
area6.3 <- getAuPub(a6Top5[3], area6List)
area6.4 <- getAuPub(a6Top5[4], area6List)
area6.5 <- getAuPub(a6Top5[5], area6List)
area7.1 <- getAuPub(a7Top5[1], area7List)
area7.2 <- getAuPub(a7Top5[2], area7List)
area7.3 <- getAuPub(a7Top5[3], area7List)
area7.4 <- getAuPub(a7Top5[4], area7List)
area7.5 <- getAuPub(a7Top5[5], area7List)
area8.1 <- getAuPub(a8Top5[1], area8List)
area8.2 <- getAuPub(a8Top5[2], area8List)
area8.3 <- getAuPub(a8Top5[3], area8List)
area8.4 <- getAuPub(a8Top5[4], area8List)
area8.5 <- getAuPub(a8Top5[5], area8List)
area9.1 <- getAuPub(a9Top5[1], area9List)
area9.2 <- getAuPub(a9Top5[2], area9List)
area9.3 <- getAuPub(a9Top5[3], area9List)
area9.4 <- getAuPub(a9Top5[4], area9List)
area9.5 <- getAuPub(a9Top5[5], area9List)
area10.1 <- getAuPub(a10Top5[1], area10List)
area10.2 <- getAuPub(a10Top5[2], area10List)
area10.3 <- getAuPub(a10Top5[3], area10List)
area10.4 <- getAuPub(a10Top5[4], area10List)
area10.5 <- getAuPub(a10Top5[5], area10List)
area11.1 <- getAuPub(a11Top5[1], area11List)
area11.2 <- getAuPub(a11Top5[2], area11List)
area11.3 <- getAuPub(a11Top5[3], area11List)
area11.4 <- getAuPub(a11Top5[4], area11List)
area11.5 <- getAuPub(a11Top5[5], area11List)


#this function gets year as numerical value from a publication
getYear <- function(pub){
        str <- pub[5]
        year <- as.numeric(substr(str, nchar(str) - 3, nchar(str)))
return(year)
}

#this function adds zeros for missing years:
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

#gets a vector of years that a particular author deliver publications
p1.1 <- table(sapply(area1.1, getYear)) / length(area1.1)
p1.2 <- table(sapply(area1.2, getYear)) / length(area1.2)
p1.3 <- table(sapply(area1.3, getYear)) / length(area1.3)
p1.4 <- table(sapply(area1.4, getYear)) / length(area1.4)
p1.5 <- table(sapply(area1.5, getYear)) / length(area1.5)
p2.1 <- table(sapply(area2.1, getYear)) / length(area2.1)
p2.2 <- table(sapply(area2.2, getYear)) / length(area2.2)
p2.3 <- table(sapply(area2.3, getYear)) / length(area2.3)
p2.4 <- table(sapply(area2.4, getYear)) / length(area2.4)
p2.5 <- table(sapply(area2.5, getYear)) / length(area2.5)
p3.1 <- table(sapply(area3.1, getYear)) / length(area3.1)
p3.2 <- table(sapply(area3.2, getYear)) / length(area3.2)
p3.3 <- table(sapply(area3.3, getYear)) / length(area3.3)
p3.4 <- table(sapply(area3.4, getYear)) / length(area3.4)
p3.5 <- table(sapply(area3.5, getYear)) / length(area3.5)
p4.1 <- table(sapply(area4.1, getYear)) / length(area4.1)
p4.2 <- table(sapply(area4.2, getYear)) / length(area4.2)
p4.3 <- table(sapply(area4.3, getYear)) / length(area4.3)
p4.4 <- table(sapply(area4.4, getYear)) / length(area4.4)
p4.5 <- table(sapply(area4.5, getYear)) / length(area4.5)
p5.1 <- table(sapply(area5.1, getYear)) / length(area5.1)
p5.2 <- table(sapply(area5.2, getYear)) / length(area5.2)
p5.3 <- table(sapply(area5.3, getYear)) / length(area5.3)
p5.4 <- table(sapply(area5.4, getYear)) / length(area5.4)
p5.5 <- table(sapply(area5.5, getYear)) / length(area5.5)
p6.1 <- table(sapply(area6.1, getYear)) / length(area6.1)
p6.2 <- table(sapply(area6.2, getYear)) / length(area6.2)
p6.3 <- table(sapply(area6.3, getYear)) / length(area6.3)
p6.4 <- table(sapply(area6.4, getYear)) / length(area6.4)
p6.5 <- table(sapply(area6.5, getYear)) / length(area6.5)
p7.1 <- table(sapply(area7.1, getYear)) / length(area7.1)
p7.2 <- table(sapply(area7.2, getYear)) / length(area7.2)
p7.3 <- table(sapply(area7.3, getYear)) / length(area7.3)
p7.4 <- table(sapply(area7.4, getYear)) / length(area7.4)
p7.5 <- table(sapply(area7.5, getYear)) / length(area7.5)
p8.1 <- table(sapply(area8.1, getYear)) / length(area8.1)
p8.2 <- table(sapply(area8.2, getYear)) / length(area8.2)
p8.3 <- table(sapply(area8.3, getYear)) / length(area8.3)
p8.4 <- table(sapply(area8.4, getYear)) / length(area8.4)
p8.5 <- table(sapply(area8.5, getYear)) / length(area8.5)
p9.1 <- table(sapply(area9.1, getYear)) / length(area9.1)
p9.2 <- table(sapply(area9.2, getYear)) / length(area9.2)
p9.3 <- table(sapply(area9.3, getYear)) / length(area9.3)
p9.4 <- table(sapply(area9.4, getYear)) / length(area9.4)
p9.5 <- table(sapply(area9.5, getYear)) / length(area9.5)
p10.1 <- table(sapply(area10.1, getYear)) / length(area10.1)
p10.2 <- table(sapply(area10.2, getYear)) / length(area10.2)
p10.3 <- table(sapply(area10.3, getYear)) / length(area10.3)
p10.4 <- table(sapply(area10.4, getYear)) / length(area10.4)
p10.5 <- table(sapply(area10.5, getYear)) / length(area10.5)
p11.1 <- table(sapply(area11.1, getYear)) / length(area11.1)
p11.2 <- table(sapply(area11.2, getYear)) / length(area11.2)
p11.3 <- table(sapply(area11.3, getYear)) / length(area11.3)
p11.4 <- table(sapply(area11.4, getYear)) / length(area11.4)
p11.5 <- table(sapply(area11.5, getYear)) / length(area11.5)
"""
library(ggplot2)
f <- ggplot(p1.1, aes(yr_frac, value, ymin = 0,
        ymax = value, colour = "grey30")) + scale_colour_identity() +
        xlim(1980, 2010) + facet_grid(variable ~
        ., scales = "free", as.table = FALSE) +
        theme_bw() + opts(panel.margin = unit(0,
        "lines"), axis.title.x = theme_blank(),
        axis.title.y = theme_blank())
"""
x11()
par(mfrow = c(5, 1))
plot(p1.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 1: Top 5 Hubs' Publications over Year"))
plot(p1.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p1.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p1.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p1.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p2.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 2: Top 5 Hubs' Publications over Year"))
plot(p2.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p2.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p2.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p2.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p3.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 3: Top 5 Hubs' Publications over Year"))
plot(p3.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p3.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p3.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p3.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p4.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 4: Top 5 Hubs' Publications over Year"))
plot(p4.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p4.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p4.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p4.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p5.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 5: Top 5 Hubs' Publications over Year"))
plot(p5.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p5.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p5.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p5.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p6.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 6: Top 5 Hubs' Publications over Year"))
plot(p6.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p6.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p6.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p6.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p7.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 7: Top 5 Hubs' Publications over Year"))
plot(p7.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p7.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p7.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p7.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p8.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 8: Top 5 Hubs' Publications over Year"))
plot(p8.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p8.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p8.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p8.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p9.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 9: Top 5 Hubs' Publications over Year"))
plot(p9.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p9.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p9.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p9.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p10.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 10: Top 5 Hubs' Publications over Year"))
plot(p10.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p10.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p10.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p10.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

x11()
par(mfrow = c(5, 1))
plot(p11.1, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
title(main = ("Area 11: Top 5 Hubs' Publications over Year"))
plot(p11.2, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p11.3, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p11.4, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
plot(p11.5, type = 'o', xlim = c(1991,2010), ylim = c(0, 0.3), xaxt = 'n')
axis(1, 1991:2010, as.character(1:20))
title(xlab = 'Year')

