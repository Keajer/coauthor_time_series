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

#get unique author list for each areac
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

#check all tops are in the hublist
hubList <- readLines(file.choose())
hubList <- gsub('\"', '', hubList)
uniqa1hub.list <- uniqa1au.list[which(uniqa1au.list %in% hubList)]
uniqa2hub.list <- uniqa2au.list[which(uniqa2au.list %in% hubList)]
uniqa3hub.list <- uniqa3au.list[which(uniqa3au.list %in% hubList)]
uniqa4hub.list <- uniqa4au.list[which(uniqa4au.list %in% hubList)]
uniqa5hub.list <- uniqa5au.list[which(uniqa5au.list %in% hubList)]
uniqa6hub.list <- uniqa6au.list[which(uniqa6au.list %in% hubList)]
uniqa7hub.list <- uniqa7au.list[which(uniqa7au.list %in% hubList)]
uniqa8hub.list <- uniqa8au.list[which(uniqa8au.list %in% hubList)]
uniqa9hub.list <- uniqa9au.list[which(uniqa9au.list %in% hubList)]
uniqa10hub.list <- uniqa10au.list[which(uniqa10au.list %in% hubList)]
uniqa11hub.list <- uniqa11au.list[which(uniqa11au.list %in% hubList)]


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
a1Top5 <- names(sort(sapply(uniqa1hub.list, countAu, totList = a1au.list), decreasing = TRUE)[1:5])
a2Top5 <- names(sort(sapply(uniqa2hub.list, countAu, totList = a2au.list), decreasing = TRUE)[1:5])
a3Top5 <- names(sort(sapply(uniqa3hub.list, countAu, totList = a3au.list), decreasing = TRUE)[1:5])
a4Top5 <- names(sort(sapply(uniqa4hub.list, countAu, totList = a4au.list), decreasing = TRUE)[1:5])
a5Top5 <- names(sort(sapply(uniqa5hub.list, countAu, totList = a5au.list), decreasing = TRUE)[1:5])
a6Top5 <- names(sort(sapply(uniqa6hub.list, countAu, totList = a6au.list), decreasing = TRUE)[1:5])
a7Top5 <- names(sort(sapply(uniqa7hub.list, countAu, totList = a7au.list), decreasing = TRUE)[1:5])
a8Top5 <- names(sort(sapply(uniqa8hub.list, countAu, totList = a8au.list), decreasing = TRUE)[1:5])
a9Top5 <- names(sort(sapply(uniqa9hub.list, countAu, totList = a9au.list), decreasing = TRUE)[1:5])
a10Top5 <- names(sort(sapply(uniqa10hub.list, countAu, totList = a10au.list), decreasing = TRUE)[1:5])
a11Top5 <- names(sort(sapply(uniqa11hub.list, countAu, totList = a11au.list), decreasing = TRUE)[1:5])

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

#takes time series talbe and return a simple array with zeros added for missing year
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

#the actual series
p1.a <- addZero(p1.1);p1.b <- addZero(p1.2);p1.c <- addZero(p1.3);p1.d <- addZero(p1.4);p1.e <- addZero(p1.5)
p2.a <- addZero(p2.1);p2.b <- addZero(p2.2);p2.c <- addZero(p2.3);p2.d <- addZero(p2.4);p2.e <- addZero(p2.5)
p3.a <- addZero(p3.1);p3.b <- addZero(p3.2);p3.c <- addZero(p3.3);p3.d <- addZero(p3.4);p3.e <- addZero(p3.5)
p4.a <- addZero(p4.1);p4.b <- addZero(p4.2);p4.c <- addZero(p4.3);p4.d <- addZero(p4.4);p4.e <- addZero(p4.5)
p5.a <- addZero(p5.1);p5.b <- addZero(p5.2);p5.c <- addZero(p5.3);p5.d <- addZero(p5.4);p5.e <- addZero(p5.5)
p6.a <- addZero(p6.1);p6.b <- addZero(p6.2);p6.c <- addZero(p6.3);p6.d <- addZero(p6.4);p6.e <- addZero(p6.5)
p7.a <- addZero(p7.1);p7.b <- addZero(p7.2);p7.c <- addZero(p7.3);p7.d <- addZero(p7.4);p7.e <- addZero(p7.5)
p8.a <- addZero(p8.1);p8.b <- addZero(p8.2);p8.c <- addZero(p8.3);p8.d <- addZero(p8.4);p8.e <- addZero(p8.5)
p9.a <- addZero(p9.1);p9.b <- addZero(p9.2);p9.c <- addZero(p9.3);p9.d <- addZero(p9.4);p9.e <- addZero(p9.5)
p10.a <- addZero(p10.1);p10.b <- addZero(p10.2);p10.c <- addZero(p10.3);p10.d <- addZero(p10.4);p10.e <- addZero(p10.5)
p11.a <- addZero(p11.1);p11.b <- addZero(p11.2);p11.c <- addZero(p11.3);p11.d <- addZero(p11.4);p11.e <- addZero(p11.5)

#attach AUs' publications per year to their names
a1AU <- as.character()
a1AU[1] <- paste(round(length(area1.1)/length(p1.a), 2), a1Top5[1], sep = ' ')
a1AU[2] <- paste(round(length(area1.2)/length(p1.b), 2), a1Top5[2], sep = ' ')
a1AU[3] <- paste(round(length(area1.3)/length(p1.c), 2), a1Top5[3], sep = ' ')
a1AU[4] <- paste(round(length(area1.4)/length(p1.d), 2), a1Top5[4], sep = ' ')
a1AU[5] <- paste(round(length(area1.5)/length(p1.e), 2), a1Top5[5], sep = ' ')

a2AU <- as.character()
a2AU[1] <- paste(round(length(area2.1)/length(p2.a), 2), a2Top5[1], sep = ' ')
a2AU[2] <- paste(round(length(area2.2)/length(p2.b), 2), a2Top5[2], sep = ' ')
a2AU[3] <- paste(round(length(area2.3)/length(p2.c), 2), a2Top5[3], sep = ' ')
a2AU[4] <- paste(round(length(area2.4)/length(p2.d), 2), a2Top5[4], sep = ' ')
a2AU[5] <- paste(round(length(area2.5)/length(p2.e), 2), a2Top5[5], sep = ' ')

a3AU <- as.character()
a3AU[1] <- paste(round(length(area3.1)/length(p3.a), 2), a3Top5[1], sep = ' ')
a3AU[2] <- paste(round(length(area3.2)/length(p3.b), 2), a3Top5[2], sep = ' ')
a3AU[3] <- paste(round(length(area3.3)/length(p3.c), 2), a3Top5[3], sep = ' ')
a3AU[4] <- paste(round(length(area3.4)/length(p3.d), 2), a3Top5[4], sep = ' ')
a3AU[5] <- paste(round(length(area3.5)/length(p3.e), 2), a3Top5[5], sep = ' ')

a4AU <- as.character()
a4AU[1] <- paste(round(length(area4.1)/length(p4.a), 2), a4Top5[1], sep = ' ')
a4AU[2] <- paste(round(length(area4.2)/length(p4.b), 2), a4Top5[2], sep = ' ')
a4AU[3] <- paste(round(length(area4.3)/length(p4.c), 2), a4Top5[3], sep = ' ')
a4AU[4] <- paste(round(length(area4.4)/length(p4.d), 2), a4Top5[4], sep = ' ')
a4AU[5] <- paste(round(length(area4.5)/length(p4.e), 2), a4Top5[5], sep = ' ')

a5AU <- as.character()
a5AU[1] <- paste(round(length(area5.1)/length(p5.a), 2), a5Top5[1], sep = ' ')
a5AU[2] <- paste(round(length(area5.2)/length(p5.b), 2), a5Top5[2], sep = ' ')
a5AU[3] <- paste(round(length(area5.3)/length(p5.c), 2), a5Top5[3], sep = ' ')
a5AU[4] <- paste(round(length(area5.4)/length(p5.d), 2), a5Top5[4], sep = ' ')
a5AU[5] <- paste(round(length(area5.5)/length(p5.e), 2), a5Top5[5], sep = ' ')

a6AU <- as.character()
a6AU[1] <- paste(round(length(area6.1)/length(p6.a), 2), a6Top5[1], sep = ' ')
a6AU[2] <- paste(round(length(area6.2)/length(p6.b), 2), a6Top5[2], sep = ' ')
a6AU[3] <- paste(round(length(area6.3)/length(p6.c), 2), a6Top5[3], sep = ' ')
a6AU[4] <- paste(round(length(area6.4)/length(p6.d), 2), a6Top5[4], sep = ' ')
a6AU[5] <- paste(round(length(area6.5)/length(p6.e), 2), a6Top5[5], sep = ' ')

a7AU <- as.character()
a7AU[1] <- paste(round(length(area7.1)/length(p7.a), 2), a7Top5[1], sep = ' ')
a7AU[2] <- paste(round(length(area7.2)/length(p7.b), 2), a7Top5[2], sep = ' ')
a7AU[3] <- paste(round(length(area7.3)/length(p7.c), 2), a7Top5[3], sep = ' ')
a7AU[4] <- paste(round(length(area7.4)/length(p7.d), 2), a7Top5[4], sep = ' ')
a7AU[5] <- paste(round(length(area7.5)/length(p7.e), 2), a7Top5[5], sep = ' ')

a8AU <- as.character()
a8AU[1] <- paste(round(length(area8.1)/length(p8.a), 2), a8Top5[1], sep = ' ')
a8AU[2] <- paste(round(length(area8.2)/length(p8.b), 2), a8Top5[2], sep = ' ')
a8AU[3] <- paste(round(length(area8.3)/length(p8.c), 2), a8Top5[3], sep = ' ')
a8AU[4] <- paste(round(length(area8.4)/length(p8.d), 2), a8Top5[4], sep = ' ')
a8AU[5] <- paste(round(length(area8.5)/length(p8.e), 2), a8Top5[5], sep = ' ')

a9AU <- as.character()
a9AU[1] <- paste(round(length(area9.1)/length(p9.a), 2), a9Top5[1], sep = ' ')
a9AU[2] <- paste(round(length(area9.2)/length(p9.b), 2), a9Top5[2], sep = ' ')
a9AU[3] <- paste(round(length(area9.3)/length(p9.c), 2), a9Top5[3], sep = ' ')
a9AU[4] <- paste(round(length(area9.4)/length(p9.d), 2), a9Top5[4], sep = ' ')
a9AU[5] <- paste(round(length(area9.5)/length(p9.e), 2), a9Top5[5], sep = ' ')

a10AU <- as.character()
a10AU[1] <- paste(round(length(area10.1)/length(p10.a), 2), a10Top5[1], sep = ' ')
a10AU[2] <- paste(round(length(area10.2)/length(p10.b), 2), a10Top5[2], sep = ' ')
a10AU[3] <- paste(round(length(area10.3)/length(p10.c), 2), a10Top5[3], sep = ' ')
a10AU[4] <- paste(round(length(area10.4)/length(p10.d), 2), a10Top5[4], sep = ' ')
a10AU[5] <- paste(round(length(area10.5)/length(p10.e), 2), a10Top5[5], sep = ' ')

a11AU <- as.character()
a11AU[1] <- paste(round(length(area11.1)/length(p11.a), 2), a11Top5[1], sep = ' ')
a11AU[2] <- paste(round(length(area11.2)/length(p11.b), 2), a11Top5[2], sep = ' ')
a11AU[3] <- paste(round(length(area11.3)/length(p11.c), 2), a11Top5[3], sep = ' ')
a11AU[4] <- paste(round(length(area11.4)/length(p11.d), 2), a11Top5[4], sep = ' ')
a11AU[5] <- paste(round(length(area11.5)/length(p11.e), 2), a11Top5[5], sep = ' ')


#making plots
plot(p1.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.15), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 1: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p1.b, type = 'o', col = 2)
lines(p1.c, type = 'o', col = 3)
lines(p1.d, type = 'o', col = 4)
lines(p1.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a1AU, lty = rep(1, 5), col = 1:5)

plot(p2.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.16), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 2: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p2.b, type = 'o', col = 2)
lines(p2.c, type = 'o', col = 3)
lines(p2.d, type = 'o', col = 4)
lines(p2.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a2AU, lty = rep(1, 5), col = 1:5)

plot(p3.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.15), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 3: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p3.b, type = 'o', col = 2)
lines(p3.c, type = 'o', col = 3)
lines(p3.d, type = 'o', col = 4)
lines(p3.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a3AU, lty = rep(1, 5), col = 1:5)

plot(p4.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.16), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 4: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p4.b, type = 'o', col = 2)
lines(p4.c, type = 'o', col = 3)
lines(p4.d, type = 'o', col = 4)
lines(p4.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a4AU, lty = rep(1, 5), col = 1:5)

plot(p5.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.21), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 5: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p5.b, type = 'o', col = 2)
lines(p5.c, type = 'o', col = 3)
lines(p5.d, type = 'o', col = 4)
lines(p5.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a5AU, lty = rep(1, 5), col = 1:5)

plot(p6.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.18), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 6: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p6.b, type = 'o', col = 2)
lines(p6.c, type = 'o', col = 3)
lines(p6.d, type = 'o', col = 4)
lines(p6.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a6AU, lty = rep(1, 5), col = 1:5)

plot(p7.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.23), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 7: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p7.b, type = 'o', col = 2)
lines(p7.c, type = 'o', col = 3)
lines(p7.d, type = 'o', col = 4)
lines(p7.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a7AU, lty = rep(1, 5), col = 1:5)

plot(p8.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.23), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 8: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p8.b, type = 'o', col = 2)
lines(p8.c, type = 'o', col = 3)
lines(p8.d, type = 'o', col = 4)
lines(p8.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a8AU, lty = rep(1, 5), col = 1:5)

plot(p9.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.20), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 9: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p9.b, type = 'o', col = 2)
lines(p9.c, type = 'o', col = 3)
lines(p9.d, type = 'o', col = 4)
lines(p9.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a9AU, lty = rep(1, 5), col = 1:5)

plot(p10.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.25), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 10: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p10.b, type = 'o', col = 2)
lines(p10.c, type = 'o', col = 3)
lines(p10.d, type = 'o', col = 4)
lines(p10.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a10AU, lty = rep(1, 5), col = 1:5)

plot(p11.a, type = 'o', xlim = c(1, 20), ylim = c(0, 0.20), xaxt = 'n', xlab = '', ylab = '')
title(main = ("Area 11: Top 5 Hubs' Publications over Year"), xlab = 'Year', ylab = 'Normalized Number of Publications')
lines(p11.b, type = 'o', col = 2)
lines(p11.c, type = 'o', col = 3)
lines(p11.d, type = 'o', col = 4)
lines(p11.e, type = 'o', col = 5)
axis(1, 1:20, as.character(1:20))
legend(locator(1), a11AU, lty = rep(1, 5), col = 1:5)

