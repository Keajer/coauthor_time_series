#parse8.R
#start with a new set of data (ACM) using old analysis

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
#after combining two set of data
#13038 publications are duplicated

uniqIDs <- unique(IDs)
#get the list of all publications and remove duplicates
pubList = list()
for(i in 1:(length(ok)-1)){
        if(unlist(strsplit(huge[(ok[i])], ' '))[2] %in% uniqIDs){
                uniqIDs <- uniqIDs[-which(uniqIDs == unlist(strsplit(huge[ok[i]], ' '))[2])]
                pubList[[length(pubList) + 1]] <- huge[(ok[i]) : (ok[i+1]-2)]
        }
}
pubList[[length(pubList) + 1]] <- huge[ok[length(ok)] : (length(huge) -1)]

pubIDs <- rep('', length(pubList))
for(i in 1:length(pubList)){
        pubIDs[i] <- unlist(strsplit(pubList[[i]][1], ' '))[2]
}

names(pubList) <- pubIDs

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

#this function gets year as numerical value from a publication
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

