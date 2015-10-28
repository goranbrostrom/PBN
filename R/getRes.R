getRes <- function(board){
    at <- which(substr(board, 1, 11) == "[ScoreTable")
    header <- board[at]
    tabl <- board[(at + 1):length(board)]
    
    header <- strsplit(header, split = ";", fixed = TRUE)[[1]]
    header <- strsplit(header, split = "\\", fixed = TRUE)
    header <- unlist(lapply(header, function(x) x[1]))
    tabl <- gsub(" rum", "_rum", tabl)
    tc <- textConnection(tabl)
    tabl <- read.table(tc, header = FALSE, stringsAsFactors = FALSE)
    close(tc)
    header <- header[4:11]
    tabl <- tabl[4:11]
    names(tabl) <- header
    is.na(tabl$Score_NS) <- tabl$Score_NS == "-"
    is.na(tabl$Score_EW) <- tabl$Score_EW == "-"
    tabl$Score_NS <- as.integer(tabl$Score_NS)
    tabl$Score_EW <- as.integer(tabl$Score_EW)
    tabl$Score_NS[is.na(tabl$Score_NS)] <- -tabl$Score_EW[is.na(tabl$Score_NS)]
    tabl$Score_EW[is.na(tabl$Score_EW)] <- -tabl$Score_NS[is.na(tabl$Score_EW)]
    ##list(header = header, tabl = tabl)
    tabl
}
