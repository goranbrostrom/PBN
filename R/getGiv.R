getGiv <- function(board){
    ## Måste fixa zoner också!
    ## OK, board innehåller nu två strängar, '[Deal...' och '[Vulnerable]]
    ## 'no' is "Board No."

    ## Update: Remove 'no' from argument list, read it from 'board' instead.
    ##         Argument 'board' is now full output from 'parse_pbn'
    no <- board[1] # Board No.
    no <- strsplit(no, split = " ", fixed = TRUE)[[1]][2]
    no <- gsub("]", "", no)
    txt <- board[2]  # Deal
    vul <- board[3]  # Vulnerability
    vul <- gsub("[", "", vul, fixed = TRUE)
    vul <- gsub("]", "", vul, fixed = TRUE)
    vul <- strsplit(vul, split = " ")[[1]][2]
    x <- strsplit(txt, split = " ")[[1]]
    x <- x[-1]
    giv <- substr(x[1], 1, 1) # Dealer
    x[1] <- strsplit(x[1], split = ":")[[1]][2]
    x[4] <- strsplit(x[4], split = "]")[[1]][1]
    if (giv == "N"){
        names(x) <- c("N", "E", "S", "W")
        giv <- "North"
    }else if (giv == "E"){
        names(x) <- c("E", "S", "W", "N")
        giv <- "East"
    }else if (giv == "S"){
        names(x) <- c("S", "W", "N", "E")
        giv <- "South"
    }else if (giv == "W"){
        names(x) <- c("W", "N", "E", "S")
        giv <- "West"
    }else{
        stop("Unknown dealer!")
    }
    out <- character(12)
    for(i in 1:12) out[i] <- "                                       "
    Ncols <- strsplit(x["N"], ".", fixed = TRUE)[[1]]
    sln <- length(Ncols)
    if (sln < 4) for (i in 1:(4-sln)) Ncols <- c(Ncols, "")
    Ncols[Ncols == ""] <- "--"
    Wcols <- strsplit(x["W"], ".", fixed = TRUE)[[1]]
    sln <- length(Wcols)
    if (sln < 4) for (i in 1:(4-sln)) Wcols <- c(Wcols, "")
    Wcols[Wcols == ""] <- "--"
    Ecols <- strsplit(x["E"], ".", fixed = TRUE)[[1]]
    sln <- length(Ecols)
    if (sln < 4) for (i in 1:(4-sln)) Ecols <- c(Ecols, "")
    Ecols[Ecols == ""] <- "--"
    Scols <- strsplit(x["S"], ".", fixed = TRUE)[[1]]
    sln <- length(Scols)
    if (sln < 4) for (i in 1:(4-sln)) Scols <- c(Scols, "")
    Scols[Scols == ""] <- "--"
    ## North:
    substr(out[1], 2, 3) <- as.character(no)
    substr(out[1], 14, 14 + 13) <- Ncols[1]
    substr(out[2], 2, 6) <- giv
    substr(out[2], 14, 14 + 13) <- Ncols[2]
    substr(out[3], 2, 7) <- vul
    substr(out[3], 14, 14 + 13) <- Ncols[3] 
    substr(out[4], 14, 14 + 13) <- Ncols[4]
    ## West and East (on the same row)
    substr(out[5], 1, 13) <- Wcols[1]
    substr(out[5], 26, 26 + 13) <- Ecols[1]
    substr(out[6], 1, 13) <- Wcols[2]
    substr(out[6], 26, 26 + 13) <- Ecols[2]
    substr(out[7], 1, 13) <- Wcols[3]
    substr(out[7], 26, 26 + 13) <- Ecols[3]
    substr(out[8], 1, 13) <- Wcols[4]
    substr(out[8], 26, 26 + 13) <- Ecols[4]
    ## South
    substr(out[9], 14, 14 + 13) <- Scols[1]
    substr(out[10], 14, 14 + 13) <- Scols[2]
    substr(out[11], 14, 14 + 13) <- Scols[3]
    substr(out[12], 14, 14 + 13) <- Scols[4]
    out
}
