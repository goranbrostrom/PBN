parse_pbn<- function(pbn){
    pbn <- readLines(pbn, encoding = "latin1")
    st <- which(substr(pbn, 1, 6) == "[Board")
    noofBoards <- length(st)
    getBoard <- function(no){
        if (no > noofBoards) error("There are only 12 boards")
        start <- st[no]
        if (no == noofBoards){
            slut <- length(pbn)
        }else{
            slut <- st[no + 1] - 1
        }
        board <- pbn[start:slut]
        out <- board[substr(board, 1, 6) == "[Deal "]
        out <- c(out, board[substr(board, 1, 6) == "[Vulne"])
        start <- which(substr(board, 1, 11) == "[ScoreTable")
        slut <- which(substr(board, 1, 8) == "[Optimum")[1] - 1
        out <- c(out, board[start:slut])
        out <- gsub("\"", "", out)
        out
    }
    Boards <- vector("list", noofBoards)
    for (i in 1:noofBoards){
        Boards[[i]] <- getBoard(i)
    }
    Boards
}
