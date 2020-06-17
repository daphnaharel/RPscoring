#' Resolve Ties
#' @description Function to resolve ties between competitors.
#' @param data dataset with competitors as rows and judges as columns
#' @param contestants vector with which contestant numbers to resolve ties for
#' @param column column  of the dash matrix to begin with
#' @return A list:
#' \item{winnerfound}{method by which winner was found}
#' \item{winner}{vector with whom the winners were}
#' @examples
#' resolveTies(testdata, c(1,2), 1)
#' @export

resolveTies <- function(data, contestants, column) {
  numJudges <- ncol(data)
  majority <- ceiling(numJudges/2)

  #start by 1s and 2s
  sumscoreThreshold <- column
  nextScore <- column+1
  sumscores <- apply(data[contestants,], 1, function(a){
    sum(a[which(a <= sumscoreThreshold)])
  })
  winner <- contestants[which.min(sumscores)]
  winnerScore <- sumscores[which(contestants == winner)]
  ## check if anyone else has the same score
  tiedwinner <- any(winnerScore == sumscores[
    which(contestants %in% setdiff(contestants,winner))])

  if (!tiedwinner) {
    return(list(winnerfound="sumscore",
                winner=winner))
  }
  if(tiedwinner) {

    # there's only one tie
    if (length(sumscores[which(as.vector(table(sumscores)) > 1)]) == 1){
      while(tiedwinner & nextScore <= ncol(data)) {
        nscores <- apply(data[contestants,], 1, function(a){
          length(which(a == nextScore))
        })
        winner <- contestants[which.max(nscores)]
        winnerScore <- nscores[which(contestants == winner)]
        tiedwinner <- any(winnerScore == nscores[
          which(contestants %in% setdiff(contestants,winner))])
        if (!tiedwinner) {
          return(list(winnerfound="nextscore",
                      winner=winner))
        } else {
          nextScore <- nextScore + 1
          }
      }
      if (nextScore > ncol(data)) {

        reducedData <- apply(data[contestants,], 2, function(a) {order(a)})

        reducedRanking <- rankContestants(reducedData)
        winner <- contestants[which(reducedRanking ==1)]
        if(length(winner) == 1)
          return(list(winnerfound="recursivecontests",
                      winner=winner))
        else{
          return(list(winnerfound = "nowinner", winner = winner))
        }

      }
    }

    ## there are more than one tie on sumscores.
    else if(length(sumscores[which(as.vector(table(sumscores)) > 1)]) > 1){

      tiebreakContestants = contestants[which(sumscores == min(sumscores))]

      reducedData <- apply(data[tiebreakContestants,], 2, function(a) {order(a)})
      reducedRanking = rankContestants(reducedData)
      winner = tiebreakContestants[order(reducedRanking, decreasing = T)]

      return(list(winnerfound="sumscoretie",
                  winner=winner))
    }

  }
}
