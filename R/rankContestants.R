#' Ranking of Contestants
#' @description Function to rank contestants
#' @param data dataset with competitors as rows and judges as columns
#' @return A vector:
#' \item{finalranking}{final rankings of the competitors}
#' @examples
#' rankContestants(testdata)
#' @export

rankContestants <- function(data) {
  dashmat <- dashmatrix(data)
  numJudges <- ncol(data)
  majority <- ceiling(numJudges/2)
  finalRanking <- rep(NA, nrow(data))
  rankPlace <- 1
  removedFromRank <- c()
  col = 1

  # This works if its truly a perfect tie, but not when called recursively
  if (all(apply(dashmat, 2, function(x) length(unique(x)) == 1) == TRUE)){
    finalRanking[1:nrow(data)] = rep(rankPlace, nrow(data))
    rankPlace = rankPlace + nrow(data)
  }
  else if (any(apply(dashmat, 2, function(x) length(unique(x)) == 1) != TRUE)) {
    while (col <= ncol(dashmat)) {
      achievedMajority <- setdiff(which(dashmat[,col] >= majority), removedFromRank)
      if (length(achievedMajority) == 0) {
        col = col + 1
      }
      else if (length(achievedMajority) == 1) {
        # majority detected
        finalRanking[achievedMajority] <- rankPlace
        removedFromRank <- c(removedFromRank, achievedMajority)
        rankPlace <- rankPlace + 1
        col = col + 1
      }
      else if (length(achievedMajority) > 1) {
        while (length(achievedMajority) >= 2) {
          ## check if there is a unique winner in this column
          winner <- achievedMajority[which.max(dashmat[achievedMajority, col])]
          winnerScore <- dashmat[winner, col]
          ## check if anyone else has the same score
          ties <- any(winnerScore == dashmat[setdiff(achievedMajority,winner), col])
          ## if not, then declare unique winner
          if (!ties) {
            finalRanking[winner] <- rankPlace
            rankPlace <- rankPlace + 1
            removedFromRank <- c(removedFromRank, winner)
            achievedMajority <- setdiff(achievedMajority, winner)
            col = col + length(winner) - 1
          }
          else {
            # resolve ties
            tieResults <- resolveTies(data, achievedMajority, col)

            if (tieResults$winnerfound == "sumscoretie"){
              finalRanking[tieResults$winner] <- c(rankPlace:c(rankPlace + length(tieResults$winner)-1))
              rankPlace <- rankPlace + length(tieResults$winner)
              removedFromRank <- c(removedFromRank, tieResults$winner)
              achievedMajority <- setdiff(achievedMajority, tieResults$winner)
              col = col + length(tieResults$winner) - 1

            }
            else if (tieResults$winnerfound == "nextscore") {
              finalRanking[tieResults$winner] <- rankPlace
              rankPlace <- rankPlace + 1
              removedFromRank <- c(removedFromRank, tieResults$winner)
              achievedMajority <- setdiff(achievedMajority, tieResults$winner)
              col = col + length(tieResults$winner) - 1
            }
            else if (tieResults$winnerfound == "recursivecontests") {
              finalRanking[tieResults$winner] <- rankPlace
              rankPlace <- rankPlace + 1
              removedFromRank <- c(removedFromRank, tieResults$winner)
              achievedMajority <- setdiff(achievedMajority, tieResults$winner)
              col = col + length(tieResults$winner) - 1
            }
            else if (tieResults$winnerfound == "sumscore"){
              finalRanking[tieResults$winner] <- rankPlace
              rankPlace <- rankPlace + 1
              removedFromRank <- c(removedFromRank, tieResults$winner)
              achievedMajority <- setdiff(achievedMajority, tieResults$winner)
              col = col + length(tieResults$winner) - 1
            }
            else if (tieResults$winnerfound == "nowinner"){
              finalRanking[tieResults$winner] <- rankPlace
              rankPlace <- rankPlace + length(tieResults$winner)
              removedFromRank <- c(removedFromRank, tieResults$winner)
              achievedMajority <- setdiff(achievedMajority, tieResults$winner)
              col = col + length(tieResults$winner) - 1
            }
          }

          if(length(achievedMajority) == 1){
            finalRanking[achievedMajority] <- rankPlace
            removedFromRank <- c(removedFromRank, achievedMajority)
            rankPlace <- rankPlace + 1
            col = col + 1
          }

        }



      }
    }
  }
  return(finalRanking)

}
