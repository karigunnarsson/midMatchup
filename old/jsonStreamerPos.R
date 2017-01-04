###########
# This is from an older version when i was using the datadump to get data, if you're working with the datadump
# and not the API you can use this to stream games from the datadump JSOn file (i could never get the standard
# R streamers to work because the format so i had to write me own).
###########


streamJSONPos <- function(con, pagesize, numMatches){
  library(jsonlite)
  library(data.table)
  library(plyr)
  library(dplyr)
  library(tidyr)
  
  ## "con" is the file connection
  ## "pagesize" is number of lines streamed for each iteration.
  ## "numMatches" is number of games we want to output
  
  outputFile <- 0
  lineCount <- 0
  matchCount <- 0
  print("Starting parsing games...")
  print(paste("Number of games parsed:",matchCount))
  # Stream in using readLines until we reach the number of matches we want.
  while(matchCount < numMatches) {

    initialJSON = readLines(con, n = pagesize)
    
    collapsedJSON <- paste(initialJSON[2:pagesize], collapse="")
    fixedJSON <- sprintf("[%s]", collapsedJSON, collapse=",")
    readJSON <- jsonlite::fromJSON(fixedJSON)
    
    lineCount <- lineCount + 1
    
    finalList <- 0
    ## Run throught he initial file
    for (i in 1:length(readJSON$match_id)) {
      ## Fetch aggregated kill data for each hero in the game
      
      lanePosList <- flatten(readJSON$players[[i]]$lane_pos)

      gameTime <- round((readJSON$duration[i]/60) / 10, digits = 0) * 10
      for(j in 1:10) {
        
        # Create a list of lane position first 10 minutes of game
        heroLanePos <- flatten(lanePosList[j,])
        lanePosLong <- as.data.frame(gather(heroLanePos))
        if(length(lanePosLong) > 0) {
          lanePosLong <- subset(lanePosLong, !is.na(lanePosLong$value)) } else {
            next
          }
        
        # We get output in weird format, so i have to do some string work to get the x and y coordintes
        lanePosLong$x <- as.numeric(substr(lanePosLong$key, 1, regexpr('\\.', lanePosLong$key)-1))
        lanePosLong$y <- as.numeric(substr(lanePosLong$key, regexpr('\\.', lanePosLong$key)+1, nchar(lanePosLong$key)))
        
        lanePosFinal <- lanePosLong[,c(3,4,2)]
        
        # Calculate weighted mid point of the lane position
        meanPos <- data.frame(meanX = weighted.mean(lanePosFinal$x, lanePosFinal$value), 
                              meanY = weighted.mean(lanePosFinal$y, lanePosFinal$value))
        
        # Calculate distance of each point from the median
        lanePosFinal$medDist <- sqrt((lanePosFinal$x - meanPos$meanX)^2 + (lanePosFinal$y - meanPos$meanY)^2)
        
        avgDistMed <- weighted.mean(lanePosFinal$medDist, lanePosFinal$value)
        
        gold <- unlist(readJSON$players[[i]]$gold_t[j])[11]
        xp <- unlist(readJSON$players[[i]]$xp_t[j])[11]
        # Create a herostring, adding tower damage and some other info for later use. Also add matchID so we can
        # use this as a basis for the dataset with heroes per match
        heroString <- as.data.frame(cbind(readJSON$match_id[[i]],
                            readJSON$radiant_win[[i]],
                            readJSON$duration[[i]],
                            readJSON$players[[i]]$player_slot[j],
                            readJSON$players[[i]]$hero_id[j],
                            gold,
                            xp,
                            meanPos$meanX,
                            meanPos$meanY,
                            avgDistMed))
        colnames(heroString)[1:10] <- c("matchID","radiantWin","duration","playerSlot",
                                       "heroID","gold","xp","meanX","meanY","avgDistMean")
        
        heroString[is.na(heroString)] <- 0
        if (j == 1) {
          matchList <- heroString
        } else {
          matchList <- rbind.fill(matchList, heroString)
        }
      }
      
      if (length(finalList) == 1) {
        finalList <- matchList
      } else {
        finalList <- rbind.fill(finalList, matchList)
      } 
    }
    
    matchCount <- matchCount + length(unique(finalList$matchID))
    
    if (length(outputFile) == 1) {
      outputFile <- finalList
    } else {
      outputFile <- rbind.fill(outputFile, finalList)
    } 
    print(paste("Number of games parsed:",matchCount))
  }
  return(outputFile)
}