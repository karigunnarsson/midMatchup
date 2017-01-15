# Load libraries needed

library(jsonlite)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

# Need to set a high scientific notation penalty, or the paste function will make the sql query with scientific numbers
options("scipen"=10)

# Number of matchIDs to extract
numMatches <- 500000

# Create SQL query text
sqlQuery <- paste("select  * from public_matches where start_time > 1483362553 AND duration > 900 AND avg_mmr > 2500 AND num_mmr > 2 limit ",
                  numMatches, ";", sep = "")

# Execute query on opendota API
gameList <- fromJSON(paste("https://api.opendota.com/api/explorer?sql=", URLencode(sqlQuery), sep=""))
gameListDF <- gameList$rows

# Initialize the dataframe
finalList <- NULL

# Iterate through all the games we have
for (i in 194228:nrow(gameListDF)) {

  # Since this takes days to run, i¨ve learnt the hard way that sometimes your computer doesn't like you and decides to restart
  # or crash, so we save every now and then.
  if (i %% 25000 == 0 ) {
    saveRDS(finalList, "matches.RDS")
  }
  
  print(paste("Parsing game: ", i))
  matchID <- gameListDF[i, "match_id"]

  startTime <- proc.time()[3]
  
  # Read the JSON (using error handling)
  readJSON <- tryCatch(
    fromJSON(paste("https://api.opendota.com/api/matches/", matchID, sep = "")),
    error = function (e) {"error"}
  )
  
  # Check if we got HTTP error, if so, go to next matchID
  if (readJSON == "error") {
    print("      Error in API fetching")
    
    endTime <- proc.time()[3]
    totTime <- endTime - startTime
    
    if (totTime >= 1) {
      print(proc.time()[3] - startTime)
      next
    } else {
      Sys.sleep(1.05-totTime)
      print(proc.time()[3] - startTime)
      next
    }
  }

  # If no cosmetics are shown, we assume it's not parsed and go to the next one
  if (is.null(readJSON$cosmetics)) {
    print(paste("      replay not parsed."))
    
    endTime <- proc.time()[3]
    totTime <- endTime - startTime
    
    if (totTime >= 1) {
      print(proc.time()[3] - startTime)
      next
    } else {
      Sys.sleep(1.05-totTime)
      print(proc.time()[3] - startTime)
      next
    }
    }
  
  # Obtain list of the position for the first 10 minute for every player
  if (is.data.frame(readJSON$players$lane_pos)) {
    lanePosList <- flatten(readJSON$players$lane_pos)
  } else {
    print(paste("      replay not parsed."))
    
    endTime <- proc.time()[3]
    totTime <- endTime - startTime
    
    if (totTime >= 1) {
      print(proc.time()[3] - startTime)
      next
    } else {
      Sys.sleep(1.05-totTime)
      print(proc.time()[3] - startTime)
      next
    }
  }
  
  # Game time in minutes
  gameTime <- round((readJSON$duration/60) / 10, digits = 0) * 10
  
  # Iterate through each player, getting the values we need.
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
    
    gold <- unlist(readJSON$players$gold_t[j])[11]
    xp <- unlist(readJSON$players$xp_t[j])[11]
    
    # Create a herostring, adding tower damage and some other info for later use. Also add matchID so we can
    # use this as a basis for the dataset with heroes per match
    heroString <- as.data.frame(cbind(readJSON$match_id,
                                      readJSON$radiant_win,
                                      readJSON$duration,
                                      readJSON$players$player_slot[j],
                                      readJSON$players$hero_id[j],
                                      gold,
                                      xp,
                                      meanPos$meanX,
                                      meanPos$meanY,
                                      avgDistMed,
                                      readJSON$players$lane[j]))
    colnames(heroString) <- c("matchID","radiantWin","duration","playerSlot",
                                    "heroID","gold","xp","meanX","meanY","avgDistMean","oDotaLane")
    
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
  print(paste("      replay parsed!"))
  
  endTime <- proc.time()[3]
  totTime <- endTime - startTime
  
  if (totTime < 1) {
    Sys.sleep(1.05-totTime)
    print(proc.time()[3] - startTime)
  }
}

# Assign a side
finalList$side <- ifelse(finalList$playerSlot < 10, "Radiant", "Dire")

# Save the file
saveRDS(finalList, "matches.RDS")