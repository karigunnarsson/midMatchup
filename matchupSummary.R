#######
# In this file we'll create the final datamart, where we'll calculate the win ratio, gold and XP 
# difference between the heroes, the output file here should contain 113x113 lines, and could
# be used as input for a website.
#######
library(plyr)
library(dplyr)

combinedFinal <- readRDS("combinedFinal.RDS")

heroList <- unique(combinedFinal$heroID)

totalMerged <- NULL
count <- 0
for(i in heroList) {
  heroGames <- select(subset(combinedFinal, combinedFinal$heroID == i), matchID, radiantWin, heroID, gold, xp, side)
  heroGames$win <- ifelse(heroGames$radiantWin == 1 & heroGames$side == "Radiant",1,
                          ifelse(heroGames$radiantWin == 0 & heroGames$side == "Dire",1,0))
  heroGamesClean <- heroGames[,c(1,3,4,5,7)]
  colnames(heroGamesClean) <- c("matchID","hero1","gold1","xp1","win")
  
  otherHeroGames <- subset(combinedFinal, combinedFinal$heroID != i)
  otherHeroGamesClean <- otherHeroGames[,c(1,3,4,5)]
  colnames(otherHeroGamesClean) <- c("matchID","hero2","gold2","xp2")
  
  mergedGames <- merge(heroGamesClean, otherHeroGamesClean, by = "matchID")
  
  totalMerged <- rbind.fill(totalMerged, mergedGames)
}

# Now to summarize the data into the final dataset
sumData <- totalMerged %>%
             group_by(hero1, hero2) %>%
             summarise(meanXP1 = mean(xp1), meanXP2 = mean(xp2),
                       meanGold1 = mean(gold1), meanGold2 = mean(gold2),
                       winRate = sum(win)/n(), numGames = n())

sumDataClean <- subset(sumData, sumData$numGames>20)

# Add hero names to make more readable
heroNames <- read.csv2("heronames.csv")
colnames(heroNames) <- c("hero_id", "heroName1")

name1 <- merge(sumDataClean, heroNames, by.x = "hero1", by.y = "hero_id", all.x = TRUE)

colnames(heroNames) <- c("hero_id", "heroName2")
name2 <- merge(name1, heroNames, by.x = "hero2", by.y = "hero_id", all.x = TRUE)

name2$XPDiff <- name2$meanXP1-name2$meanXP2
name2$goldDiff <- name2$meanGold1-name2$meanGold2

finalData <- select(name2, heroName1, heroName2, XPDiff, goldDiff, winRate)
finalData <- arrange(finalData, heroName1)

# Round numbers to make pretty
finalData$XPDiff <- round(finalData$XPDiff)
finalData$goldDiff <- round(finalData$goldDiff)
finalData$winRate <- round(finalData$winRate, digits=3)

saveRDS(finalData, "MidHeroShiny/midData.rds")

# I'm also interested in the overall most succesful hero (regardless of matchup) measured by Xp, Gold and winrate.

topHero <- totalMerged %>%
            group_by(hero1) %>%
            summarise(meanXP = mean(xp1 - xp2),
                      meanGold = mean(gold1 - gold2),
                      winRate = sum(win)/n(), 
                      numGames = n())
topHeroClean <- subset(topHero, topHero$numGames > 200)

topHeroClean <- select(merge(topHeroClean, heroNames, by.x = "hero1", by.y = "hero_id", all.x = TRUE),
                       heroName2, meanXP, meanGold, winRate, numGames)
colnames(topHeroClean) <- c("hero", "xpDiff", "goldDiff", "winPct", "numGames")

# Round numbers to make pretty
topHeroClean$xpDiff <- round(topHeroClean$xpDiff)
topHeroClean$goldDiff <- round(topHeroClean$goldDiff)
topHeroClean$winPct <- round(topHeroClean$winPct, digits=3)
