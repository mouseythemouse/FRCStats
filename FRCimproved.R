# Hi, there's like comments all over this thing, read them :)
# This R program is like super bashy and takes a long time to run
# If things don't work, just chat me or email me or talk to me and I'll fix them
# I recommend taking the team's 5-10 most reflective data points
# A good way to do the above is to write a function that takes the data points closest to the median of the dataset for columns B/C
# For column D, just consolidate the data (eg. we have 15 data points for a team, 9 0s and 6 1s, just make the new set of 5 contain 3 0s and 2 1s)
oneTeamData <- function(rawTeamData, rawTeamGear, rawTeamAuto) {
  score <- 0
  nonGearScore <- 0
  gearScore <- 0
  auto <- 0
  matches <- length(rawTeamData)
  gears <- 0
  dataList <- c(0)
  # Right now, the three lines below compute not-weighted means
  # Take out the "unique() to compute means of all points (weighted)"
  # Tradeoff: A lot more accuracy, but a lot more time
  teamData <- unique(rawTeamData)
  teamGear <- unique(rawTeamGear)
  teamAuto <- unique(rawTeamAuto)
  for (x in 1:length(teamData)) {
    for (y in 1:length(teamGear)) {
      for (z in 1:length(teamAuto)) {
        gearScore <- 0
        nonGearScore <- teamData[x] + teamAuto[z]*60
        gears <- teamGear[y]
        auto <- teamAuto[z]
        if(auto==0) {
          if(gears>=12) {
            gearScore <- 120
          } else if(gears>=6) {
            gearScore <- 80 + ((gears-6)/6)*40
          } else if(gears>=2) {
            gearScore <- 40 + ((gears-2)/4)*40
          } else if(gears>=1) {
            gearScore <- 40
          }
        } else if(auto==1) {
          if(gears>=12) {
            gearScore <- 120
          } else if(gears>=6) {
            gearScore <- 80 + ((gears-6)/6)*40
          } else if(gears>=2) {
            gearScore <- 40 + ((gears-2)/4)*40
          }
        }
        score <- gearScore + nonGearScore
        dataList <- c(dataList, score)
      }
    }
  }
  dataList <- dataList[2:length(dataList)-1]
  return(dataList)
} #works
allianceData <- function(t1,t2,t3) {
  score1 <- t1
  score2 <- t2
  score3 <- t3
  score <- 0
  scoreList <- c(0)
  for(x in 1:length(t1)) {
    for(y in 1:length(t2)) {
      for(z in 1:length(t3)) {
        score <- t1[x] + t2[y] + t3[z]
        scoreList <- c(scoreList, score)
      }
    }
  }
  return(scoreList)
}
compareData <- function(l1, l2) {
  list1 <- l1
  list2 <- l2
  g <- expand.grid(list1, list2)
  l1Bigger <- sum(g$Var1 > g$Var2)
  l2Bigger <- sum(g$Var1 < g$Var2)
  ties <- sum(g$Var1 == g$Var2)
  total <- length(list1) * length(list2)
  percents <- c(l1Bigger/total * 100, l2Bigger/total * 100, ties/total * 100)
  return(percents)
}
# Team1 of alliance1
# All column B values go here
rTD1 <- c(55,55,55,5,5,5,5,55,55)
# All column C values go here
rTG1 <- c(3,2,3,3,1,2,0,0,0)
# All column D values go here
rTA1 <-c(0,0,0,0,0,1,0,0,0)
# Team2 of alliance1, same as before
rTD2 <- c(0,5,55,55,55,55,55,55,5)
rTG2 <- c(1,1,0,0,0,1,1,1,1)
rTA2 <-c(0,0,0,0,0,0,0,0,0)
# Team3 of alliance1, same as before
rTD3 <- c(55,55,5,5,55,5,55,55,55,50,55,55,5,55,50,5,0,55,55,55.3333333333,55,55,5,5,55,5,55,55,55,55,50,55,55)
rTG3 <- c(0,3,0,1,2,2,1,2,0,1,1,0,0,5,3,1,0,4,2,5,0,3,0,1,2,0,1,0,0,2,1,1,0)
rTA3 <-c(0,1,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0)
teamOne <- (oneTeamData(rTD1,rTG1,rTA1))
teamTwo <- (oneTeamData(rTD2,rTG2,rTA2))
teamThree <- (oneTeamData(rTD3,rTG3,rTA3))
teamTotal <- allianceData(teamOne, teamTwo, teamThree)
# Team1 of alliance2, same as before
bTD1 <- c(0,0,0,0,50,0,5,55,55,55,5,55,55,55,5,55,50,5,0,0,0,0,50,0,0,5,55,55,55)
bTG1 <- c(0,1,0,1,0,0,2,0,2,1,1,1,0,1,2,0,3,2,0,1,0,0,0,0,0,1,0,2,0)
bTA1 <-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
# Team2 of alliance2, same as before
bTD2 <- c(5,55,5,55,5,5,55,55,0,5,5)
bTG2 <- c(2,3,3,0,4,1,2,2,3,0,0)
bTA2 <-c(0,0,0,0,0,0,0,0,0,0,0)
# Team3 of alliance3, same as before
bTD3 <- c(5,55,5,5,55,5,55,55)
bTG3 <- c(1,4,2,2,2,2,3,2)
bTA3 <-c(0,1,0,0,0,0,1,0)
bTeamOne <- (oneTeamData(bTD1,bTG1,bTA1))
bTeamTwo <- (oneTeamData(bTD2,bTG2,bTA2))
bTeamThree <- (oneTeamData(bTD3,bTG3,bTA3))
bTeamTotal <- allianceData(bTeamOne, bTeamTwo, bTeamThree)
# Prints three percentages, (chance of alliance1 winning, chance of alliance2 winning, chance of tie)
print(compareData(teamTotal, bTeamTotal))

