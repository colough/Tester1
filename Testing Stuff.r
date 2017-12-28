

#-- messing with a few different model types

#-- linear regression
{
require(plyr)
require(pls)
require(data.table)

setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2015 2016",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
HData$Team.Favourite = HData$Home.Favourite
HData$Opposition = HData$AwayTeam
HData$Team.Form = HData$Home.Form
HData$Opposition.Form = HData$Away.form
HData$Team.Shots.on.Target.Form = HData$Home.Shots.on.Target.Form
HData$Opposition.Shots.on.Target.Form = HData$Away.Shots.on.Target.Form
HData$Team.Shots.Conceded.Form = HData$Home.Shots.Conceded.Form
HData$Opposition.Shots.Conceded.Form = HData$Away.Shots.Conceded.Form
HData$Team.Goals.Scored.Form = HData$Home.Goals.Scored.Form
HData$Opposition.Goals.Scored.Form = HData$Away.Goals.Scored.Form
HData$Team.Goals.Conceded.Form = HData$Home.Goals.Conceded.Form
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Team.Corners.Form = HData$Home.Corners.Form
HData$Team.Fouls.Form = HData$Home.Team.Fouls.Form
HData$Team.Yellow.Cards = HData$Home.Yellow.Cards
HData$Team.Red.Cards = HData$Home.Red.Cards
HData$Opposition.Corners.Form = HData$Away.Corners.Form
HData$Opposition.Fouls.Form = HData$Away.Team.Fouls.Form
HData$Opposition.Yellow.Cards = HData$Away.Yellow.Cards
HData$Opposition.Red.Cards = HData$Away.Red.Cards
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Relative.Goals.Form = ((HData$Team.Goals.Scored.Form - HData$Team.Goals.Conceded.Form) - (HData$Opposition.Goals.Scored.Form - HData$Opposition.Goals.Conceded.Form))
HData$Team.Odds = HData$B365H
HData$Draw.Odds = HData$B365D
HData$Opposition.Odds = HData$B365A
HData$Team.AH.Odds = HData$Ave.AH.Home.Odds
HData$Team.Handicap = HData$Asian.Handicap
HData$Opposition.AH.Odds = HData$Ave.AH.Away.Odds
HData$Team.Goal.Diff = HData$Full.Time.Home.Goals - HData$Full.Time.Away.Goals
HData$Home.Away = rep("Home",nrow(HData))

#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
#-Create the variables that we need - first for all the Away matches
AData$Team.Favourite = AData$Away.Favourite
AData$Opposition = AData$HomeTeam
AData$Team.Form = AData$Away.form
AData$Opposition.Form = AData$Home.Form
AData$Team.Shots.on.Target.Form = AData$Away.Shots.on.Target.Form
AData$Opposition.Shots.on.Target.Form = AData$Home.Shots.on.Target.Form
AData$Team.Shots.Conceded.Form = AData$Away.Shots.Conceded.Form
AData$Opposition.Shots.Conceded.Form = AData$Home.Shots.Conceded.Form
AData$Team.Goals.Scored.Form = AData$Away.Goals.Scored.Form
AData$Opposition.Goals.Scored.Form = AData$Home.Goals.Scored.Form
AData$Team.Goals.Conceded.Form = AData$Away.Goals.Conceded.Form
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Team.Corners.Form = AData$Away.Corners.Form
AData$Team.Fouls.Form = AData$Away.Team.Fouls.Form
AData$Team.Yellow.Cards = AData$Away.Yellow.Cards
AData$Team.Red.Cards = AData$Away.Red.Cards
AData$Opposition.Corners.Form = AData$Home.Corners.Form
AData$Opposition.Fouls.Form = AData$Home.Team.Fouls.Form
AData$Opposition.Yellow.Cards = AData$Home.Yellow.Cards
AData$Opposition.Red.Cards = AData$Home.Red.Cards
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Relative.Goals.Form = ((AData$Team.Goals.Scored.Form - AData$Team.Goals.Conceded.Form) - (AData$Opposition.Goals.Scored.Form - AData$Opposition.Goals.Conceded.Form))
AData$Team.Odds = AData$B365A
AData$Draw.Odds = AData$B365D
AData$Opposition.Odds = AData$B365H
AData$Team.AH.Odds = HData$Ave.AH.Away.Odds
AData$Team.Handicap = HData$Asian.Handicap
AData$Opposition.AH.Odds = HData$Ave.AH.Home.Odds
AData$Team.Goal.Diff = AData$Full.Time.Away.Goals - AData$Full.Time.Home.Goals
AData$Home.Away = rep("Away",nrow(AData))

#-brill now we're cooking
#-so now that we have those two datasets let's append them and add them to our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))

ComboData <- cbind(ComboData,Team)

if(i == 1){
TeamData <- ComboData
}else{
TeamData <- rbind(TeamData,ComboData)
}

TeamData <- as.data.table(TeamData)
}


#- ok so we have a tasty little treat here where we have to order the teamdata dataframe by teams seasons and gameweeks and then
#- we have to create a lookup table which calculates our basic strength, join that to our original
#- then create a win streak count and subsequent probability variable
#- also for shits and giggles as this is just the data manipulation stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game.Week.Index)]
#-create the points the team scored
PreppedData$Team.Points <- ifelse(PreppedData$Team.Goal.Diff > 0, 3, ifelse(PreppedData$Team.Goal.Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game.Week.Index <= 6,.(Initial.Strength = ave(Team.Points)), by =.(Season, Team)]
OppStr <- PreppedData[Game.Week.Index <= 6,.(Opp.Initial.Strength = ave(Team.Points)), by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)
require(plyr)
PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
#-kl
#- nope it'll never catch on I mean "cool"
#-ok now I want to add a winning streak variable
#-for this one I'm going to have to cheat just a little bit as we need the average streak per season what we can then do is 
#- look at the average ratio of first 6 games average to season and see if there is any consistency there if so we can use the first 6
#- as a proxy for the season that we are going to be predicting
#-so for this bit of code we set up an empty data table and then set it to the filtered PreppedData for each team season combo
#- Then we add another column which will count a 1 or 0 if the team has not lost or has lost
#- subsequently we create a second column which adds up these 1's and 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]	
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win.ID <- ifelse(WinStrC$Team.Goal.Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, 1,0)
			}else{
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, WinStrC$Win.Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)	
			}
		
}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
#-now figure out what the average streak is for each team across the model time period
AveStreak <- PreppedData[,.(Ave.Streak = ave(Win.Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak.Probability[[1]] <- dpois(PreppedData$Win.Count[[1]],PreppedData$Ave.Streak[[1]])
	}else{
	PreppedData$Streak.Probability[[u]] <- dpois(PreppedData$Win.Count[[u-1]]+1,PreppedData$Ave.Streak[[u]])
	}
}

#- save it as a csv so we can have a quick look in excel
write.csv(PreppedData, "France Data For Modelling 2015 2016.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")

 #-load in the data
train <- read.csv ("France Data For Modelling 2015 2016.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38


Teams <- Teams[Teams[,1] != "Angers",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 9:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2015 2016",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Initial.Strength[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1] + ModTrain$Opp.Initial.Strength))
			}
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 1, ifelse(ModTrain$Team.Goal.Diff <0, -1, 0))
			
			
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod <- lm(Team.Goal.Diff ~
										Season +
										Team.Favourite*Month + 
										Expected.Goal.Difference +
										(Team.Form - Opposition.Form) +
										((Team.Shots.on.Target.Form + Opposition.Shots.Conceded.Form) - (Opposition.Shots.on.Target.Form + Team.Shots.Conceded.Form))*Home.Away +
										Relative.Goals.Form*Home.Away +
										Opposition +
										Team.Odds*Home.Away*Month +
										Draw.Odds +
										Streak.Probability +
										Team.Handicap*Home.Away +
										Opposition.Odds*Home.Away +
										(Team.Odds - Opposition.Odds)*Home.Away*Month
										, data=ModTrain)
			
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2015 2016",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1] - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1])
			}
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 1, ifelse(PredData$Team.Goal.Diff <0, -1, 0))
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2015 2016"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference				
Fit <- fitted(Pmod)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff
#- Resers siameses them together
Resers <- cbind(Act,Fit)
Resers <- as.data.frame(Resers)

#-now create the cartesian list of c-value possibilities
x <- c(0.1,0.25,0.5,0.75,1)
CVala <- rep(x,each=length(x))
CVala <- as.data.frame(CVala)
CValb <- rep(x,times=length(x))
CValb <- as.data.frame(CValb)
CValues <- cbind(CVala,CValb)
CValues <- as.data.frame(CValues)

#- now we run a for loop which goes through the C-value combinations and checks what the story
#- is with the old accuracy levels, for each combination we want to figure out what the maximum accuracy achieved is
#- and then we want to see where the elbow point is for the pc's
#- we'll get that by looking at where there's the greatest increase in predictions and take the first one

a <- nrow(Resers)
b <- ncol(Resers)

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers)) #- just create this as a temporary measure so that things are the right size
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0)) #- classify the actual goal difference into the results
ResP1 <- ResP1[,-1] #- get rid of the temp column cause things are cool now size wise
head(ResP1)
ResP1 <- as.data.frame(ResP1)


#-ok shtuff gets a bit mad here so pay attention:
#- so for each of the possible C-value combinations we want to check what the accuracy is
#- ToSt is to house the 
ToSt <- as.data.frame(1)
for (d in 1:nrow(CValues)){
		
		
		#- for each principle component we classify the predicted values in to what the result would be
		ResP1[,2] <- ifelse(Resers[,2] > CValues[d,1],1,ifelse(Resers[,2] < -1*CValues[d,2], -1, 0))
		
			#- Sust houses the count of correct predictions
			#- first we set it to be a series of 1's so it's the right size, as above
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			#- but now what we want to do is create a "truth" matrix where 1 indicates a correct prediction
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				#-TempSt houses the accuracy of each principle component
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l]) #- aggregate each of the columns of SuSt
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt) #- express it as a percentage of accuracy
				}
					M1 <- max(TempSt[2,]) #- this is the max accuracy level
					M3 <- match(M1, TempSt[2,]) #- what's the highest accuracy achieved by the principle components
					#- set up TempStp2 as a house for the summary information
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1] #- the Team we're modelling
					TempStp2[,2] <- CValues[d,1] #- the positive C-Value
					TempStp2[,3] <- CValues[d,2] #- the negative C-Value
					TempStp2[,4] <- M1 #- the accuracy

					TempStp2[,5] <- M3 #- the maximum accuracy
					
					#-then if this is the first set of C-Values we're running through save ToSt as TempStp2 else tack it on at the end
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}


}
#-rename the matric that has the different results based on all the c-values
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Max Acc P.C")

				PC1 <- match(max(ToSt[,4]),ToSt[,4]) #-this is the row where the maximum accuracy is across the c-values
ToStp <- ToSt[PC1,] #-ToStp is just going to be that row
#-this below bit is to make sure that if a team appears more than once in a gameweek we pick up all matches
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				#-now we are back to stitching our prediction table together
				p5a <- predict(Pmod, PredData) #- this gives a prediction for each principle component
				p5a <- as.data.frame(p5a)
				
				p5 <- as.data.frame(p5a)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4]) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "Reg Extended 2015 2016.csv")

}


#-- ordered probit

Team.Favourite + 
										Home.Away +
										Team.Favourite*Home.Away +
										Expected.Goal.Difference*Home.Away +
										(Team.Form - Opposition.Form) +
										Opposition +
										Draw.Odds +
										Opposition.Odds +
										High.Team.Form +
										Low.Team.Form +
										Team.Odds*Month +
										Opposition.Odds*Month +
										Relative.Goals.Conceded.Form +
										Relative.Goals.Form +
										(Team.Odds - Opposition.Odds)*Home.Away*Month
										
										
										
										
										
										
										
										
										
#- pc stuff with different data prep
{

require(pls)
require(data.table)

setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2014 2015",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
HData$Team.Favourite = HData$Home.Favourite
HData$Opposition = HData$AwayTeam
HData$Team.Form = HData$Home.Form
HData$Opposition.Form = HData$Away.form
HData$Team.Shots.on.Target.Form = HData$Home.Shots.on.Target.Form
HData$Opposition.Shots.on.Target.Form = HData$Away.Shots.on.Target.Form
HData$Team.Shots.Conceded.Form = HData$Home.Shots.Conceded.Form
HData$Opposition.Shots.Conceded.Form = HData$Away.Shots.Conceded.Form
HData$Team.Goals.Scored.Form = HData$Home.Goals.Scored.Form
HData$Opposition.Goals.Scored.Form = HData$Away.Goals.Scored.Form
HData$Team.Goals.Conceded.Form = HData$Home.Goals.Conceded.Form
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Team.Corners.Form = HData$Home.Corners.Form
HData$Team.Fouls.Form = HData$Home.Team.Fouls.Form
HData$Team.Yellow.Cards = HData$Home.Yellow.Cards
HData$Team.Red.Cards = HData$Home.Red.Cards
HData$Opposition.Corners.Form = HData$Away.Corners.Form
HData$Opposition.Fouls.Form = HData$Away.Team.Fouls.Form
HData$Opposition.Yellow.Cards = HData$Away.Yellow.Cards
HData$Opposition.Red.Cards = HData$Away.Red.Cards
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Relative.Goals.Form = ((HData$Team.Goals.Scored.Form - HData$Team.Goals.Conceded.Form) - (HData$Opposition.Goals.Scored.Form - HData$Opposition.Goals.Conceded.Form))
HData$Team.Odds = HData$B365H
HData$Draw.Odds = HData$B365D
HData$Opposition.Odds = HData$B365A
HData$Team.Goal.Diff = HData$Full.Time.Home.Goals - HData$Full.Time.Away.Goals
HData$Home.Away = rep("Home",nrow(HData))

#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
#-Create the variables that we need - first for all the Away matches
AData$Team.Favourite = AData$Away.Favourite
AData$Opposition = AData$HomeTeam
AData$Team.Form = AData$Away.form
AData$Opposition.Form = AData$Home.Form
AData$Team.Shots.on.Target.Form = AData$Away.Shots.on.Target.Form
AData$Opposition.Shots.on.Target.Form = AData$Home.Shots.on.Target.Form
AData$Team.Shots.Conceded.Form = AData$Away.Shots.Conceded.Form
AData$Opposition.Shots.Conceded.Form = AData$Home.Shots.Conceded.Form
AData$Team.Goals.Scored.Form = AData$Away.Goals.Scored.Form
AData$Opposition.Goals.Scored.Form = AData$Home.Goals.Scored.Form
AData$Team.Goals.Conceded.Form = AData$Away.Goals.Conceded.Form
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Team.Corners.Form = AData$Away.Corners.Form
AData$Team.Fouls.Form = AData$Away.Team.Fouls.Form
AData$Team.Yellow.Cards = AData$Away.Yellow.Cards
AData$Team.Red.Cards = AData$Away.Red.Cards
AData$Opposition.Corners.Form = AData$Home.Corners.Form
AData$Opposition.Fouls.Form = AData$Home.Team.Fouls.Form
AData$Opposition.Yellow.Cards = AData$Home.Yellow.Cards
AData$Opposition.Red.Cards = AData$Home.Red.Cards
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Relative.Goals.Form = ((AData$Team.Goals.Scored.Form - AData$Team.Goals.Conceded.Form) - (AData$Opposition.Goals.Scored.Form - AData$Opposition.Goals.Conceded.Form))
AData$Team.Odds = AData$B365A
AData$Draw.Odds = AData$B365D
AData$Opposition.Odds = AData$B365H
AData$Team.Goal.Diff = AData$Full.Time.Away.Goals - AData$Full.Time.Home.Goals
AData$Home.Away = rep("Away",nrow(AData))

#-brill now we're cooking
#-so now that we have those two datasets let's append them and add them to our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))

ComboData <- cbind(ComboData,Team)

if(i == 1){
TeamData <- ComboData
}else{
TeamData <- rbind(TeamData,ComboData)
}

TeamData <- as.data.frame(TeamData)
}

#- ok so we have a tasty little treat here where we have to order the teamdata dataframe by teams seasons and gameweeks and then
#- we have to create a lookup table which calculates our basic strength, join that to our original
#- then create a win streak count and subsequent probability variable
#- also for shits and giggles as this is just the data manipulation stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game.Week.Index)]
#-create the points the team scored
PreppedData$Team.Points <- ifelse(PreppedData$Team.Goal.Diff > 0, 3, ifelse(PreppedData$Team.Goal.Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game.Week.Index <= 6,.(Initial.Strength = ave(Team.Points)), by =.(Season, Team)]
OppStr <- PreppedData[Game.Week.Index <= 6,.(Opp.Initial.Strength = ave(Team.Points)), by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)
require(plyr)
PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
#-kl
#- nope it'll never catch on I mean "cool"
#-ok now I want to add a winning streak variable
#-for this one I'm going to have to cheat just a little bit as we need the average streak per season what we can then do is 
#- look at the average ratio of first 6 games average to season and see if there is any consistency there if so we can use the first 6
#- as a proxy for the season that we are going to be predicting
#-so for this bit of code we set up an empty data table and then set it to the filtered PreppedData for each team season combo
#- Then we add another column which will count a 1 or 0 if the team has not lost or has lost
#- subsequently we create a second column which adds up these 1's and 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]	
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win.ID <- ifelse(WinStrC$Team.Goal.Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, 1,0)
			}else{
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, WinStrC$Win.Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)	
			}
		
}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
#-now figure out what the average streak is for each team across the model time period
AveStreak <- PreppedData[,.(Ave.Streak = ave(Win.Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak.Probability[[1]] <- dpois(PreppedData$Win.Count[[1]],PreppedData$Ave.Streak[[1]])
	}else{
	PreppedData$Streak.Probability[[u]] <- dpois(PreppedData$Win.Count[[u-1]]+1,PreppedData$Ave.Streak[[u]])
	}
}

#- save it as a csv so we can have a quick look in excel
write.csv(PreppedData, "France Data For Modelling 2014 2015.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")

 #-load in the data
train <- read.csv ("France Data For Modelling 2014 2015.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38


#Teams <- Teams[Teams[,1] != "Angers",]
#Teams <- as.data.frame(Teams)
#Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
#Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Eibar",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Cordoba",]
Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 8:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Season != "2014 2015" & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2014 2015",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Initial.Strength[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1] + ModTrain$Opp.Initial.Strength))
			}
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 1, ifelse(ModTrain$Team.Goal.Diff <0, -1, 0))
			
			
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod <- mvr(Team.Goal.Diff ~
										Season +
										Team.Favourite + 
										(Team.Form - Opposition.Form) +
										(Team.Shots.on.Target.Form + Opposition.Shots.Conceded.Form)*Home.Away +
										(Team.Shots.Conceded.Form + Opposition.Shots.on.Target.Form)*Home.Away +
										Relative.Goals.Form +
										Opposition +
										(Opposition.Odds - Team.Odds) +
										Team.Odds*Month +
										Draw.Odds +
										Opposition.Odds, data=ModTrain)
			
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2014 2015",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1] - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1])
			}
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 1, ifelse(PredData$Team.Goal.Diff <0, -1, 0))
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2014 2015"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference				
Fit <- predict(Pmod, ModTrain)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff
#- Resers siameses them together
Resers <- cbind(Act,Fit)
Resers <- as.data.frame(Resers)

#-now create the cartesian list of c-value possibilities
x <- c(0.1,0.25,0.5,0.75,1)
CVala <- rep(x,each=length(x))
CVala <- as.data.frame(CVala)
CValb <- rep(x,times=length(x))
CValb <- as.data.frame(CValb)
CValues <- cbind(CVala,CValb)
CValues <- as.data.frame(CValues)

#- now we run a for loop which goes through the C-value combinations and checks what the story
#- is with the old accuracy levels, for each combination we want to figure out what the maximum accuracy achieved is
#- and then we want to see where the elbow point is for the pc's
#- we'll get that by looking at where there's the greatest increase in predictions and take the first one

a <- nrow(Resers)
b <- ncol(Resers)

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers)) #- just create this as a temporary measure so that things are the right size
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0)) #- classify the actual goal difference into the results
ResP1 <- ResP1[,-1] #- get rid of the temp column cause things are cool now size wise
head(ResP1)
ResP1 <- as.data.frame(ResP1)


#-ok shtuff gets a bit mad here so pay attention:
#- so for each of the possible C-value combinations we want to check what the accuracy is
#- ToSt is to house the 
ToSt <- as.data.frame(1)
for (d in 1:nrow(CValues)){
		
		for(f in 2:ncol(Resers)){
		#- for each principle component we classify the predicted values in to what the result would be
		ResP1[,f] <- ifelse(Resers[,f] > CValues[d,1],1,ifelse(Resers[,f] < -1*CValues[d,2], -1, 0))
		}
			#- Sust houses the count of correct predictions
			#- first we set it to be a series of 1's so it's the right size, as above
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			#- but now what we want to do is create a "truth" matrix where 1 indicates a correct prediction
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				#-TempSt houses the accuracy of each principle component
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l]) #- aggregate each of the columns of SuSt
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt) #- express it as a percentage of accuracy
				}
					M1 <- max(TempSt[2,]) #- this is the max accuracy level
					TempStp1 <- as.data.frame(1)
					#-TempSp1 tells us the where the elbow points are for the principle components	
					for (m in 2:ncol(TempSt)){
					TempStp1[,m-1] <- TempSt[1,m] - TempSt[1,m-1]
					}
					
					M2 <- match(max(TempStp1),TempStp1)+1 #- it's plus one because the first column is ignored because it can't be the elbow
					M3 <- match(M1, TempSt[2,]) #- what's the highest accuracy achieved by the principle components
					#- set up TempStp2 as a house for the summary information
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1] #- the Team we're modelling
					TempStp2[,2] <- CValues[d,1] #- the positive C-Value
					TempStp2[,3] <- CValues[d,2] #- the negative C-Value
					TempStp2[,4] <- M1 #- the accuracy
					TempStp2[,5] <- M2 #- the Elbow P.C
					TempStp2[,6] <- M3 #- the maximum accuracy
					
					#-then if this is the first set of C-Values we're running through save ToSt as TempStp2 else tack it on at the end
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}


}
#-rename the matric that has the different results based on all the c-values
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Elbow P.C", "Max Acc P.C")

				PC1 <- match(max(ToSt[,4]),ToSt[,4]) #-this is the row where the maximum accuracy is across the c-values
ToStp <- ToSt[PC1,] #-ToStp is just going to be that row
#-this below bit is to make sure that if a team appears more than once in a gameweek we pick up all matches
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				#-now we are back to stitching our prediction table together
				p5a <- predict(Pmod, PredData) #- this gives a prediction for each principle component
				p5a <- as.data.frame(p5a)
				p5b <- ncol(p5a)
				p5 <- p5a[,ToStp[1,5]] #- this select the elbow pc from the prediction matrix
				p5 <- as.data.frame(p5)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4]) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "PC Reg Extended 2014 2015.csv")

}
						


#- Bear with me here for a second
#- what about using all european data to feed the model and then predicting off of that, let's give it a go

{

require(plyr)
require(pls)
require(data.table)
#- read in the european shtuff
setwd ("C:/Users/ciana/Documents/Football Predictions/Europe/")
DATA <- read.csv("Europe Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2014 2015",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
HData$Team.Favourite = HData$Home.Favourite
HData$Opposition = HData$AwayTeam
HData$Team.Form = HData$Home.Form
HData$Opposition.Form = HData$Away.form
HData$Team.Shots.on.Target.Form = HData$Home.Shots.on.Target.Form
HData$Opposition.Shots.on.Target.Form = HData$Away.Shots.on.Target.Form
HData$Team.Shots.Conceded.Form = HData$Home.Shots.Conceded.Form
HData$Opposition.Shots.Conceded.Form = HData$Away.Shots.Conceded.Form
HData$Team.Goals.Scored.Form = HData$Home.Goals.Scored.Form
HData$Opposition.Goals.Scored.Form = HData$Away.Goals.Scored.Form
HData$Team.Goals.Conceded.Form = HData$Home.Goals.Conceded.Form
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Team.Corners.Form = HData$Home.Corners.Form
HData$Team.Fouls.Form = HData$Home.Team.Fouls.Form
HData$Team.Yellow.Cards = HData$Home.Yellow.Cards
HData$Team.Red.Cards = HData$Home.Red.Cards
HData$Opposition.Corners.Form = HData$Away.Corners.Form
HData$Opposition.Fouls.Form = HData$Away.Team.Fouls.Form
HData$Opposition.Yellow.Cards = HData$Away.Yellow.Cards
HData$Opposition.Red.Cards = HData$Away.Red.Cards
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Relative.Goals.Form = ((HData$Team.Goals.Scored.Form - HData$Team.Goals.Conceded.Form) - (HData$Opposition.Goals.Scored.Form - HData$Opposition.Goals.Conceded.Form))
HData$Team.Odds = HData$B365H
HData$Draw.Odds = HData$B365D
HData$Opposition.Odds = HData$B365A
HData$Team.AH.Odds = HData$Ave.AH.Home.Odds
HData$Team.Handicap = HData$Asian.Handicap
HData$Opposition.AH.Odds = HData$Ave.AH.Away.Odds
HData$Team.Goal.Diff = HData$Full.Time.Home.Goals - HData$Full.Time.Away.Goals
HData$Home.Away = rep("Home",nrow(HData))

#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
#-Create the variables that we need - first for all the Away matches
AData$Team.Favourite = AData$Away.Favourite
AData$Opposition = AData$HomeTeam
AData$Team.Form = AData$Away.form
AData$Opposition.Form = AData$Home.Form
AData$Team.Shots.on.Target.Form = AData$Away.Shots.on.Target.Form
AData$Opposition.Shots.on.Target.Form = AData$Home.Shots.on.Target.Form
AData$Team.Shots.Conceded.Form = AData$Away.Shots.Conceded.Form
AData$Opposition.Shots.Conceded.Form = AData$Home.Shots.Conceded.Form
AData$Team.Goals.Scored.Form = AData$Away.Goals.Scored.Form
AData$Opposition.Goals.Scored.Form = AData$Home.Goals.Scored.Form
AData$Team.Goals.Conceded.Form = AData$Away.Goals.Conceded.Form
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Team.Corners.Form = AData$Away.Corners.Form
AData$Team.Fouls.Form = AData$Away.Team.Fouls.Form
AData$Team.Yellow.Cards = AData$Away.Yellow.Cards
AData$Team.Red.Cards = AData$Away.Red.Cards
AData$Opposition.Corners.Form = AData$Home.Corners.Form
AData$Opposition.Fouls.Form = AData$Home.Team.Fouls.Form
AData$Opposition.Yellow.Cards = AData$Home.Yellow.Cards
AData$Opposition.Red.Cards = AData$Home.Red.Cards
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Relative.Goals.Form = ((AData$Team.Goals.Scored.Form - AData$Team.Goals.Conceded.Form) - (AData$Opposition.Goals.Scored.Form - AData$Opposition.Goals.Conceded.Form))
AData$Team.Odds = AData$B365A
AData$Draw.Odds = AData$B365D
AData$Opposition.Odds = AData$B365H
AData$Team.AH.Odds = HData$Ave.AH.Away.Odds
AData$Team.Handicap = HData$Asian.Handicap
AData$Opposition.AH.Odds = HData$Ave.AH.Home.Odds
AData$Team.Goal.Diff = AData$Full.Time.Away.Goals - AData$Full.Time.Home.Goals
AData$Home.Away = rep("Away",nrow(AData))

#-brill now we're cooking
#-so now that we have those two datasets let's append them and add them to our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))

ComboData <- cbind(ComboData,Team)

if(i == 1){
TeamData <- ComboData
}else{
TeamData <- rbind(TeamData,ComboData)
}

TeamData <- as.data.table(TeamData)
}


#- ok so we have a tasty little treat here where we have to order the teamdata dataframe by teams seasons and gameweeks and then
#- we have to create a lookup table which calculates our basic strength, join that to our original
#- then create a win streak count and subsequent probability variable
#- also for shits and giggles as this is just the data manipulation stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game.Week.Index)]
#-create the points the team scored
PreppedData$Team.Points <- ifelse(PreppedData$Team.Goal.Diff > 0, 3, ifelse(PreppedData$Team.Goal.Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game.Week.Index <= 6,.(Initial.Strength = ave(Team.Points)), by =.(Season, Team)]
OppStr <- PreppedData[Game.Week.Index <= 6,.(Opp.Initial.Strength = ave(Team.Points)), by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)
require(plyr)
PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
#-kl
#- nope it'll never catch on I mean "cool"
#-ok now I want to add a winning streak variable
#-for this one I'm going to have to cheat just a little bit as we need the average streak per season what we can then do is 
#- look at the average ratio of first 6 games average to season and see if there is any consistency there if so we can use the first 6
#- as a proxy for the season that we are going to be predicting
#-so for this bit of code we set up an empty data table and then set it to the filtered PreppedData for each team season combo
#- Then we add another column which will count a 1 or 0 if the team has not lost or has lost
#- subsequently we create a second column which adds up these 1's and 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]	
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win.ID <- ifelse(WinStrC$Team.Goal.Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, 1,0)
			}else{
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, WinStrC$Win.Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)	
			}
		
}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
#-now figure out what the average streak is for each team across the model time period
AveStreak <- PreppedData[,.(Ave.Streak = ave(Win.Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak.Probability[[1]] <- dpois(PreppedData$Win.Count[[1]],PreppedData$Ave.Streak[[1]])
	}else{
	PreppedData$Streak.Probability[[u]] <- dpois(PreppedData$Win.Count[[u-1]]+1,PreppedData$Ave.Streak[[u]])
	}
}

#- save it as a csv so we can have a quick look in excel
write.csv(PreppedData, "Europe Data For Modelling 2014 2015.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/Europe")

 #-load in the data
train <- read.csv ("Europe Data For Modelling 2014 2015.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38

#- we only want the teams from France for this one to actually predict so we define them here 

setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2014 2015",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)

setwd ("C:/Users/ciana/Documents/Football Predictions/Europe")
Teams <- Teams[Teams[,1] != "Angers",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 9:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Game.Week.Index > 6 & train$Season != "2014 2015" & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2014 2015",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Initial.Strength[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1] + ModTrain$Opp.Initial.Strength))
			}
			#ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 1, ifelse(ModTrain$Team.Goal.Diff <0, -1, 0))
			
			
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod <- lm(Team.Goal.Diff ~
										Season +
										Team +
										Month +
										Team.Favourite + 
										Home.Away +
										Team.Favourite*Home.Away +
										Expected.Goal.Difference +
										(Team.Form - Opposition.Form) +
										((Team.Shots.on.Target.Form + Opposition.Shots.Conceded.Form) - 
										(Opposition.Shots.on.Target.Form + Team.Shots.Conceded.Form))*Home.Away +
										Opposition +
										(Team.Odds - Opposition.Odds)
										, data=ModTrain)
			
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2014 2015",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1] - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1])
			}
			#PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 1, ifelse(PredData$Team.Goal.Diff <0, -1, 0))
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2014 2015"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
			
				
#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference				
Fit <- fitted(Pmod)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff
#- Resers siameses them together
Resers <- cbind(Act,Fit)
Resers <- as.data.frame(Resers)

#-now create the cartesian list of c-value possibilities
x <- c(0.1,0.25,0.5,0.75,1)
CVala <- rep(x,each=length(x))
CVala <- as.data.frame(CVala)
CValb <- rep(x,times=length(x))
CValb <- as.data.frame(CValb)
CValues <- cbind(CVala,CValb)
CValues <- as.data.frame(CValues)

#- now we run a for loop which goes through the C-value combinations and checks what the story
#- is with the old accuracy levels, for each combination we want to figure out what the maximum accuracy achieved is
#- and then we want to see where the elbow point is for the pc's
#- we'll get that by looking at where there's the greatest increase in predictions and take the first one

a <- nrow(Resers)
b <- ncol(Resers)

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers)) #- just create this as a temporary measure so that things are the right size
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0)) #- classify the actual goal difference into the results
ResP1 <- ResP1[,-1] #- get rid of the temp column cause things are cool now size wise
head(ResP1)
ResP1 <- as.data.frame(ResP1)


#-ok shtuff gets a bit mad here so pay attention:
#- so for each of the possible C-value combinations we want to check what the accuracy is
#- ToSt is to house the 
ToSt <- as.data.frame(1)
for (d in 1:nrow(CValues)){
		
		
		#- for each principle component we classify the predicted values in to what the result would be
		ResP1[,2] <- ifelse(Resers[,2] > CValues[d,1],1,ifelse(Resers[,2] < -1*CValues[d,2], -1, 0))
		
			#- Sust houses the count of correct predictions
			#- first we set it to be a series of 1's so it's the right size, as above
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			#- but now what we want to do is create a "truth" matrix where 1 indicates a correct prediction
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				#-TempSt houses the accuracy of each principle component
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l]) #- aggregate each of the columns of SuSt
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt) #- express it as a percentage of accuracy
				}
					M1 <- max(TempSt[2,]) #- this is the max accuracy level
					M3 <- match(M1, TempSt[2,]) #- what's the highest accuracy achieved by the principle components
					#- set up TempStp2 as a house for the summary information
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1] #- the Team we're modelling
					TempStp2[,2] <- CValues[d,1] #- the positive C-Value
					TempStp2[,3] <- CValues[d,2] #- the negative C-Value
					TempStp2[,4] <- M1 #- the accuracy

					TempStp2[,5] <- M3 #- the maximum accuracy
					
					#-then if this is the first set of C-Values we're running through save ToSt as TempStp2 else tack it on at the end
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}


}
#-rename the matric that has the different results based on all the c-values
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Max Acc P.C")

				PC1 <- match(max(ToSt[,4]),ToSt[,4]) #-this is the row where the maximum accuracy is across the c-values
ToStp <- ToSt[PC1,] #-ToStp is just going to be that row
#-this below bit is to make sure that if a team appears more than once in a gameweek we pick up all matches
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				#-now we are back to stitching our prediction table together
				p5a <- predict(Pmod, PredData) #- this gives a prediction for each principle component
				p5a <- as.data.frame(p5a)
				
				p5 <- as.data.frame(p5a)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4]) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "Reg Extended 2014 2015.csv")
}


#--- Naieve Bayes Classifier


{

require(e1071)
require(plyr)
require(pls)
require(data.table)

setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2015 2016",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)


TeamData <- data.frame


for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
HData$Team.Favourite = HData$Home.Favourite
HData$Opposition = HData$AwayTeam
HData$Team.Form = HData$Home.Form
HData$Opposition.Form = HData$Away.form
HData$Team.Shots.on.Target.Form = HData$Home.Shots.on.Target.Form
HData$Opposition.Shots.on.Target.Form = HData$Away.Shots.on.Target.Form
HData$Team.Shots.Conceded.Form = HData$Home.Shots.Conceded.Form
HData$Opposition.Shots.Conceded.Form = HData$Away.Shots.Conceded.Form
HData$Team.Goals.Scored.Form = HData$Home.Goals.Scored.Form
HData$Opposition.Goals.Scored.Form = HData$Away.Goals.Scored.Form
HData$Team.Goals.Conceded.Form = HData$Home.Goals.Conceded.Form
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Team.Corners.Form = HData$Home.Corners.Form
HData$Team.Fouls.Form = HData$Home.Team.Fouls.Form
HData$Team.Yellow.Cards = HData$Home.Yellow.Cards
HData$Team.Red.Cards = HData$Home.Red.Cards
HData$Opposition.Corners.Form = HData$Away.Corners.Form
HData$Opposition.Fouls.Form = HData$Away.Team.Fouls.Form
HData$Opposition.Yellow.Cards = HData$Away.Yellow.Cards
HData$Opposition.Red.Cards = HData$Away.Red.Cards
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Relative.Goals.Form = ((HData$Team.Goals.Scored.Form - HData$Team.Goals.Conceded.Form) - (HData$Opposition.Goals.Scored.Form - HData$Opposition.Goals.Conceded.Form))
HData$Team.Odds = HData$B365H
HData$Draw.Odds = HData$B365D
HData$Opposition.Odds = HData$B365A
HData$Team.AH.Odds = HData$Ave.AH.Home.Odds
HData$Team.Handicap = HData$Asian.Handicap
HData$Opposition.AH.Odds = HData$Ave.AH.Away.Odds
HData$Team.Goal.Diff = HData$Full.Time.Home.Goals - HData$Full.Time.Away.Goals
HData$Home.Away = rep("Home",nrow(HData))

#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
#-Create the variables that we need - first for all the Away matches
AData$Team.Favourite = AData$Away.Favourite
AData$Opposition = AData$HomeTeam
AData$Team.Form = AData$Away.form
AData$Opposition.Form = AData$Home.Form
AData$Team.Shots.on.Target.Form = AData$Away.Shots.on.Target.Form
AData$Opposition.Shots.on.Target.Form = AData$Home.Shots.on.Target.Form
AData$Team.Shots.Conceded.Form = AData$Away.Shots.Conceded.Form
AData$Opposition.Shots.Conceded.Form = AData$Home.Shots.Conceded.Form
AData$Team.Goals.Scored.Form = AData$Away.Goals.Scored.Form
AData$Opposition.Goals.Scored.Form = AData$Home.Goals.Scored.Form
AData$Team.Goals.Conceded.Form = AData$Away.Goals.Conceded.Form
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Team.Corners.Form = AData$Away.Corners.Form
AData$Team.Fouls.Form = AData$Away.Team.Fouls.Form
AData$Team.Yellow.Cards = AData$Away.Yellow.Cards
AData$Team.Red.Cards = AData$Away.Red.Cards
AData$Opposition.Corners.Form = AData$Home.Corners.Form
AData$Opposition.Fouls.Form = AData$Home.Team.Fouls.Form
AData$Opposition.Yellow.Cards = AData$Home.Yellow.Cards
AData$Opposition.Red.Cards = AData$Home.Red.Cards
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Relative.Goals.Form = ((AData$Team.Goals.Scored.Form - AData$Team.Goals.Conceded.Form) - (AData$Opposition.Goals.Scored.Form - AData$Opposition.Goals.Conceded.Form))
AData$Team.Odds = AData$B365A
AData$Draw.Odds = AData$B365D
AData$Opposition.Odds = AData$B365H
AData$Team.AH.Odds = HData$Ave.AH.Away.Odds
AData$Team.Handicap = HData$Asian.Handicap
AData$Opposition.AH.Odds = HData$Ave.AH.Home.Odds
AData$Team.Goal.Diff = AData$Full.Time.Away.Goals - AData$Full.Time.Home.Goals
AData$Home.Away = rep("Away",nrow(AData))

#-brill now we're cooking
#-so now that we have those two datasets let's append them and add them to our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))

ComboData <- cbind(ComboData,Team)

if(i == 1){
TeamData <- ComboData
}else{
TeamData <- rbind(TeamData,ComboData)
}

TeamData <- as.data.table(TeamData)
}


#- ok so we have a tasty little treat here where we have to order the teamdata dataframe by teams seasons and gameweeks and then
#- we have to create a lookup table which calculates our basic strength, join that to our original
#- then create a win streak count and subsequent probability variable
#- also for shits and giggles as this is just the data manipulation stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game.Week.Index)]
#-create the points the team scored
PreppedData$Team.Points <- ifelse(PreppedData$Team.Goal.Diff > 0, 3, ifelse(PreppedData$Team.Goal.Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game.Week.Index <= 6,.(Initial.Strength = ave(Team.Points)), by =.(Season, Team)]
OppStr <- PreppedData[Game.Week.Index <= 6,.(Opp.Initial.Strength = ave(Team.Points)), by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)
require(plyr)
PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
#-kl
#- nope it'll never catch on I mean "cool"
#-ok now I want to add a winning streak variable
#-for this one I'm going to have to cheat just a little bit as we need the average streak per season what we can then do is 
#- look at the average ratio of first 6 games average to season and see if there is any consistency there if so we can use the first 6
#- as a proxy for the season that we are going to be predicting
#-so for this bit of code we set up an empty data table and then set it to the filtered PreppedData for each team season combo
#- Then we add another column which will count a 1 or 0 if the team has not lost or has lost
#- subsequently we create a second column which adds up these 1's and 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]	
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win.ID <- ifelse(WinStrC$Team.Goal.Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, 1,0)
			}else{
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, WinStrC$Win.Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)	
			}
		
}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
#-now figure out what the average streak is for each team across the model time period
AveStreak <- PreppedData[,.(Ave.Streak = ave(Win.Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak.Probability[[1]] <- dpois(PreppedData$Win.Count[[1]],PreppedData$Ave.Streak[[1]])
	}else{
	PreppedData$Streak.Probability[[u]] <- dpois(PreppedData$Win.Count[[u-1]]+1,PreppedData$Ave.Streak[[u]])
	}
}

#- save it as a csv so we can have a quick look in excel
write.csv(PreppedData, "France Data For Modelling 2015 2016.csv")

#- in excel add the month column now
#-load the data
   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")

 #-load in the data
train <- read.csv ("France Data For Modelling 2015 2016.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)


#-ok so when it comes to the main show we'll do a recursive loop or a split apply divide thing but for now let's just do a for loop


PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38


Teams <- Teams[Teams[,1] != "Angers",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
Teams <- as.data.frame(Teams)

for (i in 1:nrow(Teams))
{
		for (j in 9:GWRange){
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[6,1] & train$Game.Week.Index > 6 & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[6,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < 18 & train$Season == "2015 2016",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Initial.Strength[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1] + ModTrain$Opp.Initial.Strength))
			}
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 1, ifelse(ModTrain$Team.Goal.Diff <0, -1, 0))
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff == 1, "Team", ifelse(ModTrain$Team.Goal.Diff == 0, "Draw", "Opposition"))
			ModTrain$Team.Goal.Diff <- as.factor(ModTrain$Team.Goal.Diff)
			
			#-then and no bells or whistles here, we run the linear regression
			Pmod2 <- naiveBayes(Team.Goal.Diff ~
										Season +
										Month +
										Team.Favourite + 
										Expected.Goal.Difference +
										(Team.Form - Opposition.Form) +
										((Team.Shots.on.Target.Form + Opposition.Shots.Conceded.Form) - (Opposition.Shots.on.Target.Form + Team.Shots.Conceded.Form)) +
										Relative.Goals.Form +
										Home.Away +
										Opposition +
										Team.Odds +
										Draw.Odds +
										Streak.Probability +
										Asian.Handicap +
										Opposition.Odds +
										(Team.Odds - Opposition.Odds), data=ModTrain)
			
			PredData <- train[train$Team == Teams[6,1] & train$Game.Week.Index == 18 & train$Season == "2015 2016",]
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			
			
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1] - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1])
			}
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 1, ifelse(PredData$Team.Goal.Diff <0, -1, 0))
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff == 1, "Team", ifelse(PredData$Team.Goal.Diff == 0, "Draw", "Opposition"))
			PredData$Team.Goal.Diff <- as.factor(PredData$Team.Goal.Diff)
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2015 2016"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference				
Fit <- fitted(Pmod)
Fit <- as.data.frame(Fit)
Act <- ModTrain$Team.Goal.Diff
#- Resers siameses them together
Resers <- cbind(Act,Fit)
Resers <- as.data.frame(Resers)

#-now create the cartesian list of c-value possibilities
x <- c(0.1,0.25,0.5,0.75,1)
CVala <- rep(x,each=length(x))
CVala <- as.data.frame(CVala)
CValb <- rep(x,times=length(x))
CValb <- as.data.frame(CValb)
CValues <- cbind(CVala,CValb)
CValues <- as.data.frame(CValues)

#- now we run a for loop which goes through the C-value combinations and checks what the story
#- is with the old accuracy levels, for each combination we want to figure out what the maximum accuracy achieved is
#- and then we want to see where the elbow point is for the pc's
#- we'll get that by looking at where there's the greatest increase in predictions and take the first one

a <- nrow(Resers)
b <- ncol(Resers)

#-first column of our new dataframe will be the actual result
ResP1 <- data.frame()
ResP1 <- rep(1,nrow(Resers)) #- just create this as a temporary measure so that things are the right size
ResP1 <- as.data.frame(ResP1)
ResP1$Act <- ifelse(Resers$Act >0,1,ifelse(Resers$Act < 0, -1, 0)) #- classify the actual goal difference into the results
ResP1 <- ResP1[,-1] #- get rid of the temp column cause things are cool now size wise
head(ResP1)
ResP1 <- as.data.frame(ResP1)


#-ok shtuff gets a bit mad here so pay attention:
#- so for each of the possible C-value combinations we want to check what the accuracy is
#- ToSt is to house the 
ToSt <- as.data.frame(1)
for (d in 1:nrow(CValues)){
		
		
		#- for each principle component we classify the predicted values in to what the result would be
		ResP1[,2] <- ifelse(Resers[,2] > CValues[d,1],1,ifelse(Resers[,2] < -1*CValues[d,2], -1, 0))
		
			#- Sust houses the count of correct predictions
			#- first we set it to be a series of 1's so it's the right size, as above
			SuSt <- rep(1,a)
			SuSt <- as.data.frame(SuSt)
			for (k in 2:ncol(ResP1)){
			#- but now what we want to do is create a "truth" matrix where 1 indicates a correct prediction
			SuSt[,k-1] <- ifelse(ResP1[,k] == ResP1[,1],1,0)
			}
			TempSt <- as.data.frame(1)
				#-TempSt houses the accuracy of each principle component
				for (l in 1:ncol(SuSt)){
				TempSt[,l] <- sum(SuSt[,l]) #- aggregate each of the columns of SuSt
				TempSt[2,l] <- TempSt[1,l] / nrow(SuSt) #- express it as a percentage of accuracy
				}
					M1 <- max(TempSt[2,]) #- this is the max accuracy level
					M3 <- match(M1, TempSt[2,]) #- what's the highest accuracy achieved by the principle components
					#- set up TempStp2 as a house for the summary information
					TempStp2 <- as.data.frame(1)
					TempStp2[,1] <- ModTrain$Team[1] #- the Team we're modelling
					TempStp2[,2] <- CValues[d,1] #- the positive C-Value
					TempStp2[,3] <- CValues[d,2] #- the negative C-Value
					TempStp2[,4] <- M1 #- the accuracy

					TempStp2[,5] <- M3 #- the maximum accuracy
					
					#-then if this is the first set of C-Values we're running through save ToSt as TempStp2 else tack it on at the end
					if(d == 1){
					ToSt<- TempStp2
					}else{
					ToSt <- rbind(ToSt,TempStp2)
					}


}
#-rename the matric that has the different results based on all the c-values
colnames(ToSt) <- c("Team", "Pos C-Value", "Neg C-Value", "Max Accuracy", "Max Acc P.C")

				PC1 <- match(max(ToSt[,4]),ToSt[,4]) #-this is the row where the maximum accuracy is across the c-values
ToStp <- ToSt[PC1,] #-ToStp is just going to be that row
#-this below bit is to make sure that if a team appears more than once in a gameweek we pick up all matches
ap <- nrow(PredData)
if(d > 1){
		for (g in 2:ap){
		ToStp[g,] <- ToStp[1,]
		}
}
				
				
				#-now we are back to stitching our prediction table together
				p5a <- predict(Pmod, PredData) #- this gives a prediction for each principle component
				p5a <- as.data.frame(p5a)
				
				p5 <- as.data.frame(p5a)
				AggP <- cbind(p1,p2,p3,p4,p5,ToStp[,2],ToStp[,3],ToStp[,4]) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "Prediction", "Pos C-Value", "Neg C-Value", "Max Accuracy")
				
				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}



write.csv(PredResults, "Reg Extended 2015 2016.csv")

}


#-- Let's give the people what they want and do some neural networks --#

{


#-- You wouldn't go shopping without your wallet and bags, don't forget your packages
require(pls)
require(data.table)
require(RSNNS)
require(plyr)
require(caret)

#-whicj project folder we want to work in 
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data/")
DATA <- read.csv("France Agg Data Input.csv", header = TRUE)

#- so what we need to do is create a unique list of the teams involved in the latest season
Teams <- DATA[DATA$Season == "2013 2014",5]
Teams <- unique(Teams)
Teams <- as.data.frame(Teams)
TeamData <- data.frame


#- Then what we do is create the dataset so that instead of having the raw data and home and away teams we create a data set where
#- we have each team and their associated opposition for the game week, easier to see correlations that make sense that way
#- below we split it by home and away and then match up by team
for(i in 1:nrow(Teams)){
HData <- DATA[DATA$HomeTeam == Teams[i,1],]
#-Create the variables that we need - first for all the home matches
HData$Team.Favourite = HData$Home.Favourite
HData$Opposition = HData$AwayTeam
HData$Team.Form = HData$Home.Form
HData$Opposition.Form = HData$Away.form
HData$Team.Shots.on.Target.Form = HData$Home.Shots.on.Target.Form
HData$Opposition.Shots.on.Target.Form = HData$Away.Shots.on.Target.Form
HData$Team.Shots.Conceded.Form = HData$Home.Shots.Conceded.Form
HData$Opposition.Shots.Conceded.Form = HData$Away.Shots.Conceded.Form
HData$Team.Goals.Scored.Form = HData$Home.Goals.Scored.Form
HData$Opposition.Goals.Scored.Form = HData$Away.Goals.Scored.Form
HData$Team.Goals.Conceded.Form = HData$Home.Goals.Conceded.Form
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Team.Corners.Form = HData$Home.Corners.Form
HData$Team.Fouls.Form = HData$Home.Team.Fouls.Form
HData$Team.Yellow.Cards = HData$Home.Yellow.Cards
HData$Team.Red.Cards = HData$Home.Red.Cards
HData$Opposition.Corners.Form = HData$Away.Corners.Form
HData$Opposition.Fouls.Form = HData$Away.Team.Fouls.Form
HData$Opposition.Yellow.Cards = HData$Away.Yellow.Cards
HData$Opposition.Red.Cards = HData$Away.Red.Cards
HData$Opposition.Goals.Conceded.Form = HData$Away.Goals.Conceded.Form
HData$Relative.Goals.Form = ((HData$Team.Goals.Scored.Form - HData$Team.Goals.Conceded.Form) - (HData$Opposition.Goals.Scored.Form - HData$Opposition.Goals.Conceded.Form))
HData$Team.Odds = HData$B365H
HData$Draw.Odds = HData$B365D
HData$Opposition.Odds = HData$B365A
HData$Team.AH.Odds = HData$Ave.AH.Home.Odds
HData$Team.Handicap = HData$Asian.Handicap
HData$Opposition.AH.Odds = HData$Ave.AH.Away.Odds
HData$Team.Goal.Diff = HData$Full.Time.Home.Goals - HData$Full.Time.Away.Goals
HData$Home.Away = rep("Home",nrow(HData))

#-now create the data for the away matches
AData <- DATA[DATA$AwayTeam == Teams[i,1],]
#-Create the variables that we need - first for all the Away matches
AData$Team.Favourite = AData$Away.Favourite
AData$Opposition = AData$HomeTeam
AData$Team.Form = AData$Away.form
AData$Opposition.Form = AData$Home.Form
AData$Team.Shots.on.Target.Form = AData$Away.Shots.on.Target.Form
AData$Opposition.Shots.on.Target.Form = AData$Home.Shots.on.Target.Form
AData$Team.Shots.Conceded.Form = AData$Away.Shots.Conceded.Form
AData$Opposition.Shots.Conceded.Form = AData$Home.Shots.Conceded.Form
AData$Team.Goals.Scored.Form = AData$Away.Goals.Scored.Form
AData$Opposition.Goals.Scored.Form = AData$Home.Goals.Scored.Form
AData$Team.Goals.Conceded.Form = AData$Away.Goals.Conceded.Form
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Team.Corners.Form = AData$Away.Corners.Form
AData$Team.Fouls.Form = AData$Away.Team.Fouls.Form
AData$Team.Yellow.Cards = AData$Away.Yellow.Cards
AData$Team.Red.Cards = AData$Away.Red.Cards
AData$Opposition.Corners.Form = AData$Home.Corners.Form
AData$Opposition.Fouls.Form = AData$Home.Team.Fouls.Form
AData$Opposition.Yellow.Cards = AData$Home.Yellow.Cards
AData$Opposition.Red.Cards = AData$Home.Red.Cards
AData$Opposition.Goals.Conceded.Form = AData$Home.Goals.Conceded.Form
AData$Relative.Goals.Form = ((AData$Team.Goals.Scored.Form - AData$Team.Goals.Conceded.Form) - (AData$Opposition.Goals.Scored.Form - AData$Opposition.Goals.Conceded.Form))
AData$Team.Odds = AData$B365A
AData$Draw.Odds = AData$B365D
AData$Opposition.Odds = AData$B365H
AData$Team.AH.Odds = AData$Ave.AH.Away.Odds
AData$Team.Handicap = AData$Asian.Handicap*-1
AData$Opposition.AH.Odds = AData$Ave.AH.Home.Odds
AData$Team.Goal.Diff = AData$Full.Time.Away.Goals - AData$Full.Time.Home.Goals
AData$Home.Away = rep("Away",nrow(AData))

#-brill now we're cooking
#-so now that we have those two datasets let's append them and add them to our major dataframe
ComboData <- HData
ComboData <- rbind(ComboData,AData)
Team <- rep(Teams[i,1],nrow(ComboData))
ComboData <- cbind(ComboData,Team)

if(i == 1){
TeamData <- ComboData
}else{
TeamData <- rbind(TeamData,ComboData)
}

TeamData <- as.data.frame(TeamData)
}

#- ok so we have a tasty little treat here where we have to order the teamdata dataframe by teams seasons and gameweeks and then
#- we have to create a lookup table which calculates our basic strength, join that to our original
#- then create a win streak count and subsequent probability variable
#- also for shits and giggles as this is just the data manipulation stage I'm going to play with data.table
PreppedData <- data.table(TeamData)
PreppedData <- PreppedData[order(Team,Season,Game.Week.Index)]
#-create the points the team scored
PreppedData$Team.Points <- ifelse(PreppedData$Team.Goal.Diff > 0, 3, ifelse(PreppedData$Team.Goal.Diff < 0, 0, 1))
#-creating the lookup table from the average over the first 6 games
AveStr <- PreppedData[Game.Week.Index <= 6,.(Initial.Strength = ave(Team.Points)), by =.(Season, Team)]
OppStr <- PreppedData[Game.Week.Index <= 6,.(Opp.Initial.Strength = ave(Team.Points)), by =.(Season, Opposition)]
#- thar be duplicates ahead, remove 'em
#-remove the key
setkey(AveStr,NULL)
AveStr <- unique(AveStr)
setkey(OppStr,NULL)
OppStr <- unique(OppStr)

PreppedData <- join(PreppedData, AveStr, by = c("Season", "Team"))
PreppedData <- join(PreppedData, OppStr, by = c("Season", "Opposition"))
#-kl
#- nope it'll never catch on I mean "cool"
#-ok now I want to add a winning streak variable
#-for this one I'm going to have to cheat just a little bit as we need the average streak per season what we can then do is 
#- look at the average ratio of first 6 games average to season and see if there is any consistency there if so we can use the first 6
#- as a proxy for the season that we are going to be predicting
#-so for this bit of code we set up an empty data table and then set it to the filtered PreppedData for each team season combo
#- Then we add another column which will count a 1 or 0 if the team has not lost or has lost
#- subsequently we create a second column which adds up these 1's and 0's into a streak

WinStrC <- data.table()
WinStreakComplete <- data.table()
for (e in 1:nrow(AveStr)){
WinStrC <- PreppedData[Team == AveStr[[e,2]] & Season == AveStr[[e,1]],]	
		for (r in 1:nrow(WinStrC)){
		WinStrC$Win.ID <- ifelse(WinStrC$Team.Goal.Diff >= 0, 1, 0)
		if(r == 1){
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, 1,0)
			}else{
			WinStrC$Win.Count[r] <- ifelse(WinStrC$Win.ID[r] >0, WinStrC$Win.Count[[r-1]]+1,0)
			}
		}
		if(r == 1){
		WinStreakComplete <- WinStrC
		}else{
			WinStreakComplete <- rbind(WinStreakComplete,WinStrC)	
			}
		
}
ncol(WinStreakComplete)
PreppedData <- WinStreakComplete
rm(WinStreakComplete)
#-now figure out what the average streak is for each team across the model time period
AveStreak <- PreppedData[,.(Ave.Streak = ave(Win.Count)), by =.(Team)]
AveStreak <- unique(AveStreak)
PreppedData <- join(PreppedData, AveStreak, by = c("Team"))

#-finally ready to add on what the chances are of increasing the non lose streak
for(u in 1:nrow(PreppedData)){
	if(u == 1){
	PreppedData$Streak.Probability[[1]] <- dpois(PreppedData$Win.Count[[1]],PreppedData$Ave.Streak[[1]])
	}else{
	PreppedData$Streak.Probability[[u]] <- dpois(PreppedData$Win.Count[[u-1]]+1,PreppedData$Ave.Streak[[u]])
	}
}

#- save it as a csv so we can have a quick look in excel if we need to
#- we shouldn't need to
write.csv(PreppedData, "France Data For Modelling 2013 2014.csv")

#----------------------- Data Prep Over let's play with Models ---------------------------#

#- where we at   
setwd ("C:/Users/ciana/Documents/Football Predictions/France/Model Data")
 #-load in the data
train <- read.csv ("France Data For Modelling 2013 2014.csv", header=TRUE)
#-you have to change the storage of tain$team to character so that the different levels plays nicely with the factors in Teams
train$Team <- as.character(train$Team)

#- Define two data frames (maybe they should be tables) which will store our results within loops and at the end of our loops
PredResults <- data.frame()
StatResults <- data.frame()
GWRange <- 38 #- 38 games in a season son

#- some of the teams in the season that we are modelling won't have played in the division previously and as such will confuse our model, when 
#- they are called, noting for it but to call on the spirit of Stalin and have them scrubbed from history
Teams <- Teams[Teams[,1] != "Angers",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Ajaccio GFCO",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Cardiff",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Crystal Palace",]
Teams <- as.data.frame(Teams)
Teams <- Teams[Teams[,1] != "Guingamp",]
Teams <- as.data.frame(Teams)

#- ok so this is the meat of the action where for every team we...
for (i in 1:nrow(Teams))
{
		for (j in 8:GWRange){ #- and for every gameweek
			#-ModTrain is a subset of our working dataset that we split by each team and for weeks past 6

			ModTrain1 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Season != "2013 2014" & train$Season != "2014 2015" & train$Season != "2015 2016",]
			ModTrain2 <- train[train$Team == Teams[i,1] & train$Game.Week.Index > 6 & train$Game.Week.Index < j & train$Season == "2013 2014",]
			ModTrain <- rbind(ModTrain1,ModTrain2)
			
			#-we'll create a couple of extra variables to make things a little easier for the model here each time
			ModTrain$High.Team.Form <- ifelse(ModTrain$Team.Form > quantile(ModTrain$Team.Form)[4], ModTrain$Team.Form, 0)
			ModTrain$Low.Team.Form <- ifelse(ModTrain$Team.Form < quantile(ModTrain$Team.Form)[2], ModTrain$Team.Form, 0)
			ModTrain$High.Opposition.Form <- ifelse(ModTrain$Opposition.Form > quantile(ModTrain$Opposition.Form)[4], ModTrain$Opposition.Form, 0)
			ModTrain$Low.Opposition.Form <- ifelse(ModTrain$Opposition.Form < quantile(ModTrain$Opposition.Form)[2], ModTrain$Opposition.Form, 0)
			ModTrain$Expected.Team.Goals <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form)
			ModTrain$Expected.Opposition.Goals <- (ModTrain$Opposition.Shots.on.Target.Form + ModTrain$Team.Shots.Conceded.Form)
			ModTrain$Relative.Form <- ModTrain$Team.Form - ModTrain$Opposition.Form
			ModTrain$Expected.Shots <- (ModTrain$Team.Shots.on.Target.Form + ModTrain$Opposition.Shots.Conceded.Form) - (ModTrain$Team.Shots.Conceded.Form + ModTrain$Opposition.Shots.on.Target.Form)
			ModTrain$Relative.Odds <- ModTrain$Opposition.Odds - ModTrain$Team.Odds
			
			for (q in 1 : nrow(ModTrain)){
			ModTrain$Expected.Goal.Difference[q] <- ((ave(rpois(50,ModTrain$Expected.Team.Goals[[q]]))[1] + ModTrain$Team.Handicap[[q]]) - (ave(rpois(50,ModTrain$Expected.Opposition.Goals[[q]]))[1]))
			}
			#- here we play a little with the dependent, kind of depends on the model we're running,in this case we want a trinomial classification
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff > 0, 1, ifelse(ModTrain$Team.Goal.Diff <0, -1, 0))
			ModTrain$Team.Goal.Diff <- ifelse(ModTrain$Team.Goal.Diff == 1, "Team", ifelse(ModTrain$Team.Goal.Diff == 0, "Draw", "Opposition"))
			ModTrain$Team.Goal.Diff <- as.factor(ModTrain$Team.Goal.Diff)
			
			
			#- for RSNNS we have to feed it the input dataset of all the variables used, so define the formula below
			variables <- c("Season","Month","Team.Favourite","Expected.Goal.Difference","Team.Form","Opposition.Form","Team.Shots.on.Target.Form",
			"Opposition.Shots.Conceded.Form","Opposition.Shots.on.Target.Form","Team.Shots.Conceded.Form","Relative.Goals.Form","Home.Away","Opposition",
			"Team.Odds","Draw.Odds","Streak.Probability","Asian.Handicap","Opposition.Odds","Relative.Odds","Expected.Shots","Relative.Form") 
			TDat1 <- ModTrain[,variables]
			#- but on top of that you can't have categorical variables within the X matrix so to speak
			#- so we use a function to turn our categorical stuff into numeric data
			TDat2 <- dummyVars("~.",data=TDat1)
			TrainDat <- data.frame(predict(TDat2, newdata = TDat1))
			#- now we need to normalize the training data
			TrainDatnames <- colnames(TrainDat)
			TrainDat <- normalizeData(TrainDat, type="0_1")
			colnames(TrainDat) <- TrainDatnames
			
			#- same for Test data
			Tester <- data.frame(ModTrain$Team.Goal.Diff)
			TDat2 <- dummyVars("~.",data=Tester)
			TestDat <- data.frame(predict(TDat2, newdata = Tester))
			#- now we need to normalize the training data
			TestDatnames <- colnames(TestDat)
			TestDat <- normalizeData(TestDat, type="0_1")
			colnames(TestDat) <- TestDatnames
			
			
			
			Pmod <- mlp(TrainDat,TestDat,size=c(5,5),maxit=100,learnFuncParams=0.2, softmax=TRUE)
			
			#- create PredData which will be what we want to predict over 
			PredData <- train[train$Team == Teams[i,1] & train$Game.Week.Index == j & train$Season == "2013 2014",]
			
			#- If any of the extra variables we created in Modtrain are used in the model we need to create them in PredData as well,so that happens here
			PredData$High.Team.Form <- ifelse(PredData$Team.Form > quantile(PredData$Team.Form)[4], PredData$Team.Form, 0)
			PredData$Low.Team.Form <- ifelse(PredData$Team.Form < quantile(PredData$Team.Form)[2], PredData$Team.Form, 0)
			PredData$High.Opposition.Form <- ifelse(PredData$Opposition.Form > quantile(PredData$Opposition.Form)[4], PredData$Opposition.Form, 0)
			PredData$Low.Opposition.Form <- ifelse(PredData$Opposition.Form < quantile(PredData$Opposition.Form)[2], PredData$Opposition.Form, 0)
			PredData$Relative.Form <- PredData$Team.Form - PredData$Opposition.Form
			PredData$Expected.Shots <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form) - (PredData$Team.Shots.Conceded.Form + PredData$Opposition.Shots.on.Target.Form)
			PredData$Relative.Odds <- PredData$Opposition.Odds - PredData$Team.Odds
			
			#-sometimes a team won't be playing in the subsequent gameweek so this database will be empty,so move on folks nothing to see here
			if(nrow(PredData) > 0){	
			PredData$Expected.Team.Goals <- (PredData$Team.Shots.on.Target.Form + PredData$Opposition.Shots.Conceded.Form)
			PredData$Expected.Opposition.Goals <- (PredData$Opposition.Shots.on.Target.Form + PredData$Team.Shots.Conceded.Form)
			for (q in 1 : nrow(PredData)){
			PredData$Expected.Goal.Difference[q] <- (ave(rpois(50,PredData$Expected.Team.Goals[[q]]))[1]  + PredData$Team.Handicap[[q]]) - ave(rpois(50,PredData$Expected.Opposition.Goals[[q]]))[1]
			}
			
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff > 0, 1, ifelse(PredData$Team.Goal.Diff <0, -1, 0))
			PredData$Team.Goal.Diff <- ifelse(PredData$Team.Goal.Diff == 1, "Team", ifelse(PredData$Team.Goal.Diff == 0, "Draw", "Opposition"))
			PredData$Team.Goal.Diff <- as.factor(PredData$Team.Goal.Diff)
			
			
			if( PredData$Opposition %in% ModTrain$Opposition){	
			#- we need to set up PredData to be in the same format as the Training and Test data
			PDat1 <- PredData[,variables]
			#- but on top of that you can't have categorical variables within the X matrix so to speak
			#- so we use a function to turn our categorical stuff into numeric data
			PDat2 <- dummyVars("~.",data=PDat1)
			PredDat <- data.frame(predict(PDat2, newdata = PDat1))
			#- now we need to normalize the training data
			PredDatnames <- colnames(PredDat)
			PredDat <- normalizeData(PredDat, type="0_1")
			colnames(PredDat) <- PredDatnames
			
				p1 <- Teams[i,1]
				p1 <- as.data.frame(p1)
				p2 <- "2013 2014"
				p3 <- PredData$Opposition
				p3 <- as.data.frame(p3)
				p4 <- PredData$Game.Week.Index
				p4 <- as.data.frame(p4)
				
				
								
				
#- Fit is a matrix of predicted values for each principle component
#-Act is the actual result in terms of goal difference				
Fit <- predict(Pmod, PredDat)
Fit <- as.data.table(Fit)

				#-now we are back to stitching our prediction table together
				AggP <- cbind(p1,p2,p3,p4,Fit) #- stitched together
				colnames(AggP) <- c("Team", "Season", "Opposition", "Game.Week.Index", "P-Draw","P-Opposition","P-Team")
				
				#- save the results
				if(i == 1 & j== 8){
				PredResults <- AggP
				}else{
				PredResults <- rbind(PredResults,AggP)
				}
				
				}
				}
		}
}


write.csv(PredResults, "Nadia's Prediction 2013 2014.csv")




}
