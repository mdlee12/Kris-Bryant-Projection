c('rvest','dplyr','pipeR','tidyr') -> packages #installs packages
lapply(packages, library, character.only = T) #installs packages

battingid=read.csv("playerid_match.csv")
battingid[] <- lapply(battingid, as.character)
majors_batting=left_join(majors_batting,battingid, by=c("maj_playerid"))
minors_batting=left_join(minors_batting,battingid, by=c("min_playerid"))
minors_batting=select(minors_batting,X,Rk,Name,Age,Tm,Lev,G,PA,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS,TB,GDP,HBP,SH,SF,IBB,Pos,season,min_playerid,maj_playerid)
majors_batting$Pos.Summary = majors_batting$Pos
minors_batting$PosSummary=minors_batting$Pos.Summary
majors_batting$Pos = paste(majors_batting$Pos.Summary, majors_batting$G, sep="-")
majors_batting=select(majors_batting,X,Rk,Name,Age,Tm,Lev,G,PA,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS,TB,GDP,HBP,SH,SF,IBB,Pos,season,min_playerid,maj_playerid)
names(majors_batting)[names(majors_batting) == 'min_playerid.x'] <- 'min_playerid'
batting=rbind(majors_batting,minors_batting)

batting$PosSummary=batting$Pos.Summary
batting=separate(batting,Pos.Summary,c("Pos1","Pos2","Pos3","Pos4","Pos5","Pos6","Pos7","Pos8","Pos9","Pos10"),sep="\\,", extra="merge")

batting=separate(batting,Pos1,c("Pos1","G1"),sep="\\-", extra="merge")
batting=separate(batting,Pos2,c("Pos2","G2"),sep="\\-", extra="merge")
batting=separate(batting,Pos3,c("Pos3","G3"),sep="\\-", extra="merge")
batting=separate(batting,Pos4,c("Pos4","G4"),sep="\\-", extra="merge")
batting=separate(batting,Pos5,c("Pos5","G5"),sep="\\-", extra="merge")
batting=separate(batting,Pos6,c("Pos6","G6"),sep="\\-", extra="merge")
batting=separate(batting,Pos7,c("Pos7","G7"),sep="\\-", extra="merge")
batting=separate(batting,Pos8,c("Pos8","G8"),sep="\\-", extra="merge")
batting=separate(batting,Pos9,c("Pos9","G9"),sep="\\-", extra="merge")
batting=separate(batting,Pos10,c("Pos10","G10"),sep="\\-", extra="merge")
batting$G1=as.numeric(batting$G1)
batting$G2=as.numeric(batting$G2)
batting$G3=as.numeric(batting$G3)
batting$G4=as.numeric(batting$G4)
batting$G5=as.numeric(batting$G5)
batting$G6=as.numeric(batting$G6)
batting$G7=as.numeric(batting$G7)
batting$G8=as.numeric(batting$G8)
batting$G9=as.numeric(batting$G9)
batting$G10=as.numeric(batting$G10)
attach(batting)
batting$DefG=colSums(rbind(G1,G2,G3,G4,G5,G6,G7,G8,G9,G10), na.rm=TRUE)

Value.Pos1 <- with(batting,
ifelse(Pos1 == "C", (G1/DefG)*240,
ifelse(Pos1 == "SS", (G1/DefG)*168,
ifelse(Pos1 == "2B", (G1/DefG)*132,
ifelse(Pos1 == "3B", (G1/DefG)*84,
ifelse(Pos1 == "MI", (G1/DefG)*66,
ifelse(Pos1 == "IF", (G1/DefG)*66,
ifelse(Pos1 == "UT", (G1/DefG)*66,
ifelse(Pos1 == "CI", (G1/DefG)*66,
ifelse(Pos1 == "OF", (G1/DefG)*48,
ifelse(Pos1 == "LF", (G1/DefG)*48,
ifelse(Pos1 == "RF", (G1/DefG)*48,
ifelse(Pos1 == "CF", (G1/DefG)*48,
ifelse(Pos1 == "1B", (G1/DefG)*12,
ifelse(Pos1 == "DH", (G1/DefG)*0,
ifelse(Pos1 == "P", (G1/DefG)*0, 0))))))))))))))))
Value.Pos2 <- with(batting,
ifelse(Pos2 == "C", (G2/DefG)*240,
ifelse(Pos2 == "SS", (G2/DefG)*168,
ifelse(Pos2 == "2B", (G2/DefG)*132,
ifelse(Pos2 == "3B", (G2/DefG)*84,
ifelse(Pos2 == "OF", (G2/DefG)*48,
ifelse(Pos2 == "LF", (G2/DefG)*48,
ifelse(Pos2 == "RF", (G2/DefG)*48,
ifelse(Pos2 == "CF", (G2/DefG)*48,
ifelse(Pos2 == "1B", (G2/DefG)*12,
ifelse(Pos2 == "DH", (G2/DefG)*0,
ifelse(Pos2 == "P", (G2/DefG)*0, 0))))))))))))
Value.Pos3 <- with(batting,
ifelse(Pos3 == "C", (G3/DefG)*240,
ifelse(Pos3 == "SS", (G3/DefG)*168,
ifelse(Pos3 == "2B", (G3/DefG)*132,
ifelse(Pos3 == "3B", (G3/DefG)*84,
ifelse(Pos3 == "OF", (G3/DefG)*48,
ifelse(Pos3 == "LF", (G3/DefG)*48,
ifelse(Pos3 == "RF", (G3/DefG)*48,
ifelse(Pos3 == "CF", (G3/DefG)*48,
ifelse(Pos3 == "1B", (G3/DefG)*12,
ifelse(Pos3 == "DH", (G3/DefG)*0,
ifelse(Pos3 == "P", (G3/DefG)*0, 0))))))))))))
Value.Pos4 <- with(batting,
ifelse(Pos4 == "C", (G4/DefG)*240,
ifelse(Pos4 == "SS", (G4/DefG)*168,
ifelse(Pos4 == "2B", (G4/DefG)*132,
ifelse(Pos4 == "3B", (G4/DefG)*84,
ifelse(Pos4 == "OF", (G4/DefG)*48,
ifelse(Pos4 == "LF", (G4/DefG)*48,
ifelse(Pos4 == "RF", (G4/DefG)*48,
ifelse(Pos4 == "CF", (G4/DefG)*48,
ifelse(Pos4 == "1B", (G4/DefG)*12,
ifelse(Pos4 == "DH", (G4/DefG)*0,
ifelse(Pos4 == "P", (G4/DefG)*0, 0))))))))))))
Value.Pos5 <- with(batting,
ifelse(Pos5 == "C", (G5/DefG)*240,
ifelse(Pos5 == "SS", (G5/DefG)*168,
ifelse(Pos5 == "2B", (G5/DefG)*132,
ifelse(Pos5 == "3B", (G5/DefG)*84,
ifelse(Pos5 == "OF", (G5/DefG)*48,
ifelse(Pos5 == "LF", (G5/DefG)*48,
ifelse(Pos5 == "RF", (G5/DefG)*48,
ifelse(Pos5 == "CF", (G5/DefG)*48,
ifelse(Pos5 == "1B", (G5/DefG)*12,
ifelse(Pos5 == "DH", (G5/DefG)*0,
ifelse(Pos5 == "P", (G5/DefG)*0, 0))))))))))))
Value.Pos6 <- with(batting,
ifelse(Pos6 == "C", (G6/DefG)*240,
ifelse(Pos6 == "SS", (G6/DefG)*168,
ifelse(Pos6 == "2B", (G6/DefG)*132,
ifelse(Pos6 == "3B", (G6/DefG)*84,
ifelse(Pos6 == "OF", (G6/DefG)*48,
ifelse(Pos6 == "LF", (G6/DefG)*48,
ifelse(Pos6 == "RF", (G6/DefG)*48,
ifelse(Pos6 == "CF", (G6/DefG)*48,
ifelse(Pos6 == "1B", (G6/DefG)*12,
ifelse(Pos6 == "DH", (G6/DefG)*0,
ifelse(Pos6 == "P", (G6/DefG)*0, 0))))))))))))
Value.Pos7 <- with(batting,
ifelse(Pos7 == "C", (G7/DefG)*240,
ifelse(Pos7 == "SS", (G7/DefG)*168,
ifelse(Pos7 == "2B", (G7/DefG)*132,
ifelse(Pos7 == "3B", (G7/DefG)*84,
ifelse(Pos7 == "OF", (G7/DefG)*48,
ifelse(Pos7 == "LF", (G7/DefG)*48,
ifelse(Pos7 == "RF", (G7/DefG)*48,
ifelse(Pos7 == "CF", (G7/DefG)*48,
ifelse(Pos7 == "1B", (G7/DefG)*12,
ifelse(Pos7 == "DH", (G7/DefG)*0,
ifelse(Pos7 == "P", (G7/DefG)*0, 0))))))))))))
Value.Pos8 <- with(batting,
ifelse(Pos8 == "C", (G8/DefG)*240,
ifelse(Pos8 == "SS", (G8/DefG)*168,
ifelse(Pos8 == "2B", (G8/DefG)*132,
ifelse(Pos8 == "3B", (G8/DefG)*84,
ifelse(Pos8 == "OF", (G8/DefG)*48,
ifelse(Pos8 == "LF", (G8/DefG)*48,
ifelse(Pos8 == "RF", (G8/DefG)*48,
ifelse(Pos8 == "CF", (G8/DefG)*48,
ifelse(Pos8 == "1B", (G8/DefG)*12,
ifelse(Pos8 == "DH", (G8/DefG)*0,
ifelse(Pos8 == "P", (G8/DefG)*0, 0))))))))))))
Value.Pos9 <- with(batting,
ifelse(Pos9 == "C", (G9/DefG)*240,
ifelse(Pos9 == "SS", (G9/DefG)*168,
ifelse(Pos9 == "2B", (G9/DefG)*132,
ifelse(Pos9 == "3B", (G9/DefG)*84,
ifelse(Pos9 == "OF", (G9/DefG)*48,
ifelse(Pos9 == "LF", (G9/DefG)*48,
ifelse(Pos9 == "RF", (G9/DefG)*48,
ifelse(Pos9 == "CF", (G9/DefG)*48,
ifelse(Pos9 == "1B", (G9/DefG)*12,
ifelse(Pos9 == "DH", (G9/DefG)*0,
ifelse(Pos9 == "P", (G9/DefG)*0, 0))))))))))))
Value.Pos10 <- with(batting,
ifelse(Pos10 == "C", (G10/DefG)*240,
ifelse(Pos10 == "SS", (G10/DefG)*168,
ifelse(Pos10 == "2B", (G10/DefG)*132,
ifelse(Pos10 == "3B", (G10/DefG)*84,
ifelse(Pos10 == "OF", (G10/DefG)*48,
ifelse(Pos10 == "LF", (G10/DefG)*48,
ifelse(Pos10 == "RF", (G10/DefG)*48,
ifelse(Pos10 == "CF", (G10/DefG)*48,
ifelse(Pos10 == "1B", (G10/DefG)*12,
ifelse(Pos10 == "DH", (G10/DefG)*0,
ifelse(Pos10 == "P", (G10/DefG)*0, 0))))))))))))
Value.Pos1=as.numeric(Value.Pos1)
Value.Pos2=as.numeric(Value.Pos2)
Value.Pos3=as.numeric(Value.Pos3)
Value.Pos4=as.numeric(Value.Pos4)
Value.Pos5=as.numeric(Value.Pos5)
Value.Pos6=as.numeric(Value.Pos6)
Value.Pos7=as.numeric(Value.Pos7)
Value.Pos8=as.numeric(Value.Pos8)
Value.Pos9=as.numeric(Value.Pos9)
Value.Pos10=as.numeric(Value.Pos10)
attach(batting)
batting$Value.Pos.Total=colSums(rbind(Value.Pos1,Value.Pos2,Value.Pos3,Value.Pos4,Value.Pos5,Value.Pos6,Value.Pos7,Value.Pos8,Value.Pos9,Value.Pos10), na.rm=TRUE)
batting$bref_season=paste(batting$min_playerid,batting$season,sep="_")
batting$LevSummary=batting$Lev
batting=separate(batting,Lev,c("Lev1","Lev2","Lev3","Lev4","Lev5","Lev6"),sep="\\,", extra="merge")
LevN.1 <- with(batting,
ifelse(Lev1 == "FRk", 0,
ifelse(Lev1 == "Rk", 0,
ifelse(Lev1 == "A-", 1,
ifelse(Lev1 == "A", 1,
ifelse(Lev1 == "A+", 1,
ifelse(Lev1 == "AA", 2,
ifelse(Lev1 == "AAA", 3, 
ifelse(Lev1 == "MLB", 4, 0)))))))))
LevN.2 <- with(batting,
ifelse(Lev2 == "FRk", 0,
ifelse(Lev2 == "Rk", 0,
ifelse(Lev2 == "A-", 1,
ifelse(Lev2 == "A", 1,
ifelse(Lev2 == "A+", 1,
ifelse(Lev2 == "AA", 2,
ifelse(Lev2 == "AAA", 3, 
ifelse(Lev2 == "MLB", 4, 0)))))))))
LevN.3 <- with(batting,
ifelse(Lev3 == "FRk", 0,
ifelse(Lev3 == "Rk", 0,
ifelse(Lev3 == "A-", 1,
ifelse(Lev3 == "A", 1,
ifelse(Lev3 == "A+", 1,
ifelse(Lev3 == "AA", 2,
ifelse(Lev3 == "AAA", 3, 
ifelse(Lev3 == "MLB", 4, 0)))))))))
LevN.4 <- with(batting,
ifelse(Lev4 == "FRk", 0,
ifelse(Lev4 == "Rk", 0,
ifelse(Lev4 == "A-", 1,
ifelse(Lev4 == "A", 1,
ifelse(Lev4 == "A+", 1,
ifelse(Lev4 == "AA", 2,
ifelse(Lev4 == "AAA", 3, 
ifelse(Lev4 == "MLB", 4, 0)))))))))
LevN.5 <- with(batting,
ifelse(Lev5 == "FRk", 0,
ifelse(Lev5 == "Rk", 0,
ifelse(Lev5 == "A-", 1,
ifelse(Lev5 == "A", 1,
ifelse(Lev5 == "A+", 1,
ifelse(Lev5 == "AA", 2,
ifelse(Lev5 == "AAA", 3, 
ifelse(Lev5 == "MLB", 4, 0)))))))))
LevN.6 <- with(batting,
ifelse(Lev6 == "FRk", 0,
ifelse(Lev6 == "Rk", 0,
ifelse(Lev6 == "A-", 1,
ifelse(Lev6 == "A", 1,
ifelse(Lev6 == "A+", 1,
ifelse(Lev6 == "AA", 2,
ifelse(Lev6 == "AAA", 3, 
ifelse(Lev6 == "MLB", 4, 0)))))))))
batting$LevEq=colMeans(rbind(LevN.1,LevN.2,LevN.3,LevN.4,LevN.5,LevN.6),na.rm=TRUE)
batting$Age=as.numeric(as.character(batting$Age))
batting=filter(batting,min_playerid!="NA")
attach(batting)

season_totals <- batting %>%
    group_by(bref_season,min_playerid,season,Name,Age) %>%
    summarize(Lev = toString(LevSummary),Tm = toString(Tm),LevEq=mean(LevEq),G = sum(G),PA = sum(PA),AB = sum(AB),R = sum(R),H = sum(H),X2B = sum(X2B),X3B = sum(X3B),HR = sum(HR),RBI = sum(RBI),SB = sum(SB),CS = sum(CS),BB = sum(BB),SO = sum(SO),BA = (sum(H)/sum(AB)),OBP = (sum(H,BB,HBP)/sum(AB,BB,HBP,SF)),SLG = (sum((H-X2B-X3B-HR),(2*X2B),(3*X3B),(4*HR))/sum(AB)), OPS = sum(OBP+SLG),TB = sum(TB), GDP = sum(GDP),HBP = sum(HBP), SH = sum(SH),SF = sum(SF),IBB = sum(IBB), ValueD = sum((DefG/sum(DefG))*Value.Pos.Total)) %>%
    arrange(bref_season)

age_totals <- season_totals %>%
    group_by(min_playerid) %>%
    arrange(Age) %>%
    mutate(G = cumsum(G)) %>%
    mutate(PA = cumsum(PA)) %>%
    mutate(AB = cumsum(AB)) %>%
    mutate(R = cumsum(R)) %>%
    mutate(H = cumsum(H)) %>%
    mutate(X2B = cumsum(X2B)) %>%
    mutate(X3B = cumsum(X3B)) %>%
    mutate(HR = cumsum(HR)) %>%
    mutate(RBI = cumsum(RBI)) %>%
    mutate(SB = cumsum(SB)) %>%
    mutate(CS = cumsum(CS)) %>%
    mutate(BB = cumsum(BB)) %>%
    mutate(SO = cumsum(SO)) %>%
    mutate(TB = cumsum(TB)) %>%
    mutate(GDP = cumsum(GDP)) %>%
    mutate(SH = cumsum(SH)) %>%
    mutate(SF = cumsum(SF)) %>%
    mutate(IBB = cumsum(IBB)) %>%
    mutate(HBP = cumsum(HBP)) %>%
    mutate(bref_age = paste(min_playerid,Age,sep="_"))


attach(age_totals)
age_totals$BA = round(((H)/(AB)),digits=3)
age_totals$OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)),digits=3)
age_totals$SLG = round(((H-X2B-X3B-HR)+(2*X2B)+(3*X3B)+(4*HR))/(AB),digits=3)
age_totals$OPS = round(((H+BB+HBP)/(AB+BB+HBP+SF))+((H-X2B-X3B-HR)+(2*X2B)+(3*X3B)+(4*HR))/(AB),digits=3)


