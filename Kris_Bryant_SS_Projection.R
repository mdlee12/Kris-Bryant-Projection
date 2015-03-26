similar <- function(p, number=10){
	P <- subset(age_totals, bref_age == p)

	age_totals$SS <- with(age_totals,
			1000-
			floor(abs(Age-P$Age)*1000)-
			floor(abs(G-P$G)/20)-
			floor(abs(AB-P$AB)/75)-
			floor(abs(R-P$R)/10)-
			floor(abs(H-P$H)/15)-
			floor(abs(X2B-P$X2B)/5)-
			floor(abs(X3B-P$X3B)/4)-
			floor(abs(HR-P$HR)/2)-
			floor(abs(RBI-P$RBI)/10)-
			floor(abs(BB-P$BB)/25)-
			floor(abs(SO-P$SO)/150)-
			floor(abs(SB-P$SB)/20)-
			floor(abs(BA-P$BA)/0.001)-
			floor(abs(SLG-P$SLG)/0.002)-
			floor(abs(ValueD-P$ValueD)))
	age_totals <- age_totals[order(age_totals$SS, decreasing=TRUE), ]
	age_totals=age_totals[ !duplicated(age_totals[, c("min_playerid")], ),]
	age_totals[1:number, ]
}

player_comps <- season_totals %>%
    filter(is.element(Age,c(23:32))) %>%
    filter(is.element(min_playerid,c("gordon001ale","dicker001mck","akins-001nic","mench-001kev","braun-001rya","teixei001mar","lamb--002jac","darnel001jam","gyorko001jed","pence-001hun"))) %>%
   select(Age,LevEq,G,PA,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS)

age_comps <- season_totals %>%
    filter(is.element(Age,c(23:32))) %>%
    filter(is.element(min_playerid,c("gordon001ale","dicker001mck","akins-001nic","mench-001kev","braun-001rya","teixei001mar","lamb--002jac","darnel001jam","gyorko001jed","pence-001hun"))) %>%
    group_by(Age) %>%
    summarise(LevEq = ceiling(mean(LevEq)),G = ceiling(mean(G)),PA = ceiling(mean(PA)),AB = ceiling(mean(AB)),R = ceiling(mean(R)),H = ceiling(mean(H)),X2B = ceiling(mean(X2B)),X3B = ceiling(mean(X3B)),HR = ceiling(mean(HR)),RBI = ceiling(mean(RBI)),SB = ceiling(mean(SB)),CS = ceiling(mean(CS)),BB = ceiling(mean(BB)),SO = ceiling(mean(SO)),TB = ceiling(mean(TB)),GDP = ceiling(mean(GDP)),SH = ceiling(mean(SH)),SF = ceiling(mean(SF)),IBB = ceiling(mean(IBB)),HBP = ceiling(mean(HBP)),Count=n()) %>%
    mutate(BA = round(((H)/(AB)),digits=3), OBP = round(((H+BB+HBP)/(AB+BB+HBP+SF)),digits=3), SLG = round(((H-X2B-X3B-HR)+(2*X2B)+(3*X3B)+(4*HR))/(AB),digits=3), OPS = round(((H+BB+HBP)/(AB+BB+HBP+SF))+((H-X2B-X3B-HR)+(2*X2B)+(3*X3B)+(4*HR))/(AB),digits=3)) %>%
   select(Age,LevEq,G,PA,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS,Count)



