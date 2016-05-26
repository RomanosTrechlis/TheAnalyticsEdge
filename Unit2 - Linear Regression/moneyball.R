# intercept -804.63
# OBP       2737.77
# SLG       1584.91
# OBP is 0.311 and SLG is 0.405
-804.63 + (2737.77 * 0.31) + (1584.91 * 0.405)

# intercept -837.38
# OOBP      2913.60
# OSLG      1514.29
# OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370
-837.38 + (2913.60 * 0.297) + (1514.29 * 0.370)




# Rank 1: the team that won the World Series
# Rank 2: the team that lost the World Series
# Rank 3: the two teams that lost to the teams in the World Series
# Rank 4: the four teams that made it past the wild card round, but lost to the above four teams
# Rank 5: the two teams that lost the wild card round
teamRank = c(1,2,3,3,4,4,4,4,5,5)
# Rank 1: San Francisco Giants (Wins = 94)
# Rank 2: Detroit Tigers (Wins = 88)  
# Rank 3: New York Yankees (Wins = 95), and 
#         St. Louis Cardinals (Wins = 88)
# Rank 4: Baltimore Orioles (Wins = 93), 
#         Oakland A's (Wins = 94), 
#         Washington Nationals (Wins = 98), 
#         Cincinnati Reds (Wins = 97)
# Rank 5: Texas Rangers (Wins = 93), and
#         Atlanta Braves (Wins = 94) 
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
# Rank 1: Boston Red Sox (Wins = 97)
# Rank 2: St. Louis Cardinals (Wins = 97)
# Rank 3: Los Angeles Dodgers (Wins = 92), and Detroit Tigers (Wins = 93)
# Rank 4: Tampa Bay Rays (Wins = 92), Oakland A's (Wins = 96), Pittsburgh Pirates (Wins = 94), and Atlanta Braves (Wins = 96)
# Rank 5: Cleveland Indians (Wins = 92), and Cincinnati Reds (Wins = 90) 
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)