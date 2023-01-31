library(baseballr)
library(ggplot2)
library(mlbplotR)
library(utils)
library(tidyverse)
library(fuzzyjoin)
###################
View(team_hitters)
View(team_pitchers)
###################
team_hitters <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2022,
                                      player_type = 'batter-team')
team_pitchers <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2022,
                                       player_type = 'pitcher-team')
#### XBA vs BA by Team Through 8/8/2022 #####
ggplot(team_hitters, aes(est_ba, ba)) + geom_point(alpha = 0) + geom_abline(slope=1, intercept=0) +
  geom_mlb_logos(aes(team_abbr = team_id), width = 0.075, alpha = 0.7) + xlim(0.205, 0.275) +
  ylim(0.205, 0.275) + xlab("xBA") + ylab("BA") + ggtitle("BA vs xBA by Team", subtitle = "Through 8/8/2022") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### xBA (offense) vs xBA (defense) by Team Through 8/8/2022 #####
xBA_hitters <- team_hitters[, c('team', 'team_id', 'est_ba')]
xBA_pitchers <- team_pitchers[, c('team', 'team_id', 'est_ba')]
colnames(xBA_pitchers) <- c('team', 'team_id', 'est_ba_against')
xBA <- inner_join(xBA_hitters, xBA_pitchers, by = c("team", "team_id"))

ggplot(xBA, aes(est_ba_against, est_ba)) + geom_point(alpha = 0) + geom_abline(slope=1, intercept=0) +
  geom_mlb_logos(aes(team_abbr = team_id), width = 0.075, alpha = 0.7) + xlim(0.21, 0.275) +
  ylim(0.21, 0.275) + xlab("xBA (Defense)") + ylab("xBA (Offense)") + ggtitle("xBA (Offense) vs xBA (Defense) by Team", subtitle = "Through 8/8/2022") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### xwOBA (offense) vs xwOBA (defense) by Team Through 8/8/2022 #####
xwOBA_hitters <- team_hitters[, c('team', 'team_id', 'est_woba')]
xwOBA_pitchers <- team_pitchers[, c('team', 'team_id', 'est_woba')]
colnames(xwOBA_pitchers) <- c('team', 'team_id', 'est_woba_against')
xwOBA <- inner_join(xwOBA_hitters, xwOBA_pitchers, by = c("team", "team_id"))

ggplot(xwOBA, aes(est_woba_against, est_woba)) + geom_point(alpha = 0) + geom_abline(slope=1, intercept=0) +
  geom_mlb_logos(aes(team_abbr = team_id), width = 0.075, alpha = 0.7) + xlim(0.275, 0.3425) +
  ylim(0.275, 0.3425) + xlab("xwOBA (Defense)") + ylab("xwOBA (Offense)") + ggtitle("xwOBA (Offense) vs xwOBA (Defense) by Team", subtitle = "Through 8/8/2022") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##### Payroll vs Win Percentage Through 8/10/2022 #####
misc_stats <- read.csv("~/Downloads/2022TeamMiscBBRef.txt")
columns <- c("team_records_wins", "team_records_losses", "team_records_team_name", "team_records_winning_percentage")
current_standings <- rbind(mlb_standings(season = 2022, standings_type = 'regularSeason', league_id = 103)[,columns],
                               mlb_standings(season = 2022, standings_type = 'regularSeason', league_id = 104)[,columns])
colnames(current_standings) <- c("Wins", "Losses", "Tm", "Win %")
payroll_graph_data <- inner_join(misc_stats, current_standings, by = c("Tm"))
payroll_graph_data$Est..Payroll.[]<-lapply(payroll_graph_data$Est..Payroll.,gsub,pattern="$",fixed=TRUE,replacement="")
payroll_graph_data$Est..Payroll. <- as.numeric(payroll_graph_data$Est..Payroll.) / 1000000
payroll_graph_data$`Win %` <- as.numeric(payroll_graph_data$`Win %`)
team_id <- read_csv("Downloads/team_id.csv")[,c(1,4)]
colnames(team_id) <- c("Tm", "team_id")
payroll_graph_data <- inner_join(payroll_graph_data, team_id, by = "Tm")

ggplot(payroll_graph_data, aes(`Win %`, Est..Payroll.)) + geom_point(alpha = 0) +
  geom_mlb_logos(aes(team_abbr = team_id), width = 0.075, alpha = 0.7) + xlim(0.3, 0.72) +
  ylim(30, 300) + xlab("Win Percentage") + ylab("Team Payroll (in millions of dollars)") + 
  ggtitle("Team Payroll vs Win Percentage for 2022 Season", subtitle = "Through 8/10/2022") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())























































##### Regression Win Total Prediction #####
regression_hitters <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2017,
                                      player_type = 'batter-team')
temp_hitters <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2018,
                                      player_type = 'batter-team')
regression_hitters <- rbind(regression_hitters, temp_hitters)
for (i in 2019:2021) {
  temp_hitters <- statcast_leaderboards(leaderboard = "expected_statistics", year = i,
                                        player_type = 'batter-team')
  regression_hitters <- rbind(regression_hitters, temp_hitters)
}
View(regression_hitters)





regression_pitchers <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2017,
                                            player_type = 'pitcher-team')
temp_pitchers <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2018,
                                      player_type = 'pitcher-team')
regression_pitchers <- rbind(regression_pitchers, temp_pitchers)
for (i in 2019:2021) {
  temp_pitchers <- statcast_leaderboards(leaderboard = "expected_statistics", year = i,
                                        player_type = 'pitcher-team')
  regression_pitchers <- rbind(regression_pitchers, temp_pitchers)
}
View(regression_pitchers)



columns <- c("team_records_season", "team_records_wins", "team_records_losses", "team_records_team_name")
regressions_standings <- rbind(mlb_standings(season = 2017, standings_type = 'regularSeason', league_id = 103)[,columns],
                               mlb_standings(season = 2017, standings_type = 'regularSeason', league_id = 104)[,columns])
for (i in 2018:2021) {
  temp_standings <- rbind(mlb_standings(season = i, standings_type = 'regularSeason', league_id = 103)[,columns],
                          mlb_standings(season = i, standings_type = 'regularSeason', league_id = 104)[,columns])
  regressions_standings <- rbind(regressions_standings, temp_standings)
}
colnames(regressions_standings) <- c("year", "wins", "losses", "team_full")
regressions_standings[regressions_standings == 'Arizona Diamondbacks'] <- 'Arizona D-backs'
regressions_standings[regressions_standings == 'Cleveland Indians'] <- 'Cleveland Guardians'
View(regressions_standings)

cols <- c("year.x", "wins", "losses", "team", "est_ba", "est_slg", "est_woba")
hitters_ready <- regex_full_join(regressions_standings, regression_hitters, by = c("year", "team_full" = "team"))
pitchers_ready <- regex_full_join(regressions_standings, regression_pitchers, by = c("year", "team_full" = "team"))
hitters_ready <- hitters_ready[,cols]
pitchers_ready <- pitchers_ready[,cols]
colnames(pitchers_ready) <- c("year", "wins", "losses", "team", "est_ba_agst", "est_slg_agst", "est_woba_agst")
colnames(hitters_ready) <- c("year", "wins", "losses", "team", "est_ba", "est_slg", "est_woba")
View(hitters_ready)
View(pitchers_ready)

regression_ready <- inner_join(hitters_ready, pitchers_ready, by = c("year", "wins", "losses", "team"))
View(regression_ready)

model <- lm(wins ~ est_ba + est_slg + est_woba + est_ba_agst + est_slg_agst + est_woba_agst,
            data = regression_ready)
summary(model)

est_wins <- function(ba, slg, woba, ba_a, slg_a, woba_a) {
  return(436.61 + ba*682.63 + slg*620.79 - woba*1557.63 + 1565.85*ba_a + 69.23*slg_a - 2218.44*woba_a)
}
current_hitters <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2022,
                                      player_type = 'batter-team')
current_pitchers <- statcast_leaderboards(leaderboard = "expected_statistics", year = 2022,
                                       player_type = 'pitcher-team')
current_hitters <- current_hitters[,c(1,2,7,10,13)]
current_pitchers <- current_pitchers[,c(1,2,7,10,13)]
colnames(current_pitchers) <- c("year", "team", "est_ba_agst", "est_slg_agst", "est_woba_agst")
current <- inner_join(current_hitters, current_pitchers, by = c("year", "team"))

current <- mutate(current, proj_wins = est_wins(est_ba, est_slg, est_woba, est_ba_agst, est_slg_agst, est_woba_agst))
View(current)
