################################################################################################
### Analysis of rushing and passing success by men in the box.
### Test year over year stability of a modified version of FANS (First and
### second down Against Neutral or Stacked boxes) by play type (run or pass)
### which filters to just plays with 7-10 yards to go for a first down.
### Display the epa per play and success rate for each team in 2018.
### For an in-depth discussion of FANS see:
### https://fivethirtyeight.com/features/for-a-passing-league-the-nfl-still-doesnt-pass-enough/
################################################################################################

library(tidyverse)
library(broom)

### load all the data
mib_6_17 <- read_csv("data/6mib-2017.csv") %>%
   mutate(mib = 6)
mib_6_18 <- read_csv("data/6mib-2018.csv") %>%
   mutate(mib = 6)
mib_7_17 <- read_csv("data/7mib-2017.csv") %>%
   mutate(mib = 7)
mib_7_18 <- read_csv("data/7mib-2018.csv") %>%
   mutate(mib = 7)
mib_8_17 <- read_csv("data/8mib-2017.csv") %>%
   mutate(mib = 8)
mib_8_18 <- read_csv("data/8mib-2018.csv") %>%
   mutate(mib = 8)

play_action_fans <- read_csv("data/pa_epa_fans_norz.csv")
pass_fans <- read_csv("data/fans-08-18-pass-plays.csv")
run_fans <- read_csv("data/fans-08-18-rush-plays.csv")
pass_fans_team <- read_csv("data/fans-08-18-pass.csv")
run_fans_team <- read_csv("data/fans-08-18-rush.csv")
all_rush_team <- read_csv("data/all_rushatt-08-18.csv")
all_pass_team <- read_csv("data/all_passatt-08-2018.csv")

mib_18 <- mib_6_18 %>%
   bind_rows(mib_7_18, mib_8_18)

mib_17 <- mib_6_17 %>%
   bind_rows(mib_7_17, mib_8_17)

### clean up the garbage
rm(mib_6_17, mib_6_18, mib_7_17, mib_7_18, mib_8_17, mib_8_18)

colnames(mib_18)
# [1] "entityName"              "playIdZ"                 "gameId"                  "gsisGameKey"             "playId"
# [6] "gameId_1"                "plainDate"               "week"                    "playoffWeek"             "teamId"
# [11] "teamName"                "token"                   "seasonType"              "ncaa"                    "nfl"
# [16] "opponentToken"           "oppAbbrevName"           "home"                    "win"                     "finalScore"
# [21] "finalScoreOpponent"      "team"                    "opponent"                "abbrevName"              "oppAbbrevName_1"
# [26] "currentScore"            "opponentCurrentScore"    "quarter"                 "gameClock"               "roadPriorScore"
# [31] "homePriorScore"          "roadAfterScore"          "homeAfterScore"          "downStart"               "distToFirstStart"
# [36] "goalDistStart"           "pos"                     "playType"                "playTypePrecise"         "passResult"
# [41] "startWp"                 "wpa"                     "startEp"                 "epa"                     "driveNumber"
# [46] "driveStartQuarter"       "driveStartDistance"      "driveStartingClock"      "drivePlays"              "driveYards"
# [51] "driveTimeOfPossession"   "driveHowEnded"           "playDesc"                "sDRGameId"               "sdrEventId"
# [56] "scheduleId"              "wallClock"               "wallClock_1"             "drivePlays_1"            "driveYards_1"
# [61] "driveTimeOfPossession_1" "driveHowEnded_1"         "onOffense"               "highlightVideo"          "highlightImageSmall"
# [66] "highlightImageLarge"     "highlightCaption"        "videoBroadcast"          "videoSideline"           "videoEndZone"
# [71] "yards"                   "date"                    "seasonWeek"              "game"                    "qtr"
# [76] "rusher"                  "passer"                  "target"                  "zebraData"               "zebraBallSnapY"
# [81] "EventSum"                "mib"

### combine all the play level data and plot a density curve of epa
pass_fans %>%
   bind_rows(run_fans) %>%
   ggplot(aes(x = epa, color = playType, fill = playType)) +
   geom_density(alpha = .3) +
   geom_vline(xintercept = 0)

### combine all the play level data and calculate mean eap for each play type
pass_fans %>%
   bind_rows(run_fans) %>%
   mutate(success = ifelse(epa > 0, 1, 0)) %>%
   group_by(playType) %>%
   summarize(epa = mean(epa),
             success = mean(success),
             count = n())

### FANS runs
### https://football.espntrumedia.com/football/teams/nfl/?pc=%7B%22fs%22%3A%22nfl%22%2C%22fgb%22%3A%22by-season%22%2C%22fsvt%22%3A%22stats%22%7D&is=true&t=%7B%22orderBy%22%3A%7B%22orderCols%22%3A%22OfSnap%22%2C%22sortOrder%22%3A%22DEFAULT%22%7D%2C%22pinnedIds%22%3A%5B%5D%2C%22customReport%22%3A%7B%22selectedReportName%22%3A%22Team%20Offensive%20Rates%22%2C%22selectedReportStatsId%22%3A92%7D%7D&f=%7B%22fdefbox%22%3A%5B7%2C11%5D%2C%22fdtf%22%3A%5B7%2C10%5D%2C%22fdtg%22%3A%5B20%2C99%5D%2C%22fd%22%3A%5B%22first%22%2C%22second%22%5D%2C%22fgt%22%3A%5B%22regular%22%5D%2C%22fprz%22%3A%22no%22%2C%22fpt%22%3A%5B%22RUSH%22%5D%2C%22fswr%22%3A%7B%22fromSeason%22%3A%222008%22%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A%222018%22%2C%22toWeek%22%3A%2217%22%7D%7D
### 54,372 run snaps

### load the data from the link above and add a new column to join on for y/y analysis
run_fans_team <- run_fans_team %>%
   mutate(season2 = season + 1)

### perform the join
run_fans_team_joined <- run_fans_team %>%
   left_join(run_fans_team, by = c("teamId", "season2" = "season"))

### visualize the rush snap distribiution
run_fans_team_joined %>%
   ggplot(aes(x = OfSnap.x)) +
   geom_histogram(alpha = .3)

### create a y/y model to test the stability
rush_snap_model <- lm(data = run_fans_team_joined, `OfSnap.y` ~ `OfSnap.x`)
tidy(rush_snap_model)

### visualize the stability
run_fans_team_joined %>%
   ggplot(aes(x = `OfSnap.x`, y = `OfSnap.y`)) +
   geom_point() +
   geom_smooth(method = "lm") +
   labs(x = "Run snaps in year Y", y = "Run snaps in year Y + 1")


### All runs
### https://football.espntrumedia.com/football/teams/nfl/?pc=%7B"fs"%3A"nfl"%2C"fgb"%3A"by-season"%2C"fsvt"%3A"stats"%7D&is=true&t=%7B"orderBy"%3A%7B"orderCols"%3A"Scs%25"%2C"sortOrder"%3A"DEFAULT"%7D%2C"pinnedIds"%3A%5B%5D%2C"customReport"%3A%7B"selectedReportName"%3A"Team%20Offensive%20Rates"%2C"selectedReportStatsId"%3A92%7D%7D&f=%7B"fgt"%3A%5B"regular"%5D%2C"fpt"%3A%5B"RUSH"%5D%2C"fswr"%3A%7B"fromSeason"%3A"2008"%2C"fromWeek"%3A"1"%2C"toSeason"%3A"2018"%2C"toWeek"%3A"17"%7D%7D
### 151,412 run snpas

### load the data from the link above and add a new column to join on for y/y analysis
all_rush_team <- all_rush_team %>%
   mutate(season2 = season + 1)

### perform the join
all_rush_team_joined <- all_rush_team %>%
   left_join(all_rush_team, by = c("teamId", "season2" = "season"))

### visualize the rush snap distribiution
all_rush_team_joined %>%
   ggplot(aes(x = OfSnap.x)) +
   geom_histogram(alpha = .3, binwidth = 5) +
   labs(x = "Run snaps in year Y", y = "Run snaps in year Y + 1")

### create a y/y model to test the stability
all_rush_snap_model <- lm(data = all_rush_team_joined, `OfSnap.y` ~ `OfSnap.x`)
tidy(all_rush_snap_model)

### visualize the stability
all_rush_team_joined %>%
   ggplot(aes(x = `OfSnap.x`, y = `OfSnap.y`)) +
   geom_point() +
   geom_smooth(method = "lm")

### FANS passes
### https://football.espntrumedia.com/football/teams/nfl/?pc=%7B%22fs%22%3A%22nfl%22%2C%22fgb%22%3A%22by-season%22%2C%22fsvt%22%3A%22stats%22%7D&is=true&t=%7B%22orderBy%22%3A%7B%22orderCols%22%3A%22OfSnap%22%2C%22sortOrder%22%3A%22DEFAULT%22%7D%2C%22pinnedIds%22%3A%5B%5D%2C%22customReport%22%3A%7B%22selectedReportName%22%3A%22Team%20Offensive%20Rates%22%2C%22selectedReportStatsId%22%3A92%7D%7D&f=%7B%22fdefbox%22%3A%5B7%2C11%5D%2C%22fdtf%22%3A%5B7%2C10%5D%2C%22fdtg%22%3A%5B20%2C99%5D%2C%22fd%22%3A%5B%22first%22%2C%22second%22%5D%2C%22fgt%22%3A%5B%22regular%22%5D%2C%22fprz%22%3A%22no%22%2C%22fpt%22%3A%5B%22PASS%22%5D%2C%22fswr%22%3A%7B%22fromSeason%22%3A%222008%22%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A%222018%22%2C%22toWeek%22%3A%2217%22%7D%7D
### 36,604 pass snaps

### load the data from the link above and add a new column to join on for y/y analysis
pass_fans_team <- pass_fans_team %>%
   mutate(season2 = season + 1)

### perform the join
pass_fans_team_joined <- pass_fans_team %>%
   left_join(pass_fans_team, by = c("teamId", "season2" = "season")) %>%
   na.omit()

### visualize the pass snap distribiution
pass_fans_team_joined %>%
   ggplot(aes(x = OfSnap.x)) +
   geom_histogram(alpha = .3, binwidth = 5)

### create a y/y model to test the stability
pass_snap_model <- lm(data = pass_fans_team_joined, `OfSnap.y` ~ `OfSnap.x`)
tidy(pass_snap_model)

### visualize the stability
pass_fans_team_joined %>%
   ggplot(aes(x = `OfSnap.x`, y = `OfSnap.y`)) +
   geom_point() +
   geom_smooth(method = "lm")

### All passes
### https://football.espntrumedia.com/football/teams/nfl/?pc=%7B%22fs%22%3A%22nfl%22%2C%22fgb%22%3A%22by-season%22%2C%22fsvt%22%3A%22stats%22%7D&is=true&t=%7B%22orderBy%22%3A%7B%22orderCols%22%3A%22Scs%25%22%2C%22sortOrder%22%3A%22DEFAULT%22%7D%2C%22pinnedIds%22%3A%5B%5D%2C%22customReport%22%3A%7B%22selectedReportName%22%3A%22Team%20Offensive%20Rates%22%2C%22selectedReportStatsId%22%3A92%7D%7D&f=%7B%22fgt%22%3A%5B%22regular%22%5D%2C%22fpt%22%3A%5B%22PASS%22%5D%2C%22fswr%22%3A%7B%22fromSeason%22%3A%222008%22%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A%222018%22%2C%22toWeek%22%3A%2217%22%7D%7D

### load the data from the link above and add a new column to join on for y/y analysis
all_pass_team <- all_pass_team %>%
   mutate(season2 = season + 1)

### perform the join
all_pass_team_joined <- all_pass_team %>%
   left_join(all_pass_team, by = c("teamId", "season2" = "season")) %>%
   na.omit()

### visualize the pass snap distribiution
all_pass_team_joined %>%
   ggplot(aes(x = OfSnap.x)) +
   geom_histogram(alpha = .3, binwidth = 5)

### create a y/y model to test the stability
all_pass_snap_model <- lm(data = all_pass_team_joined, OfSnap.y ~ OfSnap.x)
tidy(all_pass_snap_model)

### visualize the stability
all_pass_team_joined %>%
   ggplot(aes(x = `OfSnap.x`, y = `OfSnap.y`)) +
   geom_point() +
   geom_smooth(method = "lm")

### So, teams tend to pass in FANS situations in roughly the same amount as the did the previous year. And this is
### much more stable than the total number of time they pass on all downs and MIB situations

### How about as a percentage of plays? Is that more or less stable?

all_team_fans <- read_csv("data/team_off_fans-08-18.csv")

### add a new column to join on for y/y analysis
all_team_fans <- all_team_fans %>%
   mutate(season2 = season + 1,
          pass_pct = Att / (Att + Rush))

### perform the join
all_team_fans_joined <- all_team_fans %>%
   left_join(all_team_fans, by = c("teamId", "season2" = "season")) %>%
   na.omit()

### visualize the pass snap distribiution
all_team_fans_joined %>%
   ggplot(aes(x = pass_pct.x)) +
   geom_histogram(alpha = .3, binwidth = .1)

### create a y/y model to test the stability
all_team_fans_model <- lm(data = all_team_fans_joined, pass_pct.y ~ pass_pct.x)
tidy(all_team_fans_model)

### visualize the stability
all_team_fans_joined %>%
   ggplot(aes(x = `pass_pct.x`, y = `pass_pct.y`)) +
   geom_point() +
   geom_smooth(method = "lm")

### https://football.espntrumedia.com/football/teams/nfl/?pc=%7B%22fs%22%3A%22nfl%22%2C%22fgb%22%3A%22by-season%22%2C%22fsvt%22%3A%22stats%22%7D&is=true&t=%7B%22orderBy%22%3A%7B%22orderCols%22%3A%22team%22%2C%22sortOrder%22%3A%22DEFAULT%22%7D%2C%22pinnedIds%22%3A%5B%5D%2C%22customReport%22%3A%7B%22selectedReportName%22%3A%22Team%20Offensive%20Rates%22%2C%22selectedReportStatsId%22%3A92%7D%7D&f=%7B%22fdefbox%22%3A%5B7%2C11%5D%2C%22fdtf%22%3A%5B7%2C10%5D%2C%22fdtg%22%3A%5B20%2C99%5D%2C%22fd%22%3A%5B%22first%22%2C%22second%22%5D%2C%22fgt%22%3A%5B%22regular%22%5D%2C%22fprz%22%3A%22no%22%2C%22fpt%22%3A%5B%22RUSH%22%5D%2C%22fswr%22%3A%7B%22fromSeason%22%3A%222018%22%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A%222018%22%2C%22toWeek%22%3A%2217%22%7D%7D

### import data from the above link and choose the columns of interest
fans_18_pass <- read_csv("data/fans_18_pass.csv") %>%
   mutate(play_type = "pass") %>%
   select(team, play_type, pass_snaps = `OfSnap`, pass_epa = `EPA/Pl`, pass_success = `Scs%`)
fans_18_run <- read_csv("data/fans_18_rush.csv") %>%
   mutate(play_type = "run") %>%
   select(team, play_type, rush_snaps = `OfSnap`, rush_epa = `EPA/Pl`, rush_success = `Scs%`)

### combine the data for display
fans_18 <- fans_18_pass %>%
   left_join(fans_18_run, by = "team")

### https://football.espntrumedia.com/football/teams/nfl/?pc=%7B%22fs%22%3A%22nfl%22%2C%22fgb%22%3A%22by-season%22%2C%22fsvt%22%3A%22stats%22%7D&is=true&t=%7B%22orderBy%22%3A%7B%22orderCols%22%3A%22Att%22%2C%22sortOrder%22%3A%22DEFAULT%22%7D%2C%22pinnedIds%22%3A%5B%5D%2C%22customReport%22%3A%7B%22selectedReportName%22%3A%22Offensive%20Plays%22%2C%22selectedReportStatsId%22%3A105%7D%7D&f=%7B%22fdefbox%22%3A%5B7%2C11%5D%2C%22fdtf%22%3A%5B7%2C10%5D%2C%22fdtg%22%3A%5B20%2C99%5D%2C%22fd%22%3A%5B%22first%22%2C%22second%22%5D%2C%22fgt%22%3A%5B%22regular%22%5D%2C%22fnumrbs%22%3A%5B2%2C2%5D%2C%22fnumtes%22%3A%5Bnull%2Cnull%5D%2C%22fprz%22%3A%22no%22%2C%22fpt%22%3A%5B%22PASS%22%2C%22RUSH%22%5D%2C%22fswr%22%3A%7B%22fromSeason%22%3A%222018%22%2C%22fromWeek%22%3A%221%22%2C%22toSeason%22%3A%222018%22%2C%22toWeek%22%3A%2217%22%7D%7D

### It looks like on 75 of those pass attempts the 49ers had 2 RBs.
### Next highest was the Falcons at 41 pass attempts with 2 RBs

### export the data for a table
write_csv(fans_18, "fans_18.csv")

### Let's see how much an expected point is worth in terms of wins.

team_ep <- read_csv("data/team_epa.csv")

team_ep %>%
   group_by(RecordW) %>%
   summarize(ep = mean(OfEPA),
             count = n()) %>%
   arrange(-ep)

ep_on_wins <- lm(data = team_ep, RecordW ~ OfEPA)
summary <- tidy(ep_on_wins)
summary$estimate[2] * 38 - summary$std.error[2] * 1.96
summary$estimate[2] * 38 + summary$std.error[2] * 1.96
### We're looking at a win every 38 expected points or so

### visualize it
team_ep %>%
   ggplot(aes(x = OfEPA, y = RecordW)) +
   geom_point() +
   labs(x = "EPA", y = "Wins")


