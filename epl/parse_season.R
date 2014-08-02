LoadSeason <- function(filename = "EPL 13-14.csv") {
  # Loads season from specified file containing a csv with all the games
  #
  # Args:
  #   filename: Name of file to load, defaults to "EPL 13-14.csv"
  #
  # Returns:
  #   Data frame with the following columns
  #     Date.Time: Date and time of when the game began. Format is "%m/%d/%y %H:%M".
  #     Home.Team: Home team
  #     Away.Team: Away team
  #     Home.Score: Goals scored by the home team
  #     Away.Score: Goals scored by the away team
  
  games <- read.csv(filename)
  games$Date.Time <- strptime(games$Date.Time, format = "%m/%d/%y %H:%M", tz = "GMT")
  games
}

BuildLeagueTable <- function(games, start.date = "initial", finish.date = "final") {
  # Builds a league table from individual games, including only games in the provided date range
  #
  # Args:
  #   games: Data frame of individual games
  #   start.date: Only games from this date will be used to compute league table. Default or "initial" will start with the first game.
  #   finish.date: Only games up to this date will be used to compute league table. Default or "final" will finish with the last game.
  #
  # Returns:
  #   Data frame with the following columns
  #     Team: Team in position
  #     Home.Games: Number of home games played
  #     Home.Wins: Number of home wins
  #     Home.Draws: Number of home draws
  #     Home.Losses: Number of home losses
  #     Home.Scored: Number of home goals scored
  #     Home.Allowed: Number of home goals allowed
  #     Home.Goal.Difference: Difference in home goals
  #     Home.Points: Number of home points
  #     Away.Games: Number of away games played
  #     Away.Wins: Number of away wins
  #     Away.Draws: Number of away draws
  #     Away.Losses: Number of away losses
  #     Away.Scored: Number of away goals scored
  #     Away.Allowed: Number of away goals allowed
  #     Away.Goal.Difference: Difference in away goals
  #     Away.Points: Number of away points
  #     Games: Number of total games played
  #     Wins: Number of total wins
  #     Draws: Number of total draws
  #     Losses: Number of total losses
  #     Scored: Number of total goals scored
  #     Allowed: Number of total goals allowed
  #     Goal.Difference: Difference in total goals
  #     Points: Number of total points
  
  table <- data.frame(Team = levels(games$Home.Team))
  by.home.team <- split(games, games$Home.Team)
  by.away.team <- split(games, games$Away.Team)
  
  table$Home.Games <- sapply(by.home.team, nrow)
  table$Home.Wins <- sapply(by.home.team, function(x) sum(x[, 4] > x[, 5]))
  table$Home.Draws <- sapply(by.home.team, function(x) sum(x[, 4] == x[, 5]))
  table$Home.Losses <- sapply(by.home.team, function(x) sum(x[, 4] < x[, 5]))
  table$Home.Scored <- sapply(by.home.team, function(x) sum(x[, 4]))
  table$Home.Allowed <- sapply(by.home.team, function(x) sum(x[, 5]))
  table$Home.Goal.Difference <- table$Home.Scored - table$Home.Allowed
  table$Home.Points <- table$Home.Wins * 3 + table$Home.Draws
  
  table$Away.Games <- sapply(by.away.team, nrow)
  table$Away.Wins <- sapply(by.away.team, function(x) sum(x[, 4] < x[, 5]))
  table$Away.Draws <- sapply(by.away.team, function(x) sum(x[, 4] == x[, 5]))
  table$Away.Losses <- sapply(by.away.team, function(x) sum(x[, 4] > x[, 5]))
  table$Away.Scored <- sapply(by.away.team, function(x) sum(x[, 5]))
  table$Away.Allowed <- sapply(by.away.team, function(x) sum(x[, 4]))
  table$Away.Goal.Difference <- table$Away.Scored - table$Away.Allowed
  table$Away.Points <- table$Away.Wins * 3 + table$Away.Draws
  
  table$Games <- table$Home.Games + table$Away.Games
  table$Wins <- table$Home.Wins + table$Away.Wins
  table$Draws <- table$Home.Draws + table$Away.Draws
  table$Losses <- table$Home.Losses + table$Away.Losses
  table$Scored <- table$Home.Scored + table$Away.Scored
  table$Allowed <- table$Home.Allowed + table$Away.Allowed
  table$Goal.Difference <- table$Home.Goal.Difference + table$Away.Goal.Difference
  table$Points <- table$Home.Points + table$Away.Points
  
  table
}