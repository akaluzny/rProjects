LoadSeason <- function(filename = "EPL 13-14.csv") {
  # Loads season from specified file containing a csv with all the games
  #
  # Args:
  #   filename: Name of file to load, defaults to "EPL 13-14.csv"
  #
  # Returns:
  #   Data frame with the following columns
  #     Date.Time: Date and time of when the game began
  #     Home.Team: Home team
  #     Away.Team: Away team
  #     Home.Score: Goals scored by the home team
  #     Away.Score: Goals scored by the away team
  
  read.csv(filename)
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
  
  teams <- levels(games$Home.Team)
  games.by.home.team <- split(games, games$Home.Team)
  games.by.away.team <- split(games, games$Away.Team)
  games.by.team <- vector("list", length(teams))
  for(i in seq_along(teams)) {
    games.by.team[[i]] <- rbind(games.by.home.team[[i]], games.by.away.team[[i]])
  }
  games.by.team
}