LoadSeason <- function(filename = "EPL 13-14.csv") {
  # Loads season from specified file containing a csv with all the games
  #
  # Args:
  #   filename: name of file to load, defaults to "EPL 13-14.csv"
  #
  # Returns:
  #   data frame with the following columns
  #     Date.Time: date and time of when the game began
  #     Home.Team: home team
  #     Away.Team: away team
  #     Home.Score: goals scored by the home team
  #     Away.Score: goals scored by the away team
  read.csv(filename)
}

BuildLeagueTable <- (games, date = "final") {
  
}