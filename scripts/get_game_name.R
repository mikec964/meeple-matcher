GetGameName <- function(gid) {
  # Returns game name given game.id
  # To-do: Select primary English name
  games <- collection.selected[collection.selected$game.id == gid, ]
  return(games$game[1])
}
