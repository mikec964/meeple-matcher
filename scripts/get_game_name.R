GetGameName <- function(gid) {
  # Returns game name given game.id
  # To-do: Select primary English name
  games <- collection.selected[collection.selected$game.id == gid, ]
  return(games$game[1])
}

GetGameAttr <- function(game.attrs, attr) {
  # returns vector of values given attribute key

  value <- game.attrs[game.attrs$key == attr, "value"]
  return(value)
}
