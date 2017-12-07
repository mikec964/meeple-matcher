GetGameName <- function(collection, gid) {
  # Returns game name given game.id
  # To-do: Select primary English name
  games <- collection[collection$game.id == gid, ]
  return(games[1,"game"])
}

GetGameAttr <- function(game.attrs, attr) {
  # returns vector of values given attribute key

  value <- game.attrs[game.attrs$key == attr, "value"]
  return(value)
}
