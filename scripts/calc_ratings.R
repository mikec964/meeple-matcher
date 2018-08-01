# Calculate ratings for games and game attributes

# calc stats about collection sizes ---------------------------
n_collection_cust <- nrow(subset(gamer_collections_tall,
                                 gamer == customer))
n_rated_cust <- nrow(subset(gamer_collections_tall,
                            gamer == customer & !is.na(rating)))
n_collection_all <- nrow(subset(gamer_collections_tall,
                                gamer != customer))
n_rated_all <- nrow(subset(gamer_collections_tall,
                           gamer != customer & !is.na(rating)))
n_ratings <- nrow(bg_ratings_tall)

# calc ratings per game ---------------------------------------
#' Calc ratings per game by customer and overall
#'
#' From a table of gamers, games, and ratings (collections),
#' this builds a table of games and aggregate ratings.
calc_bg_ratings <- function(gamer_collections = gamer_collections_tall) {
  bg_ratings_tbl <- gamer_collections %>%
    filter(!is.na(rating)) %>%
    select(game.id, gamer, rating) %>%
    mutate(is_cust = (gamer == customer),
           c_rating = is_cust * rating,
           c_rating = na_if(c_rating, 0),
           a_rating  = (!is_cust) * rating,
           a_rating  = na_if(a_rating, 0))
  bg_ratings_tbl <- bg_ratings_tbl %>%
    group_by(game.id) %>%
    summarize(n_ratings = n(),
              rating_by_cust = sum(c_rating, na.rm = TRUE),
              rating_by_cust = na_if(rating_by_cust, 0),
              ratings_by_all  = mean(a_rating, na.rm = TRUE))
}

# calc ratings per attribute ----------------------------------
#' Per each value for a game attribute value, calculate ratings
#'
#' Game attributes are things like mechanics, categories.
#' For the mechanic attribute, values can be Card drafting, Dice rolling, etc
#' For category: Science fiction, Fantasy, Party game, Zombies etc
#' For each value this will return customer and overall ratings
#'
#' @param bg_attr "boardgamemechanic" or "boardgamecategory"
#' @param bg_attrs_tall A tall table of game.id and key/value pairs.
#' @param bg_ratings_tbl A table of games with customer and overall ratings.
#' @return A table of values and count of games with that value.
calc_attr_ratings <- function(bg_attr,
                           attrs_tall = bg_attrs_tall,
                           ratings_tbl = bg_ratings_tbl) {
  attr_stats <- attrs_tall %>%
    # tall with game.id * attribute (mechanic) rows,
    #     n (count of ratings), cust_rating, all_ratings
    group_by(game.id) %>%
    filter(key == bg_attr) %>%
    select(-one_of("key")) %>%
    inner_join(ratings_tbl, by = "game.id")
  ttl_cust_games <- sum(!is.na(bg_ratings_tbl$rating_by_cust))
  ttl_all_games <- sum(!is.na(bg_ratings_tbl$ratings_by_all))

  attr_stats2 <- attr_stats %>%
    # table of attribute values (mechanics),
    #   n_games (per attr value), and weighted ratings
    group_by(value) %>%
    summarize(n_games = n(),
              n_cust_games = sum(!is.na(rating_by_cust)),
              ratings_total = sum(n_ratings),
              cust_rating_wt = mean(rating_by_cust, na.rm = TRUE),
              all_ratings_wt = sum(ratings_by_all * n_ratings)
              / sum(n_ratings)) %>%
    arrange(desc(cust_rating_wt))

  attr_stats3 <- attr_stats2 %>%
    mutate(cust_ownership = n_cust_games / ttl_cust_games) %>%
    mutate(all_ownership = n_games / ttl_all_games) %>%
    mutate(own_ratio = cust_ownership / all_ownership)
}

