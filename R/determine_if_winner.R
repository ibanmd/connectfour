
library(magrittr)

determine_if_winner <- function(game_board){

  # Check for horizontal, vertical, and both diagonal positions
  # TODO - use symmetry instead of checking 4 cases, simplify this somehow
  # TODO - figure out why data table isn't working in here
  direction_combinations <- list(rows = lapply(1:nrow(game_board), function(x) game_board[x, ]),
                                 columns = lapply(1:ncol(game_board), function(x) game_board[, x]),
                                 upright = expand.grid(Row = 1:6, Column = 1:7) %>%
                                   data.table() %>%
                                   (function(x) {
                                     stopifnot(is.data.table(x))
                                     #print(x)
                                     x[x$Row == 6 | x$Column ==1, ]
                                     }) %>%
                                   (function(x){
                                     coords <- lapply(1:nrow(x), function(y){
                                       Filter(function(z) z[1] >= 1 & z[2] <= 7,
                                              lapply(0:6, function(z) c(-z, z) + unlist(x[y, ])))
                                     }) %>% Filter(f = function(a) length(a) >= 4)

                                     lapply(coords, function(y){
                                       unlist(lapply(y, function(z) game_board[z[1], z[2]]))
                                     })
                                   }),
                                 downright = expand.grid(Row = 1:6, Column = 1:7) %>%
                                   data.table() %>%
                                   (function(x) x[x$Row == 1 | x$Column == 1, ]) %>%
                                   (function(x){
                                     coords <- lapply(1:nrow(x), function(y){
                                       Filter(function(z) z[1] <= 6 & z[2] <= 7,
                                              lapply(0:6, function(z) z + unlist(x[y, ])))
                                     }) %>% Filter(f = function(a) length(a) >= 4)

                                     lapply(coords, function(y){
                                       unlist(lapply(y, function(z) game_board[z[1], z[2]]))
                                     })
                                   })
  )

  # Use runlengths to find if any runs are 4 or more
  game_run_lengths <- rbindlist(lapply(direction_combinations, function(x){
    rbindlist(lapply(x, function(y){
      runlength <- rle(y)
      data.table(Player = runlength$values,
                 Run_Length = runlength$lengths)
    }))
  }))

  stopifnot(is.data.table(game_run_lengths))

  # Can also check if they've won multiple ways by looking for > 4 or multiple rows
  player1win <- 1 %in% game_run_lengths[game_run_lengths$Run_Length >= 4, ]$Player
  player2win <- 2 %in% game_run_lengths[game_run_lengths$Run_Length >= 4, ]$Player

  # Check if which player won
  if(player1win & !player2win)
    return("Player 1")
  else if (!player1win & player2win)
    return("Player 2")
  else if (player1win & player2win)
    return("Both")
  else
    return("Neither")

}

