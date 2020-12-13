
# This function takes a blank board and 2 'players' and finds out who wins
# The player functions take in a matrix (game board) and return where they'd like to move
simulate_full_game <- function(player1_model = NULL, player2_model = NULL, printing = FALSE){

  # Set up a blank game board
  game_board <- create_random_game_board(num_moves = 0L)

  # Set up a list to keep track of game history (player turn,
  # game board before move, chosen position)
  game_history <- list()

  # Player 1 goes first
  player_turn <- 1
  game_turn <- 0

  # Add moves until a winner is found
  while (TRUE) {

    game_turn <- game_turn + 1

    if (printing) {
      print(player_turn)
      print(game_board)
    }


    # Based on who's turn it is, find out where they'd like to move
    if (player_turn == 1) {
      # Player 1 uses the neural net to choose its move
      position <- choose_next_move(game_board = game_board, player_model = player1_model)
    } else {
      # Player 2 goes random
      position <- choose_random_position(game_board = game_board)
    }

    game_history[[game_turn]] <- list(game_turn = game_turn,
                                      player_turn = player_turn,
                                      game_board = game_board,
                                      chosen_position = position)

    if (printing) print(paste0("position chosen = ", position))

    # Put their token in that spot
    game_board <- add_player_move(game_board = game_board,
                                  player_num = player_turn,
                                  position = position)

    if (printing) print("####")

    # Determine if they've won, if so, break
    winner <- determine_if_winner(game_board = game_board)

    # Otherwise, increment player (from 1 to 2 or from 2 to 1)
    if (winner == "Neither" & 0 %in% game_board) {
      # Game continues
      player_turn <- 3 - player_turn
    } else {
      # Game is over either as a draw or there's a winner
      break
    }

  }

  # Return full results from the game (finished board, player who won, etc)
  if (printing) {
    print(game_history)
    print(game_board)
    print(paste0("winner is ", winner))
  }

  #return("complete")

  return(list(final_game_board = game_board,
              game_history = game_history,
              winner = winner))

}

