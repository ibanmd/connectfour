
read_simulation_results <- function(simulation_output_folder, mirror = FALSE){

  # TODO - get rid of this
  library(data.table)
  library(abind)

  # Read in all of the rds files in the simulation output folder
  # Each element of simulations_runs is the following:
  # - final_game_board - 6x7 matrix with the final state (this shows a winning board)
  # - game_history, a list of lists of length 4
  #   - game_turn: numeric, what turn number is it
  #   - player_turn: 1 or 2, which player's turn is it
  #   - game_board - 6x7 matrix showing the board before the turn is taken
  #   - chosen_position - numeric, what slot did the player choose to play
  # - winner - character "Player 1" or "Player 2"
  simulation_runs <- lapply(list.files(simulation_output_folder,
                                       full.names = TRUE,
                                       pattern = ".rds$"),
                            readRDS)

  # Goal - convert all games into Player 1 perspective, and assign the following values to each
  # game state: -20 for losing board, 0 for starting board, and 20 for winning board
  # list of length number_of_games x boards_per_game of lists of
  # length 2: board from player 1's perspective, value of that board (20 = win, 0 = start, -20 = lost)
  board_value_list <- lapply(simulation_runs, function(x){

    # Get game winner
    winner <- x$winner

    # Number of turns in the game is the length of game history plus 1
    game_num_turns <- length(x$game_history) + 1

    # Create a sequence of 0 to 20 the same length as the number of game states
    board_value_seqence <- seq(0, 20, length.out = game_num_turns)

    # Create a list of game states and add the final game state to the list as well
    board_sequence <- lapply(1:(game_num_turns - 1), function(y){
      x$game_history[[y]]$game_board
    })
    board_sequence[[game_num_turns]] <- x$final_game_board

    # If mirror, then mirror all board states and double the training data
    if (mirror) {

      mirrored_board_sequence <- lapply(board_sequence, function(y) {y[,c(7:1)]})
      board_sequence <- c(board_sequence, mirrored_board_sequence)
      board_value_seqence <- c(board_value_seqence, board_value_seqence)

    }

    # Turn it into Player 1's perspective (penalties for Player 2 winning)
    if (winner == "Player 2") {
      board_value_seqence <- board_value_seqence * -1
    }

    # Return a 3D array and the values for each board
    list(board = abind(board_sequence, along = 3),
         value = board_value_seqence)

  })

  # Stack all the runs into a single 3D array of boards and a single vector of values
  board_array <- do.call(what = abind,
                         list(lapply(1:length(board_value_list), function(x){
                           board_value_list[[x]]$board
                         }),
                         along = 3))
  board_value_array <- unlist(lapply(1:length(board_value_list),
                                     function(x){
                                       board_value_list[[x]]$value
                                     }))

  return(list(board_array = aperm(board_array, c(3, 1, 2)),
              board_value_array = board_value_array))

}
