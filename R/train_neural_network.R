
library(neuralnet)

train_neural_network <- function(training_data, weights){

  # TODO - add a little noise to the vector so it's not quite all zeroes ?

  # Put all data in player 1's perspective (plus this allows some training data
  # where the opposing player goes first)

  reverse_1and2 <- Vectorize(function(game_state){

    game_state <- unlist(strsplit(game_state, ","))
    one_pos <- which(game_state == "1")
    two_pos <- which(game_state == "2")
    game_state[one_pos] <- "2"
    game_state[two_pos] <- "1"
    game_state <- paste0(game_state, collapse = ",")
    game_state

  })

  training_data[, Game_State_Player1 := ifelse(Player == 1,
                                               Game_State,
                                               reverse_1and2(Game_State))]

  # Format data into 42 columns for the game board slots and 7 for the response
  training_data_df <- data.frame(matrix(as.numeric(unlist(strsplit(paste0(training_data$Game_State_Player1,
                                                                          collapse = ","), split = ","))) +
                                          runif(n = length(training_data$Game_State_Player1), min = -0.1, max = 0.1),
                                        nrow = nrow(training_data),
                                        byrow = TRUE))

  training_names <- names(training_data_df)

  onehotcols <- data.frame(t(sapply(training_data$Chosen_Position,
                                    function(x){tmp <- rep(0, 7); tmp[x] <- 1; tmp})))
  names(onehotcols) <- paste0("Position", 1:7)

  training_data_df <- cbind(training_data_df, onehotcols)

  training_data_turn5 <- training_data_df[unlist(lapply(gregexpr("0", training_data$Game_State),
                                                        function(x) {length(x)})) < 39, ]

  # f <- one hot columns ~ all other predictor columns
  f <- as.formula(paste0(paste(names(onehotcols), collapse = "+"),
                         "~",
                         paste(training_names, collapse = "+")
                         ))

  nn_model <- neuralnet(f,
                        data = training_data_df,
                        hidden = c(3),
                        threshold = 0.1,
                        act.fct = "logistic",
                        startweights = weights,
                        #stepmax = iterations,
                        linear.output = FALSE,
                        lifesign = "minimal")

  return(nn_model)

}

