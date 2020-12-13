
library(ggplot2)
library(data.table)
library(devtools)

visualize_board <- function(game_board){

  game_board_dt <- data.table(game_board)
  game_board_dt[, Row := 1:nrow(game_board_dt)]
  game_board_dt <- melt(game_board_dt, id.vars = "Row", variable.name = "Column", value.name = "Player")
  game_board_dt[, Column := as.numeric(gsub("V", "", Column))]

  game_board_dt[, Player_Name := paste0("Player ", Player)]

  game_board_dt[, Row := factor(Row, levels = rev(unique(Row)))]

  ggplot(game_board_dt, aes(as.character(Column), Row, color = Player_Name)) +
    geom_point(size = 15) +
    scale_color_manual(values = c("Player 1" = "red",
                                  "Player 2" = "black",
                                  "Player 0" = "white")) +
    theme_bw() +
    xlab(label = "") +
    ylab(label = "")

}
