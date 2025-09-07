


#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant selects a door.
#'
#' @description
#'   `select_door()` allows the contestant to select one of 
#'   the three doors in the game.
#'
#' @details
#'   `select_door()` randomly selects one of the three doors created at the start
#'   of the game, which represents the contestant's first selection.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a number between 1 and 3 indicating the 
#'   contestant's inital selection.
#'   
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a goat door.
#'
#' @description
#'   `open_goat_door()` allows the host to open one of the remaining two doors 
#'   that were not selected by the contestant to reveal a goat.
#'
#' @details
#'   `open_goat_door()` randomly selects one of the two goat doors in the game
#'   if the contestant selected a car door; otherwise, `open_goat_door()`
#'   select the remaining goat door in the game that were not selected by the 
#'   contestant.
#'
#' @param game a length 3 character vector indicating the positions of 
#'   goats and the car.
#' @param a.pick a number between 1 and 3 indicating the contestant's selection.
#'
#' @return The function returns a number between 1 and 3 indicating the 
#'   host's selection.
#'   
#' @examples
#'   game <- create_game()
#'   first.pick <- select_door()
#'   open_goat_door(game, first.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change doors.
#'
#' @description
#'   `change_door()` give the contestant the option to stay with 
#'   their initial selection or switch to the other door that is still closed.
#'
#' @details
#'   `change_door()` assigns the contestant' inital selection as their 
#'   final decision if the contestant does not want to change; otherwise,
#'   `change_door()` selects the remaining closed door that was not
#'   previously selected by either the contestant or the host. 
#'
#' @param stay a logical vector indicating whether the contestant stay with 
#'   their initial selection or switch to a different door. This argument's 
#'   default value is TRUE.
#' @param opened.door a number between 1 and 3 indicating the host's selection.
#' @param a.pick a number between 1 and 3 indicating the contestant's selection.
#'
#' @return The function returns a number between 1 and 3 indicating the 
#'   contestant's final selection.
#'   
#' @examples
#'   game <- create_game()
#'   first.pick <- select_door()
#'   host.door <- open_goat_door(game, first.pick)
#'   change_door(stay=T, host.door, first.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant has won.
#'
#' @description
#'   `determine_winner()` determine if the contestant has won the game 
#'   based on their final door selection.
#'
#' @details
#'   `determine_winner()` determines that the contestant has won if  
#'   their final door selection contains a car; otherwise,
#'   `determine_winner()` determines that the contestant has lost the game
#'   if their final door selection contains a goat. 
#'
#' @param final.pick a number between 1 and 3 indicating the contestant's 
#'   final door selection after their stay or switch decision.
#' @param game a length 3 character vector indicating the positions 
#'   of goats and the car.
#'
#' @return The function returns a character vector of either Win or Lose
#'   indicating the results of the game.
#'   
#' @examples
#'   game <- create_game()
#'   first.pick <- select_door()
#'   host.door <- open_goat_door(game, first.pick)
#'   final.pick <- change_door(stay=T, host.door, first.pick )
#'   determine_winner(final.pick, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play a round of the Monty Hall Problem game.
#'
#' @description
#'   `play_game()` simulates a single round of the Monty Hall Problem game
#'   and returns the final outcome for both scenarios in which the  
#'   contestant engages in the "stay" and "switch" strategies.
#'
#' @details
#'   `play_game()` is a wrapper function that uses the `create_game()`,  
#'   `select_door()`, `open_goat_door()`, `change_door()`, and determine_winner()`
#'   to play through all the stages of a single round of the Monty Hall Problem game.
#'   This functions show the outcomes for both scenarios in which the contestant
#'   engages in the "stay" and "switch" strategies. 
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a dataframe with two rows and two columns. The
#'   first column indicates the strategy used by the contestant, and the
#'   second column indicates the outcome of the game for each startegy.
#'   
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play multiple rounds of the Monty Hall Problem game.
#'
#' @description
#'   `play_game()` simulates multiple rounds of the Monty Hall Problem game
#'   and returns the final outcome for both scenarios in which the  
#'   contestant engages in the "stay" and "switch" strategies for all games played.
#'
#' @details
#'   `play_n_games()` repeatedly run the `play_game()` function for `n` times.
#'.  This function is defaulted to simulate 100 games. It combines the results
#'   of all games played into a single list or dataframe and creates a summary table
#'   showing the win and lose rate of both the "stay" and "switch" strategies based on 
#`   the total number of games played.
#'
#' @param n a number indicating the number of rounds of the Monty Hall Problem game.
#'   The default value is 100.
#'
#' @return The function returns a dataframe indicating the win and lose rate
#'   for each strategy. The rows represent the strategy ("stay" or "switch"), and
#'   the columns represent the outcome ("WIN" or "LOSE").
#'   
#' @examples
#'   play_n_games()
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
