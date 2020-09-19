#'@title
#'Create a new Monty Hall Problem game.
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
#' create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Door Selection
#' @description
#'  `select_door()`The contestant selects a door.
#' @details
#' The contestant selects only one door. It can be either a goat door or a car door.
#' @param
#' no arguments are used by the function.
#' @return
#'  The function returns a numeric vector which indicates the number of the
#'  chosen door
#' @examples
#' select_door ()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Open a Goat Door
#'
#' @description
#' `open_goat_door()`This function opens one goat door only. This door should be different from the
#' goat door chosen initially.
#'
#' @details
#' The host will always open a goat door. However, it should be different
#' than the goat door chosen by contestant.
#'
#' @param ...arguments are (game, a.pick)
#' game is the one launched
#' a.pick is the initial pick of the contestant
#'
#' @return
#' The function returns a character vector of length 1
#' indicating the position of opened door.
#'
#' @examples
#' open_goat_door(game,a.pick)
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
#' Change Doors
#' @description
#' Contestant chooses to either stay on the initial selection
#' or switch to a different one
#' @details
#' The contestant has the option to proceed with the first selection of the door
#' or choose a different one that it is still closed. After the host make a selection
#' which is neither similar to the contestant's choice nor a car door, the
#' contestant is given the chance to choose.
#' @param ...arguments are (stay=T, opened.door, a.pick)
#' stay=T: True if contestant decides to proceed with the initial selection
#' stay=F: False if contestant switches the selection to a new door
#' opened.door: door opened by host
#' a.pick: initial selection of contestant
#' @return
#' The function returns a character vector of length 1
#' indicating the final pick of the contestant
#' @examples
#' change_door (stay=T,opened.door,a.pick)
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
#' Determine Winner
#'
#' @description
#' The function determines if the contestant has won or lost based on the
#' final pick. The contestant could have either stayed on the initial selection
#' or switched to a new one.
#'
#' @details
#' If the final pick by the contestant is a car door then the contestant wins.
#' However, if the final pick is a goat door, then the contestant looses.
#'
#' @param... arguments are (final.pick,game)
#' @return The function prints if its a win or a lost
#'
#' @examples determine_winner (final.pick,game)
#'
#' @export
#'
determine_winner <- function( final.pick, game)
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
#' Play Game
#'
#' @description
#' This function includes game demonstration
#'
#' @details
#' The contestant and host start a game round. Each one of them take their turn
#' of selection.
#'
#' @param ... no arguments used
#'
#' @return
#' the function returns a data frame that includes the strategy used by
#' contestant and the outcome if stayed or switched (lose or win)
#'
#' @examples
#'
#' play_game ()
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
#' Play N Games
#'
#' @description
#' Play 100 games, bind the results in a data frame and then tabulate
#' them
#'
#'
#' @details
#' Create a list to collect the results of every round (100 rounds in this case)
#' Iterate through the list until we fill all the results
#' Bind rows of results in data frame
#' Create a table of results
#'
#' @param ...argument is the number of games and here it is n=100
#'
#' @return
#' This function returns a data frame of results
#'
#' @examples
#' play_n_games (n=100)
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
