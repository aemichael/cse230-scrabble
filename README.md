# Scrabble

Group Members: Alexandra Michael (aemichae@ucsd.edu), Imran Matin (imatin@ucsd.edu)

Our proposal is to use the `brick` library to implement a command-line version of Scrabble.
We plan to begin by creating a fully local, minimum implementation of Scrabble based on the
minimum goals outlined below. If time allows, we will extend our implementation to support
one or more of the stretch goals listed below.

## Updates 11/29/2021

The architecture of our application looks similar to that for the `brick-tac-toe` starter game,
with `Board`, `Player`, and `Score` components. To model the particulars of Scrabble, we add
`TileLetter`, which represents a single letter tile in the game; and `Bag`, which models the
bag from which tiles are drawn. We also modify the `Player` component to include a seven-tile
`Rack`, which the player will use to create words.

So far, our largest challenge has been time management, since both of us have many other
commitments competing for our time. We are adjusting to the time we have available to work on this
project by splitting our original goals into a simplified set of minimum goals, requiring only
that we support one player playing Scrabble against "themselves", and a set of stretch goals
that involve supporting 2 players and more complicated scoring. (Our original stretch goals are
now labeled "Superlative Goals.") We hope to make significant progress on our stretch goals before
the deadline, but if other obligations intervene, we are confident that we can at least accomplish
our new set of minimum goals.


## Goals:

### Minimum:
- [ ] Implement a basic UI that imitates a Scrabble board and seven-tile row for the current player
- [ ] Implement actions and features required for basic Scrabble play:
  - [ ] Player can freely place and remove their tiles to and from the board during their turn
  - [ ] Player can finalize their play at the end of their turn
  - [ ] Player receives tiles from the bag to maintain a set of seven at the end of their turn
  - [ ] Track the player's current score
- [ ] Support a single player playing one round of Scrabble against "themselves", which ends
  when the bag is empty

### Stretch Goals:
- [ ] Track which tiles provide bonuses and update players' scores accordingly
- [ ] Support 2 players playing Scrabble locally on the same instance of the program
- [ ] A turn system in which each player alternates placing a word on the board
- [ ] Detect the end of the game and allow players to close or restart the game

### Superlative Goals:
- [ ] Enforce that plays are consecutive in a row or column and build off an existing play
- [ ] Implement a "challenge" mechanism that allows players to challenge each other's words in
  the turn after they are played
- [ ] Support swapping out all tiles in a player's row during their turn
- [ ] Allow players to draw tiles to determine the play order
- [ ] Support up to 4 players
- [ ] Allow players to play Scrabble over a network connection
