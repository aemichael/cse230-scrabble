# Scrabble

Group Members: Alexandra Michael (aemichae@ucsd.edu), Imran Matin (imatin@ucsd.edu)

Our proposal is to use the `brick` library to implement a command-line version of Scrabble.
We plan to begin by creating a fully local, minimum implementation of Scrabble based on the
minimum goals outlined below. If time allows, we will extend our implementation to support
one or more of the stretch goals listed below.

### Minimum Goals:
- [ ] Implement a basic UI that imitates a Scrabble board and seven-tile row for the current player
- [ ] Implement actions and features required for basic Scrabble play, including:
  - [ ] Allowing players to freely place and remove their tiles to and from the board during their turn
  - [ ] Allowing players to finalize their play at the end of their turn
  - [ ] Allowing players to withdraw tiles from the bag to maintain a set of seven
  - [ ] An turn system in which each player alternates placing a word on the board
  - [ ] Detecting the end of the game and allow players to close or restart
  - [ ] Tracking players' current scores
  - [ ] Tracking which tiles provide bonuses and update players' scores accordingly
- [ ] Support 2 players playing Scrabble locally on the same instance of the program

### Stretch Goals:
- [ ] Enforce that plays are consecutive in a row or column and build off an existing play
- [ ] Implement a "challenge" mechanism that allows players to challenge each other's words in
  the turn after they are played
- [ ] Support swapping out all tiles in a player's row during their turn
- [ ] Allow players to draw tiles to determine the play order
- [ ] Support up to 4 players
- [ ] Allow players to play Scrabble over a network connection
