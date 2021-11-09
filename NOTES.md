# We're writing Scrabble :)

Goals:
* Write Scrabble
* Play Scrabble

More specifically: Create a networked Scrabble implementation that allows for 2-4 players (ages <can use laptop>+).

Define what the state will be

Define what the events will be and how we handle them

Define what the interface will look like

Define how we will implement this over the network

## Scrabble Rules

Link to Rules Video: https://youtu.be/K1KgvZwwJqo

Objective: Spell words to score points. Have the highest total at the end of the game.

Termination: Game ends when all letters have been drawn and all letters have been used or when all possible plays have been made.

Setup: Place board down. Provide each player a tile holder. Timer is optional but can be used to limit time taken each turn. Letter tiles are placed in a pouch.

Each player draws a letter and the player that draws the letter closest to A goes first. Blank tile beats any letter. Ties mean redraw.

Each player then draws 7 tiles.

Each turn you place tiles so that you can make a word that reads top to bottom or left or right.

Each tile must touch another tile already in play and if it changes a word that word must become a new word.

Score points for each letter in all new words formed during your turn. Some letters may be counted more 2x if in more than 1 new word. If did not change a word then do not count the points.

Cannot shift or move tiles already played.

Bonus tiles mean that you apply its effects to every new word that the letter on the bonus tile affects.

Bonus letter tiles apply to each individual letter's score whereas bonus word tiles apply to the whole words score.

Star in middle does 1) determine where first word must be played, and 2) doubles the score of the first word.

If play all 7 tiles in one turn, then recieve a 50 point bonus that turn

Blank tiles can be used for whatever letter but once they are played they cannot be changed.

At the end of each turn, draw tiles from the bag until you have 7 in your tile rack.

Player then moves to next player on the left.

During your turn you may choose to not place tiles but instead swap tiles from rack for new ones in bag as long as more than 7 tiles in bag.

Unallowed words: always capitalized, abbreviations, prefixes and suffixes by themselves, words requiring an apostrophe or hyphen, or non-real or non-complete english word

People can challenge if a word is real, use a dictionary and if it is real the challenger loses next turn. If challenger is right then all tiles are taken back and they lose their turn.

All words in one play are challenged simultaneously. Only one turn is lost and all words are chellegend.

Dictionary is only allowed during a challenge.

Score kept on scorepad.

At end of the game all unplayed tiles from a palyers rack are subtracted from total score.

If player played all their tiles, then the sum of all other players unplayed tiles is added to their score

