# Installation and Running Instructions

Users need to have the following packages installed:
* Yojson
* Graphics
* camlimages

To install these enter these commands into the command line:
```opam install yojson```
```opam install graphics```
```opam install camlimages```

To build and play the game:
```make build```
```make play```
You can read instructions on how to play the game by clicking on the help icon in the bottom left corner of the start screen.

The leaderboard shown when you finish playing a level or lose in endless mode is created each time the game window is opened. Therefore, if you open the game, close it, and open it again, the scores from the first opening will not persist. 

To test the game:
First you need to comment out lines where the graphics are updated in game.ml. These lines are marked by comments with "TESTING_LINES", there are instructions on which lines to comment out before testing.
You must also comment out the final line main.ml that calls main (). 
After doing all of this you may run
```make test```

To make docs:
```make docs```
This will generate some errors and warnings. The docs are still generated though, so no worries.