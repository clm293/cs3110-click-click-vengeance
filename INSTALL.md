# Installation and Running Instructions

Users need to have the following packages installed:
* Yojson
* Graphics
* camlimages

To install these enter these commands into the command line:
```opam install yojson``` <br>
```opam install graphics``` <br>
```opam install camlimages```

To build and play the game:
```make build``` <br>
```make play``` <br>
You can read instructions on how to play the game by clicking on the help icon in the bottom left corner of the start screen.

The leaderboard shown when you finish playing a level or lose in endless mode is created each time the game window is opened. Therefore, if you open the game, close it, and open it again, the scores from the first opening will not persist. 

Sometimes the graphics will glitch because of the OCaml graphics library. There isn't anything we can do about this :(.
Also, there may be some functions in the graphics module that don't work on Windows, so this may cause errors. No one in our group has Windows so it works on Mac and we haven't gotten to try it on Windows. If any strange errors occur, trying it on Mac should help. Sorry! OCaml graphics isn't the best :/

To test the game:
First you need to comment out lines where the graphics are updated in game.ml. These lines are marked by comments with ```TESTING_LINES```, there are instructions on which lines to comment out before testing.
You must also comment out the final line main.ml that calls main (). 
After doing all of this you may run:<br>
```make test```

To make docs:<br>
```make docs```<br>
This will generate some errors and warnings. The docs are still generated though, so no worries.