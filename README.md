# BattleShip

##Running the game
### Run the game using the IDE Intellij : 
You can run the game using Intellik by importing it to the IDE then right click on BattleShip.scala > Run 'BattleShip'
![Intellij](https://i.imgur.com/AMn3NHN.png)

### Using command line
Or you can use the command line. You must install SbT : https://www.scala-sbt.org/download.html?_ga=2.224585818.1963758690.1539057558-1925506192.1539057556

Then open the command prompt and go to the root source folder of the project (BattleShip) then run :
```
sbt compile
```
Then run :
```
sbt run
```
  
  ![BattleShip](https://i.imgur.com/TKBgO6Q.png)
  
## Game Content
At the beginning you have a menu which contains 4 choices : 
 - Player Vs player in which you will play vs another player
 - Player vs Ai : You will play vs an Ai which has 3 levels : Beginner, Medum and Hard
 ![BattleShip](https://i.imgur.com/desuIYv.png)
 
 - Ai vs Ai : The goal of this mode is to test the Ai levels, it will run a 100 round between every level of the Ais and you will have the result in a file called ai_proof.csv

At the start of a game you will be asked to choose the position of your ship, its orientation (vertical or horizontal) and the
![BattleShip](https://i.imgur.com/2y6ZLp3.png)

After you placed a ship it will be displayed and move to the next ship
![BattleShip](https://i.imgur.com/4Zl7aGQ.png)

You have 5 ships for every game : Ship of size 5, ship of size 4, twi ships with the size 3 and one with the size 2.
