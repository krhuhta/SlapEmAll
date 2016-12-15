
class World {
  import scala.util.Random.shuffle
  
  private val directions = Vector(0,1,2,3)                                          //North, East, South, West)
  private val lineLength = 4                                                        //Number of steps for an enemy to take
  private val lineOfEnemies: Array[Option[Enemy]] = Array.fill(lineLength + 1)(None)//single line of enemies. Index 0 is on top of player (player dies is here), 1 is next to player and lineLenght is the furtherst away. 
  val enemies: Array[Array[Option[Enemy]]] = Array.fill(4)(lineOfEnemies)           //4 lines of enemies. Index 0 is north, 1 is east, 2 is south and 3 is west
    
  def newEnemy() = {
    enemies(shuffle(directions).head)(lineLength) = Option(new Enemy)    //Creates a new enemy to a random line's last index
  }
  def advanceEnemies() = {                                                     //Moves all enemies closer the the player by 1 step.
      for (i <- 0 until 4 ; j <- 0 to lineOfEnemies.length) {
        if (enemies(i)(j).isDefined) {
          enemies(i)(j-1) = enemies(i)(j)
          enemies(i)(j) = None 
        }
      }
  }
  def advanceGame = {                                                          //Advances the game
    advanceEnemies()
    newEnemy()
    if (!enemies.forall(_.head.isEmpty)) ??? //kill the player                 //Checks if an enemy is on top of the player (and therefor the player would lose)
    // turnCounter += 1     Score/time Counter
    // ??? graphics
  }
}



class Enemy {                  //Class for the Enemies, currently does nothing more than exits.
  
}
  
class Player(world: World) {                 //The player
  
  def hit(dir: Int) = {                      //Determins if there is an enemy to kill in the direction of the hit. Kills it.
    if (world.enemies(dir)(1).isDefined) {   
      ??? //Graphics
      world.enemies(dir)(1) = None
    } else ??? //Graphics
  }
  
}

