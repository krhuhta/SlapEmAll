
class World {
  import scala.util.Random.shuffle
  
  val directions = Vector(0,1,2,3)                                          //North, East, South, West)
  val lineLength = 4
  val BoardSize = lineLength * 2 + 1
  val enemies: Array[Array[Option[Enemy]]] = Array.fill(BoardSize, BoardSize)(None)
  
  private var gameOver = false
  def lose() = gameOver = true
  def hasLost = gameOver
  
  def newEnemy() = {
    val rand = shuffle(directions).head
    rand match {
      case 0 => enemies(0)         (lineLength)     = Some(new Enemy) // North
      case 1 => enemies(lineLength)(lineLength * 2) = Some(new Enemy) // East
      case 2 => enemies(lineLength * 2)(lineLength) = Some(new Enemy) // South
      case 3 => enemies(lineLength) (0)             = Some(new Enemy) // West
      case _ => None
    }
  }
  def advanceEnemies() = {                                                     //Moves all enemies closer the the player by 1 step.
      for (i <- 0 until enemies.size ; j <- 0 until enemies.size) {
        if (enemies(i)(j).isDefined) {
          if (i == lineLength) {
            if (j > lineLength) {
              enemies(i)(j - 1) = enemies(i)(j)
            } else enemies(i)(j + 1) = enemies(i)(j)
          } else if (i > lineLength) {
            enemies(i - 1)(j) = enemies(i)(j)
          } else enemies(i + 1)(j) = enemies(i)(j)
          enemies(i)(j) = None 
        }
      }
  }
  def advanceGame = {                                                          //Advances the game
    advanceEnemies()
    newEnemy()
    if (enemies(lineLength)(lineLength).isDefined) lose() //Checks if an enemy is on top of the player (and loses the game if true)
    // turnCounter += 1     Score/time Counter
    // ??? graphics
  }
}



class Enemy {                  //Class for the Enemies, currently does nothing more than exits. Made to ease futher development.
  
}
  
class Player(world: World) {                 //The player
  
  def hit(dir: Int) = {                      // Hits in the direction of param dir. Kills whatever there is.
    dir match {
      case 0 => world.enemies(world.lineLength - 1)(world.lineLength) = None // North
      case 1 => world.enemies(world.lineLength)(world.lineLength + 1) = None // East
      case 2 => world.enemies(world.lineLength + 1)(world.lineLength) = None // South
      case 3 => world.enemies(world.lineLength)(world.lineLength - 1) = None // West
      case _ => None
    }
  }
  
}
