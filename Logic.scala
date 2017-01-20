
class World {
  import scala.util.Random.shuffle
  
  val directions = Vector(0, 1, 2, 3)                                                //North, East, South, West)
  val lineLength = 4
  val BoardSize = lineLength * 2 + 1
  var enemies: Array[Array[Option[Enemy]]] = Array.fill(BoardSize, BoardSize)(None)  //Initial state has no enemies
  /*
   * Methods for keeping track whether the game has been lost or not.
   */
  private var gameLost = false
  def lose() = gameLost = true
  def unLose() = gameLost = false
  def hasLost = gameLost
  /*
   * Methods for keeping track whether the game has been won or not.
   */
  private var gameWon = false
  def win() = gameWon = true
  def unWin() = gameWon = false
  def hasWon = gameWon
  /*
   *  Point counter for the game
   */
  private var points = 0
  def pointCount = points
  /*
   * Boot-method resets the World's state.
   */
  def boot() = {
    points = 0
    enemies = Array.fill(BoardSize, BoardSize)(None)
    unLose()
    unWin()
  }
  /*
   * Creates a new enemy in a random direction.
   */
  def newEnemy() = {
    val rand = shuffle(directions).head
    rand match {
      case 0 => enemies         (lineLength)(0)     = Some(new Enemy) // North
      case 1 => enemies(lineLength * 2)(lineLength) = Some(new Enemy) // East
      case 2 => enemies(lineLength)(lineLength * 2) = Some(new Enemy) // South
      case 3 => enemies (0) (lineLength)            = Some(new Enemy) // West
      case _ => None
    }
  }
  /*
   * Advances all the enemies on the board one step closer to the player.
   * Each line of enemies is handeled seperately from the center of the board outwards.
   */
  def advanceEnemies() = {                                                     
      for (j <- lineLength - 1 to 0 by -1) {                    //North
        if (enemies(lineLength)(j).isDefined) {
          enemies(lineLength)(j + 1) = enemies(lineLength)(j)
          enemies(lineLength)(j) = None 
        }
      }
      for (i <- lineLength + 1 to lineLength * 2) {             //East
        if (enemies(i)(lineLength).isDefined) {     
          enemies(i - 1)(lineLength) = enemies(i)(lineLength)
          enemies(i)(lineLength) = None 
        }
      }
      for (j <- lineLength + 1 to lineLength * 2) {             //South
        if (enemies(lineLength)(j).isDefined) {           
          enemies(lineLength)(j - 1) = enemies(lineLength)(j)
          enemies(lineLength)(j) = None 
        }
      }
      for (i <- lineLength - 1 to 0 by -1) {                    //West
        if (enemies(i)(lineLength).isDefined) {           
          enemies(i + 1)(lineLength) = enemies(i)(lineLength)
          enemies(i)(lineLength) = None 
        }
      }
  }
  /*
   * Method that checks if the player has lost the game
   */
  def checkIfLost() = {
    if (enemies(lineLength)(lineLength).isDefined) lose()//Checks if an enemy is on top of the player (and loses the game if true)
  }
  /*
   * Adds 10 points to the score
   */
  def addPoints() = {
    points += 10  
  }
  /*
   * The method calls all of the 4 above methods to advance the game so that the enemies move one step.
   */
  def advanceGame() = {
    advanceEnemies()
    newEnemy()
    addPoints()
    checkIfLost() 
  }

}



class Enemy {                  //Class for the Enemies, currently does nothing more than exits. Made to ease futher development.
  
}

class Player(world: World) {                                         //The player
  
  var dirOfLastHit = -1                                              //Direction where the player should face a for a few frames after each hit.
  var timeFromLastHit = 0                                            //How many frames will the player still face in a given direction.
  
  def hit(dir: Int) = {                                              // Hits in the direction of param dir. Kills whatever there is.
    dir match {
      case 0 => {
        world.enemies(world.lineLength)(world.lineLength - 1) = None // North
        dirOfLastHit = dir
      }
      case 1 => {
        world.enemies(world.lineLength + 1)(world.lineLength) = None // East
        dirOfLastHit = dir
      }
      case 2 => {
        world.enemies(world.lineLength)(world.lineLength + 1) = None // South
        dirOfLastHit = dir
      }
      case 3 => {
        world.enemies(world.lineLength - 1)(world.lineLength) = None // West
        dirOfLastHit = dir
      }
      case _ => None
    }
  }
  
}
