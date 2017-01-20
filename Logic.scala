
class World {
  import scala.util.Random.shuffle
  
  val directions = Vector(0, 1, 2, 3)                                  //North, East, South, West)
  val lineLength = 4
  val BoardSize = lineLength * 2 + 1
  var enemies: Array[Array[Int]] = Array.fill(BoardSize, BoardSize)(0) //Initial state has no enemies. 0 is an empty square and 1 is an enemy.
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
    enemies = Array.fill(BoardSize, BoardSize)(0)
    unLose()
    unWin()
  }
  /*
   * Creates a new enemy in a random direction.
   */
  def newEnemy() = {
    val rand = shuffle(directions).head
    rand match {
      case 0 => enemies         (lineLength)(0)     = 1 // North
      case 1 => enemies(lineLength * 2)(lineLength) = 1 // East
      case 2 => enemies(lineLength)(lineLength * 2) = 1 // South
      case 3 => enemies (0) (lineLength)            = 1 // West
      case _ => None
    }
  }
  /*
   * Advances all the enemies on the board one step closer to the player.
   * Each line of enemies is handeled seperately from the center of the board outwards.
   */
  def advanceEnemies() = {                                                     
      for (j <- lineLength - 1 to 0 by -1) {                    //North
        if (enemies(lineLength)(j) == 1) {
          enemies(lineLength)(j + 1) = enemies(lineLength)(j)
          enemies(lineLength)(j) = 0 
        }
      }
      for (i <- lineLength + 1 to lineLength * 2) {             //East
        if (enemies(i)(lineLength) == 1) {     
          enemies(i - 1)(lineLength) = enemies(i)(lineLength)
          enemies(i)(lineLength) = 0 
        }
      }
      for (j <- lineLength + 1 to lineLength * 2) {             //South
        if (enemies(lineLength)(j) == 1) {           
          enemies(lineLength)(j - 1) = enemies(lineLength)(j)
          enemies(lineLength)(j) = 0 
        }
      }
      for (i <- lineLength - 1 to 0 by -1) {                    //West
        if (enemies(i)(lineLength) == 1) {           
          enemies(i + 1)(lineLength) = enemies(i)(lineLength)
          enemies(i)(lineLength) = 0 
        }
      }
  }
  /*
   * Method that checks if the player has lost the game
   * Checks if an enemy is on top of the player (and loses the game if true)
   */
  def checkIfLost() = {
    if (enemies(lineLength)(lineLength) == 1) lose()
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

class Player(world: World) {                                         //The player
  
  var dirOfLastHit = -1                                              //Direction where the player will face a for a few frames after each hit.
  var timeFromLastHit = 0                                            //How many frames will the player still face in a given direction.
  /*
   * Hits in the direction of param dir. Kills whatever there is.7
   * When this method is called, it is assumed that it is an approriate time to be called.
   */
  def hit(dir: Int) = {                                         
    dir match {
      case 0 => {
        world.enemies(world.lineLength)(world.lineLength - 1) = 0   // North
        dirOfLastHit = dir
      }
      case 1 => {
        world.enemies(world.lineLength + 1)(world.lineLength) = 0   // East
        dirOfLastHit = dir
      }
      case 2 => {
        world.enemies(world.lineLength)(world.lineLength + 1) = 0   // South
        dirOfLastHit = dir
      }
      case 3 => {
        world.enemies(world.lineLength - 1)(world.lineLength) = 0   // West
        dirOfLastHit = dir
      }
      case _ => None
    }
  }
  
}
