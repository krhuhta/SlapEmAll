
class World {
  import scala.util.Random.shuffle
  
  val directions = Vector(0, 1, 2, 3)                                          //North, East, South, West)
  val lineLength = 4
  val BoardSize = lineLength * 2 + 1
  var enemies: Array[Array[Option[Enemy]]] = Array.fill(BoardSize, BoardSize)(None)
  
  private var gameOver = false
  def lose() = gameOver = true
  def unLose() = gameOver = false
  def hasLost = gameOver

  private var time = 0
  private var points = 0
  def pointCount = points
  def addOnePoint = points += 1
  
  def boot() = {
    points = 0
    enemies = Array.fill(BoardSize, BoardSize)(None)
    unLose()
  }

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
  def advanceEnemies() = {                                                     //Moves all enemies closer the the player by 1 step.
      for (j <- lineLength - 1 to 0 by -1) {
        if (enemies(lineLength)(j).isDefined) {
          enemies(lineLength)(j + 1) = enemies(lineLength)(j)
          enemies(lineLength)(j) = None 
        }
      }
      for (i <- lineLength + 1 to lineLength * 2) {
        if (enemies(i)(lineLength).isDefined) {     
          enemies(i - 1)(lineLength) = enemies(i)(lineLength)
          enemies(i)(lineLength) = None 
        }
      }
      for (j <- lineLength + 1 to lineLength * 2) {
        if (enemies(lineLength)(j).isDefined) {           
          enemies(lineLength)(j - 1) = enemies(lineLength)(j)
          enemies(lineLength)(j) = None 
        }
      }
      for (i <- lineLength - 1 to 0 by -1) {
        if (enemies(i)(lineLength).isDefined) {           
          enemies(i + 1)(lineLength) = enemies(i)(lineLength)
          enemies(i)(lineLength) = None 
        }
      }
  }
  def checkIfLost() = {
    if (enemies(lineLength)(lineLength).isDefined) lose()//Checks if an enemy is on top of the player (and loses the game if true)
  }
  
  def addPoints() = {
    points += 10  
  }
  
  def advanceGame() = {                                                          //Advances the game
    advanceEnemies()
    newEnemy()
    addPoints()
    checkIfLost() 
  }

}



class Enemy {                  //Class for the Enemies, currently does nothing more than exits. Made to ease futher development.
  def clear = false
}
  
class Player(world: World) {                 //The player
  var dirOfLastHit = -1
  var timeFromLastHit = 0
  def hit(dir: Int) = {                      // Hits in the direction of param dir. Kills whatever there is.
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
