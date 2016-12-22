
class World {
  import scala.util.Random.shuffle
  
  val directions = Vector(0, 1, 2, 3)                                          //North, East, South, West)
  val lineLength = 4
  val BoardSize = lineLength * 2 + 1
//  private val lineOfEnemies: Array[Option[Enemy]] = Array.fill(lineLength + 1)(None)//single line of enemies. Index 0 is on top of player (player dies is here), 1 is next to player and lineLenght is the furtherst away. 
//  val enemies: Array[Array[Option[Enemy]]] = Array.fill(4)(lineOfEnemies)  //4 lines of enemies. Index 0 is north, 1 is east, 2 is south and 3 is west
  
  //def tableOfEnemies = enemies.map(_.map(_.))
  val enemies: Array[Array[Option[Enemy]]] = Array.fill(BoardSize, BoardSize)(None)
  
  private var gameOver = false
  def lose() = gameOver = true
  def hasLost = gameOver
  private var time = 0
  private var points = 0
  def pointCount = points
  
//   def newEnemy() = {
//     enemies(shuffle(directions).head)(lineLength) = Option(new Enemy)    //Creates a new enemy to a random line's last index
//   }
//   def advanceEnemies() = {                                                     //Moves all enemies closer the the player by 1 step.
//       for (i <- 0 until 4 ; j <- 0 to lineOfEnemies.length) {
//         if (enemies(i)(j).isDefined) {
//           enemies(i)(j-1) = enemies(i)(j)
//           enemies(i)(j) = None 
//         }
//       }
//   }
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
  def advanceGame() = {                                                          //Advances the game
    advanceEnemies()
    newEnemy()
    // if (!enemies.forall(_.head.isEmpty)) lose()
    if (enemies(lineLength)(lineLength).isDefined) lose() //Checks if an enemy is on top of the player (and loses the game if true)
    points += 10    
    // ??? graphics
  }
  
  def advanceGameDoubles() = {                                                          //Advances the game
    advanceEnemies()
    newEnemy()
    if (time > 10 && time % 10 == 0) newEnemy()
    // if (!enemies.forall(_.head.isEmpty)) lose()
    if (enemies(lineLength)(lineLength).isDefined) lose() //Checks if an enemy is on top of the player (and loses the game if true)
    points += 12
    // ??? graphics
    time += 1
  }
}



class Enemy {                  //Class for the Enemies, currently does nothing more than exits. Made to ease futher development.
  
}
  
class Player(world: World) {                 //The player
  
//  def hit(dir: Int) = {                      //Determins if there is an enemy to kill in the direction of the hit. Kills it.
//     if (world.enemies(dir)(1).isDefined) {   
//       ??? //Graphics
//       world.enemies(dir)(1) = None
//     } else ??? //Graphics
//   }
  def hit(dir: Int) = {                      // Hits in the direction of param dir. Kills whatever there is.
    dir match {
      case 0 => world.enemies(world.lineLength)(world.lineLength - 1) = None // North
      case 1 => world.enemies(world.lineLength + 1)(world.lineLength) = None // East
      case 2 => world.enemies(world.lineLength)(world.lineLength + 1) = None // South
      case 3 => world.enemies(world.lineLength - 1)(world.lineLength) = None // West
      case _ => None
    }
  }
  
}
