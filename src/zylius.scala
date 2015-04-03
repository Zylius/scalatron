import scala.collection.mutable.{ HashSet => MutableHashSet }
import scala.collection.mutable.{ HashMap => MutableHashMap }

object ControlFunction
{
  val goodForGoing = List('P', 'M', '_', 'B', '?')
  val goodForHunting = List('B', 'P')

  def forMaster(bot: Bot) {
    var headingChoice = bot.view.offsetToNearestInList(goodForHunting)

    var randomHeadingChoice = '_'
    if(headingChoice.toString == "None") {
      if(math.random < 0.5) randomHeadingChoice = '_' else randomHeadingChoice = '?'
      headingChoice = bot.view.offsetToFurthest(randomHeadingChoice)
    } else if(headingChoice.toString == "None") {
      headingChoice = bot.view.offsetToFurthest(if (randomHeadingChoice == '_') '?' else '_')
    }
    var heading = XY.apply(headingChoice.toString)


    val g = new Graph()
    var id = 0
    var start = new g.Node
    var target = None: Option[g.Node]
    var newNode = new g.Node
    val targetIndex = bot.view.indexFromRelPos(heading)

    for(item <- bot.view.cells) {
      newNode = g.addNode()

      if(goodForGoing.contains(item)) {
        // Item to the left
        if (id % 31 != 0) {
          val leftItem = id - 1
          if(goodForGoing.contains(bot.view.cells(leftItem))) {
            g.nodes(leftItem).connectWith(newNode)
            newNode.connectWith(g.nodes(leftItem))
            //bot.log("[Left] Connecting: " + id + " with " + leftItem)
          }
        }

        // Upper items
        if (id > 30) {
          // Upper.
          val upperItem = id - 31
          if(goodForGoing.contains(bot.view.cells(upperItem))) {
            g.nodes(upperItem).connectWith(newNode)
            newNode.connectWith(g.nodes(upperItem))
            //bot.log("[Upper] Connecting: " + id + " with " + upperItem)
          }

          // Up-right.
          if(id % 31 != 30) {
            val upRightItem = id - 30
            if(goodForGoing.contains(bot.view.cells(upRightItem))) {
              g.nodes(upRightItem).connectWith(newNode)
              newNode.connectWith(g.nodes(upRightItem))
              //bot.log("[Up-right] Connecting: " + id + " with " + upRightItem)
            }
          }

          // Up-left.
          if(id % 31 != 0) {
            val upLeftItem = id - 32
            if(goodForGoing.contains(bot.view.cells(upLeftItem))) {
              g.nodes(upLeftItem).connectWith(newNode)
              newNode.connectWith(g.nodes(upLeftItem))
              //bot.log("[Up-left] Connecting: " + id + " with " + upLeftItem + "\n\n")
            }
          }
        }
      }

      if(id == 480) {
        start = newNode
       // bot.log("Start " + id)
      }
      if(id == targetIndex) {
        target = Some(newNode)
        //bot.log("Target " + id)
      }
      id = id + 1
    }

    if(!target.isEmpty) {
      val tTarget = target.getOrElse(new g.Node)
      val dijkstra = new Dijkstra(g)
      val (_, path) = dijkstra.compute(start, tTarget)
      val shortest = dijkstra.findShortestPathToTarget(start, tTarget, path)
      heading = XY.apply(bot.view.relPosFromIndex(shortest(1).toInt).toString)
      bot.move(bot.inputAsXYOrElse("heading", heading))
      bot.status("S: [" + bot.view.cellAtRelPos(heading) + "] " + heading.toString + ". H: [" + bot.view.cellAtRelPos(XY.apply(headingChoice.toString)) + "] " + headingChoice.toString + ".")
    }
  }

  def forSlave(bot: Bot) {

  }
}

class Graph {
  var nodes: List[Node] = Nil
  var edges: List[Edge] = Nil

  class Node{
    def connectWith(node: Node): Edge = {
      val edge = newEdge(this, node)
      edges = edge :: edges
      edge
    }
    override def toString:String = this.toInt.toString
    def toInt:Int = nodes.indexOf(this)
  }

  class Edge(one: Node, other: Node)  {
    def a = one
    def b = other
    override def toString:String = one.toString + " -> " + other.toString
    def opposite(n: Node): Option[Node] =
      if(n == a) Some(b)
      else if(n == b) Some(a)
      else None
  }

  def newNode(): Node = new Node
  def newEdge(one: Node, other: Node): Edge = new Edge(one, other)
  def addNode(): Node = {
    val node = newNode()
    nodes = nodes :+ node
    node
  }
}

class Dijkstra(graph: Graph) {
  type Node = Graph#Node
  type Edge = Graph#Edge

  def compute(start: Node, target: Node): (MutableHashMap[Node, Int], MutableHashMap[Node, Node]) = {
    var queue: MutableHashSet[Node] = new MutableHashSet[Node]()
    var settled: MutableHashSet[Node] = new MutableHashSet()
    val distance: MutableHashMap[Node, Int] = new MutableHashMap()
    val path: MutableHashMap[Node, Node] = new MutableHashMap()
    queue += start
    distance(start) = 0

    def executeQueue(cond : => Boolean) : Unit = {
      if(cond) {
        val u = extractMinimum(queue, distance)
        settled += u
        relaxNeighbors(u, queue, settled, distance, path)
        executeQueue(cond)
      }
    }

    executeQueue(!queue.isEmpty)

    (distance, path)
  }

  /**
   * Finds element of Q with minimum value in D, removes it
   * from Q and returns it.
   */
  protected def extractMinimum[T](Q: MutableHashSet[T], D: MutableHashMap[T, Int]): T = {
    var u = Q.first
    Q.foreach((node) =>  if(D(u) > D(node)) u = node)
    Q -= u
    u
  }

  /**
   * For all nodes <code>v</code> not in <code>S</code>, neighbors of
   * <code>u</code>}: Updates shortest distances and paths, if shorter than
   * the previous value.
   */
  protected def relaxNeighbors(u: Node, Q: MutableHashSet[Node], S: MutableHashSet[Node],
                               D: MutableHashMap[Node, Int], P: MutableHashMap[Node, Node]): Unit = {
    for(edge <- graph.edges.filter(x => x.a == u || x.b == u)) {
      var v = if(edge.a == u) edge.b else edge.a
      if(!S.contains(v)) {
        if(!D.contains(v) || D(v) > D(u) + 1) {
          D(v) = D(u) + 1
          P(v) = u
          Q += v
        }
      }
    }
  }

  def findShortestPathToTarget(start: Graph#Node, target: Graph#Node, path: MutableHashMap[Graph#Node, Graph#Node]):  List[Graph#Node] = {
    var shortest = List(target)
    def buildShortest(cond : => Boolean) : Unit = {
      if(cond) {
        shortest ::= path(shortest.head)
        buildShortest(cond)
      }
    }
    buildShortest(shortest.head != start)
    shortest
  }
}

// -------------------------------------------------------------------------------------------------
// Framework
// -------------------------------------------------------------------------------------------------

class ControlFunctionFactory {
  def create = (input: String) => {
    val (opcode, params) = CommandParser(input)
    opcode match {
      case "React" =>
        val bot = new BotImpl(params)
        if( bot.generation == 0 ) {
          ControlFunction.forMaster(bot)
        } else {
          ControlFunction.forSlave(bot)
        }
        bot.toString
      case _ => "" // OK
    }
  }
}



trait Bot {
  // inputs
  def inputOrElse(key: String, fallback: String): String
  def inputAsIntOrElse(key: String, fallback: Int): Int
  def inputAsXYOrElse(keyPrefix: String, fallback: XY): XY
  def view: View
  def energy: Int
  def time: Int
  def generation: Int

  // outputs
  def move(delta: XY) : Bot
  def say(text: String) : Bot
  def status(text: String) : Bot
  def spawn(offset: XY, params: (String,Any)*) : Bot
  def set(params: (String,Any)*) : Bot
  def log(text: String) : Bot
}

trait MiniBot extends Bot {
  // inputs
  def offsetToMaster: XY

  // outputs
  def explode(blastRadius: Int) : Bot
}


case class BotImpl(inputParams: scala.collection.immutable.Map[String, String]) extends MiniBot {
  // input
  def inputOrElse(key: String, fallback: String) = inputParams.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = inputParams.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsXYOrElse(key: String, fallback: XY) = inputParams.get(key).map(s => XY(s)).getOrElse(fallback)

  val view = View(inputParams("view"))
  val energy = inputParams("energy").toInt
  val time = inputParams("time").toInt
  val generation = inputParams("generation").toInt
  def offsetToMaster = inputAsXYOrElse("master", XY.Zero)


  // output

  private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
  private var commands = ""                           // holds all other commands
  private var debugOutput = ""                        // holds all "Log()" output

  /** Appends a new command to the command string; returns 'this' for fluent API. */
  private def append(s: String) : Bot = { commands += (if(commands.isEmpty) s else "|" + s); this }

  /** Renders commands and stateParams into a control function return string. */
  override def toString = {
    var result = commands
    if(!stateParams.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    if(!debugOutput.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = { debugOutput += text + "\n"; this }
  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")
  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
  def set(params: (String,Any)*) = { stateParams ++= params; this }
  def set(keyPrefix: String, xy: XY) = { stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y); this }
}


// -------------------------------------------------------------------------------------------------


/** Utility methods for parsing strings containing a single command of the format
  * "Command(key=value,key=value,...)"
  */
object CommandParser {
  /** "Command(..)" => ("Command", Map( ("key" -> "value"), ("key" -> "value"), ..}) */
  def apply(command: String): (String, scala.collection.immutable.Map[String, String]) = {
    /** "key=value" => ("key","value") */
    def splitParameterIntoKeyValue(param: String): (String, String) = {
      val segments = param.split('=')
      (segments(0), if(segments.length>=2) segments(1) else "")
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)
    val opcode = segments(0)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map(splitParameterIntoKeyValue).toMap
    (opcode, keyValuePairs)
  }
}


// -------------------------------------------------------------------------------------------------


/** Utility class for managing 2D cell coordinates.
  * The coordinate (0,0) corresponds to the top-left corner of the arena on screen.
  * The direction (1,-1) points right and up.
  */
case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y

  def isNonZero = x != 0 || y != 0
  def isZero = x == 0 && y == 0
  def isNonNegative = x >= 0 && y >= 0

  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)

  def addToX(dx: Int) = XY(x + dx, y)
  def addToY(dy: Int) = XY(x, y + dy)

  def +(pos: XY) = XY(x + pos.x, y + pos.y)
  def -(pos: XY) = XY(x - pos.x, y - pos.y)
  def *(factor: Double) = XY((x * factor).intValue, (y * factor).intValue)

  def distanceTo(pos: XY): Double = (this - pos).length // Phythagorean
  def length: Double = math.sqrt(x * x + y * y) // Phythagorean

  def stepsTo(pos: XY): Int = (this - pos).stepCount // steps to reach pos: max delta X or Y
  def stepCount: Int = x.abs.max(y.abs) // steps from (0,0) to get here: max X or Y

  def signum = XY(x.signum, y.signum)

  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)

  /** Returns the direction index with 'Right' being index 0, then clockwise in 45 degree steps. */
  def toDirection45: Int = {
    val unit = signum
    unit.x match {
      case -1 =>
        unit.y match {
          case -1 =>
            if(x < y * 3) Direction45.Left
            else if(y < x * 3) Direction45.Up
            else Direction45.UpLeft
          case 0 =>
            Direction45.Left
          case 1 =>
            if(-x > y * 3) Direction45.Left
            else if(y > -x * 3) Direction45.Down
            else Direction45.LeftDown
        }
      case 0 =>
        unit.y match {
          case 1 => Direction45.Down
          case 0 => throw new IllegalArgumentException("cannot compute direction index for (0,0)")
          case -1 => Direction45.Up
        }
      case 1 =>
        unit.y match {
          case -1 =>
            if(x > -y * 3) Direction45.Right
            else if(-y > x * 3) Direction45.Up
            else Direction45.RightUp
          case 0 =>
            Direction45.Right
          case 1 =>
            if(x > y * 3) Direction45.Right
            else if(y > x * 3) Direction45.Down
            else Direction45.DownRight
        }
    }
  }

  def rotateCounterClockwise45 = XY.fromDirection45((signum.toDirection45 + 1) % 8)
  def rotateCounterClockwise90 = XY.fromDirection45((signum.toDirection45 + 2) % 8)
  def rotateClockwise45 = XY.fromDirection45((signum.toDirection45 + 7) % 8)
  def rotateClockwise90 = XY.fromDirection45((signum.toDirection45 + 6) % 8)


  def wrap(boardSize: XY) = {
    val fixedX = if(x < 0) boardSize.x + x else if(x >= boardSize.x) x - boardSize.x else x
    val fixedY = if(y < 0) boardSize.y + y else if(y >= boardSize.y) y - boardSize.y else y
    if(fixedX != x || fixedY != y) XY(fixedX, fixedY) else this
  }
}


object XY {
  /** Parse an XY value from XY.toString format, e.g. "2:3". */
  def apply(s: String) : XY = { val a = s.split(':'); XY(a(0).toInt,a(1).toInt) }

  val Zero = XY(0, 0)
  val One = XY(1, 1)

  val Right     = XY( 1,  0)
  val RightUp   = XY( 1, -1)
  val Up        = XY( 0, -1)
  val UpLeft    = XY(-1, -1)
  val Left      = XY(-1,  0)
  val LeftDown  = XY(-1,  1)
  val Down      = XY( 0,  1)
  val DownRight = XY( 1,  1)

  def fromDirection45(index: Int): XY = index match {
    case Direction45.Right => Right
    case Direction45.RightUp => RightUp
    case Direction45.Up => Up
    case Direction45.UpLeft => UpLeft
    case Direction45.Left => Left
    case Direction45.LeftDown => LeftDown
    case Direction45.Down => Down
    case Direction45.DownRight => DownRight
  }

  def fromDirection90(index: Int): XY = index match {
    case Direction90.Right => Right
    case Direction90.Up => Up
    case Direction90.Left => Left
    case Direction90.Down => Down
  }

  def apply(array: Array[Int]): XY = XY(array(0), array(1))
}


object Direction45 {
  val Right = 0
  val RightUp = 1
  val Up = 2
  val UpLeft = 3
  val Left = 4
  val LeftDown = 5
  val Down = 6
  val DownRight = 7
}


object Direction90 {
  val Right = 0
  val Up = 1
  val Left = 2
  val Down = 3
}


// -------------------------------------------------------------------------------------------------


case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size / 2, size / 2)

  def apply(relPos: XY) = cellAtRelPos(relPos)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size)
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))

  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def offsetToNearest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if( matchingXY.isEmpty )
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      nearest
    }
  }

  def offsetToNearestInList(chars: List[Char]) = {
    val matchingXY = cells.view.zipWithIndex.filter(x => chars.contains(x._1))
    if( matchingXY.isEmpty )
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).minBy(_.length)
      nearest
    }
  }

  def offsetToFurthest(c: Char) = {
    val matchingXY = cells.view.zipWithIndex.filter(_._1 == c)
    if( matchingXY.isEmpty )
      None
    else {
      val nearest = matchingXY.map(p => relPosFromIndex(p._2)).maxBy(_.length)
      nearest
    }
  }
}

