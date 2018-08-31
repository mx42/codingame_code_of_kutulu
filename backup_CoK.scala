import cok.domain.CoKAction
import cok.io.CoKIO
import cok.strategy._
import com.truelaurel.codingame.logging.CGLogger

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info
    var turn = 0
    var context = CoKIO.readContext
    try {
      while (true) {
        val state = CoKIO.readState(turn, context)
//        CGLogger.info(context)
//        CGLogger.info(state)
        val action: CoKAction = Strategy3(context).react(state)
//        val action: CoKAction = Strategy2(context).react(state)
//        val action: CoKAction = TestStrategy(context).react(state)
        context = context.copy(previousAction = Some(action),
                               previousState = Some(state))
        CoKIO.writeAction(state, action)
        turn += 1
      }
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}
package com.truelaurel.math {

  import scala.util.Random

  object Mathl {
    val random = new Random(62638886242411L)
    def halfUp(d: Double): Int =
      ((d.abs * 2 + 1) / 2).toInt * (if (d > 0) 1 else -1)
    def almostEqual(d1: Double, d2: Double): Boolean =
      Math.abs(d1 - d2) <= 0.000001
    def randomBetween(min: Double, max: Double): Double = {
      min + random.nextDouble() * (max - min)
    }
    @inline
    def sqr(x: Int) = x * x
  }
}
package cok.io {

  import cok.domain._
  import cok.domain.Constants._
  import com.truelaurel.codingame.challenge.GameIO
  import com.truelaurel.codingame.logging.CGLogger
  import com.truelaurel.math.geometry.Pos
  object CoKIO extends GameIO[CoKContext, CoKState, CoKAction] {

    override def readContext: CoKContext = {
      val width = readInt
      val rows: Seq[String] = Seq.fill(readInt)(readLine)
      val map = for ((row: String, y: Int) ← rows.zipWithIndex;
                     (cell: Char, x: Int) ← row.zipWithIndex)
        yield
          Pos(x, y) → {
            cell match {
              case '#' ⇒ MAP_WALL
              case 'w' ⇒ MAP_PORTAL
              case 'u' ⇒ MAP_SHELTER
              case _ ⇒ MAP_EMPTY
            }
          }
      val Array(sanityLossLonely, sanityLossGroup, spawnTime, wanderTime) =
        readLine.split(" ")
      CoKContext(
        Pos(width, rows.length),
        map.filter(_._2 == MAP_EMPTY).map(_._1).toSet,
        map.filter(_._2 == MAP_WALL).map(_._1).toSet,
        map.filter(_._2 == MAP_PORTAL).map(_._1).toSet,
        map.filter(_._2 == MAP_SHELTER).map(_._1).toSet,
        sanityLossLonely.toInt,
        sanityLossGroup.toInt,
        spawnTime.toInt,
        wanderTime.toInt
      )
    }

    override def readState(turn: Int, context: CoKContext): CoKState = {
      val entities: Seq[Entity] = Seq.fill(readInt) {
        val Array(entityType, entityId, x, y, param0, param1, param2) = readLine split " "
        entityType match {
          case "EXPLORER" =>
            ExplorerEntity(entityId.toInt,
                           Pos(x.toInt, y.toInt),
                           param0.toInt,
                           param1.toInt,
                           param2.toInt)
          case "WANDERER" =>
            MinionEntity(WANDERER,
                         entityId.toInt,
                         Pos(x.toInt, y.toInt),
                         param0.toInt,
                         param1.toInt,
                         param2.toInt)
          case "SLASHER" ⇒
            MinionEntity(SLASHER,
                         entityId.toInt,
                         Pos(x.toInt, y.toInt),
                         param0.toInt,
                         param1.toInt,
                         param2.toInt)
          case "EFFECT_PLAN" ⇒
            EffectPlanEntity(Pos(x.toInt, y.toInt), param0.toInt, param1.toInt)
          case "EFFECT_LIGHT" ⇒
            EffectLightEntity(Pos(x.toInt, y.toInt), param0.toInt, param1.toInt)
          case "EFFECT_YELL" ⇒
            EffectYellEntity(Pos(x.toInt, y.toInt),
                             param0.toInt,
                             param1.toInt,
                             param2.toInt)
          case "EFFECT_SHELTER" ⇒
            EffectShelterEntity(Pos(x.toInt, y.toInt), param0.toInt)
        }
      }
      NormalTurnState(
        entities
          .filter(_.isInstanceOf[ExplorerEntity])
          .asInstanceOf[Seq[ExplorerEntity]],
        entities
          .filter(_.isInstanceOf[MinionEntity])
          .asInstanceOf[Seq[MinionEntity]],
        entities
          .filter(_.isInstanceOf[EffectEntity])
          .asInstanceOf[Seq[EffectEntity]]
      )
    }

    def write(command: String, comment: Option[String]): Unit = {
      if (comment.isDefined) {
        println(s"$command ${comment.get}")
      } else println(command)
    }
    override def writeAction(state: CoKState, action: CoKAction): Unit = {
      action match {
        case WaitAction(comment) => write("WAIT", comment)
        case MoveAction(pos, comment) =>
          write(s"MOVE ${pos.x} ${pos.y}", comment)
        case LightAction(comment) ⇒ write("LIGHT", comment)
        case PlanAction(comment) ⇒ write("PLAN", comment)
        case YellAction(comment) ⇒ write("YELL", comment)
      }
    }
  }
}
package com.truelaurel.algorithm.game {

  import scala.annotation.tailrec
  import scala.util.Random
  trait GameRules[P, S <: GameState[P], M] {
    def initial: S
    def validMoves(state: S): Seq[M]
    def applyMove(state: S, move: M): S
    def outcome(state: S): Outcome[P]
    @tailrec
    final def judge(players: Map[P, S => M],
                    debug: S => Unit,
                    state: S = initial): Outcome[P] = {
      debug(state)
      outcome(state) match {
        case Undecided =>
          val p = players(state.nextPlayer)
          val m = p(state)
          judge(players, debug, applyMove(state, m))
        case o => o
      }
    }
    def randomMove(s: S): M = {
      val moves = validMoves(s)
      require(moves.nonEmpty, "no valid moves in state " + s)
      moves(Random.nextInt(moves.size))
    }
    def randomPlay(state: S): Outcome[P] =
      playUntilEnd(randomMove)(state)
    def playUntilEnd(selectMove: S => M)(state: S): Outcome[P] = {
      @tailrec
      def playRec(s: S): Outcome[P] = {
        outcome(s) match {
          case Undecided => playRec(applyMove(s, selectMove(s)))
          case decided   => decided
        }
      }
      playRec(state)
    }
  }
  trait GameState[P] {
    def nextPlayer: P
  }
  sealed trait Outcome[+P]
  case class Wins[P](p: P) extends Outcome[P]
  case object Undecided extends Outcome[Nothing]
  case object Draw extends Outcome[Nothing]
  trait RulesFor2p[S <: GameState[Boolean], M]
      extends GameRules[Boolean, S, M] {
    def judge(truePl: S => M,
              falsePl: S => M,
              debug: S => Unit): Outcome[Boolean] =
      judge(Map(true -> truePl, false -> falsePl), debug, initial)
  }
}
package com.truelaurel.codingame.challenge {

  trait GameAccumulator[Context, State, Action] {

    def accumulate(context: Context, state: State, action: Action): Context
  }

  trait GameBot[State, Action] {

    def react(state: State): Action
  }

  trait GameIO[Context, State, Action] {

    def readContext: Context

    def readState(turn: Int, context: Context): State

    def writeAction(state: State, action: Action)
  }
  import com.truelaurel.codingame.logging.CGLogger
  class GameLoop[Context, State, Action](
      gameIO: GameIO[Context, State, Action],
      myPlayer: GameBot[State, Action],
      accumulator: GameAccumulator[Context, State, Action],
      turns: Int = 200
  ) {
    def run(): Unit = {
      val time = System.nanoTime()
      val initContext = gameIO.readContext
      CGLogger.info(
        "GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
      (1 to turns).foldLeft(initContext) {
        case (c, turn) =>
          val state = gameIO.readState(turn, c)
          CGLogger.info(state)
          val time = System.nanoTime()
          val actions = myPlayer.react(state)
          CGLogger.info(
            "GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
          gameIO.writeAction(state, actions)
          accumulator.accumulate(c, state, actions)
      }
    }
  }
  trait GameSimulator[State, Action] {

    def simulate(state: State, action: Action): State
  }
  import scala.collection.mutable.ArrayBuffer
  object Undoer {
    def of(undoers: ArrayBuffer[() => Unit]): () => Unit = { () =>
      {
        var i = 0
        while (i < undoers.length) {
          undoers(i)()
          i += 1
        }
      }
    }
  }
}
package com.truelaurel.math.geometry {

  case class Pos(x: Int, y: Int) {
    def neighbours4: Seq[Pos] =
      Seq(Pos(x + 1, y), Pos(x - 1, y), Pos(x, y - 1), Pos(x, y + 1))
    def +(pos: Pos): Pos = Pos(x + pos.x, y + pos.y)
    def neighborIn(direction: Direction): Pos = direction match {
      case N  => Pos(x, y - 1)
      case S  => Pos(x, y + 1)
      case W  => Pos(x - 1, y)
      case E  => Pos(x + 1, y)
      case NE => Pos(x + 1, y - 1)
      case SE => Pos(x + 1, y + 1)
      case NW => Pos(x - 1, y - 1)
      case SW => Pos(x - 1, y + 1)
    }
    def neighborsIn(direction: Direction): Iterator[Pos] = direction match {
      case N ⇒ (y - 1).to(0).iterator.map(y2 ⇒ Pos(x, y2))
      case S ⇒ (y + 1).to(500).iterator.map(y2 ⇒ Pos(x, y2))
      case W ⇒ (x - 1).to(0).iterator.map(x2 ⇒ Pos(x2, y))
      case E ⇒ (x + 1).to(500).iterator.map(x2 ⇒ Pos(x2, y))
    }
    def distance(pos: Pos): Int =
      Math.max(Math.abs(x - pos.x), Math.abs(y - pos.y))
    def distanceManhattan(pos: Pos): Int =
      Math.abs(x - pos.x) + Math.abs(y - pos.y)
    def distanceEuclide(pos: Pos): Double =
      Math.sqrt(Math.pow(x - pos.x, 2) + Math.pow(y - pos.y, 2))
  }
  object Pos {
    val right: (Int, Int) = (1, 0)
    val down: (Int, Int) = (0, 1)
    val downRight: (Int, Int) = (1, 1)
    val downLeft: (Int, Int) = (-1, 1)
    val all = Seq(right, down, downRight, downLeft)
  }
  sealed trait Direction {
    def similar: Array[Direction]
  }
  case object N extends Direction {
    val similar: Array[Direction] = Array(NE, N, NW)
  }
  case object W extends Direction {
    val similar: Array[Direction] = Array(NW, W, SW)
  }
  case object S extends Direction {
    val similar: Array[Direction] = Array(SE, S, SW)
  }
  case object E extends Direction {
    val similar: Array[Direction] = Array(NE, SE, E)
  }
  case object NW extends Direction {
    val similar: Array[Direction] = Array(N, W, NW)
  }
  case object NE extends Direction {
    val similar: Array[Direction] = Array(NE, N, E)
  }
  case object SW extends Direction {
    val similar: Array[Direction] = Array(S, W, SW)
  }
  case object SE extends Direction {
    val similar: Array[Direction] = Array(SE, E, S)
  }
  object Direction {

    def neighborsOf(pos: Pos, size: Int): Seq[Pos] = {
      Direction.all
        .map(d => pos.neighborIn(d))
        .filter(p => p.x < size && p.x >= 0 && p.y < size && p.y >= 0)
    }
    val all = Seq(N, W, S, E, SW, SE, NW, NE)
    val cardinals = Seq(N, W, S, E)
    def apply(dir: String): Direction = dir match {
      case "N"  => N
      case "S"  => S
      case "W"  => W
      case "E"  => E
      case "NE" => NE
      case "SE" => SE
      case "NW" => NW
      case "SW" => SW
      case _    => throw new IllegalArgumentException("unknown direction " + dir)
    }
  }
  import com.truelaurel.math.Mathl
  object Vectorls {
    val origin = Vectorl(0, 0)
    val axisX = Vectorl(1, 0)
    val axisY = Vectorl(0, 1)
  }
  case class Vectorl(x: Double, y: Double) {
    lazy val mag2: Double = x * x + y * y
    lazy val mag: Double = Math.sqrt(mag2)
    lazy val norm: Vectorl = if (mag > 0) this * (1.0 / mag) else Vectorl(0, 0)
    def /(factor: Double): Vectorl = this * (1.0 / factor)
    def +(that: Vectorl): Vectorl = Vectorl(x + that.x, y + that.y)
    def -(that: Vectorl): Vectorl = Vectorl(x - that.x, y - that.y)
    def pivotTo(desired: Vectorl, maxDegree: Double): Vectorl = {
      if (mag2 == 0 || angleInDegreeBetween(desired) <= maxDegree) {
        desired.norm
      } else {
        if (this.perDotProduct(desired) > 0) {
          rotateInDegree(maxDegree).norm
        } else {
          rotateInDegree(-maxDegree).norm
        }
      }
    }
    def rotateInDegree(degree: Double): Vectorl =
      rotateInRadian(Math.toRadians(degree))
    def rotateInRadian(radians: Double): Vectorl = {
      val rotated = angleInRadian + radians
      Vectorl(Math.cos(rotated), Math.sin(rotated)) * mag
    }
    def *(factor: Double): Vectorl = Vectorl(x * factor, y * factor)
    private def angleInRadian: Double = Math.atan2(y, x)
    def angleInDegreeBetween(other: Vectorl): Double = {
      Math.toDegrees(angleInRadianBetween(other))
    }
    def angleInRadianBetween(other: Vectorl): Double = {
      val result = this.dotProduct(other) / (this.mag * other.mag)
      if (result >= 1.0) 0 else Math.acos(result)
    }
    def perDotProduct(that: Vectorl): Double = perp.dotProduct(that)
    def dotProduct(that: Vectorl): Double = x * that.x + y * that.y
    def perp = Vectorl(-y, x)
    def between(v1: Vectorl, v2: Vectorl): Boolean = {
      this.perDotProduct(v1) * this.perDotProduct(v2) < 0
    }
    def truncate = Vectorl(x.toInt, y.toInt)
    def round = Vectorl(Mathl.halfUp(x), Mathl.halfUp(y))
    override def equals(o: Any): Boolean = o match {
      case that: Vectorl =>
        Mathl.almostEqual(x, that.x) && Mathl.almostEqual(y, that.y)
      case _ => false
    }
    private def angleInDegree: Double = Math.toDegrees(angleInRadian)
  }
}
package cok.domain {

  import com.truelaurel.math.geometry.Pos
  sealed trait CoKAction
  case class WaitAction(comment: Option[String] = None) extends CoKAction
  sealed case class MoveAction(move: Pos, comment: Option[String] = None)
      extends CoKAction
  sealed case class LightAction(comment: Option[String] = None)
      extends CoKAction
  sealed case class PlanAction(comment: Option[String] = None) extends CoKAction
  sealed case class YellAction(comment: Option[String] = None) extends CoKAction
  import com.truelaurel.math.geometry.Pos
  case class CoKContext(mapDim: Pos,
                        empty: Set[Pos],
                        walls: Set[Pos],
                        portals: Set[Pos],
                        shelters: Set[Pos],
                        sanityLossSolo: Int,
                        sanityLossGroup: Int,
                        spawnTime: Int,
                        wanderTime: Int,
                        previousState: Option[CoKState] = None,
                        previousAction: Option[CoKAction] = None) {
    def walkableAt(pos: Pos): Boolean = !walls.contains(pos)
// Precompute, for each walkable tile:
// > Distance to nearest 3-path tile
// > Distance to nearest 4-path tile (if any)
    val exitNumbers: Map[Pos, Int] = (empty ++ portals ++ shelters)
      .map(p ⇒ (p, p.neighbours4.count(walkableAt)))
      .toMap
    val distance4: Map[Pos, Int] = exitNumbers
      .filter(_._2 == 4)
      .flatMap(x ⇒ setDistances(x._1))
      .groupBy(_._1)
      .map { case (pos, maps) ⇒ (pos, maps.values.min) }
    val distance3: Map[Pos, Int] = exitNumbers
      .filter(_._2 == 3)
      .flatMap(x ⇒ setDistances(x._1))
      .groupBy(_._1)
      .map { case (pos, maps) ⇒ (pos, maps.values.min) }
// recursive ...
    def setDistances(p: Pos, dist: Int = 0): Map[Pos, Int] = {
      p.neighbours4.map(p2 ⇒ (p2, dist + 1)).toMap
    }
  }
  import com.truelaurel.algorithm.game.GameState
  import com.truelaurel.math.geometry.Pos
  sealed trait CoKState
  case class NormalTurnState(
      explorers: Seq[ExplorerEntity],
      enemies: Seq[MinionEntity],
      effects: Seq[EffectEntity],
      nextPlayer: Boolean = true // ?
  ) extends GameState[Boolean]
      with CoKState {
    lazy val explorersPos: Map[Pos, Int] = explorers.map(_.pos → 1).toMap
    lazy val minionsPos: Map[Pos, Int] = enemies.map(_.pos → 2).toMap
    def safeAt(pos: Pos): Boolean = minionsPos.getOrElse(pos, 0) == 0
    def emptyAt(pos: Pos): Boolean =
      explorersPos.getOrElse(pos, minionsPos.getOrElse(pos, 0)) == 0
  }
//
//case class LightweightState(
//    nextPlayer: Boolean = true
//) extends GameState[Boolean]
//    with CoKState {}
  object Constants {
    val SPAWNING_STATE = 0
    val WANDERING_STATE = 1
    val STALKING_STATE = 2
    val RUSHING_STATE = 3
    val STUNNED_STATE = 4
    val MAP_EMPTY = 0
    val MAP_WALL = 1
    val MAP_PORTAL = 2
    val MAP_SHELTER = 3
    val WANDERER = 1
    val SLASHER = 2
  }
  import com.truelaurel.math.geometry.Pos
  sealed trait Entity
  sealed case class ExplorerEntity(
      entityId: Int,
      pos: Pos,
      health: Int,
      plansLeft: Int,
      lightsLeft: Int
  ) extends Entity
  sealed case class MinionEntity(
      entityType: Int,
      entityId: Int,
      pos: Pos,
      time: Int,
      state: Int,
      target: Int
  ) extends Entity
  sealed trait EffectEntity extends Entity
  sealed case class EffectPlanEntity(
      pos: Pos,
      timeLeft: Int,
      originEntityId: Int
  ) extends EffectEntity
  sealed case class EffectLightEntity(
      pos: Pos,
      timeLeft: Int,
      originEntityId: Int
  ) extends EffectEntity
  sealed case class EffectShelterEntity(
      pos: Pos,
      energyLeft: Int
  ) extends EffectEntity
  sealed case class EffectYellEntity(
      pos: Pos,
      timeLeft: Int,
      originEntityId: Int,
      affectedEntityId: Int
  ) extends EffectEntity
}
package com.truelaurel.codingame.logging {

  object CGLogger {
    val info = 0
    val debug = 1
    var current = info
    def debug(message: Any): Unit = log(message, debug)
    def info(message: Any): Unit = log(message, info)
    private def log(message: Any, level: Int): Unit = {
      if (level <= current) {
        System.err.println(message)
      }
    }
  }
}
package cok.strategy {

  import cok.domain.Constants._
  import cok.domain._
  import com.truelaurel.codingame.logging.CGLogger
  import com.truelaurel.math.geometry.{Direction, N, Pos}
  case class Strategy2(context: CoKContext) {
    type Score = Double
    val MAX_DIST_COMPUTE = 10
    val WANDERER_MALUS_MAX: Score = -20.0
    val SLASHER_MALUS: Score = -18.0
    val ALLIED_BONUS_MAX: Score = +10.0
    val LIGHT_BONUS_MAX: Score = +8.0
    val PLAN_BONUS_MAX: Score = +8.0
    val SHELTER_BONUS_MAX: Score = +8.0
    val NOT_FOUND: Score = -999.0
    val MAX_HEALTH_FOR_PLAN: Int = 200
    val SHELTER_MIN_ENERGY_LEFT: Int = 1
    val EXIT_BONUS: Score = +3.0
    def maxDistanceToMe(me: Pos, dist: Int)(other: Pos): Boolean =
      other.distanceManhattan(me) < dist
    def irradiate(add: Score, left: Int, subfactor: Double = 2.0)(
        pos: Seq[Pos],
        prev: Seq[Pos] = Seq()): Seq[(Pos, Score)] =
      left match {
        case 0 ⇒
          pos.map(p ⇒ p → add)
        case _ ⇒
          pos.map(p ⇒ p → add) ++
            irradiate(add - (add / (left * subfactor)), left - 1)(
              pos
                .flatMap(_.neighbours4)
                .filter(context.walkableAt)
                .filterNot(pos.contains)
                .filterNot(prev.contains),
              pos)
      }
    def getWanderersMaluses(wanderers: Seq[Pos]): Seq[(Pos, Score)] =
      wanderers.flatMap(p ⇒
        irradiate(WANDERER_MALUS_MAX, MAX_DIST_COMPUTE)(Seq(p)))
    def getSlashersMaluses(slashers: Seq[Pos]): Seq[(Pos, Score)] =
      slashers
        .flatMap { p ⇒
          Seq(p) ++ Direction.cardinals
            .flatMap { d ⇒
              p.neighborsIn(d)
            }
            .takeWhile { p ⇒
              context.walkableAt(p)
            }
        }
        .map { p ⇒
          p → SLASHER_MALUS
        }
    def getAlliesBonuses(allies: Seq[Pos]): Seq[(Pos, Score)] = {
      allies.flatMap(p ⇒ irradiate(ALLIED_BONUS_MAX, MAX_DIST_COMPUTE)(Seq(p)))
    }
    def getPlansBonuses(plans: Seq[Pos]): Seq[(Pos, Score)] = {
      plans.flatMap(p ⇒ irradiate(PLAN_BONUS_MAX, 3)(Seq(p)))
    }
    def getLightsBonuses(lights: Seq[Pos]): Seq[(Pos, Score)] = {
      lights.flatMap(p ⇒ irradiate(LIGHT_BONUS_MAX, 3)(Seq(p)))
    }
    def getSheltersBonuses(shelters: Seq[Pos]): Seq[(Pos, Score)] = {
      shelters.flatMap(p ⇒
        irradiate(SHELTER_BONUS_MAX, MAX_DIST_COMPUTE)(Seq(p)))
    }
    def getExitBonuses(pos: Seq[Pos]): Seq[(Pos, Score)] =
      pos.map(p ⇒ (p, context.exitNumbers(p) * EXIT_BONUS))
    def pickAction(s: NormalTurnState): CoKAction = {
      val me :: explorers = s.explorers
      val scores =
        (getWanderersMaluses(
          s.enemies
            .filter(_.entityType == WANDERER)
            .map(_.pos)
            .filter(maxDistanceToMe(me.pos, MAX_DIST_COMPUTE)))
          .map {
            case (k, v) ⇒
              if (k == me.pos) {
                CGLogger.info(s"DBG: Wanderer => Score $v")
              }
              (k, v)
          } ++
          getSlashersMaluses(
            s.enemies
              .filter(_.entityType == SLASHER)
              .map(_.pos)).map {
            case (k, v) ⇒
              if (k == me.pos) {
                CGLogger.info(s"DBG: Slasher => Score $v")
              }
              (k, v)
          } ++
          getAlliesBonuses(
            explorers
              .map(_.pos)
              .filter(maxDistanceToMe(me.pos, MAX_DIST_COMPUTE))).map {
            case (k, v) ⇒
              if (k == me.pos) {
                CGLogger.info(s"DBG: Ally => Score $v")
              }
              (k, v)
          } ++
          getPlansBonuses(
            s.effects
              .filter(_.isInstanceOf[EffectPlanEntity])
              .map(_.asInstanceOf[EffectPlanEntity].pos)
              .filter(maxDistanceToMe(me.pos, 3))).map {
            case (k, v) ⇒
              if (k == me.pos) {
                CGLogger.info(s"DBG: Plan => Score $v")
              }
              (k, v)
          } ++
          getLightsBonuses(
            s.effects
              .filter(_.isInstanceOf[EffectLightEntity])
              .map(_.asInstanceOf[EffectLightEntity].pos)
              .filter(maxDistanceToMe(me.pos, 3))).map {
            case (k, v) ⇒
              if (k == me.pos) {
                CGLogger.info(s"DBG: Light => Score $v")
              }
              (k, v)
          } ++
          getSheltersBonuses(
            s.effects
              .filter(_.isInstanceOf[EffectShelterEntity])
              .filter(_.asInstanceOf[EffectShelterEntity].energyLeft > SHELTER_MIN_ENERGY_LEFT)
              .map(_.asInstanceOf[EffectShelterEntity].pos)
              .filter(maxDistanceToMe(me.pos, 5))).map {
            case (k, v) ⇒
              if (k == me.pos) {
                CGLogger.info(s"DBG: Shelter => Score $v")
              }
              (k, v)
          } ++
          getExitBonuses(me.pos.neighbours4.filter(context.walkableAt)))
          .groupBy {
            _._1
          }
          .map { case (k, v) ⇒ (k, v.map(_._2).sum) }
      CGLogger.info(
        s"Current ${me.pos} => Score ${scores.getOrElse(me.pos, NOT_FOUND)}")
      me.pos.neighbours4
        .filter(context.walkableAt)
        .sortBy { p ⇒
          scores.getOrElse(p, NOT_FOUND)
        }
        .reverse
        .map { p ⇒
          val dir = p match {
            case Pos(x, y) if x == me.pos.x && y == me.pos.y - 1 ⇒ "N"
            case Pos(x, y) if x == me.pos.x && y == me.pos.y + 1 ⇒ "S"
            case Pos(x, y) if x == me.pos.x - 1 && y == me.pos.y ⇒ "W"
            case Pos(x, y) if x == me.pos.x + 1 && y == me.pos.y ⇒ "E"
          }
          CGLogger.info(s"$dir tile => Score ${scores.getOrElse(p, NOT_FOUND)}")
          p
        }
        .headOption
        .foreach { p ⇒
          if (scores.getOrElse(p, NOT_FOUND) >= scores.getOrElse(me.pos,
                                                                 NOT_FOUND))
            return MoveAction(p)
        }
      explorers
        .sortBy { _.pos.distanceManhattan(me.pos) }
        .foreach { e ⇒
          if (me.pos.distanceManhattan(e.pos) < 3 && e.health < MAX_HEALTH_FOR_PLAN && me.health < MAX_HEALTH_FOR_PLAN && me.plansLeft > 0) {
            return PlanAction(Some("i'm a man with a PLAN"))
          }
          if (scores.getOrElse(me.pos, 0.0) < -40.0 && me.lightsLeft > 0) {
            return LightAction(Some("Join the LIGHT side!"))
          }
          if (me.pos.distanceManhattan(e.pos) < 3 && (me.health > MAX_HEALTH_FOR_PLAN || me.plansLeft == 0) && scores
                .getOrElse(me.pos, 0.0) < -40) {
            return YellAction(Some("Oh, a YELLow submarine"))
          }
        }
      WaitAction(Some("Hmmm"))
    }
    def react(state: CoKState): CoKAction = {
      state match {
        case s: NormalTurnState => pickAction(s)
      }
    }
  }
  import cok.domain.Constants._
  import cok.domain.{CoKContext, _}
  import com.truelaurel.codingame.logging.CGLogger
  import com.truelaurel.math.geometry.{Direction, Pos}
  import scala.collection.mutable
  case class Strategy3(context: CoKContext) {
    type Score = Double
//  val MAX_DIST_COMPUTE = 5
//  val WANDERER_MALUS_MAX: Score = -20.0
//  val SLASHER_MALUS: Score = -20.0
//  val ALLIED_BONUS_MAX: Score = +7.0
//  val LIGHT_BONUS_MAX: Score = +5.0
//  val PLAN_BONUS_MAX: Score = +5.0
//  val SHELTER_BONUS_MAX: Score = +5.0
//  val MAX_HEALTH_FOR_PLAN: Int = 200
//  val SHELTER_MIN_ENERGY_LEFT: Score = 1
//  val EXIT_BONUS: Score = 5.0
//  val NOT_FOUND: Score = -42.0
    val MAX_DIST_COMPUTE = 10
    val WANDERER_MALUS_MAX: Score = -200.0
    val SLASHER_MALUS: Score = -200.0
    val ALLIED_BONUS_MAX: Score = +200.0
    val LIGHT_BONUS_MAX: Score = +50.0
    val PLAN_BONUS_MAX: Score = +50.0
    val SHELTER_BONUS_MAX: Score = +150.0
    val MAX_HEALTH_FOR_PLAN: Int = 220
    val SHELTER_MIN_ENERGY_LEFT: Score = 1
    val EXIT_BONUS: Score = 30.0
    val PREVIOUS_TILE: Score = -50.0
    val DEFAULT_SUBFACTOR: Double = 1
    val NOT_FOUND: Score = -100.0
    var scoreDef: mutable.Map[Pos, Seq[(String, Score)]] = mutable.Map()
    def maxDistanceToMe(me: Pos, dist: Int)(other: Pos): Boolean =
      other.distanceManhattan(me) < dist
    def radiate(add: Score, left: Int, subfactor: Double = DEFAULT_SUBFACTOR)(
        p: Pos): Seq[(Pos, Score)] =
      radiate2(add, left, subfactor)(Seq(p), Seq())
    def radiate2(add: Score, left: Int, subfactor: Double = DEFAULT_SUBFACTOR)(
        pos: Seq[Pos],
        prev: Seq[Pos] = Seq()): Seq[(Pos, Score)] = {
      left match {
        case 0 ⇒
          pos.map(p ⇒ p → add)
        case _ ⇒
          pos.map(p ⇒ p → add) ++
            radiate2(add - (add / (left * subfactor)), left - 1)(
              pos
                .flatMap(_.neighbours4)
                .filter(context.walkableAt)
                .filterNot(pos.contains)
                .filterNot(prev.contains),
              pos ++ prev)
      }
    }
//  def radiate(add: Score, left: Int)(pos: Pos): Seq[(Pos, Score)] = {
//    left match {
//      case 0 ⇒
//        Seq(pos → add)
//      case _ ⇒
//        Seq(pos → add) ++ pos.neighbours4
//          .filter(context.walkableAt)
//          .flatMap(radiate(add - (add / (left * 2)), left - 1))
//    }
//  }
    def getWanderersMaluses(wanderers: Seq[Pos]): Seq[(Pos, Score)] = {
      wanderers.flatMap(radiate(WANDERER_MALUS_MAX, MAX_DIST_COMPUTE))
    }
    def getSlashersMaluses(slashers: Seq[Pos]): Seq[(Pos, Score)] = {
// TODO Vary malus depending on current status (do not fear the (inactive) reaper
      slashers
        .flatMap { p ⇒
          Seq(p) ++ Direction.cardinals
            .flatMap { d ⇒
              p.neighborsIn(d)
            }
            .takeWhile { p ⇒
              context.walkableAt(p)
            }
        }
        .map { p ⇒
          p → SLASHER_MALUS
        } // ++ slashers.flatMap(radiate(SLASHER_MALUS, 2))
    }
    def getAlliesBonuses(allies: Seq[Pos]): Seq[(Pos, Score)] = {
      allies.flatMap(radiate(ALLIED_BONUS_MAX, MAX_DIST_COMPUTE, -0.3))
    }
    def getPlansBonuses(plans: Seq[Pos]): Seq[(Pos, Score)] = {
      plans.flatMap(radiate(PLAN_BONUS_MAX, 3))
    }
    def getLightsBonuses(lights: Seq[Pos]): Seq[(Pos, Score)] = {
      lights.flatMap(radiate(LIGHT_BONUS_MAX, 3))
    }
    def getSheltersBonuses(shelters: Seq[Pos]): Seq[(Pos, Score)] = {
      shelters.flatMap(radiate(SHELTER_BONUS_MAX, 5))
    }
    def getExitBonuses(pos: Seq[Pos]): Seq[(Pos, Score)] =
      pos.map(p ⇒ (p, context.exitNumbers(p) * EXIT_BONUS))
    def dbgScore(watchedPos: Seq[Pos])(lbl: String)(
        test: (Pos, Score)): (Pos, Score) = test match {
      case (k, v) ⇒
        if (watchedPos.contains(k)) {
          scoreDef(k) = scoreDef.getOrElse(k, Seq()) ++ Seq((lbl, v))
        }
        (k, v)
    }
    def pickAction(s: NormalTurnState): CoKAction = {
      val me :: explorers = s.explorers
      val watchedPos: Seq[Pos] =
        (me.pos.neighbours4 ++ Seq(me.pos)).filter(context.walkableAt)
      val dbg = dbgScore(watchedPos) _
      val scores =
        (getWanderersMaluses(
          s.enemies
            .filter(_.entityType == WANDERER)
            .map(_.pos)
            .filter(maxDistanceToMe(me.pos, MAX_DIST_COMPUTE)))
          .map(dbg("wanderer")) ++
          getSlashersMaluses(
            s.enemies
              .filter(_.entityType == SLASHER)
              .filter(_.state != SPAWNING_STATE)
              .map(_.pos)).map(dbg("slasher")) ++
          getAlliesBonuses(
            explorers
              .map(_.pos)
              .filter(maxDistanceToMe(me.pos, MAX_DIST_COMPUTE)))
            .map(dbg("ally")) ++
          getPlansBonuses(
            s.effects
              .filter(_.isInstanceOf[EffectPlanEntity])
              .map(_.asInstanceOf[EffectPlanEntity].pos)
              .filter(maxDistanceToMe(me.pos, 3))).map(dbg("plan")) ++
          getLightsBonuses(
            s.effects
              .filter(_.isInstanceOf[EffectLightEntity])
              .map(_.asInstanceOf[EffectLightEntity].pos)
              .filter(maxDistanceToMe(me.pos, 3))).map(dbg("light")) ++
          getSheltersBonuses(
            s.effects
              .filter(_.isInstanceOf[EffectShelterEntity])
              .filter(_.asInstanceOf[EffectShelterEntity].energyLeft > SHELTER_MIN_ENERGY_LEFT)
              .map(_.asInstanceOf[EffectShelterEntity].pos)
              .filter(maxDistanceToMe(me.pos, 5))).map(dbg("shelter")) ++
          getExitBonuses(me.pos.neighbours4.filter(context.walkableAt))
            .map(dbg("exits")) ++
          context.previousState
            .map { s ⇒
              s.asInstanceOf[NormalTurnState]
                .explorers
                .headOption
                .filterNot { e ⇒
                  context.shelters.contains(e.pos)
                }
                .map(_.pos) → PREVIOUS_TILE
            })
          .groupBy {
            _._1
          }
          .map { case (k, v) ⇒ (k, v.map(_._2).sum) }
      if (me.health > MAX_HEALTH_FOR_PLAN) {
        explorers
          .sortBy(_.pos.distanceManhattan(me.pos))
          .map { e ⇒
            CGLogger.info(
              s"Explorer ${e.entityId} dist: ${e.pos.distanceManhattan(me.pos)}")
            e
          }
          .headOption
          .foreach { e ⇒
            if (e.pos.distanceManhattan(me.pos) > 3) {
              return MoveAction(e.pos, Some("following"))
            }
          }
      }
//      .map({
//        case Pos(x, y) if x == me.pos.x && y == me.pos.y - 1 ⇒ Pos(x, y) → "N"
//        case Pos(x, y) if x == me.pos.x && y == me.pos.y + 1 ⇒ Pos(x, y) → "S"
//        case Pos(x, y) if x == me.pos.x - 1 && y == me.pos.y ⇒ Pos(x, y) → "W"
//        case Pos(x, y) if x == me.pos.x + 1 && y == me.pos.y ⇒ Pos(x, y) → "E"
//        case p ⇒ p → "Current"
//      }).toMap
      val best = scores.toList.maxBy(_._2)
      CGLogger.info(s"Best tile around: ${best._1} with score ${best._2}")
      CGLogger.info(
        s"Current ${me.pos} => Score ${scores.getOrElse(me.pos, NOT_FOUND)}")
      scoreDef(me.pos).foreach {
        case (lbl, score) ⇒ CGLogger.info(s"Current => $lbl score $score")
      }
      me.pos.neighbours4
        .filter(context.walkableAt)
        .sortBy { p ⇒
          scores.getOrElse(p, NOT_FOUND)
        }
        .reverse
        .map { p ⇒
          val dir = p match {
            case Pos(x, y) if x == me.pos.x && y == me.pos.y - 1 ⇒ "N"
            case Pos(x, y) if x == me.pos.x && y == me.pos.y + 1 ⇒ "S"
            case Pos(x, y) if x == me.pos.x - 1 && y == me.pos.y ⇒ "W"
            case Pos(x, y) if x == me.pos.x + 1 && y == me.pos.y ⇒ "E"
          }
          CGLogger.info(s"$dir tile => Score ${scores.getOrElse(p, NOT_FOUND)}")
          scoreDef(p).foreach {
            case (lbl, score) ⇒ CGLogger.info(s"$dir => $lbl score $score")
          }
          p
        }
        .headOption
        .foreach { p ⇒
          if (scores.getOrElse(p, NOT_FOUND) >= scores.getOrElse(me.pos,
                                                                 NOT_FOUND))
            return MoveAction(p)
        }
      explorers
        .filter { _.health > 0 }
        .sortBy {
          _.pos.distanceManhattan(me.pos)
        }
        .foreach { e ⇒
          if (me.pos.distanceManhattan(e.pos) < 3 && e.health < MAX_HEALTH_FOR_PLAN && me.health < MAX_HEALTH_FOR_PLAN && me.plansLeft > 0) {
            return PlanAction(Some("i'm a man with a PLAN"))
          }
          if (scores.getOrElse(me.pos, 0.0) < -50.0 && me.lightsLeft > 0) {
            return LightAction(Some("Join the LIGHT side!"))
          }
          if (me.pos.distanceManhattan(e.pos) < 3 && (me.health > MAX_HEALTH_FOR_PLAN || me.plansLeft == 0)) {
            return YellAction(Some("Look, a YELLow submarine!"))
          }
        }
      WaitAction(Some("Hmmm"))
    }
    def react(state: CoKState): CoKAction = {
      state match {
        case s: NormalTurnState => pickAction(s)
      }
    }
  }
  import cok.domain.Constants._
  import cok.domain._
  import com.truelaurel.codingame.logging.CGLogger
  import com.truelaurel.math.geometry.{Direction, Pos}
  case class TestStrategy(context: CoKContext) {
// Distance to start checking for avoidance moves
    val AVOIDANCE_START: Int = 2
    val MAX_HEALTH_FOR_PLAN: Int = 200
    val MAX_HEALTH_FOR_SOLO_PLAN: Int = 100
    val MIN_HEALTH_TO_MOVE_TO_SHELTER: Int = 150
    val MIN_DIST_TO_MOVE_TO_SHELTER: Int = 10
    def getSlasherDangerZones(slasherPos: Seq[(Int, Pos)],
                              time: Int): Seq[Pos] = {
      slasherPos
        .filter { case (t, p) ⇒ t < time }
        .map(_._2)
        .flatMap { p ⇒
          {
            Direction.cardinals
              .flatMap { d ⇒
                p.neighborsIn(d)
              }
              .takeWhile { p ⇒
                context.walkableAt(p)
              }
          } ++ Seq(p)
        }
        .distinct
    }
    def getDangerZones(minionsPos: Seq[Pos], slashers: Seq[(Int, Pos)])(
        radius: Int): Seq[Pos] =
      radius match {
        case 0 ⇒ minionsPos // ++ getSlasherDangerZones(slashers, radius)
        case _ ⇒
          getDangerZones(
            minionsPos
              .flatMap(p ⇒ p.neighbours4 ++ Seq(p))
              .distinct
              .filter(context.walkableAt),
            slashers)(radius - 1) ++ getSlasherDangerZones(slashers, radius)
      }
    def getNeighbors4(pos: Pos): Seq[Pos] =
      pos.neighbours4
        .filter(context.walkableAt)
        .sortBy(p ⇒ context.exitNumbers(p))
//      .reverse
    def noDeadEnds(pos: Pos): Boolean =
      context.exitNumbers.getOrElse(pos, 0) > 1
    def checkAhead(getDZ: Int ⇒ Seq[Pos],
                   candidates: Seq[Pos],
                   radius: Int = 1): Option[Pos] = {
      CGLogger.info(s"GDZ: Candidates: $candidates")
      CGLogger.info(s"GDZ: Radius $radius")
      candidates.size match {
        case 0 ⇒
          None
        case 1 ⇒
          candidates.headOption
        case 2 if radius > 3 ⇒
          Some(candidates.maxBy(p ⇒ context.exitNumbers(p)))
        case _ if radius < 6 ⇒
          checkAhead(
            getDZ,
            candidates
              .filter(p ⇒ !getDZ(radius).contains(p)), //.filter(noDeadEnds),
            radius + 1
          ).orElse(Some(candidates.maxBy(p ⇒ context.exitNumbers(p))))
        case _ ⇒
          None
      }
    }
    def pickAction(state: NormalTurnState): CoKAction = {
      val me :: explorers = state.explorers
      val wandererPos = state.enemies
        .filter(e ⇒
          e.entityType == WANDERER && e.pos.distanceManhattan(me.pos) < 10)
        .map(_.pos)
        .distinct
      val slasherPos = state.enemies
        .filter(e ⇒
          e.entityType == SLASHER // && (e.target == me.entityId || e.state == WANDERING_STATE)
            && e.pos.distanceManhattan(me.pos) < 10) // TODO Rather consider Slashers if Xs == Xme or Ys == Yme (or +/- 1)
        .map(e ⇒
          (e.state match {
            case STALKING_STATE ⇒ 0
            case RUSHING_STATE ⇒ 0
            case _ ⇒ List(e.time, 2).min
          }, e.pos))
        .distinct
      CGLogger.info(s"Slasher Positions: $slasherPos")
      CGLogger.info(s"Current position: ${me.pos}")
      val gDZ: Int ⇒ Seq[Pos] = getDangerZones(wandererPos, slasherPos)
      if (gDZ(AVOIDANCE_START).contains(me.pos)) {
        checkAhead(gDZ, getNeighbors4(me.pos).filterNot(wandererPos.contains))
          .foreach { p ⇒
            CGLogger.info("Moving to escape enemies")
            return MoveAction(p, Some("avoid"))
          }
      }
      if (me.lightsLeft > 0) { // && explorersPerDist.headOption.exists(
//         _.pos.distanceManhattan(me.pos) < 3)) {
        state.enemies
          .filter { m ⇒
            val dist = m.pos.distanceManhattan(me.pos)
            m.entityType == WANDERER &&
            m.state == WANDERING_STATE &&
            m.target == me.entityId &&
            dist > 1 &&
            dist <= 4
          }
          .foreach { _ ⇒
            return LightAction(Some("Embrace the LIGHT side!"))
          }
      }
      if (me.health < MIN_HEALTH_TO_MOVE_TO_SHELTER
          && context.shelters.exists(
            _.distanceManhattan(me.pos) < MIN_DIST_TO_MOVE_TO_SHELTER)) {
        if (context.shelters.contains(me.pos)) {
          return WaitAction(Some("Is it fallout76 yet?"))
        }
        MoveAction(context.shelters.toList
                     .minBy(_.distanceManhattan(me.pos)),
                   Some("Is it fallout76 yet?"))
      }
      val explorersPerDist = explorers.sortBy(_.pos.distanceManhattan(me.pos))
      explorersPerDist
// Avoid repeating last action (?)
        .find(
          e ⇒
            context.previousState
              .forall { s ⇒
                s.asInstanceOf[NormalTurnState].explorers.head.pos != e.pos
            })
        .foreach { e ⇒
// If we're both low life, heal
          if (me.pos.distanceManhattan(e.pos) < 3 && e.health < MAX_HEALTH_FOR_PLAN && me.health < MAX_HEALTH_FOR_PLAN && me.plansLeft > 0) {
            CGLogger.info("Low life: I heal")
            return PlanAction(Some("i'm a man with a PLAN"))
          }
          if (noDeadEnds(e.pos) && !me.pos.neighbours4.contains(e.pos)) {
            CGLogger.info("Following a buddy...")
            return MoveAction(e.pos, Some("follow"))
          } else {
            CGLogger.info("NOT following buddy in a dead-end")
          }
        }
      if (me.health < MAX_HEALTH_FOR_SOLO_PLAN && me.plansLeft > 0) {
        CGLogger.info("Healing...")
        return PlanAction(Some("healing 2"))
      }
      context.shelters.headOption.foreach { p ⇒
        CGLogger.info("Move to shelter")
        return MoveAction(p, Some("Moving to shelter"))
      }
      WaitAction(Some("Hmmmm"))
    }
    def react(state: CoKState): CoKAction = {
      state match {
        case s: NormalTurnState => pickAction(s)
      }
    }
  }
}
