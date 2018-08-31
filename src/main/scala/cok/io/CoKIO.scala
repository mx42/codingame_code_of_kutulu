package cok.io

import cok.domain._
import cok.domain.Constants._
import com.truelaurel.codingame.challenge.GameIO
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos

object CoKIO extends GameIO[CoKContext, CoKState, CoKAction] {

  /**
    * Reads game context from the referee system. A context stores game's global information
    */
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

  /**
    * Reads current state from the referee system. A state provides information for the current turn
    */
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

  /**
    * Writes action to the referee system
    */
  def write(command: String, comment: Option[String]): Unit = {
    if (comment.isDefined) {
      println(s"$command ${comment.get}")
    } else println(command)
  }

  override def writeAction(state: CoKState, action: CoKAction): Unit = {
    action match {
      case WaitAction(comment)      => write("WAIT", comment)
      case MoveAction(pos, comment) => write(s"MOVE ${pos.x} ${pos.y}", comment)
      case LightAction(comment) ⇒ write("LIGHT", comment)
      case PlanAction(comment) ⇒ write("PLAN", comment)
      case YellAction(comment) ⇒ write("YELL", comment)
    }
  }
}
