package cok.strategy

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
        getDangerZones(minionsPos
                         .flatMap(p ⇒ p.neighbours4 ++ Seq(p))
                         .distinct
                         .filter(context.walkableAt),
                       slashers)(radius - 1) ++ getSlasherDangerZones(slashers,
                                                                      radius)
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
