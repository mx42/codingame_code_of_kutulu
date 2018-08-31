package cok.strategy

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
    shelters.flatMap(p ⇒ irradiate(SHELTER_BONUS_MAX, MAX_DIST_COMPUTE)(Seq(p)))
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
