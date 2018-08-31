package cok.strategy

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
      p: Pos): Seq[(Pos, Score)] = radiate2(add, left, subfactor)(Seq(p), Seq())

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
    CGLogger.info(s"Current ${me.pos} => Score ${scores.getOrElse(me.pos, NOT_FOUND)}")
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
