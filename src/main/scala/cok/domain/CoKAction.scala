package cok.domain

import com.truelaurel.math.geometry.Pos

sealed trait CoKAction

case class WaitAction(comment: Option[String] = None) extends CoKAction

sealed case class MoveAction(move: Pos, comment: Option[String] = None) extends CoKAction

sealed case class LightAction(comment: Option[String] = None) extends CoKAction

sealed case class PlanAction(comment: Option[String] = None) extends CoKAction

sealed case class YellAction(comment: Option[String] = None) extends CoKAction

