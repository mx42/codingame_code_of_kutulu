package cok.domain

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
  def emptyAt(pos: Pos): Boolean = explorersPos.getOrElse(pos, minionsPos.getOrElse(pos, 0)) == 0
}
//
//case class LightweightState(
//    nextPlayer: Boolean = true
//) extends GameState[Boolean]
//    with CoKState {}
