package cok.domain

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
