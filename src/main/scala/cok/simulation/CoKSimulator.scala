package cok.simulation

import cok.domain.{CoKAction, CoKContext, CoKState}

case class CoKSimulator(context: CoKContext) {
  def next(fromState: CoKState, action: CoKAction): CoKState = {
    // > all users receive a state and respond an action
    // > YELL convert nearby actions (what about precedence?)
    // > new minions are invoked (wanderers & slashers ?)
    // > explorers move
    // > effects are applied (PLAN, LIGHT, SHELTER)
    // > minions move
    // > minions scare explorers (if possible)
    // > explorers lose sanity

    // slasher workflow:
    // > spawn when explorer life < 200
    // > SPAWNING 6 turns
    // > RUSH towards target after 1 turn
    // > STUNNED for 6 turns
    // > WANDERING towards last known position until an explorer comes in LoS
    // > STALKING for 2 turns before RUSH
    // > STUNNED for 6 turns if 2+ explorers in LoS

    fromState
  }
}
