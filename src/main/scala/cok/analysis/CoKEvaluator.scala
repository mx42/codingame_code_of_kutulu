package cok.analysis

import cok.domain.CoKState

object CoKEvaluator {
  def evaluate(state: CoKState): Double = {
    // + my health
    // - enemies targeting me
    // - enemies proximity
    // + allies proximity
    // + available bonuses
    // + proximity to a shelter
    42.0
  }
}
