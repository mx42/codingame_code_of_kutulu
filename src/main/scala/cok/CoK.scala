package cok

import cok.domain.CoKAction
import cok.io.CoKIO
import cok.strategy._
import com.truelaurel.codingame.logging.CGLogger

/**
  * Made with love by AntiSquid, Illedan and Wildum.
  * You can help children learn to code while you participate by donating to CoderDojo.
  **/
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
