package cok.math.geometry
import com.truelaurel.math.geometry.{Direction, N, S, W, E}

// TODO Add those changes to truelaurel code.

case class Pos(x: Int, y:Int) {
  def neighborsIn(direction: Direction): Iterator[Pos] = direction match {
    case N ⇒ (y - 1).to(0).iterator.map(y2 ⇒ Pos(x, y2))
    case S ⇒ (y + 1).to(500).iterator.map(y2 ⇒ Pos(x, y2))
    case W ⇒ (x - 1).to(0).iterator.map(x2 ⇒ Pos(x2, y))
    case E ⇒ (x + 1).to(500).iterator.map(x2 ⇒ Pos(x2, y))
  }

  def distanceManhattan(pos: Pos): Int =
    Math.abs(x - pos.x) + Math.abs(y - pos.y)
}

object Direction {
  val cardinals = Seq(N, W, S, E)
}
