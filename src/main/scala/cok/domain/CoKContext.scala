package cok.domain

import com.truelaurel.math.geometry.Pos

case class CoKContext(mapDim: Pos,
                      empty: Set[Pos],
                      walls: Set[Pos],
                      portals: Set[Pos],
                      shelters: Set[Pos],
                      sanityLossSolo: Int,
                      sanityLossGroup: Int,
                      spawnTime: Int,
                      wanderTime: Int,
                      previousState: Option[CoKState] = None,
                      previousAction: Option[CoKAction] = None) {
  def walkableAt(pos: Pos): Boolean = !walls.contains(pos)

  // Precompute, for each walkable tile:
  // > Distance to nearest 3-path tile
  // > Distance to nearest 4-path tile (if any)

  val exitNumbers: Map[Pos, Int] = (empty ++ portals ++ shelters)
    .map(p ⇒ (p, p.neighbours4.count(walkableAt)))
    .toMap

  val distance4: Map[Pos, Int] = exitNumbers
    .filter(_._2 == 4)
    .flatMap(x ⇒ setDistances(x._1))
    .groupBy(_._1)
    .map { case (pos, maps) ⇒ (pos, maps.values.min) }

  val distance3: Map[Pos, Int] = exitNumbers
    .filter(_._2 == 3)
    .flatMap(x ⇒ setDistances(x._1))
    .groupBy(_._1)
    .map { case (pos, maps) ⇒ (pos, maps.values.min) }

  // recursive ...
  def setDistances(p: Pos, dist: Int = 0): Map[Pos, Int] = {
    p.neighbours4.map(p2 ⇒ (p2, dist + 1)).toMap
  }
}
