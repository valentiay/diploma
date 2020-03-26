package benchmarking.script

import domain.{Point, Rule}
import zio.UIO

final case class ScriptConfig(
    dimensions: Int,
    rulesNumbers: List[Int],
    pointsNumbers: List[Int],
    genRule: UIO[Rule],
    genPoint: UIO[Point]
)
