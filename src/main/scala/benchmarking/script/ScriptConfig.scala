package benchmarking.script

import domain.{Point, Rule}
import zio.UIO

final case class ScriptConfig(
    name: String,
    dimensions: List[Int],
    rulesNumbers: List[Int],
    pointsNumbers: List[Int],
    genRule: Int => UIO[Rule],
    genPoint: Int => UIO[Point]
)
