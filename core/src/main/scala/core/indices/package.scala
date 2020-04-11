package core

import zio.RIO

package object indices {
  type ERIO[A] = RIO[zio.ZEnv, A]
}
