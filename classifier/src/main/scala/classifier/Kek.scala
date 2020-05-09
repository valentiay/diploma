package classifier

import java.util.UUID

import core.indices.ERIO
import fs2.concurrent.Queue
import fs2.Stream
import org.apache.curator.framework.CuratorFramework
import org.apache.curator.framework.recipes.nodes.PersistentNode
import org.apache.curator.framework.recipes.cache.{ChildData, NodeCache, PathChildrenCache, PathChildrenCacheEvent, PathChildrenCacheListener}
import org.apache.zookeeper.CreateMode
import org.apache.curator.utils.CloseableUtils
import org.apache.curator.utils.ZKPaths
import zio.ZIO
import zio.interop.catz._

import scala.util.{Random, Try}

class Kek(client: CuratorFramework) {
  private val membershipPath = "/classifier/members"
  private val thisId = UUID.randomUUID().toString
  private val payloads =
    Vector(
      "Hi-Diddily-Ho!",
      "Neighborino",
      "Okily Dokily!",
      "Toodily-Doo",
      "What can I ding-dong-diddily-do for you?",
      "diddly",
      "doodly"
    )
  private val pen =
    new PersistentNode(
      client,
      CreateMode.EPHEMERAL,
      false,
      ZKPaths.makePath(membershipPath, thisId),
      payloads(Random.nextInt(payloads.size)).getBytes
    )
  private val cache =
    new PathChildrenCache(client, membershipPath, true)

  def start: Unit = {
    pen.start()
    Try(cache.start())
  }

  def close(): Unit = {
    CloseableUtils.closeQuietly(cache)
    CloseableUtils.closeQuietly(pen)
  }

  def getCurrentMembers: Map[String, String] = {
    val builder = Map.newBuilder[String, String]
    var thisIdAdded = false
    val iterator = cache.getCurrentData().iterator()
    while ( {
      iterator.hasNext
    }) {
      val data = iterator.next
      val id = idFromPath(data.getPath)
      thisIdAdded = thisIdAdded || id == thisId
      builder.addOne(id -> new String(data.getData))
    }
    if (!thisIdAdded) builder.addOne(thisId -> new String(pen.getData)) // this instance is always a member
    builder.result()
  }

  def idFromPath(path: String): String = ZKPaths.getNodeFromPath(path)

  val stream: Stream[ERIO, (Int, Int)] = for {
    queue <- Stream.eval(Queue.unbounded[ERIO, (Int, Int)])
    _ <- Stream.eval(
        ZIO.runtime[zio.ZEnv].map( runtime =>
          cache.getListenable.addListener { (client: CuratorFramework, event: PathChildrenCacheEvent) =>
              val members = getCurrentMembers
              println(members)
              println(members.keys.toVector.sorted)
              val thisIdx = members.keys.toVector.sorted.indexOf(thisId)
              runtime.unsafeRunAsync_(queue.enqueue1((thisIdx, members.size)))
          }
        )
      )
    item <- queue.dequeue
  } yield item
}

object Kek {
  def rulesRangeStream(client: CuratorFramework): ERIO[Stream[ERIO, (Int, Int)]] =
    ZIO.apply {
      val kek = new Kek(client)
      kek.start
      kek.stream
    }
}