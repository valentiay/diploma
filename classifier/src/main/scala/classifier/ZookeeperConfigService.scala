package classifier

import java.util.UUID

import classifier.ZookeeperConfigService.ClusterConfig
import core.indices.ERIO
import fs2.concurrent.Queue
import fs2.Stream
import org.apache.curator.framework.{CuratorFramework, CuratorFrameworkFactory}
import org.apache.curator.framework.recipes.nodes.PersistentNode
import org.apache.curator.framework.recipes.cache.{ChildData, NodeCache, PathChildrenCache, PathChildrenCacheEvent, PathChildrenCacheListener}
import org.apache.curator.retry.ExponentialBackoffRetry
import org.apache.zookeeper.CreateMode
import org.apache.curator.utils.CloseableUtils
import org.apache.curator.utils.ZKPaths
import zio.ZIO
import zio.console._
import zio.interop.catz._

import scala.util.{Random, Try}

class ZookeeperConfigService private(client: CuratorFramework, config: ClassifierConfig) {
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

  private def idFromPath(path: String): String = ZKPaths.getNodeFromPath(path)

  private def getCurrentMembers: Map[String, String] = {
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
    if (!thisIdAdded) builder.addOne(thisId -> new String(pen.getData))
    builder.result()
  }

  private def getClusterConfig: ERIO[ClusterConfig] =
    ZIO {
      val members = getCurrentMembers
      val thisIdx = members.keys.toVector.sorted.indexOf(thisId) / config.groupSize
      val groupNumber = Math.ceil(members.size.toDouble / config.groupSize).toInt
      ClusterConfig(thisIdx, groupNumber)
    }

  val stream: Stream[ERIO, ClusterConfig] =
    for {
      q <- Stream.eval(Queue.unbounded[ERIO, ClusterConfig])
      _ <- Stream.eval(
        putStrLn("Fetching init config") *>
          getClusterConfig.flatMap(q.enqueue1)
      )
      _ <- Stream.eval {
        ZIO.runtime[zio.ZEnv].map { runtime =>
          cache.getListenable.addListener { (client: CuratorFramework, event: PathChildrenCacheEvent) =>
            runtime.unsafeRunSync(getClusterConfig.flatMap(q.enqueue1))
          }
        }
      }
      row <- q.dequeue
    } yield row
}

object ZookeeperConfigService {

  final case class ClusterConfig(index: Int, groupNumber: Int)

  def rulesRangeStream(config: ClassifierConfig): ERIO[Stream[ERIO, ClusterConfig]] =
    ZIO.apply {
      val retryPolicy = new ExponentialBackoffRetry(1000, 3)
      val client = CuratorFrameworkFactory.newClient(config.zookeeperConnect, retryPolicy)
      client.start()
      val zookeeperConfigService = new ZookeeperConfigService(client, config)
      zookeeperConfigService.start
      zookeeperConfigService.stream
    }
}