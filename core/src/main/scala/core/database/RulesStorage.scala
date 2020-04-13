package core.database

import java.util.UUID

import cats.instances.try_._
import cats.instances.vector._
import cats.syntax.option._
import cats.syntax.traverse._
import core.database.MongoRulesStorage.MongoRule
import core.domain.{Every, Exact, Greater, Interval, Less, Restriction, Rule}
import core.indices.ERIO
import reactivemongo.api.MongoConnection.ParsedURI
import reactivemongo.api.MongoConnectionOptions.Credential
import reactivemongo.api.bson.collection.BSONCollection
import reactivemongo.api.bson.exceptions.ValueDoesNotMatchException
import reactivemongo.api.bson.{BSONArray, BSONDocument, BSONDocumentHandler, BSONDouble, BSONHandler, BSONString, BSONValue, Macros, document}
import reactivemongo.api.{AsyncDriver, FailoverStrategy, MongoConnectionOptions}
import reactivemongo.core.nodeset.Authenticate
import zio.ZIO

import scala.util.{Failure, Success, Try}

trait RulesStorage {
  def getRule(id: UUID): ERIO[Rule]

  def getRules: ERIO[List[(Rule, UUID)]]

  def putRule(rule: Rule): ERIO[UUID]

  def putRules(rules: List[Rule]): ERIO[Unit]

  def clear: ERIO[Unit]
}

class MongoRulesStorage private(collection: BSONCollection) extends RulesStorage {
  def getRule(id: UUID): ERIO[Rule] = {
    val selector = document("_id" -> id.toString)

    import reactivemongo.api.bson.BSONDocumentIdentity

    ZIO.fromFuture(implicit ec => collection.find(selector, None).cursor[MongoRule]().head).map(_.rule)
  }

  def getRules: ERIO[List[(Rule, UUID)]] = {
    val selector = document("_id" ->
      document(
        "$gt" -> "00000000-0000-0000-0000-000000000000",
        "$lt" -> "ffffffff-ffff-ffff-ffff-ffffffffffff"
      )
    )

    import reactivemongo.api.bson.BSONDocumentIdentity

    ZIO.fromFuture(implicit ec =>
      collection
        .find(selector, None)
        .cursor[MongoRule]()
        .fold(List.empty[(Rule, UUID)])((res, mongoRule) => ((mongoRule.rule, UUID.fromString(mongoRule._id))) :: res)
    )
  }

  def putRule(rule: Rule): ERIO[UUID] = {
    val id = UUID.randomUUID()
    ZIO.fromFuture(implicit ec => collection.insert.one(MongoRule(id.toString, rule))).as(id)
  }

  def putRules(rules: List[Rule]): ERIO[Unit] =
    ZIO.fromFuture(implicit ec =>
      collection.insert.many(rules.map(rule => MongoRule(UUID.randomUUID().toString, rule)))
    ).unit

  def clear: ERIO[Unit] = {
    val selector = document("_id" ->
      document(
        "$gt" -> "00000000-0000-0000-0000-000000000000",
        "$lt" -> "ffffffff-ffff-ffff-ffff-ffffffffffff"
      )
    )

    import reactivemongo.api.bson.BSONDocumentIdentity

    ZIO.fromFuture{ implicit ec =>
      collection.delete().one(selector)
    }.unit
  }
}

object MongoRulesStorage {

  final case class MongoRule(_id: String, rule: Rule)

  final case class MongoConfig(hosts: List[String], user: String, password: String)

  implicit val ruleHandler: BSONHandler[Rule] =
    new BSONHandler[Rule] {
      def readTry(bson: BSONValue): Try[Rule] =
        bson match {
          case BSONArray(array) =>
            array.toVector.traverse[Try, Restriction] {
              case document: BSONDocument =>
                document.get("type") match {
                  case Some(BSONString("exact")) =>
                    document.get("value") match {
                      case Some(BSONDouble(value)) => Success(Exact(value))
                      case unknown => Failure(ValueDoesNotMatchException(unknown.toString))
                    }

                  case Some(BSONString("interval")) =>
                    (document.get("from"), document.get("to")) match {
                      case (Some(BSONDouble(from)), Some(BSONDouble(to))) => Success(Interval(from, to))
                      case unknown => Failure(ValueDoesNotMatchException(unknown.toString))
                    }

                  case Some(BSONString("less")) =>
                    document.get("to") match {
                      case Some(BSONDouble(to)) => Success(Less(to))
                      case unknown => Failure(ValueDoesNotMatchException(unknown.toString))
                    }

                  case Some(BSONString("greater")) =>
                    document.get("from") match {
                      case Some(BSONDouble(from)) => Success(Greater(from))
                      case unknown => Failure(ValueDoesNotMatchException(unknown.toString))
                    }

                  case Some(BSONString("every")) =>
                    Success(Every)

                  case unknown =>
                    Failure(ValueDoesNotMatchException(unknown.toString))
                }

              case unknown => Failure(ValueDoesNotMatchException(unknown.toString))
            }
        }

      def writeTry(rule: Rule): Try[BSONValue] =
        Try(
          BSONArray(
            rule.map {
              case Exact(value) =>
                document("type" -> "exact", "value" -> value)
              case Interval(from, to) =>
                document("type" -> "interval", "from" -> from, "to" -> to)
              case Less(to) =>
                document("type" -> "less", "to" -> to)
              case Greater(from) =>
                document("type" -> "greater", "from" -> from)
              case Every =>
                document("type" -> "every")
            }
          )
        )
    }


  implicit val mongoRuleHandler: BSONDocumentHandler[MongoRule] = Macros.handler[MongoRule]

  def make(driver: AsyncDriver, config: MongoConfig): ERIO[MongoRulesStorage] = {
    val connectionOptions = MongoConnectionOptions.default.copy(
      credentials = Map("diploma" -> Credential(config.user, config.password.some)),
      failoverStrategy = FailoverStrategy.remote
    )

    for {
      connection <- ZIO.fromFuture(implicit ec => driver.connect(config.hosts, connectionOptions))
      db <- ZIO.fromFuture(implicit ec => connection.database("diploma"))
      collection = db.collection[BSONCollection]("rules")
    } yield new MongoRulesStorage(collection)
  }
}