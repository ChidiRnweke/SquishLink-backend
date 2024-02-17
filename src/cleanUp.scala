package CleanUp
import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import doobie._
import doobie.postgres.implicits._
import doobie.postgres._
import concurrent.duration._
import java.time.LocalDateTime
import doobie.implicits._
import Shorten.Repository
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.syntax._

object CleanUp:
  private val logFactory = Slf4jLogger.create[IO]
  private val timeThreshold = IO.apply(LocalDateTime.now().minusDays(7))

  private def logResult(numDeleted: Int): IO[Unit] =
    for
      logger <- logFactory
      _ <- logger.info(s"$numDeleted records deleted from database")
    yield ()

  private def cleanUpQuery(time: LocalDateTime, xa: Transactor[IO]): IO[Int] =
    sql"""delete from links where created_at < $time""".update.run.transact(xa)

  def initiate(xa: Transactor[IO]): Stream[IO, Unit] =
    Stream
      .eval(timeThreshold.flatMap(time => cleanUpQuery(time, xa)))
      .evalMap(logResult)
      .repeat
      .metered(6.hours)
