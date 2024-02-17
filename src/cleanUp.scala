package CleanUp
import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import doobie._
import doobie.postgres.implicits._
import doobie.postgres._
import concurrent.duration._
import java.time.ZonedDateTime
import doobie.implicits._
import fs2.Stream
import org.typelevel.log4cats.Logger

object CleanUp:
  private val timeThreshold = IO.apply(ZonedDateTime.now().minusDays(1))
  private def logAmountDeleted(deleted: Int, logger: Logger[IO]) =
    logger.info(s"$deleted records deleted from database")
  private def cleanUpQuery(time: ZonedDateTime, xa: Transactor[IO]): IO[Int] =
    sql"""delete from links where created_at < ${time.toString()}""".update.run
      .transact(xa)

  def initiate(xa: Transactor[IO], logger: Logger[IO]): Stream[IO, Unit] =
    Stream
      .eval(timeThreshold.flatMap(time => cleanUpQuery(time, xa)))
      .evalMap(n => logAmountDeleted(n, logger))
      .handleErrorWith(e => Stream.eval(logger.error(e.getMessage())))
      .repeat
      .metered(1.minute)
