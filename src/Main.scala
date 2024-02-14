import Shorten.{RandomLink, InputLink, shorten}
import cats.data.EitherT
import cats.effect._
import io.circe._
import io.circe.literal._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax._
import org.typelevel.log4cats.slf4j.Slf4jLogger

extension (link: RandomLink) def asJson = json"""{"link": ${link.toString()}}"""
val log = Slf4jLogger.create[IO]

def errorLink(err: Throwable): IO[Json] =
  for
    logger <- log
    _ <- logger.error(err)(err.getMessage())
  yield json"""{"error": "An unhandled exception occurred on the server."}"""

val linkShortenService = HttpRoutes.of[IO]:
  case GET -> Root / "squish" / name => ??? // get from DB
  case req @ POST -> Root / "squish" =>
    ???

def generateNameResponse(originalLink: InputLink): IO[Response[IO]] =
  val link = EitherT(shorten(originalLink).attempt)
  link.foldF(
    e => InternalServerError(errorLink(e)),
    s => Ok(s.asJson)
  )
