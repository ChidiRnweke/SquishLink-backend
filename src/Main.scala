import LinkService.generateLinkResponse
import LinkService.generateNameResponse
import Shorten.Database.findInDatabase
import Shorten.InputLink
import Shorten.RandomLink
import Shorten.shorten
import cats.data.EitherT
import cats.effect._
import com.comcast.ip4s._
import io.circe._
import io.circe.generic.auto._
import io.circe.literal._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.syntax._
import org.http4s.server.middleware.ErrorAction

object Main extends IOApp:
  import LinkService.linkShortenService
  import ExceptionService.errorHandler

  def run(args: List[String]): IO[ExitCode] =
    EmberServerBuilder
      .default[IO]
      .withErrorHandler(errorHandler)
      .withPort(port"8080")
      .withHttpApp(linkShortenService.orNotFound)
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)

object LinkService:
  def generateLinkResponse(shortenedURL: String): IO[Response[IO]] =
    findInDatabase(shortenedURL).flatMap(res => Ok(res.asJson))

  def generateNameResponse(originalLink: InputLink): IO[Response[IO]] =
    shorten(originalLink).flatMap(link => Ok(link.asJson))

  val linkShortenService = HttpRoutes
    .of[IO]:
      case GET -> Root / "squish" / name => generateLinkResponse(name)
      case req @ POST -> Root / "squish" =>
        req.as[InputLink].flatMap(generateNameResponse)

  extension (link: RandomLink)
    def asJson = json"""{"link": ${link.toString()}}"""

object ExceptionService:
  val logger = Slf4jLogger.create[IO]

  val errorHandler: PartialFunction[Throwable, IO[Response[IO]]] =
    case e: Throwable =>
      for
        log <- logger
        _ <- log.error(e)(e.getMessage())
        err <- InternalServerError(
          json"""{"error": "An internal server error occurred."}""".noSpaces
        )
      yield (err)
