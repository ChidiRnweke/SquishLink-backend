package App
import Config.AppResources.makeTransactor
import Shorten.NameGenerator.shorten
import Shorten.ShortenedLink
import Shorten._
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

object Main extends IOApp:
  import ExceptionService.errorHandler
  import Config._

  def run(args: List[String]): IO[ExitCode] =
    Environment.loadConfig.flatMap { config =>
      val transactor = makeTransactor(config.dbConfig)
      val dbOps = DoobieDatabaseOps(transactor, config.rootUrl)
      val linkService = LinkService(dbOps = dbOps)
      EmberServerBuilder
        .default[IO]
        .withErrorHandler(errorHandler)
        .withPort(port"8080")
        .withHttpApp(linkService.linkShortenService.orNotFound)
        .build
        .use(_ => IO.never)
        .as(ExitCode.Success)
    }

case class LinkService(dbOps: DatabaseOps):
  import ShortenedLink._
  def generateLinkResponse(shortenedURL: String): IO[Response[IO]] =
    dbOps.findInDatabase(shortenedURL).flatMap { res =>
      res match
        case FoundLink(link)   => Ok(FoundLink(link).asJson)
        case NotFoundLink(err) => NotFound(NotFoundLink(err).asJson)
    }

  def shortenResponse(originalLink: String): IO[Response[IO]] =
    shorten(originalLink)(dbOps).flatMap(link => Ok(link.asJson))

  val linkShortenService = HttpRoutes.of[IO]:
    case GET -> Root / "squish" / name => generateLinkResponse(name)
    case req @ POST -> Root / "squish" =>
      req.as[Link].flatMap(input => shortenResponse(input.link))

  extension (link: RandomLink)
    def asJson = json"""{"link": ${link.toString()}}"""

object ExceptionService:
  val logger = Slf4jLogger.create[IO]
  case class InfraError(error: String)
  val errorMsg = "An internal server error occurred."
  val errorHandler: PartialFunction[Throwable, IO[Response[IO]]] =
    case e: Throwable =>
      for
        log <- logger
        _ <- log.error(e)(e.getMessage())
        err <- InternalServerError(InfraError(errorMsg).asJson)
      yield (err)
