package App
import CleanUp._
import Config.AppResources.makeHikariTransactor
import Config.AppResources.makeTransactor
import Shorten.NameGenerator.shorten
import Shorten.NameGenerator.validateInput
import Shorten.ShortenedLink
import Shorten._
import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s._
import doobie.util.transactor.Transactor
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
import org.http4s.headers.Location
import org.http4s.implicits._
import org.http4s.server.middleware.Throttle
import org.typelevel.log4cats.Logger

import concurrent.duration._

object Main extends IOApp:
  import ExceptionService.errorHandler
  import Config._

  def throttleService(service: HttpApp[IO]): IO[HttpApp[IO]] =
    Throttle.httpApp[IO](amount = 10, per = 1.second)(service)

  def run(args: List[String]): IO[ExitCode] =
    Environment.loadConfig.flatMap: config =>
      val transactor = makeHikariTransactor(config.dbConfig)
      transactor.use(xa => startApp.run(xa, config))

  def startApp: Kleisli[IO, (Transactor[IO], AppConfig), ExitCode] =
    Kleisli: (xa: Transactor[IO], config: AppConfig) =>
      val cleanUp = CleanUp.initiate(xa, config.logger).compile.drain
      val dbOps = DoobieRepository(xa, config.rootUrl)
      val shortener = LinkService(dbOps, config.logger).linkShortenService
      val app = throttleService(shortener.orNotFound).flatMap: service =>
        EmberServerBuilder
          .default[IO]
          .withErrorHandler(errorHandler(config.logger))
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(service)
          .build
          .use(_ => IO.never)

      (app, cleanUp).parTupled.as(ExitCode.Success)

case class LinkService(dbOps: Repository, logger: Logger[IO]):
  import ShortenedLink._
  import InputLink._

  val linkShortenService = HttpRoutes.of[IO]:
    case GET -> Root / "s" / name => generateLinkResponse(name)

    case req @ POST -> Root / "s" =>
      req.as[Link].flatMap(input => shortenResponse(input.link))

  private def generateLinkResponse(shortenedURL: String): IO[Response[IO]] =
    def errMsg(link: String): String =
      s"Received request for $link but could not respond."
    dbOps
      .findInDatabase(shortenedURL)
      .flatMap:
        case FoundLink(link) =>
          PermanentRedirect(Location(Uri.unsafeFromString(link)))
        case NotFoundLink(err) =>
          logger.error(errMsg(err)) *> NotFound(NotFoundLink(err).asJson)

  private def shortenResponse(originalLink: String): IO[Response[IO]] =
    validateInput(originalLink) match
      case ValidInputLink(link) =>
        shorten(link)(dbOps).flatMap(link => Ok(link.asJson))
      case err: InvalidInputLink => BadRequest(err.asJson)

object ExceptionService:
  private case class InfraError(error: String)
  private val errorMsg = "An internal server error occurred."
  def errorHandler(
      logger: Logger[IO]
  ): PartialFunction[Throwable, IO[Response[IO]]] =
    e =>
      logger.error(e)(e.getMessage()) *>
        InternalServerError(InfraError(errorMsg).asJson)
