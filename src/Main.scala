package App
import CleanUp._
import Config.AppResources._
import Shorten.NameGenerator._
import Shorten._
import Shorten._
import cats._
import cats.data._
import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s._
import doobie._
import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.dsl.io._
import org.http4s.ember.server._
import org.http4s.headers.Location
import org.http4s.implicits._
import org.http4s.server.middleware.Throttle
import org.typelevel.log4cats.Logger

import concurrent.duration._
import org.http4s.headers.`Content-Type`

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
    case req @ GET -> Root / "s" / name =>
      generateLinkResponse(name, req.contentType)

    case req @ POST -> Root / "s" =>
      req.as[Link].flatMap(input => shortenResponse(input.link))

  private def generateLinkResponse(
      shortenedURL: String,
      contentType: Option[`Content-Type`]
  ): IO[Response[IO]] =
    dbOps
      .findInDatabase(shortenedURL)
      .flatMap:
        case FoundLink(link) =>
          PermanentRedirect(Location(Uri.unsafeFromString(link)))

        case NotFoundLink(link) => handleMissingLink(link, contentType, logger)

  private def handleMissingLink(
      link: String,
      content: Option[`Content-Type`],
      logger: Logger[IO]
  ): IO[Response[IO]] =
    val errMsg = s"Received request for $link but could not respond."
    val redirect =
      "https://" + dbOps.rootURL.stripSuffix("/s/") + "/squish/missing-url/"

    val response = content match
      case Some(ct) if ct.mediaType.isApplication =>
        NotFound(NotFoundLink(errMsg).asJson)

      case _ => PermanentRedirect(Location(Uri.unsafeFromString(redirect)))
    logger.error(errMsg) >> response

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
