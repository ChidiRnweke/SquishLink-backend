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
import concurrent.duration.DurationInt
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.headers.Location
import org.http4s.server.middleware.Throttle
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.syntax._
import Config.AppResources.makeHikariTransactor
import Shorten.NameGenerator.validateInput

object Main extends IOApp:
  import ExceptionService.errorHandler
  import Config._

  def throttleService(service: HttpApp[IO]): IO[HttpApp[IO]] =
    Throttle.httpApp[IO](amount = 10, per = 1.second)(service)

  def run(args: List[String]): IO[ExitCode] =
    Environment.loadConfig.flatMap: config =>
      val transactor = makeHikariTransactor(config.dbConfig)
      transactor.use: xa =>
        val dbOps = DoobieRepository(xa, config.rootUrl)
        val shortener = LinkService(dbOps = dbOps).linkShortenService
        throttleService(shortener.orNotFound).flatMap: service =>
          EmberServerBuilder
            .default[IO]
            .withErrorHandler(errorHandler)
            .withHost(ipv4"0.0.0.0")
            .withPort(port"8080")
            .withHttpApp(service)
            .build
            .use(_ => IO.never)
            .as(ExitCode.Success)

case class LinkService(dbOps: Repository):
  import ShortenedLink._
  import InputLink._

  val linkShortenService = HttpRoutes.of[IO]:
    case GET -> Root / "s" / name => generateLinkResponse(name)

    case req @ POST -> Root / "s" =>
      req.as[Link].flatMap(input => shortenResponse(input.link))

  private def generateLinkResponse(shortenedURL: String): IO[Response[IO]] =
    dbOps
      .findInDatabase(shortenedURL)
      .flatMap:
        case FoundLink(link) =>
          PermanentRedirect(Location(Uri.unsafeFromString(link)))
        case NotFoundLink(err) => NotFound(NotFoundLink(err).asJson)

  private def shortenResponse(originalLink: String): IO[Response[IO]] =
    validateInput(originalLink) match
      case ValidInputLink(link) =>
        shorten(link)(dbOps).flatMap(link => Ok(link.asJson))
      case err: InvalidInputLink => BadRequest(err.asJson)

object ExceptionService:
  private val logger = Slf4jLogger.create[IO]
  private case class InfraError(error: String)
  private val errorMsg = "An internal server error occurred."
  val errorHandler: PartialFunction[Throwable, IO[Response[IO]]] =
    case e =>
      for
        log <- logger
        _ <- log.error(e)(e.getMessage())
        err <- InternalServerError(InfraError(errorMsg).asJson)
      yield (err)
