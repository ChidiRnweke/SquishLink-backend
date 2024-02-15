import Shorten._
import cats.effect._
import munit.CatsEffectSuite
import munit._
import io.circe._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityCodec.circeEntityDecoder
import org.http4s.circe.CirceEntityEncoder._
import Config.DbConfig
import Config.AppResources.makeTransactor
import scala.sys.process._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.implicits._
import org.http4s._
import org.http4s.server.middleware.ErrorAction.httpApp
import cats.syntax.all._

class MockDatabaseOps(unique: Boolean = true) extends DatabaseOps:
  val rootURL = "localhost"
  def validateUniqueness(input: RandomLink): IO[Boolean] = IO.pure(unique)
  def storeInDatabase(original: String, link: RandomLink): IO[Unit] = IO.unit
  def findInDatabase(shortenedURL: String): IO[ShortenedLink] =
    IO.pure(ShortenedLink.FoundLink("test"))

class NameGeneratorSuite extends CatsEffectSuite:
  test("Generating a name twice in a row results in different names"):
    val input = "test"
    val db = MockDatabaseOps()
    for
      name1 <- NameGenerator.shorten(input)(db)
      name2 <- NameGenerator.shorten(input)(db)
    yield assertNotEquals(name1, name2)

  test("Generating a name that isn't unique results in retries."):
    val input = "test"
    val db = MockDatabaseOps(false)
    val result =
      for name1 <- NameGenerator.shorten(input)(db)
      yield name1
    assertIOBoolean(result.attempt.flatMap {
      case Right(value) => IO.pure(false)
      case Left(value)  => IO.pure(true)
    })

class IntegrationSuite extends CatsEffectSuite:
  override def beforeAll(): Unit =
    Seq("docker", "compose", "-f", "docker-compose-test.yml", "up", "-d").!!

  override def afterAll(): Unit = Seq("docker", "compose", "down", "-v").!!

  private val testConfig = DbConfig(
    driver = "org.postgresql.Driver",
    url = "jdbc:postgresql://localhost:5432/test",
    user = "test",
    password = "test"
  )
  private val transactor = makeTransactor(testConfig)
  val db = DoobieDatabaseOps(transactor, "http://localhost/squish/")
  val httpApp = App.LinkService(db).linkShortenService.orNotFound
  val client: Client[IO] = Client.fromHttpApp(httpApp)

  test("Generating a name twice in a row results in different names"):
    val input = "test"
    for
      name1 <- NameGenerator.shorten(input)(db)
      name2 <- NameGenerator.shorten(input)(db)
    yield assertNotEquals(name1, name2)

  test("It is possible to find a link that was put into the DB."):
    val input = "test"
    for
      name1 <- NameGenerator.shorten(input)(db)
      orig <- db.findInDatabase(name1.link.stripPrefix(db.rootURL))
      output = orig match
        case ShortenedLink.FoundLink(original)    => original
        case ShortenedLink.NotFoundLink(notFound) => notFound
    yield assertEquals(input, output)

  test("Shortening a link and then retrieving it returns the original link"):
    val body = Link("www.example.com")
    val request: Request[IO] =
      Request(method = Method.POST, uri = uri"/squish")
        .withEntity(body)

    for
      resp <- client.expect[Link](request)
      uri = Uri
        .unsafeFromString(resp.link.trim())
      original <- client.expect[Link](uri)
    yield assertEquals(body.link, original.link)

  test("Visiting a link that wasn't shortened results in a 404"):

    val body = Uri.unsafeFromString("www.example.com")

    for original <- client.statusFromUri(body)
    yield assertEquals(original, Status.NotFound)
