import Shorten._
import cats.effect._
import munit.CatsEffectSuite
import munit._
import Config.DbConfig
import Config.AppResources.makeTransactor
import scala.sys.process._

class MockDatabaseOps(unique: Boolean = true) extends DatabaseOps:
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
      orig <- db.findInDatabase(db.rootURL ++ name1.toString())
      output = orig match
        case ShortenedLink.FoundLink(original)    => original
        case ShortenedLink.NotFoundLink(notFound) => notFound
    yield assertEquals(input, output)
