import Shorten._
import cats.effect._
import munit.CatsEffectSuite
import munit._
import Config.DbConfig

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
