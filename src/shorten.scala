package Shorten
import cats.Applicative
import cats.effect._
import cats.effect.std.{Random, Env}
import doobie._
import doobie.implicits._
import fs2.Stream
import cats.syntax.all._

import java.io.File

case class RandomLink(
    adjective: String,
    noun: String,
    number: Int
):
  override def toString(): String = s"$adjective$noun$number"
case class InputLink(link: String)

object NameGenerator:
  import Shorten.Database.{validateUniqueness, storeInDatabase}
  def shorten(input: InputLink): IO[RandomLink] =
    for
      randomName <- generateName
      _ <- storeInDatabase(input.link, randomName)
    yield (randomName)

  private def linesFromFile(path: String): IO[List[String]] =
    fs2.io.file
      .Files[IO]
      .readAll(fs2.io.file.Path(path))
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .compile
      .toList

  private def constructName(
      adjective: String,
      noun: String,
      number: Int
  ): RandomLink =
    val positiveNumber = Math.abs(number)
    RandomLink(adjective, noun, positiveNumber)

  private val adjectivesList = linesFromFile("src/data/adjectives.txt")
  private val nounsList = linesFromFile("src/data/animals.txt")

  private def generateRandomName: IO[RandomLink] =
    for
      rng <- Random.scalaUtilRandom[IO]
      adjectives <- adjectivesList
      nouns <- nounsList
      noun <- rng.elementOf(nouns)
      adjective <- rng.elementOf(adjectives)
      number <- rng.nextInt
    yield constructName(adjective, noun, number)

  private def generateName: IO[RandomLink] =
    for
      suggestion <- generateRandomName
      notUnique <- validateUniqueness(suggestion)
      name <- if (notUnique) IO.pure(suggestion) else generateName
    yield (name)

object Environment:
  private def getEnv(name: String): IO[String] =
    Env[IO]
      .get(name)
      .flatMap(
        _.liftTo[IO](
          new Exception(s"Environment variable $name was not present")
        )
      )

  private val userName = getEnv("POSTGRES_USER")
  private val password = getEnv("POSTGRES_PASSWORD")
  private val DBName = getEnv("POSTGRES_DB")
  private val port = getEnv("POSTGRES_PORT")
  private val host = getEnv("POSTGRES_HOST")

  val rootURL = getEnv("ROOT_URL")

  private def makeTransactor(
      userName: String,
      password: String,
      host: String,
      port: String,
      DBName: String
  ): Transactor[IO] =
    val url = s"jdbc:postgresql://$host:$port/$DBName"
    Transactor.fromDriverManager[IO](
      driver = "org.postgresql.Driver",
      url = url,
      user = userName,
      password = password,
      None
    )
  val transactor =
    Applicative[IO].map5(userName, password, host, port, DBName)(makeTransactor)

object Database:
  import Environment.{transactor, rootURL}

  private def existenceQuery(link: RandomLink): IO[Boolean] =
    val query =
      sql"""
    select exists (
      select 1
      from links 
      where number = ${link.number}
      and adjective = ${link.adjective}  
      and noun = ${link.noun})""".query[Boolean].unique
    transactor.flatMap(query.transact(_))

  def validateUniqueness(input: RandomLink): IO[Boolean] =
    existenceQuery(input)

  private def insertQuery(
      original: String,
      url: String,
      link: RandomLink
  ): ConnectionIO[Int] =
    val fullURL = url + link.toString()
    sql"""
      insert into links (original_url, url, adjective, noun, number)
      values ($original, $fullURL, ${link.adjective}, 
      ${link.noun}, ${link.number})""".update.run

  def storeInDatabase(original: String, link: RandomLink): IO[Unit] =
    for
      xa <- transactor
      url <- rootURL
      _ <- insertQuery(original, url, link).transact(xa)
    yield ()

  private def findQuery(shortenedURL: String): ConnectionIO[InputLink] =
    sql"""select original_url 
    from links where url = $shortenedURL
    """.query[InputLink].unique

  def findInDatabase(shortenedURL: String): IO[InputLink] =
    transactor.flatMap(xa => findQuery(shortenedURL).transact(xa))
