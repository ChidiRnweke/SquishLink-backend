import java.io.File
import java.io.PrintWriter
import scala.io.Source

val writer = PrintWriter(File("src/data/animals.txt"))
val animals = Source
  .fromFile("animals.json")
  .getLines()
  .map(_.trim())
  .filter(_.startsWith("\""))
  .map(_.replace("\"", ""))
  .map(_.replace(",", ""))
  .map(_.replace(" ", "-"))
  .mkString("\n")

writer.write(animals)
writer.close()
