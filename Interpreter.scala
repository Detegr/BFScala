class Interpreter(prog: String)
{
  val statements=collection.immutable.HashMap(
    '<' -> (() => moveLeft),
    '>' -> (() => moveRight),
    '+' -> (() => increment),
    '-' -> (() => decrement),
    '.' -> (() => output),
    ',' -> (() => input),
    '[' -> (() => whileStart),
    ']' -> (() => whileEnd)
  )

  private def moveRight = println("moveRight");
  private def moveLeft = println("moveLeft");
  private def increment = println("increment")
  private def decrement = println("decrement")
  private def output = println("output")
  private def input = println("input")
  private def whileStart = println("whileStart")
  private def whileEnd = println("whileEnd")

  def executeStatement(schar: Char)
  {
    val f=statements get schar match
    {
      case Some(f) => f
      case None    => println("Invalid character: " + schar);
    }
  }

  def execute
  {
    prog foreach executeStatement
  }
}

object Interpreter
{
  def main(args: Array[String])
  {
    val ip=new Interpreter("foobar")
    ip.execute
  }
}
