class BFState(val state: Array[Byte] = null, val cursor: Int = 0, val jumpPoint: Int = 0, val programCounter: Int=0) 
{
  val bfProgramSize=30000
  val prog = state match {
    case null => new Array[Byte](bfProgramSize)
    case st   => st
  }
}

class Interpreter(prog: String)
{
  private val progLen=prog.length
  private val statements=collection.immutable.HashMap(
    '<' -> ((st) => moveLeft(st)),
    '>' -> ((st) => moveRight(st)),
    '+' -> ((st) => increment(st)),
    '-' -> ((st) => decrement(st)),
    '.' -> ((st) => output(st)),
    ',' -> ((st) => input(st)),
    '[' -> ((st) => whileStart(st)),
    ']' -> ((st) => whileEnd(st))
  )

  private def moveRight(state: BFState) : BFState =
    new BFState(state.prog, state.cursor+1, state.jumpPoint, state.programCounter+1)

  private def moveLeft(state: BFState) : BFState =
    new BFState(state.prog, state.cursor-1, state.jumpPoint, state.programCounter+1)

  private def increment(state: BFState) : BFState = {
    state.prog(state.cursor)=(state.prog(state.cursor).toByte + 1).toByte
    return nextStatement(state)
  }

  private def decrement(state: BFState) : BFState = {
    state.prog(state.cursor)=(state.prog(state.cursor).toByte - 1).toByte
    return nextStatement(state)
  }

  private def output(state: BFState) : BFState = {
    print(state.prog(state.cursor).toChar)
    return nextStatement(state)
  }

  private def input(state: BFState) : BFState =
    // TODO: Get input
    return nextStatement(state)

  private def whileStart(state: BFState) : BFState =
    return new BFState(state.prog, state.cursor, state.programCounter+1, state.programCounter+1)

  private def whileEnd(state: BFState) : BFState =
    return new BFState(state.prog, state.cursor, state.jumpPoint, state.prog(state.cursor) match {
      case some if some > 0 => state.jumpPoint
      case _                => state.programCounter+1
    })

  private def nextStatement(state: BFState) : BFState =
    new BFState(state.prog, state.cursor, state.jumpPoint, state.programCounter+1)

  private def executeStatement(state: BFState) : BFState =
    state.programCounter match {
      case `progLen` => return null
      case _ => executeStatement(
        statements get prog(state.programCounter) match {
          case Some(f) => f(state)
          case None    => nextStatement(state)
        })
    }

  def execute = executeStatement(new BFState)
}

object Interpreter
{
  def main(args: Array[String]) {
    new Interpreter("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++." +
                    "<<+++++++++++++++.>.+++.------.--------.>+.>.").execute
  }
}
