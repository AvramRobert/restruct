package parser

enum ParsingResult[+A]:
  case Success(result: A) extends ParsingResult[A]
  case Failure(message: String) extends ParsingResult[Nothing]

extension [A] (result: ParsingResult[A])
  def map[B](f: A => B): ParsingResult[B] = result match {
    case ParsingResult.Success(result) => ParsingResult.Success(f(result))
    case ParsingResult.Failure(msg) => ParsingResult.Failure(msg)
  }
  
  def isFailure: Boolean = result match {
    case ParsingResult.Failure(_) => true
    case _ => false
  }
  def isSuccess: Boolean = !isFailure
  def failure: Option[String] = result match {
    case ParsingResult.Failure(msg) => Some(msg)
    case _ => None
  }
  def success: Option[A] = result match {
    case ParsingResult.Success(result) => Some(result)
    case _ => None
  }

