package animal

import scala.io._
import java.io._

object objAkinator {

  trait ABanimal
  case class Animal(nom:String) extends ABanimal
  case class Question(q:String, oui:ABanimal, non:ABanimal) extends ABanimal


  def main(args: Array[String]) {
    println("Hello, world!")
  }

  def jeuSimple(a:ABanimal):Boolean = a match{
    case Question(_,o,_) => jeuSimple(o)
    case Question(_,_,n) => jeuSimple(n)
    case Animal(_) => true
  }

  val a = Question("Est-ce qu'il a des ailes ?",
            Question("Est-ce qu'il a des plumes ?",
              Question("Est-ce qu'il a un goitre ?",
               Animal("Pélican"),Animal("Pigeon")),
              Question("Est-ce qu'il a des poils ?",
                Animal("Chauve-souris"),Animal("Ptérodactyle"))),
            Question("Est-ce qu'il ronronne ?",
              Animal("Chat"),Animal("Chien")))

}