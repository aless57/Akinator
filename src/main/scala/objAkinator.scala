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

  def jeuSimple(a:ABanimal,it:Iterator[String]) : Boolean = a match {
    case Question(question, oui, non) => {
      println(question)
      val response = it.next()
      if (response.equals("o")) jeuSimple(oui, it)
      else if (response.equals("n")) jeuSimple(non, it)
      else {
        println("Répondre par o ou par n !")
        jeuSimple(a, it)
      }
    }
    case Animal(nom) => {
      println("Pensez-vous à : "+nom)
      val r = it.next()
      if(r.equals("o")) true
      else if(r.equals("n")) false
      else {
        println("Répondre par oui (o) ou par non (n) !")
        jeuSimple(a, it)
      }
    }
  }


}