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
    case Question(q, o, n) => {
      println(q)
      val r = it.next()
      if (r.equals("o")) {
        jeuSimple(o, it)
      }
      else if (r.equals("n")) {
        jeuSimple(n, it)
      } else {
        println("Repondre par o ou par n !")
        jeuSimple(a, it)
      }
    }
    case Animal(nom) => {
      println("Pensez-vous à : "+nom)
      val r = it.next()
      if(r.equals("o")) {
        true
      } else if(r.equals("n")) {
        false
      } else{
        println("Repondez par oui (o) ou par non (n) !")
        jeuSimple(a,it)
      }
    }
  }


}