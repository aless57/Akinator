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


  def jeuLog(a:ABanimal,it:Iterator[String]) : List[String] = {

    //définition du aux
    def aux(aban:ABanimal,listString:List[String]) : List[String] = aban match {

      //cas où on arrive sur une question
      case Question(q, o, n) => {
        println(q)
        val iter = it.next()
        if (iter.equals("o"))
          aux(o,listString++ List(iter))

        else if (iter.equals("n")){
          aux(n,listString++ List(iter))

        }
        else {
          println("Repondez par oui (o) ou par non (n) !");
          jeuLog(aban, it)

        }
      }

      //cas où on arrive en bout d'arbre
      case Animal(nom) => {
        println("Pensez-vous à : " + nom)
        val r = it.next()
        if (r.equals("o")){
          listString++List(r)

        }
        else if (r.equals("n")){
          listString++List(r)

        }
        else {
          println("Repondez par oui (o) ou par non (n) !")
          aux(aban,listString)

        }
      }
    }

    aux(a,List())
  }

  def jeuApprentissage(a:ABanimal,it:Iterator[String]): ABanimal = a match {
      //Dans le cas où nous sommes face à une question
    case Question(q, o, n) => {
      println(q)
      val iter = it.next()
      //on regarde si la réponse est oui, si c'est le cas, on va rajouter la réponse à l'arbre
      if (iter.equals("o")) {
        Question(q, jeuApprentissage(o, it), n)
      }
      //on regarde si la réponse est non, si c'est le cas, on va rajouter la réponse à l'arbre
      else if (iter.equals("n")) {
        Question(q, o, jeuApprentissage(n, it))
      } else {
        println("Repondez par oui (o) ou par non (n) !")
        //on met en place la récursivité
        jeuApprentissage(a, it)
      }
    }
  }



}