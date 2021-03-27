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

  /* Q1 Q2 */

  def jeuSimple(a:ABanimal,it:Iterator[String]) : Boolean = a match {

    //cas où on arrive sur une question
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

    //cas où on arrive en bout d'arbre
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


  /* Q3 */

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

  /* Q4 */
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
    case Animal(a) => {
      println("Pensez-vous à : " + a)
      val rep = it.next()
      if(rep == "o"){
        println("J'ai gagné !")
        Animal(a)
      }else if(rep == "n"){
        println("J'ai perdu ! Quelle est la bonne réponse ? ")
        val res = it.next()
        println("Quelle question permet de différencier " + res + " de " + a +" ?")
        val q = it.next()
        println("Quelle est la réponse à cette question pour " + res + " ?")
        val repQ = it.next()
        if(repQ == "o"){
          Question(q,Animal(res),Animal(a))
        }else if(repQ == "n"){
          Question(q,Animal(a),Animal(res))
        }else
          throw new Exception("Réponse non valide")
      }else
        throw new Exception("Réponse non valide")

    }
  }

  /* Q5 */

  def lecture(f: Iterator[String]): ABanimal = {
    val nextVal = f.next()
    if (nextVal.startsWith("Question :")) Question(nextVal.slice(10, nextVal.length), lecture(f), lecture(f))
    else Animal(nextVal)
  }

  def fichierToABanimal(nomf : String) :ABanimal = {
    try{
      val fichier = Source.fromFile("src/" + nomf)
      val file = fichier.getLines().toList.iterator
      fichier.close()
      lecture(file)
    }
    catch {
    case e : FileNotFoundException => throw new FileNotFoundException("Le fichier n'existe pas")
  }
  }

  /* Q6 */

  def ABanimalToFichier(nomf : String, a : ABanimal) : Unit = {

    def auxABanimalToFichier(f : FileWriter, ab : ABanimal) : Unit = ab match{
      case Animal(animal) => f.write(animal + "\r\n")
      case Question(q, o, n) => f.write("Question :" + q + "\r\n");
        auxABanimalToFichier(f, o);
        auxABanimalToFichier(f, n)
    }

    val ecriture = new FileWriter(new File("src/" + nomf));
    auxABanimalToFichier(ecriture, a)
    ecriture.close()
  }

  /* Q7 */





}