package animal

import animal.objAkinator.{Animal, Question, jeuSimple}
import org.scalatest._

import scala.io.Source
import objAkinator._

import scala.::

class testAkinator extends FunSuite{

  val a = Question("Est-ce qu'il a des ailes ?",
    Question("Est-ce qu'il a des plumes ?",
      Question("Est-ce qu'il a un goitre ?",
        Animal("Pelican"),Animal("Pigeon")),
      Question("Est-ce qu'il a des poils ?",
        Animal("Chauve-souris"),Animal("Pterodactyle"))),
    Question("Est-ce qu'il ronronne ?",
      Animal("Chat"),Animal("Chien")))


  test("jeuSimple que oui"){
    val ret = jeuSimple(a,List("o","o","o","o").iterator)
    assert(ret)
  }
  test("jeuSimple que non"){
    val ret = jeuSimple(a,List("n","n","o").iterator)
    assert(ret)
  }

  test("jeuLog 1"){
    val ret = jeuLog(a,List("o","o","o","o").iterator)
    assert(ret.equals(List("o","o","o","o")))
  }

  test("jeuLog 2"){
    val ret = jeuLog(a,List("n","n","o").iterator)
    assert(ret.equals(List("n","n","o")))
  }

  //test qu'il me reste à faire

  /*test("jeuApprentissage 1"){
    val ret = jeuApprentissage(a,List("o","o","o","n").iterator)
    val a2 = Question("Est-ce qu'il a des ailes ?",
      Question("Est-ce qu'il a des plumes ?",
        Question("Est-ce qu'il a un goitre ?",
          Animal("Pelican"),Animal("Pigeon")),
        Question("Est-ce qu'il a des poils ?",
          Animal("Chauve-souris"),Animal("Pterodactyle"))),
      Question("Est-ce qu'il ronronne ?",
        Animal("Chat"),Animal("Chien")))
    assert(ret.equals(a2))
  }**/

}
