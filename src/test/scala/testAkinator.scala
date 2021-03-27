package animal

import java.io.FileNotFoundException

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

/*Tests de JeuSimple*/
  test("jeuSimple que oui"){
    val ret = jeuSimple(a,List("o","o","o","o").iterator)
    assert(ret)
  }
  test("jeuSimple que non"){
    val ret = jeuSimple(a,List("n","n","o").iterator)
    assert(ret)
  }

  /*Tests de JeuLog*/
  test("jeuLog 1"){
    val ret = jeuLog(a,List("o","o","o","o").iterator)
    assert(ret.equals(List("o","o","o","o")))
  }

  test("jeuLog 2"){
    val ret = jeuLog(a,List("n","n","o").iterator)
    assert(ret.equals(List("n","n","o")))
  }

 /*Test de JeuApprentissage*/
  test("jeuApprentissage 1"){
    val ret = jeuApprentissage(a,List("o","o","o","n","Chauve-souris","Est-ce une maladie pour cet animal ?","o").iterator)
    val a2 = Question("Est-ce qu'il a des ailes ?",
      Question("Est-ce qu'il a des plumes ?",
        Question("Est-ce qu'il a un goitre ?",
          Question("Est-ce une maladie pour cet animal ?",
            Animal("Chauve-souris"), Animal("Pelican")),
          Animal("Pigeon")),
        Question("Est-ce qu'il a des poils ?",
          Animal("Chauve-souris"),Animal("Pterodactyle"))),
      Question("Est-ce qu'il ronronne ?",
        Animal("Chat"),Animal("Chien")));
    assert(ret.equals(a2))
  }

  test("jeuApprentissage 2"){
    val ret = jeuApprentissage(a,List("n","n","n","Lapin","Est-ce qu'il a de grandes dents ?","o").iterator)
    val a2 = Question("Est-ce qu'il a des ailes ?",
      Question("Est-ce qu'il a des plumes ?",
        Question("Est-ce qu'il a un goitre ?",
           Animal("Pelican"), Animal("Pigeon")),
        Question("Est-ce qu'il a des poils ?",
          Animal("Chauve-souris"),Animal("Pterodactyle"))),
      Question("Est-ce qu'il ronronne ?",
        Animal("Chat"),
        Question("Est-ce qu'il a de grandes dents ?",
          Animal("Lapin"),Animal("Chien"))))
    assert(ret.equals(a2))
  }


  /*Tests fichierToABanimal */
  test("fichierToABanimal 1 sans erreur"){
    val ret = jeuApprentissage(a,List("n","n","n","Lapin","Est-ce qu'il a de grandes dents ?","o").iterator)
    val a2 = fichierToABanimal("test1Q5.txt")
    assert(ret.equals(a2))
  }

  test("fichierToABanimal 2 sans erreur"){
    val ret = jeuApprentissage(a,List("o","o","o","n","Chauve-souris","Est-ce une maladie pour cet animal ?","o").iterator)
    val a2 = fichierToABanimal("test2Q5.txt")
    assert(ret.equals(a2))
  }

  test("fichierToABanimal avec erreur"){
    intercept[FileNotFoundException] {
      val a2 = fichierToABanimal("super.txt")
    }
  }


  /*Test ABanimalToFichier*/
  test("ABanimalToFichier 1"){
    ABanimalToFichier("test1Q6.txt",a)
    val a2 = fichierToABanimal("test1Q6.txt")
    assert(a2.equals(a))
  }

  test("ABanumalToFichier 2"){
    val ret = jeuApprentissage(a,List("n","n","n","Lapin","Est-ce qu'il a de grandes dents ?","o").iterator)
    ABanimalToFichier("test2Q6.txt",ret)
    val a2 = fichierToABanimal("test2Q6.txt")
    assert(ret.equals(a2))
  }

  /*Test jeuSimpleJNSP*/
  test("jeuSimpleJNSP 1 "){
    var bool = jeuSimpleJNSP(a, List("o","x","x","n","n","o","o").iterator)
    assert(bool)
  }

  test("jeuSimpleJNSP 2 "){
    var bool = jeuSimpleJNSP(a, List("o","x","x","n","n","o","n").iterator)
    assert(!bool)
  }



}
