package plugins

import scala.collection.View.Single

abstract class Pluginable { 
  def plugin(s: String): String = s
}
//reverts text
trait Reverting extends Pluginable {
    abstract override def plugin(s: String): String = super.plugin(s.reverse)
}
//converts all chars in text to lowerCase
trait LowerCasing extends Pluginable {
    abstract override def plugin(s: String): String = super.plugin(s.toLowerCase())
}
//removes all duplicated spaces in text, example: “ala ma kota” => “ala ma kota”
trait SingleSpacing extends Pluginable {
    abstract override def plugin(s: String): String = super.plugin(s.replaceAll("\\s+", " "))
}
//remove all spaces in text, example: “ala ma kota” => “alamakota”
trait NoSpacing extends Pluginable {
    abstract override def plugin(s: String): String = super.plugin(s.replaceAll("\\s", ""))
}
//removes all chars which occur more than once, example: “alzaa cda” => “lzcd”
trait DuplicateRemoval extends Pluginable {
    abstract override def plugin(s: String): String = super.plugin(s.filter(c => c != ' ' && s.indexOf(c) == s.lastIndexOf(c)))
}
//rotates text once, example: “abc” => “cab”
trait Rotating extends Pluginable {
    abstract override def plugin(s: String): String = super.plugin(s.tail :+ s.head)
}
//duplicates every second char in text, example: “abcd” => “abbcdd”
trait Doubling extends Pluginable {
    abstract override def plugin(s: String): String = {
        val result = super.plugin(s.zipWithIndex.map { case (char, index) =>
            if (index % 2 == 1) char.toString * 2 else char.toString
        }.mkString)
        result
    }
}
//removes every second char in text, example: “ab cd” => “a d”
trait Shortening extends Pluginable {
    abstract override def plugin(s: String): String = {
        val shortened = s.zipWithIndex.collect { case (c, i) if i % 2 == 0 => c }.mkString
        super.plugin(shortened)
    }
}

object Actions {
    //plugin applying plugins with order: SingleSpacing => Doubling => Shortening
    val ActionA: Pluginable = new Pluginable with Shortening with Doubling with SingleSpacing
    //plugin applying plugins with order: NoSpacing => Shortening => Doubling
    val actionB: Pluginable = new Pluginable with Doubling with Shortening with NoSpacing
    //plugin applying plugins with order: LowerCasing => Doubling
    val actionC: Pluginable = new Pluginable with LowerCasing with Doubling
    //plugin applying plugins with order: DuplicateRemoval => Rotating
    val actionD: Pluginable = new Pluginable with Rotating with DuplicateRemoval
    //plugin applying plugins with order: NoSpacing => Shortening => Doubling => Reverting
    val actionE: Pluginable = new Pluginable with Reverting with Doubling with Shortening with NoSpacing
    // plugin applying plugin Rotating 5-times
    val actionF: Pluginable = new Pluginable {
        override def plugin(s: String): String = (1 to 5).foldLeft(s)((acc, _) => new Pluginable with Rotating().plugin(acc))
    }
    //plugin applying plugins with order: actionA => actionB
    val actionG: Pluginable = new Pluginable {
        override def plugin(s: String): String = actionB.plugin(ActionA.plugin(s))
    }

}