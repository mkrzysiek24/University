import plugins._

object Main extends App {
  val revertingPlugin = new Pluginable with Reverting
  assert(revertingPlugin.plugin("hello") == "olleh", "Reverting test failed!")

  val lowerCasingPlugin = new Pluginable with LowerCasing
  assert(lowerCasingPlugin.plugin("HeLLo") == "hello", "LowerCasing test failed!")

  val singleSpacingPlugin = new Pluginable with SingleSpacing
  assert(singleSpacingPlugin.plugin("ala  ma    kota") == "ala ma kota", "SingleSpacing test failed!")

  val noSpacingPlugin = new Pluginable with NoSpacing
  assert(noSpacingPlugin.plugin("ala ma kota") == "alamakota", "NoSpacing test failed!")

  val duplicateRemovalPlugin = new Pluginable with DuplicateRemoval
  assert(duplicateRemovalPlugin.plugin("alzaa cda") == "lzcd", "DuplicateRemoval test failed!")

  val rotatingPlugin = new Pluginable with Rotating
  assert(rotatingPlugin.plugin("abc") == "bca", "Rotating test failed!")

  val doublingPlugin = new Pluginable with Doubling
  assert(doublingPlugin.plugin("abcd") == "abbcdd", "Doubling test failed!")

  val shorteningPlugin = new Pluginable with Shortening
  assert(shorteningPlugin.plugin("ab cd") == "a d", "Shortening test failed!")

  val actionATest = Actions.ActionA.plugin("abcdef")
  println(Actions.ActionA.plugin("abcdef"))
  assert(actionATest == "abdef", s"ActionA test failed! Expected: 'abdef', Actual: '$actionATest'")
  
  val actionBTest = Actions.actionB.plugin("ala  ma kota")
  assert(actionBTest == "aaaaooa", s"ActionB test failed! Expected: 'aaaaooa', Actual: '$actionBTest'")
  
  val actionCTest = Actions.actionC.plugin("HeLLo")
  assert(actionCTest == "heelllo", s"ActionC test failed! Expected: 'heLLo', Actual: '$actionCTest'")
  
  val actionDTest = Actions.actionD.plugin("alzaa cda") 
  assert(actionDTest == "zcdl", s"ActionD test failed! Expected: 'zcdl', Actual: '$actionDTest'")

  val actionETest = Actions.actionE.plugin("ala ma kota")
  assert(actionETest == "aooaaaa", s"ActionE test failed! Expected: 'aooaaaa', Actual: '$actionETest'")
  
  val actionFTest = Actions.actionF.plugin("abcde")
  assert(actionFTest == "abcde", s"ActionF test failed! Expected: 'abcde', Actual: '$actionFTest'")
  
  val actionGTest = Actions.actionG.plugin("ala  ma    kota")
  assert(actionGTest == "ammktt", s"ActionG test failed! Expected: 'ammktt', Actual: '$actionGTest'")
}
