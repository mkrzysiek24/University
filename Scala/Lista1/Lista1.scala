import scala.io.Source

object Lista1 {
    //scalar product of two vectors xs and ys
  def scalarUgly(xs: List[Int], ys: List[Int]) = {
    require(xs.size == ys.size)
    var result = 0
    var index = 0
    while(index < xs.size) {
      result += xs(index) * ys(index)
      index += 1
    }
    result
  }

  def scalar(xs: List[Int], ys: List[Int]) = {
    (for ((x, y) <- xs zip ys) yield x * y).sum
  }
  //quicksort algorithm
  def sortUgly(xs: List[Int]): List[Int] = {
      xs match {
          case Nil => Nil
          case x::xs => {
              var left: List[Int] = Nil
              var right: List[Int] = Nil
              var list = xs
              while(list.nonEmpty) {
                val value  = list.head
                if(value < x) {
                  left = value :: left
                } else {
                  right = value :: right
                }
                list = list.tail
              }
              sortUgly(left) ++ (x :: sortUgly(right))
          }
      }
  }

  def sort(xs: List[Int]): List[Int] = {
    xs match {
      case Nil => Nil
      case x::xs => {
        val right = sort(for (elem <- xs if elem >= x) yield elem);
        val left  = sort(for (elem <- xs if elem <  x) yield elem);
        left ++ (x :: right)
      }
    }
  }

  //checks if n is prime
  def isPrimeUgly(n: Int): Boolean = {
    if (n <= 1) return false
    var i = 2
    var maxIter = math.sqrt(n).toInt
    while (i <= maxIter) {
      if (n % i == 0) return false
      i += 1
    }
    true
  }

  def isPrime(n: Int): Boolean = {
      if (n <= 1) return false

      for (i <- 2 to math.sqrt(n).toInt) {
          if (n % i == 0) return false
      }
      true
  }

  //for given positive integer n, find all pairs of integers i and j,
  //where 1 ≤ j < i < n such that i + j is prime
  def primePairsUgly(n: Int): List[(Int, Int)] = {
    var pairs: List[(Int, Int)] = List.empty
    var i = 2
    while (i < n) {
      var j = 1
      while (j < i) {
        val sum = i + j
        if (isPrimeUgly(sum)) {
          pairs = (i, j) :: pairs
        }
        j += 1
      }
      i += 1
    }
    pairs
  }

  def primePairs(n: Int): List[(Int, Int)] = {
    for {
      i <- 2 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j)
  }.toList

  //create a list with all lines from given file
  val filesHere = new java.io.File(".").listFiles.filter(_.isFile)

  def fileLinesUgly(file: java.io.File): List[String] = {
    var lines: List[String] = List.empty
    val source = Source.fromFile(file)
    var line: String = null
    val iter = source.getLines()
    while (iter.hasNext) {
      line = iter.next()
      lines = line :: lines
    }
    source.close()
    lines.reverse // Odwracamy listę, ponieważ dodawaliśmy linie na początku
  }

  def fileLines(file: java.io.File): List[String] = {
    scala.io.Source.fromFile(file).getLines().toList
  }

  //print names of all .scala files which are in filesHere & are non empty
  def printNonEmptyUgly(pattern: String): Unit = {
    var i = 0
    while (i < filesHere.length) {
      val file = filesHere(i)
      if (file.getName.endsWith(".scala")) {
        var nonEmpty = false
        val source = Source.fromFile(file)
        val iter = source.getLines()
        while (iter.hasNext && !nonEmpty) {
          val line = iter.next()
          if (line.nonEmpty) nonEmpty = true
        }
        source.close()
        if (nonEmpty) println(file.getName)
      }
      i += 1
    }
  }
  def printNonEmpty(pattern: String): Unit = {
    for {
      file <- filesHere if file.getName.endsWith(".scala")
      if Source.fromFile(file).getLines().exists(_.nonEmpty)
    } println(file.getName())
  }

  // Zadanie 1
  def test(): Unit = {
    // Zadanie 1
    val xs = List(1, 2, 3)
    val ys = List(4, 5, 6)
    val resultUgly = scalarUgly(xs, ys)
    println("Scalar product (Ugly):", resultUgly)
    val result = scalar(xs, ys)
    println("Scalar product:", result)
    
    // Zadanie 2
    val unsortedList = List(4, 2, 7, 1, 9, 3)
    val uglySorted = sortUgly(unsortedList)
    val sorted = sort(unsortedList)
    println("UglySorted list:", uglySorted)
    println("Sorted list:", sorted)
    
    // Zadanie 3
    println("isPrimeUgly(11):", isPrimeUgly(11))
    println("isPrimeUgly(12):", isPrimeUgly(12))
    println("isPrimeUgly(13):", isPrimeUgly(13))
    println("isPrime(11):", isPrime(11))
    println("isPrime(12):", isPrime(12))
    println("isPrime(13):", isPrime(13))
    
    // Zadanie 4
    println("Prime pairs (Ugly):", primePairsUgly(20))
    println("Prime pairs:", primePairs(20))
    
    // Zadanie 5
    /*
    filesHere.foreach { file =>
        val linesUgly = fileLinesUgly(file)
        val lines = fileLines(file)

        println("fileLinesUgly:")
        linesUgly.foreach(println)

        println("\nfileLines:")
        lines.foreach(println)
    }
    */
    
    // Zadanie 6
    println("printNonEmptyUgly:")
    printNonEmptyUgly(".scala")
    println("\nprintNonEmpty:")
    printNonEmpty(".scala")
  }

}