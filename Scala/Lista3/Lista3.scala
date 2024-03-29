object Utils {
    def isSorted(as: List[Int], ordering: (Int, Int) => Boolean): Boolean =
        as match {
            case Nil | List(_) => true
            case (a::b::as)    => ordering(a,b) && isSorted(b::as, ordering)
        }

    def isAscSorted(as: List[Int]) = isSorted(as, _<_)

    def isDescSorted(as: List[Int]) = isSorted(as, _>_)

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
        l match {
            case Nil   => z
            case x::xs => foldLeft(xs, f(z, x))(f)
        }

    def sum(l: List[Int]) = foldLeft(l, 0)(_+_)

    def length[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    def compose[A, B, C](f: B => C, g: A => B) = (x:A) => f(g(x))

    def repeated[A](f: A => A, n: Int): A => A = {
        if (n <= 0) {
            throw new java.lang.IllegalArgumentException("n isnt positive")
        }

        var id = (x:A) => x
        for (i <- 1 to n) {
            id = compose(f, id)
        }

        id
    }

    def curry[A, B, C](f: (A, B) => C) = (a: A) => (b: B) => f(a,b)

    def uncurry[A, B,C](f: A => B => C) = (a: A, b: B) => f(a)(b)

    def unSafe[T](ex: Exception)(block: => T): T = {
        try {
            block
        } catch {
            case e: Exception =>
            println(s"An error occurred: ${e.getMessage}")
            throw ex
        }
    }   
}

object UtilsTest extends App {

    val list = List(1, 2, 3, 4, 5)
    println(Utils.isAscSorted(list))
    println(Utils.isDescSorted(list))
    println(Utils.sum(list))
    println(Utils.length(list))

    val add = (a: Int, b: Int) => a + b
    val curriedAdd = Utils.curry(add)
    println(curriedAdd(1)(2))

    val curriedReverse = Utils.uncurry(curriedAdd)
    println(curriedReverse(1, 2))

    val f: Int => String = (x: Int) => (x * 2).toString
    val g: Int => Int = (x: Int) => x + 1

    val h: Int => String = Utils.compose(f, g)
    println(h(4))

    val double: Int => Int = (x: Int) => x * 2
    val doubleTwice: Int => Int = Utils.repeated(double, 3)
    println(doubleTwice(4))

    def divide(a: Int, b: Int): Int = {
        if (b == 0) throw new ArithmeticException("Division by zero")
        else a / b
    }

    val result = Utils.unSafe(new Exception("Error occurred")) {
        divide(10, 0)
    }
}