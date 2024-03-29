package numbers {
    class Rational private (val n: Int, val d: Int) {
    require(d != 0, "Denominator can't be zero.")

    private def gcd(a: Int, b: Int): Int = {
        if (b == 0) a else gcd(b, a % b)
    }

    private val gcdFraction: Int = gcd(n.abs, d.abs)

    val numerator: Int = n / gcdFraction
    val denominator: Int = d / gcdFraction

    def +(other: Rational): Rational =
        new Rational(
            numerator * other.denominator + other.numerator * denominator,
            denominator * other.denominator
        )

    def -(other: Rational): Rational =
        new Rational(
            numerator * other.denominator - other.numerator * denominator,
            denominator * other.denominator
        )

    def *(other: Rational): Rational =
        new Rational(
            numerator * other.numerator,
            denominator * other.denominator
    )

    def /(other: Rational): Rational =
        new Rational(
            numerator * other.denominator,
            denominator * other.numerator
    )


    override def toString: String = {
        if (denominator == 1) s"$numerator"
        else if (numerator.abs > denominator.abs) {
            val num = numerator / denominator
            val fract = new Rational(numerator % denominator, denominator)
            s"$num ${fract.toString}"
        } else {
            s"$numerator/$denominator"
        }
    } 

    def toDouble: Double = numerator/denominator

    }

    object Rational {
        def apply(numerator: Int, denominator: Int): Rational = new Rational(numerator, denominator)
        def apply(numerator: Int): Rational = new Rational(numerator, 1)
        def apply(): Rational = new Rational(1, 1)
        def zero: Rational = new Rational(0, 1)
    }
   
}

object RationalsTest extends App {
    import numbers._
    val frac1 = Rational(2, 3)
    val frac2 = Rational(6, 7)

    println(s"Example 1:")
    println(s"Fraction 1: $frac1")
    println(s"Fraction 2: $frac2")

    val sum = frac1 + frac2
    println(s"Sum: $sum")

    val diff = frac1 - frac2
    println(s"Difference: $diff")

    val mult = frac1 * frac2
    println(s"Product: $mult")

    val div = frac1 / frac2
    println(s"Quotient: $div")
    println()
}

package figures {

    import numbers._

    class Point(val x: Rational, val y: Rational) {
        def distance(other: Point): Double = {
            val xLen = x - other.x
            val yLen = y - other.y
            scala.math.sqrt(((xLen * xLen) + (yLen * yLen)).toDouble)
        }
    }

    object Point {
        def apply(x: Rational, y: Rational): Point = new Point(x, y)
        def apply(x: Int, y: Int): Point = new Point(Rational(x), Rational(y))
        def apply(xy: (Int, Int)): Point = new Point(Rational(xy._1), Rational(xy._2))
        def apply(x: Int, y: Rational): Point = new Point(Rational(x), y)
        def apply(x: Rational, y: Int): Point = new Point(x, Rational(y))
        def apply(x: Rational): Point = new Point(x, Rational())  
        def apply(): Point = new Point(Rational(), Rational()) 
    }

    trait Figure {
        def area: Double
        val description: String
    }

    class Triangle(val p1: Point, val p2: Point, val p3: Point) extends Figure {
        require(p1 != p2 && p2 != p3 && p1 != p3, "All triangle vertices must be unique.")

        val (a, b, c) = (p1.distance(p2), p2.distance(p3), p3.distance(p1))

        require(a + b > c, "Not a triangle.")
        require(a + c > b, "Not a triangle.")
        require(c + b > a, "Not a triangle.")

        def area: Double = {
            val p = (a + b + c)/2
            Math.sqrt(p * (p - a) * (p - b) * (p - c))
        }

        override val description: String = "Triangle"

        def this(p1: (Int, Int), p2: (Int, Int), p3: (Int, Int)) = {
            this(Point(p1), Point(p2), Point(p3))
        }
    }

    
    class Rectangle(val a: Point, val b: Point, val c: Point, val d: Point) extends Figure {
        require(a != b && b != c && c != d && d != a, "Points must be different.")

        val (abSize, cdSize) = (a.distance(b), c.distance(d))
        val (bcSize, daSize) = (b.distance(c), d.distance(a))

        require(abSize == cdSize && bcSize == daSize, "Opposite sides are different.")

        def area: Double = abSize * bcSize

        override val description: String = "Rectangle"

        def this(a: (Int, Int), b: (Int, Int), c: (Int, Int), d: (Int, Int)) = {
            this(Point(a), Point(b), Point(c), Point(d))
        }
    }
    
    class Square(a: Point, b: Point, c: Point, d: Point) extends Rectangle(a, b, c, d) {
        require(abSize == bcSize && cdSize == daSize, "Sides must be equal")

        override val description: String = "Square"

        def this(a: (Int, Int), b: (Int, Int), c: (Int, Int), d: (Int, Int)) =
            this(Point(a), Point(b), Point(c), Point(d))

        def this(left_top_corner: Point, size: Rational) =
            this(
                left_top_corner,
                Point(left_top_corner.x + size, left_top_corner.y),
                Point(left_top_corner.x + size, left_top_corner.y - size),
                Point(left_top_corner.x, left_top_corner.y - size)
            )

        def this(left_top_corner: Point, size: Int) =
            this(
                left_top_corner,
                Point(left_top_corner.x + Rational(size), left_top_corner.y),
                Point(left_top_corner.x + Rational(size), left_top_corner.y - Rational(size)),
                Point(left_top_corner.x, left_top_corner.y - Rational(size))
            )
    }

    object Figure {
        def areaSum(figures: List[Figure]): Double = (figures map (_.area)).sum

        def printAll(figures: List[Figure]): Unit = figures.foreach(figure => println(s"${figure.description} Area: ${figure.area}"))
    }
}

object FiguresTests extends App {
    import numbers._
    import figures._

    def Area(figure: Figure) = {
        println(s"${figure.description} area = ${figure.area}")
    }

    val triangle  = new Triangle((0, 0), (10, 0), (5, 10))
    val square    = new Square(Point(0,0), 8)
    val rectangle = new Rectangle((0,10), (10, 10), (10, 0), (0, 0))

    Area(triangle)
    Area(square)
    Area(rectangle)

    val list = List(square, rectangle, triangle)

    println(s"Sum: ${Figure.areaSum(list)}")
    Figure.printAll(list)
}
