package sputt

import Vectors.*

class Vect2DTest extends munit.FunSuite:
    test("vector add") {
        val a = Vect2D(1.2, -3.4)
        val b = Vect2D(5, 6.78)
        assert(a + b == Vect2D(1.2 + 5, -3.4 + 6.78))
    }

    test("vector add inverse") {
        val a = Vect2D(3.1415, 42)
        assert(a + (-1 * a) == Vect2D(0, 0))
    }

    test("vector dot normal") {
        val a = Vect2D(0.123, 4.567)
        assert((a dot a.normal) == 0)
    }

    test("vector unit length") {
        val a = Vect2D(-0.012, 345.6)
        assert(a.unit.mag == 1)
    }

    test("vector cross unit normal") {
        val a = Vect2D(2.718, 0.618)
        assert((a cross a.unitNormal) == a.mag)
    }
end Vect2DTest
