package sputt

import scala.annotation.targetName

object Points:
    import Vectors.*

    opaque type Point2D = (Double, Double)

    object Point2D:
        def apply(x: Double, y: Double): Point2D = (x, y)

    extension (p: Point2D)
        def x: Double = p._1
        def y: Double = p._2

        def +(v: Vect2D): Point2D = (p.x + v.x, p.y + v.y)
        def -(q: Point2D): Vect2D = Vect2D(p.x - q.x, p.y - q.y)

        def distToSegment(p0: Point2D, p1: Point2D): Double = {
            val d = p1 - p0
            val len = d.mag

            if (len == 0) {
                (p - p0).mag
            } else {
                val t0 = ((p - p0) dot d) / (len * len)
                val t = 0.0 max (1.0 min t0)
                val v = p0 + t * d
                (p - v).mag
            }
        }


object Vectors:
    import Points.*

    opaque type Vect2D = (Double, Double)

    object Vect2D:
        def apply(x: Double, y: Double): Vect2D = (x, y)

    extension (v: Vect2D)
        def x: Double = v._1
        def y: Double = v._2

        def +(w: Vect2D): Vect2D = (v.x + w.x, v.y + w.y)
        def -(w: Vect2D): Vect2D = (v.x - w.x, v.y - w.y)

        def *(k: Double): Vect2D = (v.x * k, v.y * k)

        infix def dot(w: Vect2D): Double = v.x * w.x + v.y * w.y
        infix def cross(w: Vect2D): Double = v.x * w.y - v.y * w.x

        def mag: Double = math.sqrt(v dot v)
        def arg: Double = math.atan2(v.y, v.x)

        def unit: Vect2D = {
            val theta = v.arg
            (math.cos(theta), math.sin(theta))
        }

        def normal: Vect2D = (-v.y, v.x)
        def unitNormal: Vect2D = normal.unit

    extension (k: Double)
        def *(v: Vect2D): Vect2D = v * k
end Vectors

object Transforms:
    import Points.*
    import Vectors.*

    opaque type Transform = (Double, Double, Double, Double, Double, Double)

    object Transform:
        def apply(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double): Transform =
            (a, b, c, d, e, f)
        
        def rotate(theta: Double, p: Point2D = Point2D(0, 0)): Transform = {
            val c = math.cos(theta)
            val s = math.sin(theta)
            (c, s, -s, c, p.x * (1 - c) + p.y * s, p.y * (1 - c) - p.x * s)
        }

        def scale(sx: Double, sy: Double): Transform = (sx, 0, 0, sy, 0, 0)

        def translate(v: Vect2D): Transform = (1, 0, 0, 1, v.x, v.y)

    extension (m: Transform)
        def a: Double = m._1
        def b: Double = m._2
        def c: Double = m._3
        def d: Double = m._4
        def e: Double = m._5
        def f: Double = m._6

        @targetName("ofVector")
        def of(v: Vect2D): Vect2D =
            Vect2D(m.a * v.x + m.c * v.y + m.e, m.b * v.x + m.d * v.y + m.f)

        @targetName("ofPoint")
        def of(p: Point2D): Point2D =
            Point2D(m.a * p.x + m.c * p.y + m.e, m.b * p.x + m.d * p.y + m.f)
                
        def *(m2: Transform): Transform =
            (m.a * m2.a + m.c * m2.b,
             m.b * m2.a + m.d * m2.b,
             m.a * m2.c + m.c * m2.d,
             m.b * m2.c + m.d * m2.d,
             m.a * m2.e + m.c * m2.f + m.e,
             m.b * m2.e + m.d * m2.f + m.f
            )
end Transforms
