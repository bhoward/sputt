package sputt

import sputt.Points.Point2D
import sputt.Vectors.Vect2D
import sputt.Transforms.Transform

trait Wall:
  def collide(p0: Point2D, p1: Point2D, v: Vect2D): (Point2D, Vect2D, Double)
  def transform(xform: Transforms.Transform): Wall
end Wall

case class LineWall(p: Point2D, q: Point2D) extends Wall:
  val n = Config.BALL_RADIUS * (q - p).unitNormal
  val w0 = p + n
  val w1 = q + n

  def collide(
      p0: Point2D,
      p1: Point2D,
      v: Vect2D
  ): (Point2D, Vect2D, Double) = {
    val dp = p1 - p0
    val dw = w1 - w0
    if ((dp cross dw) > 0) {
      val t = intersect(p0, p1, w0, w1)
      if (0 <= t && t <= 1) {
        val u = intersect(w0, w1, p0, p1)
        if (0 <= u && u <= 1) {
          val p = p0 + t * dp
          val n = dw.unitNormal
          val rv = v - 2 * (v dot n) * n

          return (p, rv, t)
        }
      }
    }

    (p1, v, 1)
  }

  def transform(xform: Transform): Wall = LineWall(xform(p), xform(q))

  private def intersect(
      p0: Point2D,
      p1: Point2D,
      q0: Point2D,
      q1: Point2D
  ): Double = {
    val r = p1 - p0
    val s = q1 - q0
    ((q0 - p0) cross s) / (r cross s)
  }
end LineWall

case class PointWall(p: Point2D) extends Wall:
  def collide(
      p0: Point2D,
      p1: Point2D,
      v: Vect2D
  ): (Point2D, Vect2D, Double) = {
    if (p.distToSegment(p0, p1) <= Config.BALL_RADIUS) {
      val t = intersectCircle(p0, p1, p, Config.BALL_RADIUS)
      val q = p0 + t * (p1 - p0)
      val n = (q - p).unit
      val rv = v - 2 * (v dot n) * n

      (q, rv, t)
    } else {
      (p1, v, 1)
    }
  }

  def transform(xform: Transform): Wall = PointWall(xform(p))

  private def intersectCircle(
      p0: Point2D,
      p1: Point2D,
      q: Point2D,
      r: Double
  ): Double = {
    val d = p1 - p0
    val v = p0 - q

    val a = d dot d
    val b = v dot d
    val c = (v dot v) - r * r

    (-b - math.sqrt(b * b - a * c)) / a
  }
end PointWall
