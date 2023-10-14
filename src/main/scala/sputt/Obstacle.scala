package sputt

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.Image

import sputt.Points.Point2D

trait Obstacle:
  def wallsAt(t: Double): Seq[Wall]

  def render(ctx: CanvasRenderingContext2D, t: Double): Unit
end Obstacle

// List the vertices in clockwise order
case class Boundary(vertices: Point2D*) extends Obstacle:
  val walls = (vertices map (p => PointWall(p)))
    ++ ((vertices zip (vertices.tail :+ vertices.head)) map ((p, q) =>
      LineWall(p, q)
    ))

  def wallsAt(t: Double): Seq[Wall] = walls

  def render(ctx: CanvasRenderingContext2D, t: Double): Unit = {
    ctx.save()

    ctx.beginPath()
    ctx.moveTo(vertices(0).x, vertices(0).y)
    for (v <- vertices.tail) {
      ctx.lineTo(v.x, v.y)
    }
    ctx.closePath()
    ctx.clip()

    ctx.lineWidth = 3
    ctx.lineJoin = "round"
    ctx.strokeStyle = "rgbs(0, 255, 0, 0.5)"
    ctx.stroke()

    ctx.lineWidth = 1
    ctx.strokeStyle = "black"
    ctx.stroke()

    ctx.restore()
  }
end Boundary

// List the vertices in counter-clockwise order
case class PolyObstacle(vertices: Point2D*) extends Obstacle:
  val walls = (vertices map (p => PointWall(p)))
    ++ ((vertices zip (vertices.tail :+ vertices.head)) map ((p, q) =>
      LineWall(p, q)
    ))

  def wallsAt(t: Double): Seq[Wall] = walls

  def render(ctx: CanvasRenderingContext2D, t: Double): Unit = {
    ctx.save()

    ctx.beginPath()
    ctx.moveTo(vertices(0).x, vertices(0).y)
    for (v <- vertices.tail) {
      ctx.lineTo(v.x, v.y)
    }
    ctx.closePath()

    ctx.lineWidth = 3
    ctx.lineJoin = "round"
    ctx.strokeStyle = "rgba(0, 255, 0, 0.5)"
    ctx.stroke()

    ctx.lineWidth = 1
    ctx.strokeStyle = "black"
    ctx.stroke()

    ctx.clip()
    ctx.globalCompositeOperation = "copy"
    ctx.fillStyle = "rgba(0, 0, 0, 0)"
    ctx.fill()

    ctx.restore()
  }
end PolyObstacle

case class OneWay(p: Point2D, q: Point2D) extends Obstacle:
  val walls = Seq(PointWall(p), LineWall(p, q), PointWall(q))

  def wallsAt(t: Double): Seq[Wall] = walls

  def render(ctx: CanvasRenderingContext2D, t: Double): Unit = {
    ctx.save()

    val n = 10 * (q - p).unitNormal
    val p2 = p + n
    val q2 = q + n

    ctx.beginPath()
    ctx.moveTo(p.x, p.y)
    ctx.lineTo(q.x, q.y)
    ctx.lineTo(q2.x, q2.y)
    ctx.lineTo(p2.x, p2.y)
    ctx.closePath()
    ctx.clip()

    ctx.beginPath()
    ctx.moveTo(p.x, p.y)
    ctx.lineTo(q.y, q.y)

    ctx.lineWidth = 3
    ctx.lineCap = "round"
    ctx.strokeStyle = "rgba(255, 255, 0, 0.5)"
    ctx.stroke()

    ctx.lineWidth = 1
    ctx.strokeStyle = "black"
    ctx.stroke()

    ctx.restore()
  }
end OneWay

case class Sprite(x: Double, y: Double, w: Double, h: Double, img: Image)
    extends Obstacle:
  val walls = {
    val TL = Point2D(x, y)
    val BL = Point2D(x, y + h)
    val TR = Point2D(x + w, y)
    val BR = Point2D(x + w, y + h)

    Seq(
      PointWall(TL),
      LineWall(TL, BL),
      PointWall(BL),
      LineWall(BL, BR),
      PointWall(BR),
      LineWall(BR, TR),
      PointWall(TR),
      LineWall(TR, TL)
    )
  }
  def wallsAt(t: Double): Seq[Wall] = walls

  def render(ctx: CanvasRenderingContext2D, t: Double): Unit = {
    ctx.drawImage(img, x, y, w, h)
  }
end Sprite

case class TransformObstacle(
    obstacle: Obstacle,
    fun: Double => Transforms.Transform
) extends Obstacle:
  def wallsAt(t: Double): Seq[Wall] = {
    val xform = fun(t)
    val walls = obstacle.wallsAt(t)

    walls map (_.transform(xform))
  }

  def render(ctx: CanvasRenderingContext2D, t: Double): Unit = {
    ctx.save()
    val xform = fun(t)
    ctx.transform(xform.a, xform.b, xform.c, xform.d, xform.e, xform.f)
    obstacle.render(ctx, t)
    ctx.restore()
  }
end TransformObstacle
