package sputt

import sputt.Points.Point2D

trait Obstacle:
  def wallsAt(t: Double): Seq[Wall]

  def render(): Unit // TODO
end Obstacle

case class Boundary(vertices: Point2D*) extends Obstacle:
    val walls = (vertices map (p => PointWall(p)))
        ++ ((vertices zip (vertices.tail :+ vertices.head)) map ((p, q) => LineWall(p, q)))

    def wallsAt(t: Double): Seq[Wall] = walls

    def render(): Unit = ???