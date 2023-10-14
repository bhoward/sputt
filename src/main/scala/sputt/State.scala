package sputt

import sputt.Points.Point2D
import sputt.Vectors.Vect2D
import org.scalajs.dom.HTMLCanvasElement

// Temporary
trait Hole {
  val tee: Point2D
  val goal: Point2D
  val goalRadius: Double
  val obstacles: Seq[Obstacle]
  def surface(p: Point2D): Surface
}

trait Surface {
  val gravity: Vect2D
  val friction: Double
}

final case class State(
    hole: Hole,
    ball: Point2D,
    velocity: Vect2D,
    shots: Int,
    done: Boolean,
    stop: Boolean,
    t: Double
):
  def render(canvas: HTMLCanvasElement): Unit = ???

  def hit(v: Vect2D): State = {
    // TODO play puttSound -- move outside?
    return this.copy(velocity = v, shots = shots + 1)
  }

  def step(newt: Double): State = {
    if (stop) return this

    val dt = newt - t
    val surf = hole.surface(ball)
    val v1 = velocity + surf.gravity * dt

    val reducedSpeed = (v1.mag - surf.friction * dt) max 0
    val v = reducedSpeed * v1.unit

    val newball = ball + v * dt
    val d = newball - ball

    if (d.mag > 0) {
      val x = for
        obstacle <- hole.obstacles
        wall <- obstacle.wallsAt(newt)
      yield wall.collide(ball, newball, v)

      val (pColl, vColl, tColl) = x.minBy(_._3)

      // TODO play collisionSound if tColl < 1 -- move outside?

      if (hole.goal.distToSegment(ball, pColl) < hole.goalRadius) {
        // TODO play sinkSound -- move outside?
        this.copy(
          ball = hole.goal,
          velocity = Vect2D(0, 0),
          t = newt,
          done = true
        )
      } else {
        this.copy(ball = pColl, velocity = vColl, t = t + tColl * dt)
      }
    } else {
      this.copy(ball = newball, velocity = v, t = newt)
    }
  }
end State
