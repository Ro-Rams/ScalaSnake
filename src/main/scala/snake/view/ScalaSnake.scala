package snake.view

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.CanvasRenderingContext2D

import scala.scalajs.js.timers._
import scala.scalajs.js.JSApp
import scala.util.Random

case class Position(val x: Int, val y: Int)
case class ApplePosition(val position: Position)
case class PowerUpPosition(val position: Position, val powerup: PowerUp)
case class SnakePosition(val position: Position, val direction: String)

case class PowerUp(position: Position, val name: String, val color: String)

object SimulateMove extends JSApp {

  val pointSize = 5

  def main(): Unit = {
    val canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
    run(canvas)
  }

  private def run(canvas: html.Canvas) = {
    val canvasBounds = Position(canvas.width, canvas.height)
    val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    renderer.font = "bold 32px Arial"
    clearCanvas(renderer, canvasBounds)

    var intervalHandle: SetIntervalHandle = null
    var timeoutHandle: SetTimeoutHandle = null
    var score = 0
    var gameOver = false
    var pause = false
    var snakeSize = 3
    var snakeColor = "white"
    val speed = 50
    var renderPositions: List[SnakePosition] = List(SnakePosition(randomPosition(canvasBounds), "Right"))
    var apple = ApplePosition(randomPosition(canvasBounds))
    var powerup = randomPowerup(canvasBounds)
    var direction = "Right"

    dom.document.onkeypress = (e: dom.KeyboardEvent) => {
      dom.console.log(e.key)
      e.key match {
        case "ArrowLeft" | "a" => if (!pause && renderPositions.head.direction != "Right") direction = "Left"
        case "ArrowRight" | "d" => if (!pause && renderPositions.head.direction != "Left") direction = "Right"
        case "ArrowUp" | "w" => if (!pause && renderPositions.head.direction != "Down") direction = "Up"
        case "ArrowDown" | "s" => if (!pause && renderPositions.head.direction != "Up") direction = "Down"
        case "Escape" => pause = !pause
        case " " => pause = !pause
        case "r" if (pause || gameOver) => {
          score = 0
          gameOver = false
          pause = false
          snakeSize = 3
          direction = "Right"
          clearCanvas(renderer, canvasBounds)
          renderPositions = List(SnakePosition(randomPosition(canvasBounds), "Right"))
          apple = ApplePosition(randomPosition(canvasBounds))
        }
      }
    }

    intervalHandle = setInterval(speed) { render }


    def render: Unit = {
      if (!gameOver && !pause) {
        renderPositions = nextPosition(renderPositions.head)
        val head = renderPositions.head
        if (hitEdge(head) || !renderSnake(renderer, renderPositions, snakeSize, snakeColor)) {
          gameOver = true
          showEndScreen(renderer, canvasBounds, renderPositions, score)
        } else {
          if (head.position == apple.position) eatApple
          if (head.position == powerup.position) eatPowerUp
          renderRect(renderer, apple.position, "red")
          renderRect(renderer, powerup.position, powerup.color)
        }
      }

      def nextPosition(head: SnakePosition) = direction match {
        case "Left" => List(SnakePosition(Position(head.position.x - pointSize, head.position.y), "Left")) ::: renderPositions
        case "Right" => List(SnakePosition(Position(head.position.x + pointSize, head.position.y), "Right")) ::: renderPositions
        case "Up" => List(SnakePosition(Position(head.position.x, head.position.y - pointSize), "Up")) ::: renderPositions
        case "Down" => List(SnakePosition(Position(head.position.x, head.position.y + pointSize), "Down")) ::: renderPositions
      }

      def hitEdge(head: SnakePosition) = {
        head.position.x < 0 || head.position.x > canvas.width || head.position.y > canvas.height || head.position.y < 0
      }

      def eatApple = {
        snakeSize = snakeSize + 2
        score = score + 1
        renderRect(renderer, apple.position, "black")
        apple = ApplePosition(randomPosition(canvasBounds))
      }

      def eatPowerUp = {
        powerup match {
          case PowerUp(_, "SlowDown", _) => {
            snakeColor = powerup.color
            clearInterval(intervalHandle)
            if (timeoutHandle != null) clearTimeout(timeoutHandle)
            intervalHandle = setInterval(speed * 2) { render }
            timeoutHandle = setTimeout(10000) {
              clearInterval(intervalHandle)
              snakeColor = "white"
              intervalHandle = setInterval(speed) { render }
            }
          }
          case PowerUp(_, "Teleport", _) => {
            renderPositions = List(SnakePosition(randomPosition(canvasBounds), direction)) ::: renderPositions
            score = score + 1
          }
        }
        renderRect(renderer, powerup.position, "black")
        powerup = randomPowerup(canvasBounds)
      }
    }
  }

  private def randomPowerup(canvasBounds: Position): PowerUp = Random.nextInt(2) match {
    case 0 => PowerUp(randomPosition(canvasBounds), "SlowDown", "Blue")
    case 1 => PowerUp(randomPosition(canvasBounds), "Teleport", "Purple")
  }

  private def clearCanvas(renderer: CanvasRenderingContext2D, canvasBounds: Position) = {
    renderer.fillStyle = "black"
    renderer.fillRect(0, 0, canvasBounds.x, canvasBounds.y)
  }

  private def randomPosition(canvasBounds: Position): Position = Position(Random.nextInt(canvasBounds.x / 5) * 5, Random.nextInt(canvasBounds.y / 5) * 5)

  private def renderSnake(renderer: CanvasRenderingContext2D, renderPositions: List[SnakePosition], snakeSize: Int, snakeColor: String): Boolean = {
    val (white, black) = renderPositions.splitAt(snakeSize)
    var rendered = Set[Position]()
    if (gameOver(white)) return false
    else {
      for (p <- white) {
        renderRect(renderer, p.position, snakeColor)
        rendered = rendered + p.position
      }
      for (p <- black) {
        if (!rendered.contains(p.position)) {
          renderRect(renderer, p.position, "black")
        }
      }
    }
    true
  }

  private def gameOver(renderPositions: List[SnakePosition]) = {
    var rendered = Set[Position]()
    var gameOver = false
    for (p <- renderPositions) {
      if (rendered.contains(p.position)) gameOver = true
      rendered = rendered + p.position
    }
    gameOver
  }

  private def renderRect(renderer: CanvasRenderingContext2D, p: Position, style: String) = {
    renderer.fillStyle = style
    renderer.fillRect(p.x, p.y, pointSize, pointSize)
  }

  private def showEndScreen(renderer: CanvasRenderingContext2D, canvasBounds: Position, renderedLocations: List[SnakePosition], score: Int) = {
    renderer.fillStyle = "black"
    renderer.fillRect(0, 0, canvasBounds.x, canvasBounds.y)
    renderer.fillStyle = "white"
    var timeout = 2
    for (p <- renderedLocations) {
      setTimeout(timeout) {
        renderer.fillRect(p.position.x, p.position.y, pointSize, pointSize)
      }
      timeout = timeout + 2
    }
    setTimeout(timeout + 18) {
      renderer.fillStyle = "green"
      renderer.fillText(score.toString, (canvasBounds.x / 2) - 17, (canvasBounds.y / 2) + 8)
    }
  }
}
