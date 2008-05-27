)package com.scala_anim
import java.awt.image.BufferedImage
import java.awt.Color
import scala.collection.mutable.ResizableArray
import scala.collection.mutable.ArrayBuffer
import com.scala_anim.canvas.Canvas
import com.scala_anim.event._
import com.scala_anim.geom.Conversions._

trait Draggable extends SceneTree{

  protected var dragListener:EventListener = { e => }
  protected var preDragX:Float = 0
  protected var preDragY:Float = 0

  // TODO Do we e:Event to be able to get the mouse position?
  protected def startDrag(e:Event) = {
    stage.removeEventListener(MouseEvent.MOUSE_DRAGGED, dragListener) 

    preDragX = x
    preDragY = y

    val m = e.asInstanceOf[MouseEvent]
    val origin = originGlobalized 
    val offset = (m.x - origin.x, m.y - origin.y)
    val self = this

    dragListener = { e =>
      e.stopPropagation
      val m = e.asInstanceOf[MouseEvent]
      val newPt = parent.globalToLocal((m.x - offset._1, m.y - offset._2))
      x = newPt.x
      y = newPt.y
    }
    stage.addEventListener(MouseEvent.MOUSE_DRAGGED, dragListener) 
  }

  protected def stopDrag(){
    stage.removeEventListener(MouseEvent.MOUSE_DRAGGED, dragListener)
  }
  
}
