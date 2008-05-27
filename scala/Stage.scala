package com.scala_anim
import javax.swing._
import java.awt.event._
import java.awt.image.BufferedImage
import java.awt._
import scala.collection.mutable.ResizableArray
import scala.collection.mutable.ArrayBuffer
import com.scala_anim.geom.Utils._
import com.scala_anim.event._

object Stage{

  var stage:Stage = new Stage()

  def resetStage() = {
    stage = new Stage()
    stage
  }

  def backStage = stage.backStage
  
}


class Stage() extends Sprite {
  override def addToInitialParent(){}
  override def isStage = true
  override def parent = this

  val backStage:BackStage = new BackStage() // Careful, backStage.parent is null here!
  this += backStage

  def resize(w:Float, h:Float){
    width = w
    height = h
    canvas.setFill(Color.gray, 1.0f)
    canvas.setStroke(Color.gray, 1, 1.0f)
    canvas.rect(0, 0, width, height)
  }

  override def dispose() {
    removeAllEventListeners()
    removeAllChildren()
  }

}

class BackStage() extends Sprite {
  override def addToInitialParent(){}
  override protected def redrawPartially(g:Graphics2D) {}
  override protected def redrawCompletely(g:Graphics2D) {}
  override def containsGlobalPoint(pt:Pt):Boolean = false
  override def containsPoint(pt:Pt):Boolean = false
  override def propagateEvent(e:PropagatingEvent){}
  override def propagateEventCaptureOnly(e:PropagatingEvent){}
}
