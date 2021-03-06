package agh

import scalafx.application.JFXApp3
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.VBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.text.Text
import scalafx.beans.property.IntegerProperty
import scalafx.beans.property.StringProperty
import scalafx.beans.property.ObjectProperty
import scalafx.beans.property.BooleanProperty

import scalafx.scene.control.Button
import scalafx.beans.binding.Bindings
import scalafx.scene.control.TextField
import scalafx.scene.control.ChoiceBox
import javafx.event.EventHandler
import javafx.event.ActionEvent
import javafx.scene.input.KeyEvent
import javafx.scene.input.KeyCode
import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableArray
import scalafx.scene.control.TableColumn._
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert._
import scalafx.scene.control.{TableColumn, TableView}
import agh.Surface._
import agh.DisplayPlayer


object ScalaFXHelloWorld extends JFXApp3 {
  val fetcher = new Fetcher
  val name =  StringProperty("")
  val buttonText = StringProperty("Compare!")
  val players = ObservableBuffer[DisplayPlayer]()
  val selectionProperty = StringProperty("")
  val st = Array("Hard","Grass","Carpet","Clay")
  val surfaces = ObservableBuffer[String]()
  surfaces.addAll(st)
  //val surfaces = new ObjectProperty[ObservableBuffer[String]](surfaceNames, "sfc" )
  //surfaces.addAll("Hard","Grass","Carpet","Clay")
  val textValue = StringProperty("")
    class EnterHandler extends EventHandler[KeyEvent]
  {
    def handle(ev:KeyEvent)={
      if(ev.getCode == KeyCode.ENTER){
        val decoded =  fetcher.decode(textValue())
        if(decoded == null){
        new Alert(AlertType.Information,"No matching players found!"){
        }.showAndWait()
        }else{
          players+= decoded
          textValue.value = ""
        }


      }
    }
  }
  def mapPlayers(p:DisplayPlayer):Int={
    p.id()
  }
  class ButtonHandler extends EventHandler[ActionEvent]
  {
    def handle(ev:ActionEvent)={
      val comparator = new Comparator()
      //val alertMessageProperty = StringProperty("Comparing players...")
      
      new Alert(AlertType.Information,"Comparing players..."){
        resizable() = true
        headerText = "Comparison:"
        contentText = comparator.compareGMM(players.map(mapPlayers).toArray, (Surface.withNameOpt(selectionProperty()).getOrElse(Hard)))
      }.showAndWait()
    }
  }
  val enterHandler  = new EnterHandler
  val buttonHandler = new ButtonHandler
  //val currentId =  IntegerProperty(0)
  //currentId <== fetcher.decode(name.value)
  
  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      //    initStyle(StageStyle.Unified)
      title = "ScalaFX Hello World"
      scene = new Scene {
        fill = Color.rgb(38, 38, 38)
        content = new VBox {
          padding = Insets(50, 80, 50, 80)
          children = Seq(
            new Text {
              text = "Ultimate"
              style = "-fx-font: normal bold 20pt sans-serif"
              fill = new LinearGradient(
                endX = 0,
                stops = Stops(Red, DarkRed))
            },
            new Text {
              text = "Tennis player comparator"
              style = "-fx-font: italic bold 40pt sans-serif"
              fill = new LinearGradient(
                endX = 0,
                stops = Stops(White, DarkGray)
              )
              effect = new DropShadow {
                color = DarkGray
                radius = 15
                spread = 0.25
              }
            },
            new TextField{
              onKeyPressed=enterHandler
              textValue <==> text
            },
            new Button{
              text <== buttonText
              onAction=buttonHandler
            },
            new ChoiceBox(surfaces){
              centerShape = true
              value  <==> selectionProperty
            },
            new TableView[DisplayPlayer](players){
              columns++=Seq(
                new TableColumn[DisplayPlayer,String] {
                  text = "full name"
                  cellValueFactory = _.value.name
                }
                ,new TableColumn[ DisplayPlayer,Int] {
                  text = "id"
                  cellValueFactory = _.value.id
                }
              )
            }
          )
          //children ++= textField
        }
      }
    }
  }
}