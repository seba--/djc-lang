package util.vis

import javafx.embed.swing.JFXPanel

import djc.lang.TypedLanguage

import scalafx.Includes._
import scalafx.application.{Platform, JFXApp}
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Orientation}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.control.TreeView._
import scalafx.scene.control.TreeCell._
import scalafx.scene.control.TableColumn._

import scalafx.scene.layout.{StackPane, AnchorPane}
import scalafx.scene.layout.AnchorPane._
import scalafx.scene.layout.StackPane._
import scalafx.scene.control.SplitPane
import scalafx.scene.control.SplitPane._
import scalafx.scene.control.ScrollPane
import scalafx.scene.control.ScrollPane._
import scalafx.scene.control.TitledPane
import scalafx.scene.control.TitledPane._
import scalafx.stage
import scalafx.stage.Stage


/**
 * Visualize trees for debugging purposes
 *
 * @see Model
 */
class TreeInspector(tree: ModelNode) {
  type Field = (String, Any)

  val treeView = new TreeView[ModelNode] {
    minWidth = 100
    editable = false
    cellFactory = { v =>
      new TreeCell[ModelNode] {
        treeItem.onChange {
          (_, _, p) =>
            text = if (p != null) p.value().name else ""
        }
      }
    }
    selectionModel().selectedItem.onChange {
      (_,_,newVal) =>
        tableView.items = if (newVal != null) newVal.value().fieldSeq else ObservableBuffer()
        textRep.text = if (newVal != null) newVal.value().rep.toString else ""
    }
    root = toTreeItem(tree)
    showRoot = true
  }
  val textRep = new TextArea {
    wrapText = true
    editable = false
  }
  val textRepScrollPane = new ScrollPane {
    content = textRep
  }
  val textRepTitle = new TitledPane {
    text = "String Representation"
    collapsible = false
    animated = false
    content = new StackPane {
      content = textRepScrollPane
    }
  }
  val fieldCol = new TableColumn[Field, String] {
    text = "Field"
    editable = false
    cellValueFactory = { x => ObjectProperty[String](x.value._1) }
  }
  val valueCol = new TableColumn[Field, String] {
    text = "Value"
    editable = false
    cellValueFactory = { x => ObjectProperty[String](x.value._2.toString) }
  }
  val tableView = new TableView[Field](ObservableBuffer(tree.fields.toSeq)) {
    editable = false
    columns +=(fieldCol, valueCol)
    placeholder = new Label("No fields available")
  }
  val tableScrollPane = new ScrollPane {
    content = tableView
  }

  val sceneRoot = new StackPane {
    prefWidth = 1024
    prefHeight = 768
    content = List(new SplitPane {
      dividerPositions = 0.5
      items.addAll(
        new StackPane {
          content = treeView
        },
        new StackPane {
          content = new SplitPane {
            dividerPositions = 0.5
            orientation = Orientation.VERTICAL
            items.addAll(
              new StackPane {
                content = tableScrollPane
              },
              new AnchorPane {
                content = textRepTitle
                setAnchors(textRepTitle, 0, 0, 0, 0)
              })
          }
        })
    })
  }

  textRep.prefWidth    <== textRepScrollPane.width
  textRep.prefHeight   <== textRepScrollPane.height
  tableView.prefHeight <== tableScrollPane.height
  tableView.prefWidth  <== tableScrollPane.width

  val stage = new Stage {
    title = "Tree Inspector"
    scene = new Scene {
      root = sceneRoot
    }
  }

  private def toTreeItem(tree: ModelNode): TreeItem[ModelNode] = new TreeItem(tree) {
    expanded = false
    children = {
      tree.children map { toTreeItem(_) }
    }
  }
}

object TreeInspector {
  def apply(tree: ModelNode) = {
    //forces initialization of jfx
    new JFXPanel()

    Platform.runLater {
      val inspector = new TreeInspector(tree)
      inspector.stage.showAndWait()
    }
  }
}

object Inspector extends App {
  val model = Model.apply[TypedLanguage.type](djc.lang.lib.combinators.recovery.MkRecover.impl)
  TreeInspector(model)
}
