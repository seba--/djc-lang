package util.vis

import javafx.embed.swing.JFXPanel

import com.sun.xml.internal.ws.policy.sourcemodel.ModelNode

import scalafx.Includes._
import scalafx.application.Platform
import scalafx.scene.Scene
import scalafx.scene.layout.StackPane
import scalafx.scene.web.WebView
import scalafx.stage.Stage


/**
 * Visualize trees for debugging purposes
 *
 * @see Model
 */
class TreeInspector(tree: Any, winTitle: String) {
  val webView = new WebView {
    engine.loadContent(HighlightSyntax(tree))
  }

  val sceneRoot = new StackPane {
    prefWidth = 1024
    prefHeight = 768
    content = webView
  }

  val stage = new Stage {
    title = "Tree Inspector: " + winTitle
    scene = new Scene {
      root = sceneRoot
    }
  }
}

object TreeInspector {
  def apply(tree: Any, title: String) = {
    //forces initialization of jfx
    new JFXPanel()

    Platform.runLater {
      val inspector = new TreeInspector(tree, title)
      inspector.stage.showAndWait()
    }
  }
}

object Inspector extends App {
  val model = djc.lang.lib.combinators.recovery.MkRecover.impl
  TreeInspector(model, "MkRecover")
}
