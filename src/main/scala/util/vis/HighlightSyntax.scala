package util.vis

import java.util.Locale

import scala.io.Source

object HighlightSyntax {
  val codeFont = if (isMac) "Menlo" else "monospace"
  //val highlightjs = loadResourceAsString(this, "/highlight/highlight.js")
  val jquery = loadResourceAsString(this, "/highlight/jquery.js")
  val css = loadResourceAsString(this, "/highlight/github.css")
  val fold = loadResourceAsString(this, "/highlight/fold.js")

  val html =
  <html>
    <head>
      <script type="text/javascript">
        @@jquery@@
      </script>
      <style>
        @@css@@
        pre code {{
          font: normal 12pt {codeFont};
        }}
        .selected {{
          background: #d3d3d3;
        }}
        .child.collapsed {{
          background: yellow !important;
        }}
        .fold.collapsed {{
          color: red !important;
        }}
        .fold {{
          cursor: pointer;
        }}
      </style>
    </head>
    <body>
      <pre><code class="scala hljs">@@code@@</code></pre>
      <script type="text/javascript">
        @@fold@@
      </script>
    </body>
  </html>

  lazy val htmlSource = html.mkString
    .replace("@@css@@", css).replace("@@jquery@@", jquery).replace("@@fold@@", fold)

  def apply(any: Any): String = {
    val scalaCode = HTMLPrettyPrint.pretty_any(any)
    val src = htmlSource.replace("@@code@@", scalaCode)

    src
  }

  private def isMac: Boolean = {
    val os = System.getProperty("os.name").toLowerCase(new Locale(""))
    os.indexOf("mac") >= 0
  }
  private def loadResourceAsString(reference: Any, path: String): String = {
    val in = reference.getClass.getResourceAsStream(path)
    Source.fromInputStream(in).mkString
  }
}
