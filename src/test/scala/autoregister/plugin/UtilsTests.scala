package autoregister.plugin

import utest._

object UtilsTests extends TestSuite {
  def tests = TestSuite {
    import Utils._
    case class N(name: String, nodes: N*)
    def p(positiveResults: String*)(negativeResults: String*)(toTest: N) {
      val results = positiveResults.map(r => (r, true)) ++ negativeResults.map(r => (r, false))
      for {
        (key, value) <- results
      } assert(toTest.treeCollectFirst((_: N).nodes.toList, s => Option(s.name).filter(key == _)).isDefined == value)
    }
    "1" - {
      p("A")("Z") {
        N("A")
      }
    }
    "2" - {
      p("A", "B", "C", "D")("Z") {
        N(
          "A",
          N("B"),
          N("C"),
          N("D")
        )
      }
    }
    "3" - {
      p("A", "B", "C")("Z") {
        N(
          "A",
          N(
            "B",
            N("C")
          )
        )
      }
      p("A", "B", "C", "D", "E")("Z") {
        N(
          "A",
          N(
            "B",
            N("C")
          ),
          N(
            "D",
            N("E")
          )
        )
      }
      p("A", "B", "C", "D", "E", "F", "G")("Z") {
        N(
          "A",
          N(
            "B",
            N("C"),
            N("D")
          ),
          N(
            "E",
            N("F"),
            N("G")
          )
        )
      }
    }
    //treeCollectFirst
  }
}