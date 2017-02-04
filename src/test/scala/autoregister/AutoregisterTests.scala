package autoregister

import utest._
import TestUtils.make

object AutoregisterTests extends TestSuite {
  def tests = TestSuite {
    'samefile{
      'simple - make("samefile/Simple.scala")
      'descendent - make("samefile/Descendent.scala")
      'secondlevel - make("samefile/SecondLevel.scala")
    }
    'independent{
      'simple - make("independent/simple/")
      'descendent - make("independent/descendent/")
      'secondlevel - make("independent/secondlevel/")
    }
  }
}