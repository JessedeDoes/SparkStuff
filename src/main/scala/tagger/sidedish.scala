package tagger

import scala.xml.{Node, Text}

object sidedish
{
  def getTextButNotIn(x:Node,tagName:String):String =
   {
       if (x.isInstanceOf[Text])
         x.text
       else
         x.child.filter(_.label != tagName).map(getTextButNotIn(_,tagName)).mkString("")
   }
}
