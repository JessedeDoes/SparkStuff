/**
  * Created by does on 6/30/17.
  */
import scala.xml._
object FCS
{
  def toXML(c:Concordance):Elem =
  {
    <Advanced>
      <Segments>
        {c("word").zipWithIndex.map(
         { case (w,i) => <Segment id={"s" + i}>{c("punct")(i) + w}</Segment>}
        )
        }
      </Segments>
      <Layers>
        {c.tokenProperties.map(
        {case (n,a) =>
          <Layer id={n}>
            {a.zipWithIndex.map( { case (w,i) =>
            if (i >= c.hitStart && i < c.hitEnd)
            <Span highlight="bold" ref={"s" + i}>{w}</Span>
             else
            <Span ref={"s" + i}>{w}</Span>} )}
          </Layer>}
      )
        }
      </Layers>
    </Advanced>
  }
}
