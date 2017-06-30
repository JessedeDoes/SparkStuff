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
        {klooi.withOffsetsAndIndexDiscountedForExtraBlank(c("word")).map(
         { case ((w,s,e),i) => <Segment start={s.toString} end={e.toString} id={"s" + i}>{w}</Segment>}
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
            <Span ref={"s" + i}>{w}</Span>} )}adetu
          </Layer>}
      )
        }
      </Layers>
    </Advanced>
  }
}
