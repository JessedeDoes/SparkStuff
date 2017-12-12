package tokenizer

trait EntityReplacer
{
   def substitute(s:String, mapping:Map[String,String]):String
}
