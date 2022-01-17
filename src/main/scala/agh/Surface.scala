package agh

object Surface extends Enumeration{
    type Surface = Value
    val Hard, Grass, Carpet, Clay = Value
    def withNameOpt(s:String):Option[Value] = values.find(_.toString == s)
}
