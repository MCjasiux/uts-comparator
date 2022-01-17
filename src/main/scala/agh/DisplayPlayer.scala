package agh

import scalafx.beans.property.StringProperty
import scalafx.beans.property.ObjectProperty

class DisplayPlayer(name_ : String,id_ : Integer){
  val name = new StringProperty(this, "name", name_ )
  val id = new ObjectProperty[Int](this, "id", id_ )  //nie, nie wolno IntegerProperty, trzeba ObjectProperty[Int]!
}