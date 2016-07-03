trait SuperType {
  def someFun: Int
}

class SubType extends SuperType {
  def someFun = 5
}

class SubSubType extends SubType {
  def subFun: String = "hello"
  override def someFun = 10
}

val sst = new SubSubType
sst.subFun
sst.someFun

// type upper bound
// can take all subtypes of super type
def typedFun[S <: SuperType](input: S) = input.someFun

typedFun(sst)
typedFun(new SubType)

def anotherTypedFun[S >: SubType](input: S) =
  println("in fun anotherTypedFun")

anotherTypedFun(sst)
anotherTypedFun(new SubType)

