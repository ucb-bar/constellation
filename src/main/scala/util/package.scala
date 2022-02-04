package constellation
package object util {
  object ArbiterPolicy extends Enumeration {
    type ArbiterPolicy = Value
    val LowestFirst, RoundRobin, Random = Value
  }
}
