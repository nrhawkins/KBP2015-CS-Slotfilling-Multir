package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

object KBPQueryEntityType extends Enumeration{
  type KBPQueryEntityType = Value
  val GPE, ORG, PER = Value
  
  def fromString(str: String) = str.trim.toLowerCase match {
    case "gpe" | "geopoliticalentity" => GPE
    case "org" | "organization" => ORG
    case "per" | "person" => PER
    case _ => throw new RuntimeException(s"Invalid KBPQueryEntityType: $str")
  }
  
  def toString(t: KBPQueryEntityType) = t match {
    case GPE => "GPE"
    case ORG => "ORG"
    case PER => "PER"
  }
}
