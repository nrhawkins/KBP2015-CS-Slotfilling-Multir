package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import scala.io.Source
import KBPQueryEntityType._
import java.net.URL
import edu.knowitall.common.Resource

case class Slot(name: String, maxResults: Int, patterns: Seq[SlotPattern]) {
  require(name == name.trim)
  
  def isLocation = if(!name.contains("gpe") && (name.contains("city") || name.contains("country") || 
      name.contains("stateorprovince") || name.contains("cities") || name.contains("countries") || 
      name.contains("states"))) true else false
  def isCountry = if(name.contains("country")) true else false
  def isCity = if(name.contains("city")) true else false
  def isStateOrProvince = if(name.contains("stateorprovince")) true else false
  def isCountryList = if(name.contains("countries")) true else false
  def isCityList = if(name.contains("cities")) true else false
  def isStateOrProvinceList = if(name.contains("states")) true else false
  def isList = if(maxResults > 1) true else false
  def isDate = if(name.contains("date")) true else false
  def isAlternateName = if(name.contains("alternate_names")) true else false
  def isCauseOfDeath = if(name.contains("cause_of_death")) true else false
  def isTitle = name.equals("per:title")
  
  def isPerson = if(name.equals("per:alternate_names") || name.equals("per:spouse") || 
      name.equals("per:children") || name.equals("per:parents") || name.equals("per:siblings") || 
      name.equals("per:other_family") || name.equals("org:members") || name.equals("org:shareholders") ||
      name.equals("org:founded_by") || name.equals("org:top_members_employees") || 
      (name.contains("gpe:") && !name.contains("headquarters") )) true else false
  def isOrganization = if(name.equals("per:employee_or_member_of") || name.equals("org:alternate_names") ||
      name.equals("org:member_of") || name.equals("per:schools_attended") || 
      name.equals("org:subsidiaries") || name.equals("org:parents") ||
      name.contains("gpe:headquarters") ) true else false
  def isGPE = if(isLocation) true else false     
      
  def slotfillType = if(isGPE) "GPE" else if(isPerson) "PER" else if(isOrganization) "ORG" else "STRING"  
  
}

object Slot {
  
  private def requireResource(urlString: String): URL = {
    val url = getClass.getResource(urlString)
    require(url != null, "Could not find resource: " + urlString)
    url
  }
  
  private val personResource = "/edu/washington/cs/knowitall/kbp2014/multir/slotfiller/PersonSlotTypes.txt"
  private def personUrl = requireResource(personResource)
  private def personPatternUrl = requireResource(SlotPattern.personPatternResource)
  
  private def organizationResource = "/edu/washington/cs/knowitall/kbp2014/multir/slotfiller/OrganizationSlotTypes.txt"
  private def organizationUrl = requireResource(organizationResource)
  private val organizationPatternUrl = requireResource(SlotPattern.organizationPatternResource)

  private def gpeResource = "/edu/washington/cs/knowitall/kbp2014/multir/slotfiller/GeoPoliticalEntitySlotTypes.txt"
  private def gpeUrl = requireResource(gpeResource)
  private val gpePatternUrl = requireResource(SlotPattern.gpePatternResource)
  
  private def loadSlots(slotUrl: URL, patternUrl: URL, slotPrefix: String): Set[Slot] = {
    
    Resource.using(Source.fromURL(slotUrl)) { slotSource =>
      Resource.using(Source.fromURL(patternUrl)) { patternSource =>
        // filter and split pattern lines
        val validPatternLines =
          patternSource.getLines.drop(1).map(_.trim).filter(_.contains(slotPrefix)).filterNot(_.startsWith(","))
          
        val patternFields = validPatternLines.map(_.replace(",", " ,").split(",").map(_.trim)).toStream
        // group by slot name
        val slotPatterns = patternFields.groupBy(_(0))
        slotSource.getLines.filter(_.nonEmpty).map(_.trim).map { slotName =>
          fromNameAndPatterns(slotName, slotPatterns.getOrElse(slotName, Seq.empty))
        } toSet
      }
    }
  }
  
  private def fromNameAndPatterns(slotString: String, patternFields: Seq[Array[String]]): Slot = {
    val headPattern = patternFields.head
    
    val maxValues = headPattern(1).toInt
    // Array(slotName, maxValues, relString, arg2Begins, entityIn, slotFillIn, slotType, _*)
    val slotType = {
      val field = if (headPattern.length >= 7) headPattern(6).trim else ""
      if (field.isEmpty()) None else Some(field)
    }
    
    val patterns = patternFields.flatMap(fields => SlotPattern.read(fields)).distinct
    
    Slot(slotString, maxValues, patterns)
  }
  
  
  lazy val personSlots = loadSlots(personUrl, personPatternUrl, "per:")

  lazy val orgSlots = loadSlots(organizationUrl, organizationPatternUrl, "org:")
  
  lazy val gpeSlots = loadSlots(gpeUrl, gpePatternUrl, "gpe:")
  
  lazy val allSlots = personSlots ++ orgSlots ++ gpeSlots

  
  def fromName(name: String) =
    allSlots.find(_.name == name).getOrElse { throw new RuntimeException("Invalid slot name: " + name) }
  
  def getSlotTypesList(kbpQueryEntityType: KBPQueryEntityType) = {
    kbpQueryEntityType match {
      case GPE => gpeSlots
      case ORG => orgSlots
      case PER => personSlots
    }
  }
  
  def addPatterns(slots: Set[Slot], patterns: Iterable[SlotPattern]): Set[Slot] = {
    val patternsMap = patterns.groupBy(_.slotName)
    
    slots.map { slot => slot.copy(patterns = slot.patterns ++ patternsMap.getOrElse(slot.name, Nil))}
  }
}


