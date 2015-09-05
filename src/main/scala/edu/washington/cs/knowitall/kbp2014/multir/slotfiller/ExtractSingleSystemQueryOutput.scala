package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source

import com.typesafe.config.ConfigFactory


object ExtractSingleSystemQueryOutput {

  val config = ConfigFactory.load("kbp-2015-extract-single-system-query-output.conf")
  val inQueriesFileName_multir = config.getString("in-file-r2-queries-multir")
  val inQueriesFileName_implie = config.getString("in-file-r2-queries-implie")
  val inQueriesFileName_openie = config.getString("in-file-r2-queries-openie")
  val inOutputFileName_multir = config.getString("in-file-r2-output-multir")
  val inOutputFileName_implie = config.getString("in-file-r2-output-implie")
  val inOutputFileName_openie = config.getString("in-file-r2-output-openie")
  val outFileName_multir = config.getString("out-file-single-system-multir")
  val outFileName_implie = config.getString("out-file-single-system-implie")
  val outFileName_openie = config.getString("out-file-single-system-openie")
  val outStatsFileName = config.getString("out-stats-file")
      
  def main(args: Array[String]) {
  
    //val outStreamMultir = new PrintStream(outFileName_multir)
    //val outStreamImplie = new PrintStream(outFileName_implie)
    //val outStreamOpenie = new PrintStream(outFileName_openie)

    val outStatsStream = new PrintStream(outStatsFileName)
    
    // -----------------------------------------------------------------
    // The round2 output is based on round1 output from all 3 systems.
    // This script extracts from the round2 output just those entries
    // which come from a single system's round1 output
    // -----------------------------------------------------------------
    
    // -------------------------------------------
    // Round2 Query IDs
    // -------------------------------------------
    // Does file exist?
    //if (!Files.exists(Paths.get(inQueriesFileName))) {
    //  System.out.println(s"Input file $inQueriesFileName doesn't exist!  " + s"Exiting...")
    //  sys.exit(1)
    //} 
   
    // Read file, line by line to get the round2 query id's,
    // based on a single system's round1 output
    
    // --------------------------
    // Multir
    // --------------------------
    val round2QueryIDsMultir = Source.fromFile(inQueriesFileName_multir).getLines().map(line => {
      //val tokens = line.trim.split("\"")
      val tokens = line.trim.split("=\"")
      //println("tokens size: " + tokens.size)
      if(tokens.size == 2){ 
        tokens(1).split("\"")(0)
      }
      else "badline"
    }).toList.filter(s => s!= "badline")
    
    // --------------------------
    // Implie
    // --------------------------
    val round2QueryIDsImplie = Source.fromFile(inQueriesFileName_implie).getLines().map(line => {
      //val tokens = line.trim.split("\"")
      val tokens = line.trim.split("=\"")
      //println("tokens size: " + tokens.size)
      if(tokens.size == 2){ 
        tokens(1).split("\"")(0)
      }
      else "badline"
    }).toList.filter(s => s!= "badline")
    
    // --------------------------
    // Openie
    // --------------------------
    val round2QueryIDsOpenie = Source.fromFile(inQueriesFileName_openie).getLines().map(line => {
      //val tokens = line.trim.split("\"")
      val tokens = line.trim.split("=\"")
      //println("tokens size: " + tokens.size)
      if(tokens.size == 2){ 
        tokens(1).split("\"")(0)
      }
      else "badline"
    }).toList.filter(s => s!= "badline")
        
    val queriesMultir = KBPQuery.parseKBPQueries(inQueriesFileName_multir, "round2")	 
    val queriesImplie = KBPQuery.parseKBPQueries(inQueriesFileName_implie, "round2")
    val queriesOpenie = KBPQuery.parseKBPQueries(inQueriesFileName_openie, "round2")
    
    println("Multir ids: " + round2QueryIDsMultir.size)
    println("Implie ids: " + round2QueryIDsImplie.size)
    println("Openie ids: " + round2QueryIDsOpenie.size)
    
    println("Multir num parsed queries: " + queriesMultir.size)
    println("Implie num parsed queries: " + queriesImplie.size)
    println("Openie num parsed queries: " + queriesOpenie.size)
    
    val queriesMI = round2QueryIDsMultir.toSet ++ round2QueryIDsImplie.toSet
    val queriesMO = round2QueryIDsMultir.toSet ++ round2QueryIDsOpenie.toSet
    val queriesIO = round2QueryIDsImplie.toSet ++ round2QueryIDsOpenie.toSet    
    
    println("Sum m+i queries: " + queriesMI.size + " vs " + (queriesMultir.size.toInt + queriesImplie.size.toInt))
    println("Sum m+o queries: " + queriesMO.size + " vs " + (queriesMultir.size.toInt + queriesOpenie.size.toInt))
    println("Sum i+o queries: " + queriesIO.size + " vs " + (queriesImplie.size.toInt + queriesOpenie.size.toInt))
    
    //return
    
    
    
    // ---------------------------------------------------------------------------
    // Write new file containing only id's in the round2QueryIDs
    // ---------------------------------------------------------------------------
    // Does file exist?
    //if (!Files.exists(Paths.get(inOutputFileName))) {
    //  System.out.println(s"Input file $inOutputFileName doesn't exist!  " + s"Exiting...")
    //  sys.exit(1)
    //} 
    //---------------------
    // Multir
    //---------------------
    var multirCount = 0
    var notMultirCount = 0
    Source.fromFile(inOutputFileName_multir).getLines().foreach(line => {
      try{
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {                        
            if(round2QueryIDsMultir.contains(tokens(0))){   
              //outStreamMultir.println(line)
              multirCount += 1
            }
            else if(round2QueryIDsImplie.contains(tokens(0)) || round2QueryIDsOpenie.contains(tokens(0))) notMultirCount += 1
          }
          case _ =>   
        }
      }catch{
        case e: Exception =>
      }  
    })   
    //---------------------
    // Implie
    //---------------------
    var implieCount = 0
    var notImplieCount = 0
    Source.fromFile(inOutputFileName_implie).getLines().foreach(line => {
      try{
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {                        
            if(round2QueryIDsImplie.contains(tokens(0))){   
              //outStreamImplie.println(line)
              implieCount += 1
            }
            else if(round2QueryIDsMultir.contains(tokens(0)) || round2QueryIDsOpenie.contains(tokens(0))) notImplieCount += 1
          }
          case _ =>   
        }
      }catch{
        case e: Exception =>
      }  
    })   
    //---------------------
    // Openie
    //---------------------
    var openieCount = 0
    var notOpenieCount = 0
    Source.fromFile(inOutputFileName_openie).getLines().foreach(line => {
      try{
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {                        
            if(round2QueryIDsOpenie.contains(tokens(0))){   
              //outStreamOpenie.println(line)
              openieCount += 1
            }
            else if(round2QueryIDsImplie.contains(tokens(0)) || round2QueryIDsMultir.contains(tokens(0))) notOpenieCount += 1
          }
          case _ =>   
        }
      }catch{
        case e: Exception =>
      }  
    })       
    
    
    // ---------------------------------------
    // Write stats to file
    // ---------------------------------------
    outStatsStream.println("round2QueryIDsMultir size: " + round2QueryIDsMultir.size)
    outStatsStream.println("round2QueryIDsImplie size: " + round2QueryIDsImplie.size)
    outStatsStream.println("round2QueryIDsOpenie size: " + round2QueryIDsOpenie.size)
    
    outStatsStream.println("multir count: " + multirCount)
    outStatsStream.println("not multir count: " + notMultirCount)
    outStatsStream.println("sum: " + (multirCount+notMultirCount))
    
    outStatsStream.println("implie count: " + implieCount)
    outStatsStream.println("not implie count: " + notImplieCount)
    outStatsStream.println("sum: " + (implieCount+notImplieCount))
    
    outStatsStream.println("openie count: " + openieCount)
    outStatsStream.println("not openie count: " + notOpenieCount)
    outStatsStream.println("sum: " + (openieCount+notOpenieCount))
    
    // ---------------------------------------
    // Close output streams
    // ---------------------------------------
    //outStreamMultir.close()
    //outStreamImplie.close()
    //outStreamOpenie.close()
    
    
    //val implieIDSet = round2QueryIDsImplie.toSet
    //val openieIDSet = round2QueryIDsOpenie.toSet

    //implieIDSet.foreach(i => if(openieIDSet.contains(i)) outStatsStream.println(i))
    
    outStatsStream.close()
    println("closed output streams")
    
  }
    
  
}
  
