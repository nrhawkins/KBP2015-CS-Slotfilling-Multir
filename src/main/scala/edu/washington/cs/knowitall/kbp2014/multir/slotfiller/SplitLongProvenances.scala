package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source

import com.typesafe.config.ConfigFactory


object SplitLongProvenances {

  val config = ConfigFactory.load("kbp-2015-split-long-provenances.conf")

  val inFileName_combine_r1 = config.getString("in-file-combine_r1")
  val inFileName_combine_r2 = config.getString("in-file-combine_r2")

  val inFileName_multir_r1 = config.getString("in-file-multir_r1")
  val inFileName_multir_r2 = config.getString("in-file-multir_r2")
  
  val inFileName_implie_r1 = config.getString("in-file-implie_r1")
  val inFileName_implie_r2 = config.getString("in-file-implie_r2")
  
  val inFileName_openie_r1 = config.getString("in-file-openie_r1")
  val inFileName_openie_r2 = config.getString("in-file-openie_r2")
  
  val outFileName_combine_r1 = config.getString("out-file-combine_r1")
  val outFileName_combine_r2 = config.getString("out-file-combine_r2")
  
  val outFileName_multir_r1 = config.getString("out-file-multir_r1")
  val outFileName_multir_r2 = config.getString("out-file-multir_r2")
  
  val outFileName_implie_r1 = config.getString("out-file-implie_r1")
  val outFileName_implie_r2 = config.getString("out-file-implie_r2")
  
  val outFileName_openie_r1 = config.getString("out-file-openie_r1")
  val outFileName_openie_r2 = config.getString("out-file-openie_r2")
  
  val outStatsFileName = config.getString("out-stats-file")
      
  def main(args: Array[String]) {
    
    val outCombineR1Stream = new PrintStream(outFileName_combine_r1)
    val outCombineR2Stream = new PrintStream(outFileName_combine_r2)
    
    val outMultirR1Stream = new PrintStream(outFileName_multir_r1)
    val outMultirR2Stream = new PrintStream(outFileName_multir_r2)
    
    val outImplieR1Stream = new PrintStream(outFileName_implie_r1)
    val outImplieR2Stream = new PrintStream(outFileName_implie_r2)
    
    val outOpenieR1Stream = new PrintStream(outFileName_openie_r1)
    val outOpenieR2Stream = new PrintStream(outFileName_openie_r2)    
    
    val outStatsStream = new PrintStream(outStatsFileName)
    
    // ---------------------------------------------------------------------------
    // Check the lines for provenance offset spans which are > 150 chars
    // ---------------------------------------------------------------------------
    
    //---------------------
    // Combine - R1
    //---------------------
    var lineCountCombine_r1 = 0
    var tooLongCombine_r1 = 0
    var maxProvLengthCombine_r1 = 0
    Source.fromFile(inFileName_combine_r1).getLines().foreach(line => {
      try{
        lineCountCombine_r1 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountCombine_r1 + " " + provLength)
            if(provLength > 150){ 
             tooLongCombine_r1 += 1 
             if(provLength > maxProvLengthCombine_r1) maxProvLengthCombine_r1 = provLength                                  

             val splitProv = splitProvenance(docid,start,end)
               outCombineR1Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                 "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outCombineR1Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   

    //---------------------
    // Combine - R2
    //---------------------
    var lineCountCombine_r2 = 0
    var tooLongCombine_r2 = 0
    var maxProvLengthCombine_r2 = 0
    Source.fromFile(inFileName_combine_r2).getLines().foreach(line => {
      try{
        lineCountCombine_r2 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountCombine_r2 + " " + provLength)
            if(provLength > 150){ 
              tooLongCombine_r2 += 1 
              if(provLength > maxProvLengthCombine_r2) maxProvLengthCombine_r2 = provLength                                  
              val splitProv = splitProvenance(docid,start,end)
                outCombineR2Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outCombineR2Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   
    
    //---------------------
    // Multir - R1
    //---------------------
    var lineCountMultir_r1 = 0
    var tooLongMultir_r1 = 0
    var maxProvLengthMultir_r1 = 0
    Source.fromFile(inFileName_multir_r1).getLines().foreach(line => {
      try{
        lineCountMultir_r1 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountMultir_r1 + " " + provLength)
            if(provLength > 150){ 
              tooLongMultir_r1 += 1 
              if(provLength > maxProvLengthMultir_r1) maxProvLengthMultir_r1 = provLength
              val splitProv = splitProvenance(docid,start,end)
                outMultirR1Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outMultirR1Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   

    //---------------------
    // Multir - R2
    //---------------------
    var lineCountMultir_r2 = 0
    var tooLongMultir_r2 = 0
    var maxProvLengthMultir_r2 = 0
    Source.fromFile(inFileName_multir_r2).getLines().foreach(line => {
      try{
        lineCountMultir_r2 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountMultir_r2 + " " + provLength)
            if(provLength > 150){ 
              tooLongMultir_r2 += 1 
              if(provLength > maxProvLengthMultir_r2) maxProvLengthMultir_r2 = provLength                                  
              val splitProv = splitProvenance(docid,start,end)
                outMultirR2Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outMultirR2Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   
    
    //---------------------
    // Implie - R1
    //---------------------
    var lineCountImplie_r1 = 0
    var tooLongImplie_r1 = 0
    var maxProvLengthImplie_r1 = 0
    Source.fromFile(inFileName_implie_r1).getLines().foreach(line => {
      try{
        lineCountImplie_r1 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountImplie_r1 + " " + provLength)
            if(provLength > 150){ 
              tooLongImplie_r1 += 1 
              if(provLength > maxProvLengthImplie_r1) maxProvLengthImplie_r1 = provLength                                  
              val splitProv = splitProvenance(docid,start,end)
                outImplieR1Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outImplieR1Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   

    //---------------------
    // Implie - R2
    //---------------------
    var lineCountImplie_r2 = 0
    var tooLongImplie_r2 = 0
    var maxProvLengthImplie_r2 = 0
    Source.fromFile(inFileName_implie_r2).getLines().foreach(line => {
      try{
        lineCountImplie_r2 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountImplie_r2 + " " + provLength)
            if(provLength > 150){ 
              tooLongImplie_r2 += 1 
              if(provLength > maxProvLengthImplie_r2) maxProvLengthImplie_r2 = provLength                                  
              val splitProv = splitProvenance(docid,start,end)
                outImplieR2Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outImplieR2Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   
    
    //---------------------
    // Openie - R1
    //---------------------
    var lineCountOpenie_r1 = 0
    var tooLongOpenie_r1 = 0
    var maxProvLengthOpenie_r1 = 0
    Source.fromFile(inFileName_openie_r1).getLines().foreach(line => {
      try{
        lineCountOpenie_r1 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountOpenie_r1 + " " + provLength)
            if(provLength > 150){ 
              tooLongOpenie_r1 += 1 
              if(provLength > maxProvLengthOpenie_r1) maxProvLengthOpenie_r1 = provLength                                  
              val splitProv = splitProvenance(docid,start,end)
                outOpenieR1Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outOpenieR1Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   

    //---------------------
    // Openie - R2
    //---------------------
    var lineCountOpenie_r2 = 0
    var tooLongOpenie_r2 = 0
    var maxProvLengthOpenie_r2 = 0
    Source.fromFile(inFileName_openie_r2).getLines().foreach(line => {
      try{
        lineCountOpenie_r2 += 1
        val tokens = line.trim.split("\t")
        tokens.size match {
          case s if s == 8 => {               
            
            val docid = tokens(3).split(":")(0)
            val start = tokens(3).split(":")(1).split("-")(0).toInt
            val end = tokens(3).split(":")(1).split("-")(1).toInt
            val provLength = end-start+1
            //println(lineCountOpenie_r2 + " " + provLength)
            if(provLength > 150){ 
              tooLongOpenie_r2 += 1 
              if(provLength > maxProvLengthOpenie_r2) maxProvLengthOpenie_r2 = provLength                                  
              val splitProv = splitProvenance(docid,start,end)
                outOpenieR2Stream.println(tokens(0)+ "\t" + tokens(1) + "\t" + tokens(2) + "\t" + splitProv + "\t" + tokens(4) + "\t" + tokens(5) + 
                  "\t" + tokens(6) + "\t" + tokens(7))
            }
            else{
              outOpenieR2Stream.println(line)
            }
          }
          case _ => println("Error: found " + tokens.size + " tokens")  
        }
      }catch{
        case e: Exception =>
      }  
    })   
    
    // ---------------------------------------
    // Write stats to file
    // ---------------------------------------

    outStatsStream.println("combine r1 (line count, too long count, max prov length): " + lineCountCombine_r1 + " " + tooLongCombine_r1 + " " + maxProvLengthCombine_r1)
    outStatsStream.println("combine r2 (line count, too long count, max prov length): " + lineCountCombine_r2 + " " + tooLongCombine_r2 + " " + maxProvLengthCombine_r2)
    outStatsStream.println("combine (line count, too long count): " + (lineCountCombine_r1+lineCountCombine_r2) + " " + (tooLongCombine_r1+tooLongCombine_r2))
    
    outStatsStream.println("multir r1 (line count, too long count, max prov length): " + lineCountMultir_r1 + " " + tooLongMultir_r1 + " " + maxProvLengthMultir_r1)
    outStatsStream.println("multir r2 (line count, too long count, max prov length): " + lineCountMultir_r2 + " " + tooLongMultir_r2 + " " + maxProvLengthMultir_r2)
    outStatsStream.println("multir (line count, too long count): " + (lineCountMultir_r1+lineCountMultir_r2) + " " + (tooLongMultir_r1+tooLongMultir_r2))
    
    outStatsStream.println("implie r1 (line count, too long count, max prov length): " + lineCountImplie_r1 + " " + tooLongImplie_r1 + " " + maxProvLengthImplie_r1)
    outStatsStream.println("implie r2 (line count, too long count, max prov length): " + lineCountImplie_r2 + " " + tooLongImplie_r2 + " " + maxProvLengthImplie_r2)
    outStatsStream.println("implie (line count, too long count): " + (lineCountImplie_r1+lineCountImplie_r2) + " " + (tooLongImplie_r1+tooLongImplie_r2))
    
    outStatsStream.println("openie r1 (line count, too long count, max prov length): " + lineCountOpenie_r1 + " " + tooLongOpenie_r1 + " " + maxProvLengthOpenie_r1)
    outStatsStream.println("openie r2 (line count, too long count, max prov length): " + lineCountOpenie_r2 + " " + tooLongOpenie_r2 + " " + maxProvLengthOpenie_r2)
    outStatsStream.println("openie (line count, too long count): " + (lineCountOpenie_r1+lineCountOpenie_r2) + " " + (tooLongOpenie_r1+tooLongOpenie_r2))
    
    // ---------------------------------------
    // Close output streams
    // ---------------------------------------
    outStatsStream.close()
   
    println("closed output streams")
   
    //doesn't split 
    val testProv1 = "NYT_ENG_20130819.0084:573-722"
    println("testProv: " + testProv1 + " " + splitProvenance("NYT_ENG_20130819.0084",573,722))    
    //splits into two
    val testProv2 = "NYT_ENG_20130819.0084:573-835"
    println("testProv: " + testProv2 + " " + splitProvenance("NYT_ENG_20130819.0084",573,835))    
    //splits into three
    val testProv3 = "NYT_ENG_20130819.0084:573-900"
    println("testProv: " + testProv3 + " " + splitProvenance("NYT_ENG_20130819.0084",573,900))    
   //splits into four    
    val testProv4 = "NYT_ENG_20130819.0084:573-1050"
    println("testProv: " + testProv4 + " " + splitProvenance("NYT_ENG_20130819.0084",573,1050))    
    
  }
    
  def splitProvenance(docid: String, start: Int, end: Int) : String = {
    
    val span = end-start+1    
    val halfSpan = span/2

    span match{
      //don't need to split
      case s if s <= 150 => docid + ":" + start + "-" + end
      //split into two
      case s if s <= 300 => docid + ":" + start + "-" + (start+149) + ", " + docid + ":" + (end-149) + "-" + end 
      //split into three
      case s if s <= 450 => {
        println("sp: splitting into 3")
        docid + ":" + start + "-" + (start+149) + ", " + docid + ":" + (start+halfSpan-75) + "-" + (start+halfSpan+74) + 
        ", " + docid + ":" + (end-149) + "-" + end
        } 
      //split into four
      case s if s > 450 => docid + ":" + start + "-" + (start+149) + ", " + docid + ":" + (start+150) + "-" + (start+150+149) +
        ", " + docid + ":" + (end-299) + "-" + (end-150) + ", " + docid + ":" + (end-149) + "-" + end
 
    }
    
  }
  
  
}
  
