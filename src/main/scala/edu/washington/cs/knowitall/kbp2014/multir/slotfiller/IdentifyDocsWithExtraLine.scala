package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}

import scala.collection.JavaConversions._
import scala.io.Source
import collection.JavaConverters._

import com.typesafe.config.ConfigFactory

object IdentifyDocsWithExtraLine {

  val config = ConfigFactory.load("kbp-2015-identify-docs-with-extra-line.conf")
  val coldStartFilesFileName = config.getString("cold-start-files-file")
  val outputFileName = config.getString("output-file")
  
  
  def main(args: Array[String]) {

     // Access the Cold Start Corpus, Solr Index
     val corpus = "cs"
    
     val drop = args(0).toInt
     val dropRight = args(1).toInt

     val outStream = new PrintStream(outputFileName)
     
     SolrHelper.setConfigurations(corpus, false)
     
     var fileNameSet: Set[String] = Set()
    
    // -----------------------------------------------------
    // Read the list of file names in the cold start corpus     
    // ----------------------------------------------------- 
    val inputFilename = coldStartFilesFileName
    // Does file exist?
    if (!Files.exists(Paths.get(inputFilename))) {
      System.out.println(s"coldStartFiles file $inputFilename doesn't exist!  " + s"Exiting...")
      sys.exit(1)
    } 
    // Read file, line by line
    var coldStartFileNames = Source.fromFile(inputFilename).getLines().toList
    
    coldStartFileNames = for(name <- coldStartFileNames) yield {
      val tokens = name.trim.split("\\.") 
      tokens.size match {
        case 2 => tokens(0)
        case 3 if tokens(1) == "mpdf" => tokens(0)
        case 3 => tokens(0) + "." + tokens(1)
        case _ => {println(name)
                   "not-a-file"
        }
      }      
    }

    //Drop file names from bad lines, if any
    coldStartFileNames = coldStartFileNames.filter(n => n != "not-a-file")
 
    //Check for duplicate file names
    coldStartFileNames.foreach(n => {
        if(!fileNameSet.contains(n)) fileNameSet += n
        else{
          println(n)
        }
      }
    )
   
    println("Num cs files: " + coldStartFileNames.size)
    println("outputFileName: " + outputFileName)
    
    val docsToProcess = coldStartFileNames.drop(drop).dropRight(dropRight).toSet

    //val docsToProcess = Set("NYT_ENG_20131231.0228")
    //val docsToProcess = Set("fffaa1de2ac709e2d7771a1ba40a816c.mpdf","fffaa1de2ac709e2d7771a1ba40a816c")

    println("Num files to process: " + docsToProcess.size)

        
    var docCount = 0
    var docCountExtraLine = 0
    for(document <- docsToProcess){          

      try{
        docCount += 1  
        val rawDoc = SolrHelper.getRawDoc(document)
  
        if(rawDoc.startsWith("<?xml")){
          outStream.println(document)
          docCountExtraLine += 1
        }        
        
      }
      catch {case e: Exception => 
	    { e.printStackTrace()
	      println("EXCEPTION: " + document) 
	    }	  
	  }	
    
    }
    
    outStream.close()
    
    println("total docs: " + docCount)
    println("docs with extra line: " + docCountExtraLine)
    
  }//main
  
}
  
