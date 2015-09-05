package edu.washington.cs.knowitall.kbp2014.multir.slotfiller

import java.io._
import java.nio.file.{Paths, Files}
import collection.JavaConverters._
import java.util.Properties

import scala.collection.JavaConversions._
import scala.io.Source

import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.ling.CoreAnnotations.DocIDAnnotation
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.dcoref.CorefCoreAnnotations
import edu.stanford.nlp.dcoref.SieveCoreferenceSystem
import edu.stanford.nlp.dcoref.Document
import edu.stanford.nlp.dcoref.RuleBasedCorefMentionFinder
import edu.stanford.nlp.dcoref.Dictionaries
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation

//import edu.illinois.cs.cogcomp.wikifier.Linker

//this class was used in tac2013 for tagging
//import edu.knowitall.tac2013.app.SemanticTaggers
//import edu.knowitall.taggers.tag.TaggerCollection

import KBPQueryEntityType._

import edu.washington.multir.sententialextraction.DocumentExtractor
//import edu.washington.multir.preprocess.CorpusPreprocessing

import edu.washington.multirframework.argumentidentification.KBPTaggersArgumentIdentification
//import edu.washington.multirframework.corpus.CorpusInformationSpecification.SentDocNameInformation.SentDocName;
//import edu.washington.multirframework.corpus.Corpus
//import edu.washington.multirframework.corpus.MyCorpus
//import edu.washington.multirframework.corpus.DefaultCorpusInformationSpecification
//import edu.washington.multirframework.corpus.DocumentInformationI
//import edu.washington.multirframework.corpus.DocCorefInformation
//import edu.washington.multirframework.corpus.SentInformationI
//import edu.washington.multirframework.corpus.SentNamedEntityLinkingInformation
//import edu.washington.multirframework.corpus.TokenInformationI
//import edu.washington.multirframework.corpus.SentNamedEntityLinkingInformation.NamedEntityLinkingAnnotation


object RunKBP2015MultirExtractorCorpusSerialized {  
  
  val annotatorHelper = new StanfordAnnotatorHelperMethods()

  //val cis  = new DefaultCorpusInformationSpecification()
  //val javaDocInfoList = new java.util.ArrayList[DocumentInformationI]()
  //javaDocInfoList.add(new DocCorefInformation())
  //cis.addDocumentInformation(javaDocInfoList)
  //val javaSentInfoList = new java.util.ArrayList[SentInformationI]()
  //javaSentInfoList.add(new SentNamedEntityLinkingInformation())
  //cis.addSentenceInformation(javaSentInfoList)
    
  
  def main(Args: Array[String]){
    
      var runID = "UWashington4"
      var detailed = false
      //var detailed = true
      // --------------------------------
      // For SingleNameResolver
      // --------------------------------
      //val round1QueriesFile = "tac_2014_kbp_english_cold_start_evaluation_queries.xml"
      //val round2QueriesFile = "multir_round2_queries.xml"
      //val round1QueriesFile = "queries2015_r1.xml"
      //val round2QueriesFile = "queries2015_r1.xml"
      
      println("Running KBP!")  
      
      // ---------------------------------------------------------------------
      // Get args
      // ---------------------------------------------------------------------
      
      val modePreprocessed = Integer.parseInt(Args(1))
      val corpusOldNew = Args(2)
      val relevantDocsFileName = Args(3)
      val relevantDocsFile = new File(relevantDocsFileName)      
      val outputStream = new PrintStream(Args(4))
      val roundID = Args(5)
      val batchDrop = Args(6).toInt
      val batchDropRight = Args(7).toInt
      val pathToSerializedCorpus = Args(8)
      
      println("roundID: " + roundID)
      println("batchDrop: " + batchDrop)
      println("batchDropRight: " + batchDropRight)
      println("corpus: " + corpusOldNew)
      
      //val testDocNameWithPath = pathToSerializedCorpus + "ENG_NW_001278_20131128_F00012R2Z.ann"
      //val testDocNameWithPath = pathToSerializedCorpus + "ENG_NW_001278_20131128_F00012NM1.ann"
      //val testDoc = Serializer.deserialize(testDocNameWithPath).asInstanceOf[Annotation]
      //val testDocSentences = testDoc.get(classOf[SentencesAnnotation]).asScala.toList           
      //val testDocDocID = testDoc.get(classOf[DocIDAnnotation])
      //println("testDoc ID + num sentences: " + testDocDocID + " " + testDocSentences.size)      
      //return
      
      // ---------------------------------------------------------------------
      // Select the Solr Index
      // ---------------------------------------------------------------------
            
      SolrHelper.setConfigurations(corpusOldNew, false)
      
      // ---------------------------------------------------------------------
      // Parse queries
      // ---------------------------------------------------------------------
      
      val queriesNoAliases = KBPQuery.parseKBPQueries(Args(0), roundID)	  
      //val queries = KBPQuery.getAliases(queriesNoAliases)

      println("Number of Queries: " + queriesNoAliases.size)
      
      
      val outFmt = detailed match {
             case true => OutputFormatter.detailedAnswersOnly(outputStream, runID)
             case false => OutputFormatter.formattedAnswersOnly(outputStream, runID)
      }

      // ----------------------------------------------------------------------------
	  // Specify Multir Extractor
      // ----------------------------------------------------------------------------
      
	  val multirExtractor = new MultiModelMultirExtractorVersionColdStart()	  
      //val multirExtractor = new SingleModelMultirExtractorVersion1()	
	  
	  
	  // ----------------------------------------------------------------------------
	  // Single Name Resolver
      // ---------------------------------------------------------------------------
	  
	    //var queryNameSetRound2 = Set[String]()
        //var queryNameSetRound1 = Set[String]()

        //queryNameSetRound1 = KBPQuery.parseKBPQueriesToGetNames(round1QueriesFile)
        //queryNameSetRound2 = KBPQuery.parseKBPQueriesToGetNames(round2QueriesFile)           
        
        //println("SNR: r1 size: " + queryNameSetRound1.size)
        //println("SNR: r2 size: " + queryNameSetRound2.size)
      
        // For PER queries which have a single name, replace that name with a full name,
        // if one can be determined
	    var qc = 0
        for(query <- queriesNoAliases){	    

          qc += 1
          println("query: " + qc + " " + query.name + " " + query.id)
          
	      var singleQueryNamePER = false

	      try{
	      
	      singleQueryNamePER = query.entityType match{
            case PER if(query.name.split(" ").size == 1) => {
              
              val docNameWithPath = pathToSerializedCorpus + query.doc + ".ann"

              //println("snr: " + docNameWithPath)
              //Get doc from serialized corpus
              
              if (Files.exists(Paths.get(docNameWithPath))) {

                //println("snr: deserializing doc")
                
                val doc = Serializer.deserialize(docNameWithPath).asInstanceOf[Annotation]
                
                //println("snr call: " + query.name)
                
                val (single, qname) = SingleNameResolver.singleQueryName(doc, query, annotatorHelper)
                if(!single) { query.name = qname 
                              query.aliases = List(query.name)
                              println("snr replacement: " + qname)
                            }                    
                single
              }
              else true                  
        	}
	        case _ => false
	      }          
        
	      }catch {case e: Exception => 
	        {e.printStackTrace()
	         println("EXCEPTION: SingleNameResolver") 
	        }	  
	      } 
	  
        }
	  //return
	  
	  // ----------------------------------------------------------------------------	
      // Get Relevant Docs
	  // ----------------------------------------------------------------------------
	  
	  val queries = KBPQuery.getAliases(queriesNoAliases)
	  
	  //queries.foreach(q => q.aliases.foreach(a => println(a)))
	  
	  val entityRelevantDocSerialization = {
		    
	        if(relevantDocsFile.exists()){
		      QuerySetSerialization.getRevelantDocIdMap(relevantDocsFileName)
		    }
	        
	        // LATER: insert modePreprocessed here, when get connection to Derby DB
	        // mode 0 - don't have linking information, select reldocs which contain queryname
	        // mode 1 - use linking information to select the reldocs
	        
		    else{
		      // make this map and write it out
              val qm = SolrHelper.getRelevantDocuments(queries)
              // use cold start method if working with a list of files which are a subset of 
              // and existing solr index
		      //val qm = SolrHelper.getRelevantDocumentsColdStart(queries)
		      val qidMap = qm.toList.map(f => (f._1.id,f._2)).toMap
		      QuerySetSerialization.writeRelevantDocIdMap(qidMap, relevantDocsFileName)
		      qidMap
		    }
      }  
	  
	  // --------------------------------------------------------------------------------------	
      // Group the Queries - create a list of List[KBPQuery], 
	  // each element of the list is a list of KBPQueries which have the same query name,
	  // so that the doc processing can be shared
	  // --------------------------------------------------------------------------------------
	  
      var querySet: Set[String] = Set()
      var sameQueries : List[List[KBPQuery]] = List()
    
      for(query <- queries){
        val qn = query.name
      
        if(!querySet.contains(qn)){
          //add query name to the set, which indicates it has been processed
          querySet += qn   
          //get all matching queries for this query name
          val matchingQueries = queries.filter(q => qn.equals(q.name))
          sameQueries = sameQueries ++ List(matchingQueries)        
        } 
      }	  
	
      println("sameQueries size: " + sameQueries.size)	  

      //var qcount = 0
      //sameQueries.foreach(q => { qcount +=1 
      //                           println("query " + qcount + ": " + q(0).name)
      //                         })
      //return
      
      //--------------------------------------------------------------------------
      //If we want to run a subset of the collapsed queries, this is that subset
      //--------------------------------------------------------------------------
      val sameQueriesBatch = sameQueries.drop(batchDrop).dropRight(batchDropRight)
      
      println("sameQueriesBatch size: " + sameQueriesBatch.size)	  
      
      // ----------------------------------------------------------------
      // Examine memory use
      // ----------------------------------------------------------------

      println
	  println("total memory: " + Runtime.getRuntime().totalMemory())
      //the Xmx value
      println("max memory: " + Runtime.getRuntime().maxMemory())
      println("free memory: " + Runtime.getRuntime().freeMemory())  		        
      println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + 
        Runtime.getRuntime().freeMemory()))
      println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
      println    

      var queryCount = 0
      var docCount = 0
      
      for(sameQueryName <- sameQueriesBatch){        

        queryCount += 1
        
        val firstQuery = sameQueryName(0) 
        val queryName = firstQuery.name
        println
        println("query " + queryCount + ": " + firstQuery.id + " " + queryName)
        println
        
        // -------------------------------------------------------------------
        // singleQueryNamePER -- If query entityType is PER and query name is 
        // a single name (i.e. couldn't be resolved to a full name above),
        // set flag here to true, to use to drop it
        // -------------------------------------------------------------------
        
	    var singleQueryNamePER = false
     
        singleQueryNamePER = firstQuery.entityType match{
            case PER if(queryName.split(" ").size == 1) => true
	        case _ => false
	    }
      
        // -----------------------------------------------------------------------
        // anyRelevantSlots -- If the query set has no slots being filled by
        // this PERLOC Multir model, set flag here to false, to use to drop it
        // -----------------------------------------------------------------------
                
        var anyRelevantSlots = false
        
        val sameQueriesRelevantSlot = sameQueryName.filter(q => ColdStartSlots_Multir.slots.contains(q.slotsToFill.toList(0).name))
        
        println("sameQueriesRelevantSlot size: " + sameQueriesRelevantSlot.size)        
              
        if(sameQueriesRelevantSlot.size > 0) anyRelevantSlots = true  
                
        // ------------------------------------------------------------------------------------------------
        // Proceed if this set of queries does not have a single PER name, and has relevant slots to fill
        // ------------------------------------------------------------------------------------------------
        if(!singleQueryNamePER && anyRelevantSlots){
          
	      try{

	        // --------------------------------------------------------------------------
	        // Set of Relevant Extraction Candidates from the set of relevant documents
	        // --------------------------------------------------------------------------
	        var allRelevantCandidates: Seq[Candidate] = Seq()
	        //var allRelevantCandidates = collection.mutable.Map[Int, Seq[Candidate]]()
	        //var slotfillFreq = collection.mutable.Map[String, Int]().withDefaultValue(0)
	        
	        // --------------------------------------
            // Process Documents for this Query Set  
            // -------------------------------------- 
	        
		    var relevantDocs: Set[String] = Nil.toSet    		              	      
	        if(entityRelevantDocSerialization.contains(firstQuery.id)){
		      relevantDocs = entityRelevantDocSerialization(firstQuery.id).toSet}
		    //val relevantDocs = entityRelevantDocSerialization(firstQuery.id).toSet		      
            println("# relDocs: " + relevantDocs.size)
	        
		    /*val documents :List[Option[Annotation]] = {		    
		      modePreprocessed match {
		        case 0 => processDocuments(relevantDocs)
		        case _ => processDocuments(relevantDocs)	
		        //case _ => processDocuments(nwDocuments)		      
		      } 
		    }*/
		    
            if(relevantDocs.size > 0){
            
		    println("Getting Extractions")
		  
		    
            for(document <- relevantDocs){

              docCount += 1
              
              val fullDocNameWithPath = pathToSerializedCorpus + document + ".ann"
              
              //println("fdn: " + fullDocNameWithPath)
              
              if (Files.exists(Paths.get(fullDocNameWithPath))) { 

                try{
                
                //println("deserializing doc")
                
                //if serialized doc file exists, deserialize the doc                 
                val doc = Serializer.deserialize(fullDocNameWithPath).asInstanceOf[Annotation]
                //for some docs, a doc file exists, but there is no data in it; 
                //for example, if the doc wasn't in the solr index, or maybe if it timed-out during processsing
                val docSentences = doc.get(classOf[SentencesAnnotation]).asScala.toList      

                //println("num doc sentences: " + docSentences.size)
                
                if(docSentences.size > 0){
                  val extractions = multirExtractor.extract(doc, firstQuery).asScala		          		          		     

                  //var queryCount = 0
                  //for(query <- sameQueriesRelevantSlot){	
                    
                    val relevantCandidates = FilterExtractionResults.filterResults(FilterExtractionResults.wrapWithCandidate(extractions), firstQuery, Some(doc))     
                    // if any relevant candidates, add to map
                    if(relevantCandidates.size > 0) {
                      allRelevantCandidates = allRelevantCandidates ++ relevantCandidates
                      //if(allRelevantCandidates.contains(queryCount)){
                      //  allRelevantCandidates(queryCount) = allRelevantCandidates(queryCount) ++ relevantCandidates 
                      //}
                      //else{
                      //  allRelevantCandidates(queryCount) = relevantCandidates
                      //}
                    //}
                    //queryCount += 1
                    
                  }
                  
                } //if serialized doc has data
                
                }catch {case e: Exception => 
	              {e.printStackTrace()
	               println("EXCEPTION: Document Level") 
	              }	  
                }   
              
              } //if serialized doc file exists
            } //for doc

		    //println
		    //println("allRelevantCandidates size: " + allRelevantCandidates.size)
            //for(key <- allRelevantCandidates.keys){
            //  println("key: " + key)
            //  allRelevantCandidates(key).foreach(c => println(c.extr.getArg1()))              
            //}
		    
		    println
		    println("All Relevant Candidates size: " + allRelevantCandidates.size)
            allRelevantCandidates.foreach(c => println(c.extr.getArg1() + " " + c.extr.getArg2() + " " + c.extr.getRel() + " " + c.extr.getScore()))

		    val kbpAllRelevantCandidates = FilterExtractionResults.substituteKBPRelationsColdStart(allRelevantCandidates, firstQuery)
		    
		    println
		    println("kbpAllRelevantCandidates size: " + kbpAllRelevantCandidates.size)
		    
		    println
		    println("Processing Each Query in the Set")
		    
		    var querySetCount = 0
            for(query <- sameQueriesRelevantSlot){		      
              
		      val slots = query.slotsToFill
		      
		      //val queryARC = allRelevantCandidates
		      
		      println("query: " + slots.toList(0).name)
              println("query et: " + query.entityType.toString())
              //println("qARC size: " + queryARC.size)
              //queryARC.foreach(c => println(c.extr.getArg1() + " " + c.extr.getArg2() + " " + c.extr.getRel() + " " + c.extr.getScore()))
              
              //var kbpAllRelevantCandidates: Seq[Candidate] = Seq()
              //if(allRelevantCandidates.contains(queryCount)){
              //  kbpAllRelevantCandidates = FilterExtractionResults.substituteKBPRelationsColdStart(allRelevantCandidates(querySetCount), query)
              //}                
              
		      println("karc size: " + kbpAllRelevantCandidates.size)
		      kbpAllRelevantCandidates.foreach(c => println(c.extr.getArg1() + " " + c.extr.getArg2() + " " + c.extr.getRel() + " " + c.extr.getScore()))

		      println("slots size: " + slots.size)
		      
		      val bestAnswers = slots map { slot => ( slot, SelectBestAnswers.reduceToMaxResults(slot, kbpAllRelevantCandidates.filter(_.extr.getRel() == slot.name)) ) } toMap
		      
		      println("ba size: " + bestAnswers.size)         	      
		      
		      outFmt.printAnswers(bestAnswers, query)     
		    
		      querySetCount += 1        
		    }
		    
		    }
		    else { 
		      println("Query Set " + queryCount + ": No Relevant Docs")
		    } 
		  
	    }
	    catch {case e: Exception => 
	      {e.printStackTrace()
	         println("EXCEPTION: " + firstQuery.id + " " + firstQuery.name) 
	         //outFmt.printEmpty(query)
	      }	  
	    }		    		  	 
      }   
	  else{
	    println("Skipping this query: anyRelevantSlots= " + anyRelevantSlots + " singleQueryNamePER: " + singleQueryNamePER)
	    //Don't need to print NIL for Cold Start
	    //outFmt.printEmpty(query)
	  }
	     
	  println("Finished, going to next query")
      println
	    
    } //queries
		  	  
	  println("Finished with Queries")
	  
	  outputStream.close()
	  
	  println("Closed outputStreams")

	  println
	  println("total memory: " + Runtime.getRuntime().totalMemory())
      //the Xmx value
      println("max memory: " + Runtime.getRuntime().maxMemory())
      println("free memory: " + Runtime.getRuntime().freeMemory())  		        
      println("computed free memory: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory() + Runtime.getRuntime().freeMemory()))
      println("used memory: " + (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()))
      
      println
      println("doc count: " + docCount)
	  
  } //main
  
  
  /*def processDocumentsStanford(documents: Set[String]):  List[Option[Annotation]]  = {
    println("Number of docs = " + documents.size)
       var start :Long = 0
       var end: Long = 0    
    for(doc <- documents.toList) yield{
      start = System.currentTimeMillis()
      val a =processDocument(doc)
      end = System.currentTimeMillis()
      println("Document took " + (end-start) + " milliseconds")
      a
    }
  }*/
  
  /*def processDocuments(documents: Set[String]): List[Option[Annotation]] = {
    println("Number of docs = " + documents.size)
    var startTime :Long = 0
	var endTime: Long = 0    	 
	var docCount = 0
	var docs = documents.toList
	// Setting max number of documents to 500
    val maxSize = 500
    //val maxSize = 5
    if(docs.size > maxSize){docs = docs.dropRight(docs.size-maxSize)}
    println("Docs.size: " + docs.size)
    //for(doc <- documents.toList) yield{
    for(doc <- docs) yield{
      docCount = docCount + 1
      println("Processing Doc # :" + docCount + " " + doc)
      var a :Option[Annotation] = None
      val t = new Thread {
        override def run() {    
          startTime = System.currentTimeMillis()
          //a =processDocument(doc)
          a = stanfordProcessDocument(doc)
          endTime = System.currentTimeMillis()
          println("Thread: Document took " + (endTime-startTime) + " milliseconds")      
        }
      }                                              
      t.start()
      //t.join(10000)
      t.join(180000) 
      t.stop()
      a
    }
  }*/
  
  /*def cjParseDocument(docName: String): Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      if(rawDoc.length < 20000){
      val preprocessedAndParsedDoc = CorpusPreprocessing.getTestDocumentFromRawString(rawDoc,docName)
      println("Document was cj parsed")
      Some(preprocessedAndParsedDoc)
      }
      else{
        None
      }
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }*/
  
  /*def linkDocument(docName: String): Option[Annotation] ={
    try{
        val rawDoc = SolrHelper.getRawDoc(docName)
        val processedDoc = new Annotation(rawDoc)
        println("Document was linked")
        Some(processedDoc)
      }
      catch{
        case e: Exception => e.printStackTrace()
        None
      }
  } */
  
  /*def processDocument(docName: String) : Option[Annotation]  ={ 
    try{          
      println("Processing document " +docName)
      val stanfordDoc = stanfordProcessDocument(docName)
      val cjParsedDoc = cjParseDocument(docName)
      //val linkedDoc = linkDocument(docName)      	     
    	     
      if(stanfordDoc.isDefined && cjParsedDoc.isDefined /*&& linkedDoc.isDefined*/){
        val ann = joinAnnotations(stanfordDoc.get,cjParsedDoc.get,new Annotation());
    	//println("have NEL = "+ann.get(classOf[NamedEntityLinkingAnnotation]))
    	Some(ann)
    	//joinAnnotations(stanfordDoc.get,cjParsedDoc.get,linkedDoc.get)
      }
      else{
        None
      }
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
      
  }*/
  
  /*def joinAnnotations(stanfordDoc: Annotation, cjParsedDoc: Annotation, linkedDoc: Annotation) : Annotation = {  
    //add coref annotations to cjParsedDoc
    cjParsedDoc.set(classOf[CorefCoreAnnotations.CorefChainAnnotation],stanfordDoc.get(classOf[CorefCoreAnnotations.CorefChainAnnotation]))
    cjParsedDoc.set(classOf[NamedEntityLinkingAnnotation],linkedDoc.get(classOf[NamedEntityLinkingAnnotation]))
    cjParsedDoc
  }*/
  
  /*def stanfordProcessDocument(docName: String) : Option[Annotation] = {
    try{
      val rawDoc = SolrHelper.getRawDoc(docName)
      if(rawDoc.length < 20000){
        val processedDoc = new Annotation(rawDoc)
        annotatorHelper.getCorefPipeline().annotate(processedDoc)
        //need to set below when not running CJ
        processedDoc.set(classOf[DocIDAnnotation], docName)
        println("Document was Stanford Annotated: " + processedDoc.get(classOf[DocIDAnnotation]))
        //this is null below, comes from CJ call
        //println("DwSA SentDocName: " + processedDoc.get(classOf[SentDocName]))
        Some(processedDoc)
      }
      else{
        None
      }  
    }
    catch{
      case e: Exception => e.printStackTrace()
      None
    }
  }*/
  

}
