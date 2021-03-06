package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import org.apache.commons.io.IOUtils;

import edu.knowitall.collection.immutable.Interval;
//import edu.knowitall.tac2013.solr.query.SolrHelper;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefChain.CorefMention;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefClusterIdAnnotation;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefGraphAnnotation;
import edu.stanford.nlp.ling.CoreAnnotations.*;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.time.TimeAnnotations.TimexAnnotation;
import edu.stanford.nlp.time.Timex;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.util.IntTuple;
import edu.stanford.nlp.util.Pair;
import edu.washington.cs.knowitall.kbp2014.multir.slotfiller.util.DocUtils;



public class StanfordAnnotatorHelperMethods {
	
	//private final  StanfordCoreNLP suTimePipeline;
	private final  StanfordCoreNLP corefPipeline;
	//private final StanfordCoreNLP chinesePipeline;
	private Map<String,Annotation> corefAnnotationMap;
	private Map<String,Annotation> suTimeAnnotationMap;
	
	
	public StanfordAnnotatorHelperMethods(){
		Properties suTimeProps = new Properties();
		suTimeProps.put("annotators", "tokenize, ssplit, pos, lemma, cleanxml, ner");
		suTimeProps.put("sutime.binders", "0");
		suTimeProps.put("clean.datetags","datetime|date|dateline");
		//this.suTimePipeline = new StanfordCoreNLP(suTimeProps);
		
		
		Properties corefProps = new Properties();
	    corefProps.put("annotators", "tokenize, cleanxml, ssplit, pos, lemma, ner, parse, dcoref");
	    corefProps.put("clean.allowflawedxml", "true");
	    corefProps.put("ner.useSUTime", "false");
	    //clean all xml tags
		this.corefPipeline = new StanfordCoreNLP(corefProps);

		/*Properties chineseProps = new Properties(); 	    
	    chineseProps.put("annotators", "segment, ssplit, pos, ner");
	    chineseProps.put("outputFormat", "xml");
	    chineseProps.put("customAnnotatorClass.segment", 
    			"edu.stanford.nlp.pipeline.ChineseSegmenterAnnotator");
	    chineseProps.put("segment.model", 
    			"edu/stanford/nlp/models/segmenter/chinese/ctb.gz");
	    chineseProps.put("segment.sighanCorporaDict", 
    			"edu/stanford/nlp/models/segmenter/chinese");
	    chineseProps.put("segment.serDictionary", 
    			"edu/stanford/nlp/models/segmenter/chinese/dict-chris6.ser.gz");
	    chineseProps.put("segment.sighanPostProcessing", "true");
	    chineseProps.put("ssplit.boundaryTokenRegex", "[.]|[!?]+|[。]|[！？]+");
	    chineseProps.put("pos.model", 
    			"edu/stanford/nlp/models/pos-tagger/chinese-distsim/chinese-distsim.tagger");
	    chineseProps.put("ner.model", 
    			"edu/stanford/nlp/models/ner/chinese.misc.distsim.crf.ser.gz");
	    chineseProps.put("ner.applyNumericClassifiers", "false");
	    chineseProps.put("ner.useSUTime", "false");
	    chineseProps.put("encoding", "utf-8");
	    chineseProps.put("inputEncoding", "utf-8");
	    chineseProps.put("outputEncoding", "utf-8");
	    chineseProps.put("parse.model", 
    			"edu/stanford/nlp/models/lexparser/chinesePCFG.ser.gz");
	    */
		//this.chinesePipeline = new StanfordCoreNLP(chineseProps);
		
		corefAnnotationMap = new HashMap<String,Annotation>();
		suTimeAnnotationMap = new HashMap<String,Annotation>();

	}
	
	//public StanfordCoreNLP getChinesePipeline(){return chinesePipeline;}
	public StanfordCoreNLP getCorefPipeline(){return corefPipeline;}
	
	public static void main(String[] args) throws FileNotFoundException, IOException{
		StanfordAnnotatorHelperMethods sh = new StanfordAnnotatorHelperMethods();
		//Annotation annotation = new Annotation("John Smith ate lunch.");
		//sh.corefPipeline.annotate(annotation);
		//for (Integer key: annotation.get(CorefChainAnnotation.class) .keySet()) {
	    //  for (CorefMention mention: annotation.get(CorefChainAnnotation.class).get(key).getMentionsInTextualOrder()) {
		//		System.out.println(mention.mentionSpan);
		//	}
		//}
	    //sh.runSuTime("testXMLDoc");
		
	}
	
	public void clearHashMaps(){
		corefAnnotationMap.clear();
		suTimeAnnotationMap.clear();
	}
	
	/*public void runSuTime(String docID) throws FileNotFoundException, IOException{
		Annotation document;
		if(suTimeAnnotationMap.containsKey(docID)){
			document = suTimeAnnotationMap.get(docID);
		}
		else{
		  String filePathPlusDocId = this.filePath+docID;
		  FileInputStream in = new FileInputStream(new File(filePathPlusDocId));
		  String fileString = IOUtils.toString(in,"UTF-8");
		  in.close();
		
		  document = new Annotation(fileString);
		  suTimePipeline.annotate(document);
		  suTimeAnnotationMap.put(docID, document);
		}
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
	    for(CoreMap sentence: sentences){
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		String word = token.get(TextAnnotation.class);
	    		String ne = token.get(NamedEntityTagAnnotation.class);
	    		String net = token.get(NormalizedNamedEntityTagAnnotation.class);
	    		Timex tt = token.get(TimexAnnotation.class);
	    		String tts = "";
	    		if(tt != null){
	    			tts = tt.value();
	    		}
	    		System.out.println(word+ " " + ne + " " + net + " " + tts);
	    	}
	    }
	    
	    String s =document.get(NamedEntityTagAnnotation.class);
	    System.out.println(s);

	} */
	
	private String normalizeTimex(Timex t){
		if(t.timexType() == "DATE"){
	      String timexString = t.value();
	      if (timexString == null) return "";
	      String formattedString = normalizeDate(timexString);
		  return formattedString;
		}
		else{
			return "";
		}
	}
	
	private String normalizeDate(String dateString){
		  String formattedString = null;
	      if(Pattern.matches("\\w{4}", dateString)){
	    	  formattedString = dateString +"-XX-XX";
	      }
	      else if(Pattern.matches("\\w{2}-\\w{2}",dateString)){
	    	  formattedString = "XXXX-" + dateString; 
	      }
	      else if(Pattern.matches("\\w{4}-\\w{2}", dateString)){
	    	  formattedString = dateString + "-XX";
	      }
		  
	      if(formattedString == null){
	    	  return dateString;
	      }
	      else{
	    	  return formattedString;
	      }
	}
	

	
	/*public String getNormalizedDate(Interval charInterval, String docId, String originalString) throws IOException{
		Annotation document;
		if(suTimeAnnotationMap.containsKey(docId)){
			document = suTimeAnnotationMap.get(docId);
		}
		else{
			String xmlDoc = SolrHelper.getRawDoc(docId);
			if(xmlDoc.trim().isEmpty()){
				return originalString;
			}
			document = new Annotation(xmlDoc);
			suTimePipeline.annotate(document);
			suTimeAnnotationMap.put(docId, document);
		}
	
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		    for(CoreMap sentence: sentences){
		    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
		    		Timex tt = token.get(TimexAnnotation.class);
		    		if(charInterval.intersects(Interval.closed(token.beginPosition(), token.endPosition()))){
		    			if(tt != null && tt.value() != null){
		    				return normalizeTimex(tt);
		    			}
		    		}
		    	}
		    }
	       return normalizeDate(originalString);
	} */
	
	public List<String> getNamesFromCorefMentions(String docId, Interval interval) {
		
		String rawDoc = SolrHelper.getRawDoc(docId);
		
		Annotation document = new Annotation(rawDoc);
		corefPipeline.annotate(document);
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		/*for(Integer i : graph.keySet()){
			System.out.println("GROUP " + i);
			CorefChain x = graph.get(i);
			for( CorefMention m : x.getMentionsInTextualOrder()){
				System.out.println(m.mentionSpan + " " + m.sentNum + " " + m.startIndex);
			}
		}*/

		// -----------------------------------------------
		// Get corefClusterID for the query name
		// -----------------------------------------------
		
		Integer corefClusterID = null;
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		/*for(CoreMap sentence : sentences){
			for(CoreLabel token : sentence.get(TokensAnnotation.class)){
				
			}
		}*/
		//List<Pair<IntTuple,IntTuple>> x  = document.get(CorefGraphAnnotation.class);

		int s = 0, t = 0;
	    for(CoreMap sentence: sentences){
	    	t = 0;
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		
	    		//if(token.originalText().contentEquals("Donald")) System.out.println("Donald: " + token.beginPosition());	    				
	    	    //if(token.beginPosition() == 3009) {
	    	      //System.out.println("3009: " + s + " " + t + " " + token.originalText() + " " + interval.start() + " " + (token.beginPosition() == interval.start()));
                  //System.out.println(token.sentIndex() + " " + token.index());}
	    		if(token.beginPosition() == interval.start()){
	    			for(Integer i : graph.keySet()){
	    				//System.out.println("GROUP " + i);
	    				CorefChain x = graph.get(i);
	    				for( CorefMention m : x.getMentionsInTextualOrder()){
	    					//System.out.println(m.mentionSpan + " " + m.sentNum + " " + m.startIndex);
	    					if(m.sentNum==(s+1) && m.startIndex <= (t+1) && (t+1) <= m.endIndex)
	    						corefClusterID = m.corefClusterID;
	    				}
	    			}			
	    			
	    			//System.out.println("Assigning corefClusterID");
	    			//corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    			//System.out.println(sentence.get(TokensAnnotation.class).get(token.index()).get(CorefClusterIdAnnotation.class));
	    			//System.out.println(sentence.get(TokensAnnotation.class).get(token.index()+1).get(CorefClusterIdAnnotation.class));
	    			//System.out.println("corefClusterID = " + corefClusterID);
	    		}
	    		t++;
	    	}
	    	s++;
	    }
		
	    /*if(corefClusterID != null){
	    	return graph.get(corefClusterID).getMentionsInTextualOrder();
	    }
	    else{
	    	return new ArrayList<CorefMention>();
	    }*/
	    
	    List<String> fullNameList = new ArrayList<String>();
	    List<CorefMention> corefMentions = new ArrayList<CorefMention>();
	    List<CorefMention> properCorefMentions = new ArrayList<CorefMention>();
	    
	    //System.out.println("FN corefClusterID: " + corefClusterID);
	    
	    if(corefClusterID != null){
    	  corefMentions = graph.get(corefClusterID).getMentionsInTextualOrder();
    	  
    	  //System.out.println("FN corefMentions size: " + corefMentions.size());
    	  
    	  for(CorefMention m : corefMentions){
    		  if(m.mentionType.toString().contains("PROPER")) properCorefMentions.add(m);		                	            	
	      }
    	  
    	  //System.out.println("FN properCorefMentions size: " + properCorefMentions.size());
    	  
    	  //
    	  for( CorefMention m : properCorefMentions){
    		  CoreMap sentence = sentences.get(m.sentNum -1);
    		  List<CoreLabel> tokens = sentence.get(TokensAnnotation.class);
    		  for(int i = (m.startIndex-1); i < (m.endIndex); i++){
    			  String ner = tokens.get(i).get(CoreAnnotations.NamedEntityTagAnnotation.class);
    			  if(ner.toString().equals("PERSON")){
    				  
    				  //System.out.println("FN getting Full Name: " + tokens.get(i).originalText());
    				  String name = getRelevantStringSequence(tokens, i, m.endIndex, "PERSON");
    				  fullNameList.add(name);
    				  i += name.split(" ").length; 
    			  }
    		  }		  
    	  }
	    }	
	    
	    //System.out.println("FN fullNameList size: " + fullNameList.size());
	    for(String n : fullNameList){ System.out.println("FN: " + n);}
	    
	    return fullNameList;   	
	}
	
    public List<String> getFullNamesFromCorefMentions(Annotation document, Interval interval) {
		
		//String rawDoc = SolrHelper.getRawDoc(docId);
		//Annotation document = new Annotation(rawDoc);
		//corefPipeline.annotate(document);
		
		Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
		/*for(Integer i : graph.keySet()){
			System.out.println("GROUP " + i);
			CorefChain x = graph.get(i);
			for( CorefMention m : x.getMentionsInTextualOrder()){
				System.out.println(m.mentionSpan + " " + m.sentNum + " " + m.startIndex);
			}
		}*/

		// -----------------------------------------------
		// Get corefClusterID for the query name
		// -----------------------------------------------
		
		Integer corefClusterID = null;
		
		List<CoreMap> sentences = document.get(SentencesAnnotation.class);
		/*for(CoreMap sentence : sentences){
			for(CoreLabel token : sentence.get(TokensAnnotation.class)){
				
			}
		}*/
		//List<Pair<IntTuple,IntTuple>> x  = document.get(CorefGraphAnnotation.class);

		int s = 0, t = 0;
	    for(CoreMap sentence: sentences){
	    	t = 0;
	    	for(CoreLabel token: sentence.get(TokensAnnotation.class)){
	    		
	    		//if(token.originalText().contentEquals("Donald")) System.out.println("Donald: " + token.beginPosition());	    				
	    	    //if(token.beginPosition() == 3009) {
	    	      //System.out.println("3009: " + s + " " + t + " " + token.originalText() + " " + interval.start() + " " + (token.beginPosition() == interval.start()));
                  //System.out.println(token.sentIndex() + " " + token.index());}
	    		if(token.beginPosition() == interval.start()){
	    			for(Integer i : graph.keySet()){
	    				//System.out.println("GROUP " + i);
	    				CorefChain x = graph.get(i);
	    				for( CorefMention m : x.getMentionsInTextualOrder()){
	    					//System.out.println(m.mentionSpan + " " + m.sentNum + " " + m.startIndex);
	    					if(m.sentNum==(s+1) && m.startIndex <= (t+1) && (t+1) <= m.endIndex)
	    						corefClusterID = m.corefClusterID;
	    				}
	    			}			
	    			
	    			//System.out.println("Assigning corefClusterID");
	    			//corefClusterID = token.get(CorefClusterIdAnnotation.class);
	    			//System.out.println(sentence.get(TokensAnnotation.class).get(token.index()).get(CorefClusterIdAnnotation.class));
	    			//System.out.println(sentence.get(TokensAnnotation.class).get(token.index()+1).get(CorefClusterIdAnnotation.class));
	    			//System.out.println("corefClusterID = " + corefClusterID);
	    		}
	    		t++;
	    	}
	    	s++;
	    }
		
	    /*if(corefClusterID != null){
	    	return graph.get(corefClusterID).getMentionsInTextualOrder();
	    }
	    else{
	    	return new ArrayList<CorefMention>();
	    }*/
	    
	    List<String> fullNameList = new ArrayList<String>();
	    List<CorefMention> corefMentions = new ArrayList<CorefMention>();
	    List<CorefMention> properCorefMentions = new ArrayList<CorefMention>();
	    
	    //System.out.println("FN corefClusterID: " + corefClusterID);
	    
	    if(corefClusterID != null){
    	  corefMentions = graph.get(corefClusterID).getMentionsInTextualOrder();
    	  
    	  //System.out.println("FN corefMentions size: " + corefMentions.size());
    	  
    	  for(CorefMention m : corefMentions){
    		  if(m.mentionType.toString().contains("PROPER")) properCorefMentions.add(m);		                	            	
	      }
    	  
    	  //System.out.println("FN properCorefMentions size: " + properCorefMentions.size());
    	  
    	  //
    	  for( CorefMention m : properCorefMentions){
    		  CoreMap sentence = sentences.get(m.sentNum -1);
    		  List<CoreLabel> tokens = sentence.get(TokensAnnotation.class);
    		  //System.out.println("sahm: tokens size: " + m.mentionSpan + " " + m.sentNum + " " + m.endIndex + " " + tokens.size());
    		  for(int i = (m.startIndex-1); i < (m.endIndex); i++){
    			  //System.out.println("sahm i: " + i);
                  String ner = "";
                  if(i < tokens.size()){
    			    ner = tokens.get(i).get(CoreAnnotations.NamedEntityTagAnnotation.class);
    			  }
    			  if(ner.toString().equals("PERSON")){
    				  
    				  //System.out.println("FN getting Full Name: " + tokens.get(i).originalText());
    				  String name = getRelevantStringSequence(tokens, i, m.endIndex, "PERSON");
    				  //System.out.println("FN Full Name: " + name);
    				  fullNameList.add(name);
    				  i += name.split(" ").length - 1;
    				  //i += name.split(" ").length; 
    			  }
    		  }		  
    	  }
	    }	
	    
	    //System.out.println("FN fullNameList size: " + fullNameList.size());
	    //for(String n : fullNameList){ System.out.println("FN: " + n);}
	    
	    return fullNameList;   	
	}
	
    public String getRelevantStringSequence(List<CoreLabel> tokens, Integer i, Integer endIndex, String ner){
		
		String relevantStringSequence = tokens.get(i).originalText();
		i++;
		
		while(i < endIndex){

			String nextNer = tokens.get(i).get(CoreAnnotations.NamedEntityTagAnnotation.class);
			if(ner.equals(nextNer)){
				relevantStringSequence = relevantStringSequence + " " + tokens.get(i).originalText();
			}
			else{
				break;
			}			
			i++;
		}		

		return relevantStringSequence;
	}
	
	
	public List<CorefMention> getCorefMentions(String xmlString, Interval interval) {
		
      Annotation document = new Annotation(xmlString);

      corefPipeline.annotate(document);

      Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
  
      for(Integer i : graph.keySet()){
        System.out.println("GROUP " + i);
        CorefChain x = graph.get(i);
        for( CorefMention m : x.getMentionsInTextualOrder()){
          System.out.println(m.mentionSpan);
        }
      }

      Integer corefClusterID = null;

      List<CoreMap> sentences = document.get(SentencesAnnotation.class);
      for(CoreMap sentence : sentences){
        for(CoreLabel token : sentence.get(TokensAnnotation.class)){

        }
      }
      List<Pair<IntTuple,IntTuple>> x = document.get(CorefGraphAnnotation.class);


      for(CoreMap sentence: sentences){
        for(CoreLabel token: sentence.get(TokensAnnotation.class)){
          if(token.beginPosition() == interval.start()){
            corefClusterID = token.get(CorefClusterIdAnnotation.class);
          }
        }
      }

      if(corefClusterID != null){
        return graph.get(corefClusterID).getMentionsInTextualOrder();
      }
      else{
        return new ArrayList<CorefMention>();
      }

   }


/**
* Provides a lookup method for taking corefMentions and finding their NER tagged
* substrings.
* @param annotatedDocument
* @param position
* @return
*/
private Interval getNamedEntityAtPosition(Annotation annotatedDocument, IntTuple position, KBPQueryEntityType entityType){

  return Interval.open(0, 1);
}


private CoreLabel getTokenBeginningAtByteOffset(Annotation annotatedDocument, Integer beg){

  List<CoreMap> sentences = annotatedDocument.get(SentencesAnnotation.class);
  for(CoreMap sentence : sentences){
    for(CoreLabel token : sentence.get(TokensAnnotation.class)){
      if(token.beginPosition() == beg ){
        return token;
      }
    }
  }
  return null;
}

/**
* Given the information from a CorefMention determine the byte offsets
* of the whole mention and return as a knowitall Interval.
* @param document
* @param sentNum
* @param startIndex
* @param endIndex
* @return
*/

private Interval getCharIntervalFromCorefMention(Annotation document, Integer sentNum, Integer startIndex, Integer endIndex){

  List<CoreMap> sentences = document.get(SentencesAnnotation.class);
  CoreMap sentence = sentences.get(sentNum-1);
  List<CoreLabel> tokens = sentence.get(TokensAnnotation.class);
  List<CoreLabel> spanningTokens = new ArrayList<CoreLabel>();
  
  for(int i = startIndex; i < endIndex; i++){
    spanningTokens.add(tokens.get(i-1));
  }

  return Interval.closed(spanningTokens.get(0).beginPosition(),spanningTokens.get(spanningTokens.size()-1).endPosition());

}

public Interval getIntervalOfKBPEntityMention(String kbpEntityString, Interval originalInterval, String docID){

	Annotation document;

	if(corefAnnotationMap.containsKey(docID)){
      document = corefAnnotationMap.get(docID);
    }
    else{
      String xmlDoc = SolrHelper.getRawDoc(docID);
      if(xmlDoc.trim().isEmpty()){
        return null;
      }

      document = new Annotation(xmlDoc);

      try{
        System.out.println("Annotating document "+ docID);
        System.out.println("Document has size " + DocUtils.docLength(docID));
        corefPipeline.annotate(document);
        System.out.println("Done Annotating document "+ docID);
        corefAnnotationMap.put(docID, document);
      }
      catch (Exception e){
        if(corefAnnotationMap.containsKey(docID)){
          corefAnnotationMap.remove(docID);
        }
        return null;
      }
    }

    //get token of possible coref mention
    CoreLabel token = getTokenBeginningAtByteOffset(document, originalInterval.start());
    if(token == null){
      return null;
    }
    Integer corefID = token.get(CorefClusterIdAnnotation.class);
    if(corefID == null){
      return null;
    }
    Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
    List<CorefMention> mentionsInOrder = graph.get(corefID).getMentionsInTextualOrder();

    for(CorefMention corefMention : mentionsInOrder){
      if (corefMention.mentionSpan.trim().toLowerCase().equals(kbpEntityString.trim().toLowerCase())){
        // this is a match and the originalInterval corefers to the kbpEntityString
        // return the proper interval of this mention of the kbpEntityString
        return getCharIntervalFromCorefMention(document,corefMention.sentNum,corefMention.startIndex,corefMention.endIndex);
      }
    }

    return null;
  }
	
  public Boolean inSameCorefChainKBPEntityMentionAndQueryName(String kbpEntityString, String arg1String, Interval originalInterval, Annotation document){

    // Search through each coref chain looking for arg1 (specified by originalInterval) and kbpEntityString   

	//Boolean containsArg1String = false;
	//Boolean containsEntityString = false;
	  Boolean inSameCorefChain = false;
	  
    //System.out.println("SHM:" +  arg1String + " " + kbpEntityString );
	
	for (Integer key: document.get(CorefChainAnnotation.class) .keySet()) {
		
		Boolean containsArg1String = false;
		Boolean containsEntityString = false;
		inSameCorefChain = false;
		
      for (CorefMention mention: document.get(CorefChainAnnotation.class).get(key).getMentionsInTextualOrder()) {
			
    	  if (mention.mentionSpan.trim().toLowerCase().equals(kbpEntityString.trim().toLowerCase()))
    	  {  containsEntityString = true;
    	     //System.out.println("Contains Entity String");
    	  }
    	  if (mention.mentionSpan.trim().toLowerCase().equals(arg1String.trim().toLowerCase()))
    	  {  containsArg1String = true; 
    	     //System.out.println("Contains Arg1 String");
    	  }	  
          if(containsEntityString & containsArg1String) {
        	  //System.out.println("Contains Both Strings");
        	  inSameCorefChain = true; 
        	  break;
          }
    	  
      }
      if(inSameCorefChain) break;
    }  	  
	  
    //Map<Integer, CorefChain> graph = document.get(CorefChainAnnotation.class);
    //System.out.println("Stanford: mentions: " + graph.size());
    //System.out.println("Stanford: mentions: " + graph.keySet());
    //List<CorefMention> mentionsInOrder = graph.get(corefID).getMentionsInTextualOrder();
    
    //for(CorefMention corefMention : mentionsInOrder){
      //System.out.println("Stanford: mention:" + corefMention.toString() ); 	
      //if (corefMention.mentionSpan.trim().toLowerCase().equals(kbpEntityString.trim().toLowerCase())){
        // this is a match and the originalInterval corefers to the kbpEntityString
        // return the proper interval of this mention of the kbpEntityString
    	 
       // return getCharIntervalFromCorefMention(document,corefMention.sentNum,corefMention.startIndex,corefMention.endIndex);
      //}
   // }

    return inSameCorefChain;
  }


}
