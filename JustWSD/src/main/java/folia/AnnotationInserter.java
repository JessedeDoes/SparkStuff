package folia;


import org.apache.tools.tar.*;
import java.io.*;
import java.util.zip.*;
import java.util.*;
import org.w3c.dom.*;

import org.ivdnt.util.XML;

public class AnnotationInserter
{

	String xmlFile1 = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor/dscXML/1.2.1.HUMAN_ANNOTATIONS/human.annotations.DSC.nouns.xml";
	String xmlFile2 = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor/dscXML/1.2.1.HUMAN_ANNOTATIONS/human.annotations.DSC.adjs.xml";
	String xmlFile3 = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor/dscXML/1.2.1.HUMAN_ANNOTATIONS/human.annotations.DSC.verbs.xml";

	String DSCFolder = "/mnt/Projecten/Taalbank/CL-SE-data/Corpora/DutchSemCor";
	String CrossFoldFolder =  DSCFolder + "/FoldCross-2014-04-24/FoldCross";

	int outputSentences = 0;
	int tokensSeen = 0;
	int sensesFound=0;
	int sensesToFind=0;
	int previousPortion = 0;
	int filesSeen=0;
	String archiveFolder = "/datalokaal/Corpus/OpenSonar/";
	String archiveFolder1 = "/datalokaal/Corpus/OpenSonar/CrossFold";
	TarInputStream tar;
	TarEntry current;
	int portionSize;
	Map<String,String> annotationMap = new HashMap<String,String>();
	Map<String,String> lemmaMap = new HashMap<String,String>();
	Set<String> foundIdSet = new HashSet<String>();
	Set<String> usefulFiles = new HashSet<String>();
	String outputFileName = "/mnt/Scratch/jesse/SONAR/sonarSemcor.out.0.xml.gz";
	PrintWriter out = null;

	public void readXMLAnnotations(String fileName)
	{
		try
		{
			Document d = XML.parse(fileName);
			List<Element> tokens = XML.getElementsByTagname(d.getDocumentElement(), "token", false);
			int k=0;
			for (Element t: tokens)
			{
				String tokenId = t.getAttribute("token_id");
				String sense = t.getAttribute("sense");
				annotationMap.put(tokenId, sense);
				lemmaMap.put(tokenId,  t.getAttribute("lemma"));
				String fileId = tokenId.replaceAll("\\..*", "");
				usefulFiles.add(fileId);
				sensesToFind++;
				if (k++ % 1000 == 0)
				{
					System.err.println(tokenId + " " + sense + "  " + fileId);
				}
			}
		} catch (Exception e)
		{

		}
	}

	public void checkArchives(String folderName)
	{
		File d = new File(folderName);
		try
		{
			//out = new PrintWriter(new FileWriter(new File(this.outputFileName)));
			out = 
					new PrintWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(new File(this.outputFileName)))));
		} catch (IOException e1)
		{
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		for (File f: d.listFiles())
		{
			if (f.getName().endsWith("tar.gz"))
			{
				try
				{
					checkFilesInArchive(f.getCanonicalPath());
				} catch (IOException e)
				{
					e.printStackTrace();
				}
			}
		}
		for (String tid: this.annotationMap.keySet())
		{
			if (!this.foundIdSet.contains(tid))
			{
				System.err.println("TOKEN NOT FOUND IN CORPUS: " + tid);
			}
		}
	}
	
	public static File copyStream(InputStream fis) throws Exception 
	{
		File f = File.createTempFile("aap", "txt");
		FileOutputStream fos = new FileOutputStream(f);
		try {
			byte[] buf = new byte[1024];
			int i = 0;
			while ((i = fis.read(buf)) != -1) 
			{
				fos.write(buf, 0, i);
			}
			return f;
		} 
		catch (Exception e) {
			throw e;
		}
		finally {
			//if (fis != null) fis.close();
			if (fos != null) fos.close();
		}
	}

	public void checkFilesInArchive(String archiveFilename)
	{
		try
		{
			tar = new TarInputStream(new GZIPInputStream(new FileInputStream(archiveFilename))); 

			int k=0;
			while ((current = tar.getNextEntry()) != null)
			{
				String name  =  current.getName();
				String finalPart = name.replaceAll(".*\\/", "");
				String fileId = finalPart.replaceAll("\\..*", "");
				filesSeen++;
				if (k++ % 10000 == 0)
				{
					System.err.println("entry name:"  + current.getName() + " /// " + fileId + " total entries seen so far: " + filesSeen);
					//System.err.println("words seen so far: " + tokensSeen);
				}	

				long s = current.getSize();

				if (!current.isDirectory()    &&  (usefulFiles.contains(fileId)))
				{
					File tempFile = copyStream(tar);
					tempFile.deleteOnExit();
					Document d = XML.parse(tempFile.getCanonicalPath(),false);
					tempFile.delete();

					//System.err.println(content);
					List<Element> words = XML.getElementsByTagname(d.getDocumentElement(), "w", false);
					tokensSeen += words.size();
					//System.err.println("parsed XML file:"  + current.getName() + " words: " + words.size());
					Set<Element> interestingSentences= new HashSet<Element>();
					for (Element w: words)
					{
						String tokenId = w.getAttribute("xml:id");
						String sense=null;
						if ((sense = annotationMap.get(tokenId)) != null)
						{
							this.foundIdSet.add(tokenId);
							//System.err.println("Hah! " + sense + " : "   +  this.lemmaMap.get(tokenId) + "@" + tokenId + "  match nr : " + sensesFound++  +  " out of: " + this.sensesToFind);
							if (!usefulFiles.contains(fileId))
							{
								System.err.println("CRASH!");
								System.exit(1);
							}
							w.setAttribute("sense", sense);
							w.setAttribute("lemmaCheck",  this.lemmaMap.get(tokenId));
							Element p = (Element) w.getParentNode();
							while (p != null && p.getNodeType() == Node.ELEMENT_NODE)
							{
								if (p.getNodeName() == "s")
								{
									interestingSentences.add(p);
									this.outputSentences++;
									break;
								}
								p = (Element) p.getParentNode();
							}
						}
					}
					for (Element sent: interestingSentences)
					{
						out.println(XML.NodeToString(sent));
					}
					out.flush();
					if (this.outputSentences  > 0 && this.outputSentences - previousPortion > 500)
					{
						out.close();
						String newName = this.getOutputFileName(outputSentences);
						out = new PrintWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(new File(newName)))));
						//out = new PrintWriter(new FileWriter(new File())));
						previousPortion = outputSentences;
					}
					// <w xml:id="WR-P-E-F-0000001334.head.1.s.1.w.1">
					//System.err.println(current.getFile());
					//f.storeFile(tar,current.getName());
				}
			}
		} catch (Exception e)
		{
			e.printStackTrace();
		}
	} 

	private String getOutputFileName(int outputSentences2)
	{
		// TODO Auto-generated method stub
		System.err.println("at sentence " + outputSentences2);
		return "/mnt/Scratch/jesse/SONAR/sonarSemcor.out." +outputSentences2 + ".xml.gz";
	}



	/**
	 * WR-P-E-J-0000076136.p.1.s.41.w.13	d_a-158535	lijvig	a	3	Elizabeth;Marlisa	2011-12-27 14:11:01;2011-10-02 11:31:48
	 * @param fileName
	 */
	public void collectSentencesFromTabSep(String fileName)
	{
		int k=0;
		try
		{
			BufferedReader br = new BufferedReader(new FileReader(fileName));
			String line;
			while (( line = br.readLine() )!= null)
			{
				String[] parts = line.split("\\s*\\t\\s*");
				String tokenId = parts[0];
				String senseId = parts[1];
				String lemma = parts[2];
				String PoS = parts[3];
				lemma = lemma.trim();
				PoS = PoS.trim();
				//System.err.println(lemma + ":" + PoS);
				annotationMap.put(tokenId, senseId);
				lemmaMap.put(tokenId,  lemma +  ":" + PoS);
				String fileId = tokenId.replaceAll("\\..*", "");
				usefulFiles.add(fileId);
				sensesToFind++;
				if (k++ % 10000 == 0)
				{
					System.err.println(tokenId + " " + senseId + "  " + fileId);
				}
			}
		} catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void collectSentencesFromDir(String dirName)
	{
		File f = new File(dirName);
		for (File fn: f.listFiles())
		{
			try
			{
				if (fn.isDirectory())
					collectSentencesFromDir(fn.getCanonicalPath());
				else
					collectSentencesFromTabSep(fn.getCanonicalPath());
			} catch (Exception e)
			{
				e.printStackTrace();
			}
		}
	}

	protected static void collectSentencesFromDscXML(AnnotationInserter ai)
	{
		ai.readXMLAnnotations(ai.xmlFile1);
		ai.readXMLAnnotations(ai.xmlFile2);
		ai.readXMLAnnotations(ai.xmlFile3);
		ai.checkArchives(ai.archiveFolder);
	}


	public static void main(String[] args)
	{
		AnnotationInserter ai = new AnnotationInserter();
		//collectSentencesFromDscXML(ai);
		ai.collectSentencesFromDir(ai.CrossFoldFolder);
		ai.checkArchives(ai.archiveFolder);
	}
}

/**
 * <tokens>
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-A-0000524280.p.7.s.1.w.29" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-P-F-0000000013.p.1428.s.1.w.22" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-J-0000032165.p.5.s.47.w.21" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-J-0000037084.p.7.s.2.w.6" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-A-0000565068.p.12.s.2.w.6" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="CGN-comp-g_fn000174.s.23.w.6" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-J-0000013471.p.3.s.2.w.10" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="CGN-comp-l_fv600043.s.11.w.3" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-J-0000067966.p.3.s.6.w.16" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-P-F-0000000013.p.1889.s.1.w.16" />
<token lemma="vitaal" pos="a" sense="c_546738" token_id="WR-P-E-J-0000056096.p.6.s.5.w.3" />
 */