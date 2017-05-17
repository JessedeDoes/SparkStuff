package folia;

import org.w3c.dom.*;

import org.ivdnt.util.XML;

import java.util.*;
import java.io.*;
import java.util.zip.*;

public class Folia
{
	public Document document;
	
	public Folia(String fileName)
	{
		try
		{
			InputStream s0 = new FileInputStream(new File(fileName));
			InputStream s1;
			if (fileName.endsWith(".gz"))
			{
				s1 = new GZIPInputStream(s0);
			} else
			{
				s1 = s0;
			}
			document = XML.parseStream(s1, false);
		}  catch (Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public static String getWord(Element t)
	{
		 return XML.getElementByTagname(t, "t").getTextContent();
	}

	public static String getPoS(Element t)
	{
		List<Element> poss = XML.getElementsByTagnameAndAttribute(t, "pos", "set", 
				"http://ilk.uvt.nl/folia/sets/frog-mbpos-cgn", false);
		return poss.get(0).getAttribute("class");
	}
	
	public static String getLemma(Element t)
	{
		List<Element> lemmata = XML.getElementsByTagnameAndAttribute(t, "lemma", "set", 
				"http://ilk.uvt.nl/folia/sets/frog-mblem-nl", false);
		return lemmata.get(0).getAttribute("class");
	}
}
