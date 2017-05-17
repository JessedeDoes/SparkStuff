package wsd;

import org.w3c.dom.*;

import org.ivdnt.util.XML;

import folia.Folia;

import java.util.*;
import java.io.*;
import java.util.zip.*;

public class WSDInstance implements Serializable
{
	private static final long serialVersionUID = 1L;
	String lemma;
	String pos;
	public String senseId;
	public String tokenId;
	public String word;
	//transient Element targetToken;
	//transient List<Element> tokenElements;
	transient public List<Token> tokens = new ArrayList<Token>(); // make this transient and save arrays in the instance base
	public int targetPosition;
	int firstToken;
	int length;
	
	//transient Element sentence;
	
	public String toString()
	{
		return this.wordKey()  + "\t" +  this.senseId + "\t"  + this.plainSentence();
	}
	
	public String plainSentence()
	{
		String r = "";
		for (Token t: this.tokens)
		{
			if (r.length()  > 0)
				r += " ";
			r += t.getWord();
			if (t.getPosition() == targetPosition)
			{
				r += "/" + this.senseId;
			}
		}
		return r;
	}
	
	public String wordAt(int k)
	{
		int  p = this.targetPosition + k;
	
		if (p  >=0 && p < this.length)
		{
			return this.tokens.get(p).getWord();
		}
		return null;
	}
	
	public String PoSAt(int k)
	{
		int  p = this.targetPosition + k;
	
		if (p  >=0 && p < this.length)
		{
			return this.tokens.get(p).getPoS();
		}
		return null;
	}
	
	public String lemmaAt(int k)
	{
		int  p = this.targetPosition + k;
	
		if (p  >=0 && p < this.length)
		{
			return this.tokens.get(p).getLemma();
		}
		return null;
	}
	
	public WSDInstance(Element sentence, Element token)
	{
		Element targetToken = token;
		lemma = Folia.getLemma(token);
		pos = Folia.getPoS(token);
		senseId = token.getAttribute("sense");
		word = Folia.getWord(token);
		tokenId = token.getAttribute("xml:id");
		List<Element> tokenElements = XML.getElementsByTagname(sentence, "w", false);
		for (int i=0; i < tokenElements.size(); i++)
		{
			Element te = tokenElements.get(i);
			if (tokenElements.get(i) == token)
			  targetPosition = i;
			Token t = new Token();
			t.setPosition(i);
			t.setPoS(Folia.getPoS(te));
			t.setLemma(Folia.getLemma(te));
			t.setWord(Folia.getWord(te));
			t.setTokenId(te.getAttribute("xml:id"));
			tokens.add(t);
		}
	}
	
	@Override
	public boolean equals(Object other)
	{
		WSDInstance o = (WSDInstance) other;
		return o.tokenId.equals(this.tokenId);
	}
	
	@Override
	public int hashCode()
	{
		return this.tokenId.hashCode();
	}
	
	public String basicPoS()
	{
		String p = pos;
		if (p.startsWith("N"))
		{
			return "n";
		}
		if (p.startsWith("ADJ"))
		{
			return "a";
		}
		if (p.startsWith("WW"))
		{
			return "v";
		}
		return "?";
	}
	
	
	
	public String wordKey()
	{
		return lemma + ":"  + basicPoS();
	}
	
	public static List<WSDInstance> getInstances(Element sentence)
	{
		List<WSDInstance> wsdi = new ArrayList<WSDInstance>();
		List<Element> tokens = XML.getElementsByTagname(sentence, "w", false);
		for (Element t: tokens)
		{
			if (t.getAttribute("sense") != null && t.getAttribute("sense").length() > 0 )
			{
				System.err.println(Folia.getLemma(t));
				wsdi.add(new WSDInstance(sentence, t));
			}
		}
		return wsdi;
	}
}
