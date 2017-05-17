package wsd;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.*;
import java.io.*;

import org.w3c.dom.Element;

import org.ivdnt.util.XML;

import folia.Folia;


public class WSDInstanceBase extends HashMap<String, Set<WSDInstance>> implements Serializable
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	transient List<String> words = new ArrayList<String> ();
	transient List<String> lemmata = new ArrayList<String> ();
	transient List<String> PoS = new ArrayList<String> ();
	transient List<String> tokenIds = new ArrayList<String> ();

	String[] wordsArray ;
	String[] lemmaArray;
	String[]  PoSArray;
	String[] tokenIdArray;

	private void createTokenArrays()
	{
		words = new ArrayList<String> ();
		 lemmata = new ArrayList<String> ();
		PoS = new ArrayList<String> ();
		tokenIds = new ArrayList<String> ();

		int p=0;
		for (Set<WSDInstance> V: this.values())
		{
			for (WSDInstance w:  V)
			{
				w.firstToken = p;
				for (Token t: w.tokens)
				{
					words.add(t.getWord());
					lemmata.add(t.getLemma());
					PoS.add(t.getPoS());
					tokenIds.add(t.getTokenId());
					p++;
				}
				w.length = w.tokens.size();
			}
		}
		wordsArray  = new String[p];
		lemmaArray  = new String[p];
		PoSArray  = new String[p];
		tokenIdArray  = new String[p];
		wordsArray =words.toArray(wordsArray);
		lemmaArray = lemmata.toArray(lemmaArray);
		PoSArray = PoS.toArray(PoSArray);
		tokenIdArray = tokenIds.toArray(tokenIdArray);
		System.err.println("Arrays created; Total tokens: " + p);
	}
	
	public int nSenses(String key)
	{
		Set<String> senses = new HashSet<String>();
		for (WSDInstance i: this.get(key))
		{
			senses.add(i.senseId);
		}
		return senses.size();
	}
	
	private void unpackTokenArrays()
	{
		for (Set<WSDInstance> V: this.values())
		{
			for (WSDInstance w:  V)
			{
				w.tokens = new ArrayList<Token>();
				int p = w.firstToken;
				for (int i=p; i < p + w.length; i++)
				{
					Token t  = new Token();
					t.setPosition(i  - w.firstToken);
					t.setWord(wordsArray[i]);
					t.setLemma(lemmaArray[i]);
					t.setPoS(PoSArray[i]);
					t.setTokenId(tokenIdArray[i]);
					w.tokens.add(t);
				}
			}
		}
		wordsArray = lemmaArray = tokenIdArray = PoSArray = null;
	}
	
	public void saveToFile(String fileName)
	{
		try
		{
			createTokenArrays();
			FileOutputStream fileOut =
					new FileOutputStream(fileName);
			ObjectOutputStream out =
					new ObjectOutputStream(fileOut);
			out.writeObject(this);
			out.close();
			fileOut.close();
		} catch(IOException i)
		{
			i.printStackTrace();
		}
	}

	public  static WSDInstanceBase loadFromFile(String fileName)
	{
		try
		{

			FileInputStream fileIn =
					new FileInputStream(fileName);
			ObjectInputStream in =
					new ObjectInputStream(fileIn);
			WSDInstanceBase t = (WSDInstanceBase) in.readObject();
			in.close();
			fileIn.close();
			t.unpackTokenArrays();
			return t;
		} catch(Exception i)
		{
			i.printStackTrace();
			return null;
		}
	}

	public static void collectWSDInstances(Map<String, Set<WSDInstance>> map, Folia f)
	{
		List<Element> sentences = XML.getElementsByTagname(f.document.getDocumentElement(), 
				"s", false);
		System.err.println("sentences in file: " + sentences.size());
		for (Element s: sentences)
		{
			List<WSDInstance> instances = WSDInstance.getInstances(s);
			for (WSDInstance wsd: instances)
			{
				String key = wsd.wordKey();
				if (map.get(key) == null)
					map.put(key, new HashSet<WSDInstance>());
				map.get(key).add(wsd);
			}
		}
	}
	
	public void sample(int sampleSize)
	{
		Set<String> keySet = this.keySet();
		List<String> keys = new ArrayList<String>();
		keys.addAll(keySet);
		Collections.shuffle(keys);
		Set<String> newKeySet = new HashSet<String>();
		for (int i=0; i < sampleSize; i++) newKeySet.add(keys.get(i));
		for (String s: keys)
		{
			if (!newKeySet.contains(s))
				this.remove(s);
		}
	}
	
	public WSDInstanceBase(File directory)
	{
		int k=0;
		for (String f: directory.list())
		{
			if (f.toLowerCase().contains("xml"))
			{
				try
				{
					String full = directory.getCanonicalPath() + "/" + f;
					System.err.println(full);
					Folia folia = new Folia(full);
					collectWSDInstances(this,folia);
					//if (k++ > 100) break;
				} catch (Exception e)
				{
					e.printStackTrace();
				}
			}
		}
		System.err.println("size: " + this.totalSize() + " " +  this.keySet());
	}

	public int totalSize()
	{
		int n=0;
		for (String s: this.keySet())
		{
			Set<WSDInstance> v = this.get(s);
			n += v.size();
		}
		return n;
	}

	public void print()
	{
		for (String s: this.keySet())
		{
			Set<WSDInstance> v = this.get(s);
			for (WSDInstance i: v)
				System.out.println(i);
		}
	}

	public static void main(String[] args)
	{
		String baseDir = "n:/Taalbank/cl-se-data/corpora/dutchsemcor/sentencesfromsonarwithannotation/Corrected";
		if (args.length > 0)
			baseDir = args[0];
		String instanceBaseFile = "s:/jesse/instanceBase";
		if (args.length > 1)
			instanceBaseFile = args[1];

		WSDInstanceBase b ;
		boolean create = true;
		if (create)
		{
			b = new WSDInstanceBase(new 
					File(baseDir));
			// b.print();
			b.saveToFile(instanceBaseFile);
			System.err.println("instance base saved!");
		};

		b = WSDInstanceBase.loadFromFile(instanceBaseFile);
		// b.print();
		b.sample(300);
		b.saveToFile(instanceBaseFile + ".sample");
	}
}
