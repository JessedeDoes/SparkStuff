package wsd;
import java.io.Serializable;


public class Token implements Serializable
{
	private String lemma;
	private String PoS;
	private String tokenId;
	private String word;
	private int position;
	public String getWord()
	{
		return word;
	}
	public void setWord(String word)
	{
		this.word = word;
	}
	public int getPosition()
	{
		return position;
	}
	public void setPosition(int position)
	{
		this.position = position;
	}
	public String getPoS()
	{
		return PoS;
	}
	public void setPoS(String poS)
	{
		PoS = poS;
	}
	public String getLemma()
	{
		return lemma;
	}
	public void setLemma(String lemma)
	{
		this.lemma = lemma;
	}
	public String getTokenId()
	{
		return tokenId;
	}
	public void setTokenId(String tokenId)
	{
		this.tokenId = tokenId;
	}
}