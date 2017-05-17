package wsd.features;

import impact.ee.classifier.Feature;
import impact.ee.tagger.Context;

public class LemmaAtFeature extends Feature
{
	private static final long serialVersionUID = 1L;
	int k;
	boolean toLowercase = true;
	
	public LemmaAtFeature(int x)
	{
		k=x;
		name = "word_" + k;
	}
	
	public String getValue(Object o)
	{
		String s = ((wsd.WSDInstance) o).lemmaAt(k);
		if (s == null)
			return "#";
		return toLowercase?s.toLowerCase():s;
	}
}
