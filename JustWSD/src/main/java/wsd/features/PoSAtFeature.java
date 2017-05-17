package wsd.features;

import impact.ee.classifier.Feature;
import impact.ee.tagger.Context;

public class PoSAtFeature extends Feature
{
	private static final long serialVersionUID = 1L;
	int k;
	boolean toLowercase = true;
	
	public PoSAtFeature(int x)
	{
		k=x;
		name = "pos_" + k;
	}
	
	public String getValue(Object o)
	{
		String s = ((wsd.WSDInstance) o).PoSAt(k);
		if (s == null)
			return "#";
		return toLowercase?s.toLowerCase():s;
	}
}
