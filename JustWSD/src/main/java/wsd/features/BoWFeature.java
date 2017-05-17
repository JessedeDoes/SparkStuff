package wsd.features;

import impact.ee.classifier.Distribution;
import impact.ee.classifier.StochasticFeature;


import java.util.*;

import wsd.WSDInstance;


public class BoWFeature extends StochasticFeature
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	int k=0;

	public BoWFeature(int k)
	{
		this.k = k;
		this.name = "bow_" + k;
	}
	
	
	@Override
	public Distribution getValue(Object o)
	{
		Distribution d = new Distribution();
		WSDInstance c = (WSDInstance) o;
		Set<String> bag = new HashSet<String>();
		for (int i=1; i <  k; i++ )
		{
			bag.add(c.wordAt(k));
			bag.add(c.wordAt(-k));
		}
		bag.remove(null);
		for (String s: bag)
			d.incrementCount(s);
		d.computeProbabilities();	
		//System.err.println(d);
		return d;
	}
}