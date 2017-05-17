package wsd.features;

import impact.ee.classifier.Distribution;
import impact.ee.classifier.StochasticFeature;


import java.util.*;

import wsd.WSDInstance;


public class BoLFeature extends StochasticFeature
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	int k=0;

	public BoLFeature(int k)
	{
		this.k = k;
		this.name = "bol_" + k;
	}
	
	@Override
	public Distribution getValue(Object o)
	{
		Distribution d = new Distribution();
		WSDInstance c = (WSDInstance) o;
		Set<String> bag = new HashSet<String>();
		for (int i=1; i <  k; i++ )
		{
			bag.add(c.lemmaAt(k));
			bag.add(c.lemmaAt(-k));
		}
		bag.remove(null);
		for (String s: bag)
			d.incrementCount(s);
		d.computeProbabilities();	
		//System.err.println(d);
		return d;
	}
}