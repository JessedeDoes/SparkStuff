package wsd.features;

import impact.ee.classifier.Distribution;
import impact.ee.classifier.StochasticFeature;


import java.util.*;

import wsd.WSDInstance;


public class BoCLFeature extends StochasticFeature
{
	/**
	 * 
	 */
	List<ClusterAtFeature> clusterFeatures = new ArrayList<ClusterAtFeature>();
	
	private static final long serialVersionUID = 1L;
	int k=0;

	public BoCLFeature(int k)
	{
		this.k = k;
		this.name = "boc_" + k;
		for (int i=1; i < k; i++)
		{
			clusterFeatures.add(new ClusterAtFeature(4,k));
			clusterFeatures.add(new ClusterAtFeature(4,-k));
		}
	}
	
	@Override
	public Distribution getValue(Object o)
	{
		Distribution d = new Distribution();
		WSDInstance c = (WSDInstance) o;
		Set<String> bag = new HashSet<String>();
		for (ClusterAtFeature cf: clusterFeatures)
			bag.add(cf.getValue(o));

		bag.remove(null);
		for (String s: bag)
			d.incrementCount(s);
		d.computeProbabilities();	
		//System.err.println(d);
		return d;
	}
}