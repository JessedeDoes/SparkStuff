package wsd.features;
import impact.ee.classifier.Feature;
import impact.ee.tagger.Context;
import impact.ee.tagger.features.*;

public class ClusterAtFeature  extends Feature
{
	public static String clusterFileName = "Data/brownClusters.from.sanders.txt";
	ClusterFeature base=null;

	public ClusterAtFeature(int d, int k)
	{
		this(clusterFileName,d,k);
	}
	
	public ClusterAtFeature(String fileName, int d, int k)
	{
		base = new ClusterFeature(fileName, d, k) 
		{
			@Override
			public String getValue(Object o)
			{
				try
				{

					String s = ((wsd.WSDInstance) o).wordAt(this.k);
					if (s == null)
						return "#";

					String cluster = this.word2cluster.get(s);
					
					if (cluster != null && cluster.length() > this.depth)
					{
						return cluster.substring(0, this.depth);
					}
				} catch (Exception e)
				{
					e.printStackTrace();
				}
				return null;
			}
		};
		this.name = base.name;
	}
	
	@Override
	public String getValue(Object o)
	{
		return base.getValue(o);
	}
}
