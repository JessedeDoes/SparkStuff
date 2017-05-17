package wsd.features;

import java.util.List;

public interface ScoreFunction
{
	public List<Double> score(String sentence, List<String> candidates);
}