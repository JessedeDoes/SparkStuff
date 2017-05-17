package util;
import java.util.*;
import java.awt.Color;


public class ColorUtils
{
	
	public static String getRGBString(Color c)
	{
		return "#"  + String.format("%02x%02x%02x",  c.getRed(), c.getGreen(), c.getBlue());
	}
	
	public static List<String> colorLabels(List<String> labels)
	{
		Map<String,String> colorMap = new HashMap();
		for (String l:  labels)
		{
			colorMap.put(l, "black");
		}
		List<Color> colors = pick(colorMap.keySet().size());
		int k=0;
		for (String l:  colorMap.keySet())
		{
			colorMap.put(l, getRGBString(colors.get(k++)));
		}
		List<String> colorList = new ArrayList();
		for (String l: labels)
		{
			colorList.add(colorMap.get(l));
		}
		return colorList;
	}
	public static List<Color> pick(int num) {
		List<Color> colors = new ArrayList<Color>();
		if (num < 2)
			return colors;
		float dx = 1.0f / (float) (num - 1);
		for (int i = 0; i < num; i++) {
			colors.add(get(i * dx));
		}
		return colors;
	}

	public static Color get(float x) {
		float r = 0.0f;
		float g = 0.0f;
		float b = 1.0f;
		if (x >= 0.0f && x < 0.2f) {
			x = x / 0.2f;
			r = 0.0f;
			g = x;
			b = 1.0f;
		} else if (x >= 0.2f && x < 0.4f) {
			x = (x - 0.2f) / 0.2f;
			r = 0.0f;
			g = 1.0f;
			b = 1.0f - x;
		} else if (x >= 0.4f && x < 0.6f) {
			x = (x - 0.4f) / 0.2f;
			r = x;
			g = 1.0f;
			b = 0.0f;
		} else if (x >= 0.6f && x < 0.8f) {
			x = (x - 0.6f) / 0.2f;
			r = 1.0f;
			g = 1.0f - x;
			b = 0.0f;
		} else if (x >= 0.8f && x <= 1.0f) {
			x = (x - 0.8f) / 0.2f;
			r = 1.0f;
			g = 0.0f;
			b = x;
		}
		return new Color(r, g, b);
	}
	
	public static void main(String[] args)
	{
		String[] aap = {"aap", "noot", "aap", "mies", "aap"};
		List<String> l = Arrays.asList(aap);
		System.out.println(l +  "->" + colorLabels(l));
	}
}
