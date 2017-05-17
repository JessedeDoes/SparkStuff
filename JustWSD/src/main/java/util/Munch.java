package util;

import java.io.BufferedReader;
import java.io.FileReader;

public class Munch {
	public static String munch(String fileName)
	{
		String r="" ;
		try
		{
			BufferedReader b = new BufferedReader(new FileReader(fileName));
			String line;
			while ((line = b.readLine()) != null )
			{
				r +=  line +  "\n";
			}
		} catch (Exception e)
		{
			
		}
		return r;
	}
}
