package wsd;

import java.io.File;

public class DumpInstanceBase 
{
	public static void main(String[] args)
	{
	
		String instanceBaseFile = "s:/jesse/instanceBase";
		if (args.length > 0)
			instanceBaseFile = args[0];

		WSDInstanceBase b ;
		

		b = WSDInstanceBase.loadFromFile(instanceBaseFile);
		b.print();
		
	}
}
