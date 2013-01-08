package cks.test;

import java.util.List;
import java.io.PrintWriter;
import java.io.File;

public abstract class Generator
{
	public abstract Processor.Factory generate(PrintWriter err, List<String> cksToolCommand, File typeFile, String typeName, File baseDir);
}
