package cks.test;

import java.io.PrintWriter;
import java.io.File;

public abstract class Impl
{
	public abstract Generator makeGenerator(PrintWriter err, File implsBaseDir);
}
