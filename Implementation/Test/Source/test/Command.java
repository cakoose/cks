package cks.test;

import cakoose.util.text.StringUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Command
{
	public final Map<String,String> env;
	public final List<String> command;

	public Command(Map<String,String> env, List<String> command)
	{
		this.env = env;
		this.command = command;
	}

	public Command(List<String> command)
	{
		this(new HashMap<String,String>(), command);
	}

	public Command(String... command)
	{
		this(new HashMap<String,String>(), Arrays.asList(command));
	}

	public Command appendArgs(Collection<String> args)
	{
		return new Command(new HashMap<String,String>(this.env), extendList(command, args));
	}

	private static <T> List<T> extendList(List<T> base, Collection<T> more)
	{
		ArrayList<T> l = new ArrayList<T>(base);
		l.addAll(more);
		return l;
	}

	public String toStringCommand()
	{
		StringBuilder sb = new StringBuilder();
		String sep = "";
		for (String s : command) {
			sb.append(sep); sep = " ";
			sb.append(StringUtil.jq(s));
		}
		return sb.toString();
	}
}
