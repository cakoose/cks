package cks.workbench;

import javax.swing.*;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

public class WorkbenchApplet extends JApplet
{
	public void init()
	{
		try {
			SwingUtilities.invokeAndWait(new Runnable() {
				public void run()
				{
					try {
						UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
					}
					catch (Exception ex) {
						ex.printStackTrace(System.err);
						return;
					}

					String type = decode(getParameter("type"));
					String value = decode(getParameter("value"));

					Workbench workbench = new Workbench(Workbench.getDefaultConfig(), type, value);
					setContentPane(workbench);
				}
			});
		}
		catch (Exception ex) {
			ex.printStackTrace(System.err);
		}
	}

	private static String decode(String v)
	{
		if (v == null) return "";
		try {
			return URLDecoder.decode(v, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			System.err.println("Unable to decode URL: " + e.getMessage());
			return "";
		}
	}
}

