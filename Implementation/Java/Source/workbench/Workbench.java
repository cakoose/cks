package cks.workbench;

import cakoose.util.Either;
import cakoose.util.Func;
import cks.Maybe;
import cakoose.util.Pair;
import cks.io.*;
import cks.dynamic.GSurfaceConverter;
import cks.dynamic.GValue;
import cks.surface.SurfaceParser;
import cks.type.model.TDef;
import cks.type.model.TExpr;
import cks.type.model.TKind;
import cks.type.model.TModule;
import cks.type.model.TOpaque;
import cks.type.parser.TypeParser;
import cks.type.resolver.TypeResolver;
import cks.surface.model.Value;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import java.awt.*;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.*;
import java.util.List;

public class Workbench extends JPanel
{
	private Editor typeEditor;
	private Editor valueEditor;

	private JTextArea typeFeedback;
	private JTextArea valueFeedback;

	public Workbench(final Config config, String initialType, String initialValue)
	{
		this.setLayout(new BorderLayout());
		this.setBackground(config.background);

		JPanel labelPanel = new JPanel(new GridLayout(1, 2, 2, 2));
		this.add(labelPanel, BorderLayout.NORTH);

		labelPanel.setBackground(config.labelBackground);

		JLabel typeLabel = new JLabel("CKS Type:");
		typeLabel.setBackground(config.labelBackground);
		typeLabel.setForeground(config.labelForeground);
		labelPanel.add(typeLabel);

		JLabel valueLabel = new JLabel("CKS Value:");
		valueLabel.setBackground(config.labelBackground);
		valueLabel.setForeground(config.labelForeground);
		labelPanel.add(valueLabel);

		JPanel editorPanel = new JPanel(new GridLayout(1, 2, 2, 2));
		this.add(editorPanel, BorderLayout.CENTER);

		typeEditor = makeEditor(config);
		valueEditor = makeEditor(config);

		editorPanel.add(typeEditor.scrollPane, null);
		editorPanel.add(valueEditor.scrollPane, null);

		JPanel feedbackPanel = new JPanel(new GridLayout(1, 2, 2, 2));
		this.add(feedbackPanel, BorderLayout.SOUTH);

		typeFeedback = new JTextArea();
		valueFeedback = new JTextArea();
		typeFeedback.setBackground(config.messageBackground);
		valueFeedback.setBackground(config.messageBackground);
		typeFeedback.setWrapStyleWord(true);
		valueFeedback.setWrapStyleWord(true);
		valueFeedback.setLineWrap(true);
		typeFeedback.setLineWrap(true);
		typeFeedback.setEditable(false);
		valueFeedback.setEditable(false);
		typeFeedback.setRows(2);
		valueFeedback.setRows(2);

		feedbackPanel.add(typeFeedback, null);
		feedbackPanel.add(valueFeedback, null);

		DValue<Maybe<String>> typeInputV = new DocumentDValue(typeEditor.textArea.getDocument(), 200);
		DValue<Maybe<String>> valueInputV = new DocumentDValue(valueEditor.textArea.getDocument(), 200);

		class CheckResult
		{
			public final TExpr expr;
			public final String error;

			CheckResult(TExpr expr, String error)
			{
				this.expr = expr;
				this.error = error;
			}
		}

		DValue<Either<CheckResult,Problem>> typeCheckV = DValue.Func(typeInputV, new Func<Maybe<String>,Either<CheckResult,Problem>>() {
			public Either<CheckResult,Problem> run(Maybe<String> min)
			{
				if (min.isNothing()) {
					return Either.Left(null);
				}
				String in = min.getJust();
				if (in.trim().length() == 0) return Either.Right(null);

				try {
					CksTextTokenizer st = new CksTextTokenizer(new StringReader(in));
					TModule module = TypeParser.parse(st);

					if (Thread.interrupted()) return null;

					Maybe<List<Problem>> resolutionProblems = TypeResolver.run(TOpaque.StandardContext, module);
					if (resolutionProblems.isJust()) {
						return Either.Right(resolutionProblems.getJust().get(0));
					}

					TDef def = module.defs.values().iterator().next();
					if (!def.getKind().equals(TKind.Base)) {
						return Either.Left(new CheckResult(null, "Can't check against \"" + def.name + "\".  It requires type arguments."));
					}

					// Create a reference to the type they requested.
					TExpr expr = new TExpr(new TExpr.Ref.Direct(new SourcePos(1, 1), def));

					return Either.Left(new CheckResult(expr, null));
				}
				catch (ProblemException ex) {
					return Either.Right(ex.problems.get(0));
				}
				catch (IOException ex) {
					AssertionError ae = new AssertionError("IOException from StringReader?");
					ae.initCause(ex);
					throw ae;
				}
			}
		});

		DValue<Pair<Either<CheckResult,Problem>,Maybe<String>>> valueCheckInputV = DValue.Pair(typeCheckV, valueInputV);

		DValue<Either<ValueResult,Problem>> valueCheckV = DValue.Func(valueCheckInputV, new Func<Pair<Either<CheckResult,Problem>,Maybe<String>>,Either<ValueResult,Problem>>() {
			@Override
			public Either<ValueResult,Problem> run(Pair<Either<CheckResult, Problem>, Maybe<String>> pin)
			{
				if (pin.right.isNothing()) return Either.Left(null);
				if (pin.left.isRight()) return Either.Left(null);

				String in = pin.right.getJust();
				CheckResult cr = pin.left.getLeft();

				if (cr == null) return Either.Left(null);
				if (in.trim().length() == 0) return Either.Right(null);

				if (cr.error != null) return Either.Left(new ValueResult(0, cr.error));
				TExpr expr = cr.expr;
				assert expr != null;

				Value parsedValue;
				try {
					parsedValue = SurfaceParser.parse(new CksTextTokenizer(new StringReader(in)));
				}
				catch (ProblemException ex) {
					return Either.Right(ex.problems.get(0));
				}
				catch (IOException ex) {
					AssertionError ae = new AssertionError("IOException from StringReader?");
					ae.initCause(ex);
					throw ae;
				}

				if (Thread.currentThread().isInterrupted()) return null;

				GValue value;
				try {
					value = GSurfaceConverter.read(expr, parsedValue);
				}
				catch (ProblemException ex) {
					return Either.Right(ex.problems.get(0));
				}

				if (Thread.currentThread().isInterrupted()) return null;

				ByteArrayOutputStream bout = new ByteArrayOutputStream();
				try {
					value.writeBinary(bout);
				}
				catch (IOException ex) {
					AssertionError ae = new AssertionError("IOException from StringReader?");
					ae.initCause(ex);
					throw ae;
				}
				return Either.Left(new ValueResult(bout.size(), null));
			}
		});

		typeCheckV.addObserver(new DValue.Observer<Either<CheckResult, Problem>>()
		{
			public void changed(Either<CheckResult,Problem> newValue)
			{
				if (newValue.isRight()) {
					Problem p = newValue.getRight();
					if (p != null) {
						typeFeedback.setForeground(config.messageError);
						setText(typeFeedback, p.toString());
					} else {
						setText(typeFeedback, "");
					}
				}
				else {
					CheckResult cr = newValue.getLeft();
					if (cr == null) {
						typeFeedback.setForeground(config.messageProcessing);
					} else {
						typeFeedback.setForeground(config.messageSuccess);
						setText(typeFeedback, "ok");
					}
				}
			}
		});

		valueCheckV.addObserver(new DValue.Observer<Either<ValueResult, Problem>>()
		{
			public void changed(Either<ValueResult, Problem> newValue)
			{
				if (newValue.isRight()) {
					Problem p = newValue.getRight();
					if (p != null) {
						valueFeedback.setForeground(config.messageError);
						setText(valueFeedback, p.toString());
					} else {
						setText(valueFeedback, "");
					}
				}
				else {
					ValueResult vr = newValue.getLeft();
					if (vr == null) {
						valueFeedback.setForeground(config.messageProcessing);
					}
					else if (vr.error != null) {
						valueFeedback.setForeground(config.messageError);
						setText(valueFeedback, vr.error);
					}
					else {
						valueFeedback.setForeground(config.messageSuccess);
						setText(valueFeedback, "Binary Size: " + vr.binarySize + " bytes");
					}
				}
			}
		});

		typeEditor.textArea.setText(initialType);
		valueEditor.textArea.setText(initialValue);
	}

	private static final class Editor
	{
		public final JTextArea textArea;
		public final JScrollPane scrollPane;
		public final TextLineNumber lineNumbers;

		private Editor()
		{
			this.textArea = new JTextArea();
			this.textArea.setTabSize(2);
			this.scrollPane = new JScrollPane(textArea);
			this.lineNumbers = new TextLineNumber(this.textArea, 2);
			scrollPane.setRowHeaderView(lineNumbers);
			scrollPane.setBorder(BorderFactory.createEmptyBorder());
			scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		}
	}

	public static Editor makeEditor(Config config)
	{
		Editor editor = new Editor();
		editor.textArea.setBorder(BorderFactory.createLineBorder(config.editorBorder));
		editor.textArea.setBackground(config.editorBackground);
		editor.textArea.setForeground(config.editorForeground);
		Font existingFont = editor.textArea.getFont();
		editor.textArea.setFont(new Font(config.editorFontFamily, Font.PLAIN, existingFont.getSize()));

		editor.lineNumbers.setBorderGap(0);
		editor.lineNumbers.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 4));
		editor.lineNumbers.setBackground(config.editorBackground);
		editor.lineNumbers.setForeground(config.editorForeground);
		editor.lineNumbers.setCurrentLineForeground(config.editorForeground);

		return editor;
	}


	private static final class DocumentDValue extends DValue.Delayed<String>
	{
		private final Document d;

		public DocumentDValue(Document d, int delay)
		{
			super(Maybe.Just(getText(d)), delay);
			this.d = d;

			d.addDocumentListener(new DocumentListener() {
				public void insertUpdate(DocumentEvent e) { DocumentDValue.this.changeStarted(); }
				public void removeUpdate(DocumentEvent e) { DocumentDValue.this.changeStarted(); }
				public void changedUpdate(DocumentEvent e) { DocumentDValue.this.changeStarted(); }
			});
		}

		public String pullCurrent()
		{
			return getText(d);
		}
	}

	private static final class ValueResult
	{
		public int binarySize;
		public String error;

		private ValueResult(int binarySize, String error)
		{
			this.binarySize = binarySize;
			this.error = error;
		}
	}

	private static void setText(JTextArea label, String text)
	{
		label.setText(text);
		label.setRows(3);
	}

	private static String getText(Document d)
	{
		try {
			return d.getText(0, d.getLength());
		} catch (BadLocationException ex) {
			AssertionError ae = new AssertionError("wtf");
			ae.initCause(ex);
			throw ae;
		}
	}

	public static Config getDefaultConfig()
	{
		Color background = Color.WHITE;
		Color editorBorder = new Color(0xbe, 0xd5, 0xe6);
		Color editorForeground = new Color(0x08, 0x28, 0x50);
		Color editorBackground = Color.WHITE;

		Color messageBackground = editorBackground;
		Color messageError = new Color(0x48, 0x14, 0x28);
		Color messageSuccess = Color.BLACK;
		Color messageProcessing = new Color(0x80, 0x80, 0x80);

		Color labelBackground = Color.WHITE;
		Color labelForeground = Color.BLACK;

		String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
		String editorFont;
		HashSet<String> fontSet = new HashSet<String>();
		for (String font : fonts) {
			fontSet.add(font.toLowerCase());
		}
		if (fontSet.contains("consolas")) {
			editorFont = "consolas";
		}
		else if (fontSet.contains("bitstream vera sans mono")) {
			editorFont = "bitstream vera sans mono";
		}
		else if (fontSet.contains("andale mono")) {
			editorFont = "andale mono";
		}
		else {
			editorFont = Font.MONOSPACED;
		}

		return new Config(background, editorBorder, editorFont, editorForeground, editorBackground,
			              messageBackground, messageError, messageSuccess, messageProcessing,
		                  labelBackground, labelForeground);
	}

	public static void main(String[] args)
	{
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch (Exception ex) {
			ex.printStackTrace(System.err);
			return;
		}

		String type;
		String value;

		if (args.length == 0) {
			type = "";
			value = "";
		}
		else if (args.length == 1) {
			type = slurpFile(args[0]);
			value = "";
		}
		else if (args.length == 2) {
			type = slurpFile(args[0]);
			value = slurpFile(args[1]);
		}
		else {
			System.err.println("Too many arguments.  Expecting zero, one, or two");
			System.exit(1); throw new AssertionError("unreachable");
		}

		Workbench workbench = new Workbench(getDefaultConfig(), type, value);
		JFrame frame = new JFrame("CKS Workbench");
		frame.setSize(640, 480);
		frame.setContentPane(workbench);
		frame.setVisible(true);
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	}

	private static String slurpFile(String fileName)
	{
		try {
			FileReader in = new FileReader(fileName);
			try {
				StringBuilder buf = new StringBuilder();
				while (true) {
					int c = in.read();
					if (c == -1) {
						return buf.toString();
					}
					buf.append((char) c);
				}
			}
			finally {
				in.close();
			}
		}
		catch (IOException ex) {
			System.err.println("Error reading file \"" + fileName + "\": " + ex.getMessage());
			System.exit(1); return null;
		}
	}

	public static class Config
	{
		public final Color background;
		public final Color editorBorder;
		public final String editorFontFamily;
		public final Color editorForeground;
		public final Color editorBackground;
		public final Color messageBackground;
		public final Color messageError;
		public final Color messageSuccess;
		public final Color messageProcessing;
		public final Color labelBackground;
		public final Color labelForeground;

		public Config(Color background, Color editorBorder, String editorFontFamily, Color editorForeground, Color editorBackground, Color messageBackground, Color messageError, Color messageSuccess, Color messageProcessing, Color labelBackground, Color labelForeground)
		{
			this.background = background;
			this.editorBorder = editorBorder;
			this.editorFontFamily = editorFontFamily;
			this.editorForeground = editorForeground;
			this.editorBackground = editorBackground;
			this.messageBackground = messageBackground;
			this.messageError = messageError;
			this.messageSuccess = messageSuccess;
			this.messageProcessing = messageProcessing;
			this.labelBackground = labelBackground;
			this.labelForeground = labelForeground;
		}
	}
}
