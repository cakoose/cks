package cks.test;

import cakoose.util.exec.Exec;
import cakoose.util.text.StringUtil;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringReader;
import java.nio.charset.CharacterCodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class Processor
{
    public abstract Result convert(String input, String output);
    public abstract Result compare(String input1, String input2);
    public abstract void kill();

    public static final class Result
    {
        public final String[] out;
        public final String[] err;
        public final boolean done;

        public Result(String[] out, String[] err, boolean done)
        {
            this.out = out;
            this.err = err;
            this.done = done;
        }
    }

    public static abstract class Factory
    {
        public abstract Processor create() throws IOException;

        public static final class Persistent extends Factory
        {
            private final Command command;

            public Persistent(Command command)
            {
                this.command = command;
            }

            public Processor create() throws IOException
            {
                return new Processor.Persistent(command);
            }
        }
    }

    public static final class Persistent extends Processor
    {
        private Process process;

        private PrintWriter stdin;
        private BufferedReader stdout;
        private BufferedReader stderr;

        public Persistent(Command command)
            throws IOException
        {
            Exec exec = new Exec(command.command);
            exec.environment().putAll(command.env);
            process = exec.launch();
            stdin = new PrintWriter(new OutputStreamWriter(process.getOutputStream(), "UTF-8"));
            stdout = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF-8"));
            stderr = new BufferedReader(new InputStreamReader(process.getErrorStream(), "UTF-8"));
        }

        @Override
        public Result convert(String input, String output)
        {
            return run("convert", input, output);
        }

        @Override
        public Result compare(String input1, String input2)
        {
            return run("compare", input1, input2);
        }

        private Result run(String... input)
        {
            for (String line : input) {
                stdin.println(line);
            }
            stdin.flush();

            Collector stdoutCollector = new Collector(">", stdout);
            Collector stderrCollector = new Collector("!", stderr);

            Thread stdoutThread = new Thread(stdoutCollector);
            stdoutThread.start();
            Thread stderrThread = new Thread(stderrCollector);
            stderrThread.start();

            boolean interrupted = false;
            try {
                stderrThread.join(5000);
                stdoutThread.join(5000);
            }
            catch (InterruptedException ex) {
                interrupted = true;
            }

            boolean done = interrupted || (stdoutCollector.state != Collector.State.Ok) || (stderrCollector.state != Collector.State.Ok);

            if (stdoutCollector.ex != null) {
                stdoutCollector.ex.printStackTrace(System.err);
            }
            if (stderrCollector.ex != null) {
                stderrCollector.ex.printStackTrace(System.err);
            }

            if (done) {
                process.destroy();
            }

            return new Result(stdoutCollector.take(), stderrCollector.take(), done);
        }

        @Override
        public void kill()
        {
            stdin.close();
            process.destroy();
        }

        private static final class Collector implements Runnable
        {
            public final String prefix;
            public final BufferedReader in;
            public final List<String> list;

            public enum State { Running, Ok, Eof, Exception, }

            public State state;
            public Exception ex;

            private Collector(String prefix, BufferedReader in)
            {
                this.prefix = prefix;
                this.in = in;
                this.list = new ArrayList<String>();

                this.state = State.Ok;
            }

            public void run()
            {
                state = State.Running;
                ex = null;

                try {
                    while (true) {
                        String line = in.readLine();
                        if (line == null) {
                            state = State.Eof;
                            break;
                        }
                        if (line.equals(prefix + ".")) {
                            state = State.Ok;
                            break;
                        }
                        if (line.startsWith(prefix + " ")) {
                            list.add(line.substring(2));
                        }
                        else {
                            state = State.Exception;
                            this.ex = new Exception("Invalid input: " + StringUtil.jq(line));
                            break;
                        }
                    }
                }
                catch (IOException ex) {
                    state = State.Eof;
                }
                catch (Exception ex) {
                    ex.printStackTrace(System.err);
                    state = State.Exception;
                    this.ex = ex;
                }
            }

            public String[] take()
            {
                String[] lines = list.toArray(new String[list.size()]);
                this.list.clear();
                return lines;
            }
        }
    }

    public static abstract class Simple extends Processor
    {
        private final Command commandBase;

        public Simple(Command commandBase)
        {
            this.commandBase = commandBase;
        }

        public List<String> getConvertArgs(String input, String output)
        {
            return Arrays.asList("convert", input, output);
        }

        public List<String> getCompareArgs(String input1, String input2)
        {
            return Arrays.asList("compare", input1, input2);
        }

        @Override
        public Result convert(String input, String output)
        {
            return run(getConvertArgs(input, output));
        }

        @Override
        public Result compare(String input1, String input2)
        {
            return run(getCompareArgs(input1, input2));
        }

        private Result run(List<String> input)
        {
            Exec.StringOutputCollector stdout = new Exec.StringOutputCollector();
            Exec.StringOutputCollector stderr = new Exec.StringOutputCollector();

            try {
                Command command = commandBase.appendArgs(input);

                Exec exec = new Exec(command.command);
                exec.environment().putAll(command.env);

                Exec.Handle handle = exec.launch(Exec.EmptyInput, stdout, stderr);

                handle.waitFor(10000);
            }
            catch (InterruptedException ex) {
            }
            catch (IOException ex) {
            }
            String[] stdoutLines;
            String[] stderrLines;
            try {
                stdoutLines = splitLines(stdout.getString());
            }
            catch (CharacterCodingException ex) {
                stdoutLines = new String[0];
            }
            try {
                stderrLines = splitLines(stderr.getString());
            }
            catch (CharacterCodingException ex) {
                stderrLines = new String[0];
            }
            return new Result(stdoutLines, stderrLines, false);
        }

        private static String[] splitLines(String input)
        {
            // Silly use of BufferedReader so we get the same line break rules.
            try {
                BufferedReader in = new BufferedReader(new StringReader(input));
                ArrayList<String> list = new ArrayList<String>();
                while (true) {
                    String line = in.readLine();
                    if (line == null) break;
                    list.add(line);
                }
                return list.toArray(new String[list.size()]);
            }
            catch (IOException ex) {
                throw new AssertionError("should be impossible");
            }
        }

        @Override
        public void kill()
        {
            // No persistent process, so nothing to kill.
        }
    }
}
