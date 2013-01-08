package cks.workbench;

import cks.Maybe;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.concurrent.ExecutionException;

public abstract class DValue<T>
{
	private T current;
	private ArrayList<Observer<T>> observers = new ArrayList<Observer<T>>(2);

	protected DValue(T current)
	{
		this.current = current;
	}

	// -----------------------------------------------------------------------------

	public T get() { return current; }

	protected void change_(T newValue)
	{
		this.current = newValue;
		notifyObservers(newValue);
	}

	public final void addObserver(Observer<T> observer)
	{
		observers.add(observer);
	}

	protected void notifyObservers(T value)
	{
		for (Observer<T> observer : observers) {
			observer.changed(value);
		}
	}

	public static abstract class Observer<T>
	{
		public abstract void changed(T newValue);
	}

	// -----------------------------------------------------------------------------

	public static final class Basic<T> extends DValue<T>
	{
		public Basic(T current)
		{
			super(current);
		}

		public void change(T newValue)
		{
			change_(newValue);
		}
	}

	// -----------------------------------------------------------------------------

	public static abstract class Delayed<T> extends DValue<Maybe<T>>
	{
		private final Timer timer;

		public Delayed(Maybe<T> initial, int delay)
		{
			super(initial);

			timer = new Timer(0, null);
			timer.setRepeats(false);
			timer.setInitialDelay(delay);
			timer.stop();

			timer.addActionListener(new ActionListener()
			{
				public void actionPerformed(ActionEvent e)
				{
					change_(Maybe.Just(pullCurrent()));
				}
			});
		}

		public void changeStarted()
		{
			change_(Maybe.<T>Nothing());
			timer.restart();
		}

		public abstract T pullCurrent();
	}

	public static <P1, R> Func<P1,R> Func(DValue<P1> input, cakoose.util.Func<P1, R> func_)
	{
		return new Func<P1,R>(input, func_);
	}

	public static final class Func<P1, R> extends DValue<R>
	{
		private final cakoose.util.Func<P1, R> func;
		private SwingWorker<R,Object> worker;

		public Func(DValue<P1> input, cakoose.util.Func<P1, R> func_)
		{
			super(func_.run(input.get()));
			this.func = func_;
			input.addObserver(new Observer<P1>()
			{
				public void changed(final P1 newValue)
				{
					if (worker != null) {
						worker.cancel(true);
					}
					worker = new SwingWorker<R,Object>()
					{
						protected R doInBackground()
						{
							return func.run(newValue);
						}
						protected void done()
						{
							if (isCancelled()) return;
							try {
								change_(get());
							}
							catch (ExecutionException ex) {
								ex.printStackTrace(System.err);
							}
							catch (InterruptedException ex) {
								// ignore
							}
							worker = null;
						}
					};
					worker.execute();
				}
			});
		}
	}

	public static <A,B> Pair<A,B> Pair(DValue<A> a_, DValue<B> b_)
	{
		return new Pair<A,B>(a_, b_);
	}

	public static final class Pair<A,B> extends DValue<cakoose.util.Pair<A,B>>
	{
		private final DValue<A> a;
		private final DValue<B> b;

		public Pair(DValue<A> a_, DValue<B> b_)
		{
			super(cakoose.util.Pair.mk(a_.get(), b_.get()));
			this.a = a_;
			this.b = b_;

			a_.addObserver(new Observer<A>() {
				public void changed(A newValue) {
					change_(cakoose.util.Pair.mk(newValue, b.get()));
				}
			});

			b_.addObserver(new Observer<B>() {
				public void changed(B newValue) {
					change_(cakoose.util.Pair.mk(a.get(), newValue));
				}
			});
		}
	}
}
