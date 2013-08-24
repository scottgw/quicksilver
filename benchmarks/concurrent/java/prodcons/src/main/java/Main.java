package ch.ethz.se.concbench.prodcons;

import java.util.Vector;

public class Main {
    static int maxIters;
    static int numEach;
    static Integer last = 0;
    private static class Consumer implements Runnable {
	private SharedQueue<Integer> queue;
		
	public Consumer (SharedQueue<Integer> queue) {
	    this.queue = queue;
	}
		
	public void run () {

	    for (int i = 0; i < maxIters; i++) {
		try {
		    last = queue.dequeue();
		} catch (InterruptedException e) {
		    // TODO Auto-generated catch block
		    e.printStackTrace();
		}
	    }
	}
    }
	
    private static class Producer implements Runnable {
	private SharedQueue<Integer> queue;
		
	public Producer (SharedQueue<Integer> queue) {
	    this.queue = queue;
	}
		
	public void run () {
	    for (int i = 0; i < maxIters; i++) {
		queue.enqueue(i);
	    }
	}
    }
	
    public static void main (String args[]) {
        maxIters = Integer.parseInt(args[0]);
	numEach = Integer.parseInt(args[1]);

	long start = System.nanoTime();
	Vector <Thread> threads = new Vector<Thread> ();
	SharedQueue <Integer> queue = new SharedQueue <Integer> (); 

	for (int i = 0; i < numEach; i++) 
	{
	    Thread t = new Thread (new Producer (queue));
	    t.start();
	    threads.add(t);
	}

	for (int i = 0; i < numEach; i++) {
	    Thread t = new Thread (new Consumer (queue));
	    t.start();
	    threads.add(t);
	}

	for (Thread t : threads) {
	    try {
		t.join();
	    } catch (InterruptedException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	    }
	}
	long end = System.nanoTime();
	long duration = end - start;

	System.out.println("" + last);
	System.out.println("" + ((double)duration / 1000000000.0));
    }
}
