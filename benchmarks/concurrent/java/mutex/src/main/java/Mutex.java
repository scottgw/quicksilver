package ch.ethz.se.concbench.mutex;

import java.util.concurrent.locks.ReentrantLock;
import java.util.Vector;

public class Mutex {
    static int x = 0;
    static final Object lock = new Object();
    static int maxIters;

    static class Worker implements Runnable {
        public Worker() {
        }

        public void run() {
            for (int i = 0; i < maxIters; i++) {
		synchronized (lock)
		    { 
			x = (x * 3 + 7) / 15 + x;
		    }
            }
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        maxIters = Integer.parseInt (args[0]);
        int n = Integer.parseInt(args[1]);

	long start = System.nanoTime();
        Vector<Thread> workers = new Vector<Thread>();

        for (int i = 0; i < n; i++) {
            Thread worker = new Thread(new Worker());
            workers.add(worker);
            worker.start();
        }

        try {
            for (Thread t : workers) {
                t.join();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
	long end = System.nanoTime();
	long duration = end - start;

	System.out.println("" + ((double)duration / 1000000000.0));
    }

}
