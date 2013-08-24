// package ch.ethz.se.concbench.condition;
import java.util.Vector;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Condition {

    static class Shared {
        int x;
        Lock l;
        java.util.concurrent.locks.Condition evenCond;
        java.util.concurrent.locks.Condition oddCond;
		
        Shared() {
            x = 0;
            l = new ReentrantLock();
            evenCond = l.newCondition();
            oddCond = l.newCondition();
        }

        void evenUpdate() throws InterruptedException {
            l.lock();
			
            while (!(x % 2 == 0)) {
                evenCond.await();
            }

            x++;
            oddCond.signal();
            l.unlock();
        }

        void oddUpdate() throws InterruptedException {
            l.lock();
            while (!(x % 2 == 1)) {
                oddCond.await();
            }

            x++;
            evenCond.signal();
            l.unlock();
        }
    }

    static int maxIters = 5000;

    static class Consumer implements Runnable {
        Shared shared;

        Consumer(Shared s) {
            shared = s;
        }

        public void run() {
            for (int i = 0; i < maxIters; i++) {
                try {
                    shared.evenUpdate();
                } catch (InterruptedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    static class Producer implements Runnable {
        Shared shared;

        Producer(Shared s) {
            shared = s;
        }

        public void run() {
            for (int i = 0; i < maxIters; i++) {
                try {
                    shared.oddUpdate();
                } catch (InterruptedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    public static void main(String[] args) {
        maxIters = Integer.parseInt(args[0]);
        int n = Integer.parseInt(args[1]);

	long start = System.nanoTime();

        Vector<Thread> threads = new Vector<Thread>();
        Shared s = new Shared();

        for (int i = 0; i < n; i++) {
            Thread t = new Thread(new Consumer(s));
            threads.add(t);
            t.start();

            t = new Thread(new Producer(s));
            threads.add(t);
            t.start();
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

	System.out.println("" + ((double)duration / 1000000000.0));
    }

}
