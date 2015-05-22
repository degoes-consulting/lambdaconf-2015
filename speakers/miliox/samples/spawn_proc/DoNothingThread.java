public class DoNothingThread extends Thread
{
    public static void main(String[] args) {
        long t0 = System.nanoTime();
        Thread t = new DoNothingThread();
        t.start();
        try { t.join(); } catch (InterruptedException ie) {}
        long t1 = System.nanoTime();
        System.out.println("deltaTime: " + (t1 - t0));
    }
}


