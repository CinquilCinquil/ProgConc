package Models;

import java.util.ArrayList;

public class WorkerManager {
    private ArrayList<Thread> workers;

    public WorkerManager() {
        this.workers = new ArrayList<Thread>();
    }

    public void addWorker(Thread worker) {
        this.workers.add(worker);
    }

    public boolean workers_alive() {

        while (!workers.isEmpty()) {
            for (Thread t : workers) {
                if (!t.isAlive()) {
                    workers.remove(t);
                    break;
                }
                else {
                    return true;
                }
            }
        }

        return false;
    }
}
