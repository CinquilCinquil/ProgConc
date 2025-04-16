import Models.BM25;
import Models.DocumentData;
import Models.Query;
import Models.WorkerManager;

import java.io.File;
import java.io.IOException;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    static public void main(String args[]) {

        String path = "../../data/subset/";
        File[] files = (new File(path)).listFiles();

        if (files != null) {

            Query query = new Query("partial function");
            BM25 bm25 = new BM25(query);

            WorkerManager workerManager = new WorkerManager();

            for (File file : files) {
                String filename = file.getName();

                Runnable runnable = new Runnable() {
                    @Override
                    public void run() {
                        try {
                            bm25.add(new DocumentData(path + filename, query));
                            System.out.println("Successfully read the file " + filename);
                        }
                        catch (IOException e) {
                            System.out.println("Could not read the file " + filename);
                        }
                    }
                };
                Thread t = Thread.ofVirtual().start(runnable);
                workerManager.addWorker(t);
            }

            workerManager.wait_workers();

            System.out.println("Processed "  + bm25.size() + " out of " + files.length + " files");
            System.out.println("Most relevant doc: " + bm25.get_most_relevant_doc());

        }
        else {
            System.out.println("No documents found");
        }
    }

}