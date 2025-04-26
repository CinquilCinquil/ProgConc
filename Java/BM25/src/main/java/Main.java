import Models.BM25;
import Models.DocumentData;
import Models.Query;
import Models.WorkerManager;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    static public void main(String args[]) {

        System.setErr(new PrintStream(OutputStream.nullOutputStream())); //TODO: Remove

        String path = "../../data/subset/";
        File[] files = (new File(path)).listFiles();

        if (files != null) {

            Query query = new Query("partial function");
            BM25 bm25 = new BM25(query);

            WorkerManager workerManager = new WorkerManager();
            final int n_threads = 40;

            for (int i = 0; i < n_threads; i++) {

                int files_per_thread = files.length/n_threads;
                int upper_limit = i == n_threads - 1 ? files.length : (i + 1) * files_per_thread;
                File[] file_batch = Arrays.copyOfRange(
                        files, i * files_per_thread, upper_limit);

                Thread t = spawn_thread(file_batch, path, query, bm25);
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

    private static Thread spawn_thread(File[] files, String path, Query query, BM25 bm25) {

        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                for (File file : files) {
                    String filename = file.getName();
                    try {
                        bm25.add(new DocumentData(path + filename, query));
                        System.out.println("Successfully read the file " + filename);
                    } catch (IOException e) {
                        System.out.println("Could not read the file " + filename);
                    }
                }
            }
        };

        return Thread.ofVirtual().start(runnable);
    }

}