package Hybridv;

import Hybridv.Models.BM25;
import Hybridv.Models.DocumentData;
import Hybridv.Models.Query;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.concurrent.CountDownLatch;

import static Hybridv.Models.Utils.create_batch;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    public static int n_threads = 40;
    public static String query_text = "partial function";
    public static String path = "../../data/subset/";
    public static int total_processed_docs = 0;

    static public void main(String[] args) {

        System.setErr(new PrintStream(OutputStream.nullOutputStream())); //TODO: Remove

        File[] files = (new File(path)).listFiles();

        if (files != null) {

            Query query = new Query(query_text);
            BM25 bm25 = new BM25(query);

            final int actual_n_threads = Math.min(n_threads, files.length);
            CountDownLatch controller = new CountDownLatch(actual_n_threads);

            for (int i = 0; i < actual_n_threads; i++) {
                File[] file_batch = create_batch(i, actual_n_threads, files);
                spawn_thread(file_batch, path, query, bm25, controller);
            }

            try {
                controller.await();
            }
            catch (InterruptedException e) {
                System.out.println("One or more threads have been interrupted in Hybridv.Main");
            }

            System.out.println("Processed "  + bm25.size() + " out of " + files.length + " files");
            System.out.println("Most relevant doc: " + bm25.get_most_relevant_doc());
            total_processed_docs = bm25.size();
        }
        else {
            System.out.println("No documents found");
        }
    }

    /*
        Processes the content of 'files' on a separate thread
     */
    private static void spawn_thread(
            File[] files, String path, Query query, BM25 bm25, CountDownLatch controller) {

        Runnable runnable = () -> {
            for (File file : files) {
                String filename = file.getName();
                try {
                    bm25.add(new DocumentData(path + filename, query));
                    System.out.println("Successfully read the file " + filename);
                } catch (IOException e) {
                    System.out.println("Could not read the file " + filename);
                }
            }
            controller.countDown();
        };

        Thread.ofVirtual().start(runnable);
    }

}