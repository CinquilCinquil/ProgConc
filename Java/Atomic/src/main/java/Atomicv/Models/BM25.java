package Atomicv.Models;

import java.util.ArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

import static Atomicv.Models.Utils.create_batch;

public class BM25 {

    public double k = 1.5, b = 0.75; // BM25 parameters
    public static int n_threads = 40;

    private double avgdl; // Average document length
    private final Query query;
    protected final ArrayList<DocumentData> docs;
    private final ArrayList<Integer> amount_of_documents_with_token;

    public BM25(Query query) {
        this.query = query;
        this.docs = new ArrayList<>();

        this.amount_of_documents_with_token = new ArrayList<>();
        for (int i = 0; i < query.get_length(); i++) {
            amount_of_documents_with_token.add(0);
        }
    }

    public double score(DocumentData doc, Query query) {

        double sum = 0;
        double doc_rate = (double) doc.get_n_tokens() / this.avgdl;

        for (int i = 0;i < query.get_length();i ++) {
            double freq = doc.get_token_frequency(i);
            sum += IDF(i) * freq / (freq + k*(1 + b*(-1 + doc_rate)));
        }

        return sum;
    }

    public synchronized void add(DocumentData doc) {
        this.docs.add(doc);
        this.avgdl += doc.get_n_tokens();
        update_IDF(doc);
    }

    public void update_IDF(DocumentData doc) {
        for (int i = 0;i < query.get_length();i ++)
            if (doc.has_token(i))
                amount_of_documents_with_token.set(i, amount_of_documents_with_token.get(i) + 1);
    }

    public double IDF(int i) {

        int N = docs.size();
        int nqi = amount_of_documents_with_token.get(i);

        return Math.log(1 + (N - nqi +  0.5)/(nqi + 0.5));
    }

    public String get_most_relevant_doc() {

        /*
            Calculates the score for each doc on a separate thread
         */

        this.avgdl *= 1.0 / docs.size();

        final int actual_n_threads = Math.min(n_threads, docs.size());
        final CountDownLatch controller = new CountDownLatch(actual_n_threads);
        Auction auction = new Auction(controller);

        for (int i = 0; i < actual_n_threads; i++) {
            ArrayList<DocumentData> doc_batch = create_batch(i, actual_n_threads, docs);
            auction.spawn_thread(doc_batch, query);
        }

        try {
            controller.await();
        }
        catch (InterruptedException e) {
            System.out.println("One or more threads have been interrupted in BM25");
        }

        DocumentData most_relevant_doc = auction.get_highest_bidder();
        return most_relevant_doc == null ? "" : most_relevant_doc.get_name();
    }

    public int size() {
        return docs.size();
    }

    // Method Necessary for JMH tests
    public void resetAvgdl() {
        this.avgdl = 2 * docs.size();
    }

    public class Auction {
        public AtomicReference<DocumentData> highest_bidder;
        protected final AtomicReference<Double> highest_bidder_score;
        protected final CountDownLatch controller;

        public Auction(CountDownLatch controller) {
            this.controller = controller;
            this.highest_bidder = new AtomicReference<>(null);
            this.highest_bidder_score = new AtomicReference<>(Double.NEGATIVE_INFINITY);
        }

        public void challenge_highest_bidder(Double bid, DocumentData doc) {
            if (bid >= highest_bidder_score.get()) {
                highest_bidder_score.set(bid);
                highest_bidder.set(doc);
            }
        }

        public DocumentData get_highest_bidder() {
            return highest_bidder.get();
        }

        public void spawn_thread(ArrayList<DocumentData> docs, Query query) {
            Runnable runnable = () -> {
                for (DocumentData doc : docs) {
                    double scr = score(doc, query);
                    challenge_highest_bidder(scr, doc);
                }
                controller.countDown();
            };
            Thread.ofPlatform().start(runnable);
        }
    }

}