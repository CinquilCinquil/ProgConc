package JMHTests;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.CountDownLatch;

import Models.BM25;
import Models.DocumentData;
import Models.Query;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;

/*
@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 3)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Mode.Throughput)
public class BM25_BenchTest {

    public static int e4 = 10000, e3 = 1000, e2 = 100, e1 = 10;
    public static Query query;
    public static BM25 bm25;
    public static BM25 bm25_heavy;
    public static BM25Copy bm25_copy;
    public static DocumentData doc;
    public static Random random;
    public final static String test_dir = "../../data/tests/";
    public final static String name1 = test_dir + "CUDA Thread-Indexing Cheatsheet.pdf";

    public static class BM25Copy extends BM25 {

        public AuctionCopy auction;

        public BM25Copy(CountDownLatch controller) {
            super(query);
            auction = new AuctionCopy(controller);
        }

        public ArrayList<DocumentData> get_doc_list() {
            return this.docs;
        }

        public class AuctionCopy extends Auction {
            public AuctionCopy(CountDownLatch controller) {
                super(controller);
            }
        }
    }

    @Setup
    public void setup() {
        query = new Query("cool example");
        bm25 = new BM25(query);
        bm25_heavy = new BM25(query);
        bm25_copy = new BM25Copy(new CountDownLatch(0));
        random = new Random();

        try {
            doc = new DocumentData(name1, query);
            bm25.add(doc);

            for (int i = 0; i < e2; i++) {
                bm25_heavy.add(doc);
                bm25_copy.add(doc);
            }

        } catch (IOException e) {
            System.out.println("JMH_Test docs IOException (id1). This should not happen.");
        }
    }

    @Benchmark
    public void test_constructor(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            BM25 bm25 = new BM25(query);
            bh.consume(bm25);
        }
    }

    @Benchmark
    public void test_score(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            double score = bm25.score(doc, query);
            bh.consume(score);
        }
    }

    @Benchmark
    public void test_add(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            BM25 bm25 = new BM25(query);
            bm25.add(doc);
            bh.consume(bm25);
        }
    }

    @Benchmark
    public void test_update_IDF(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            BM25 bm25 = new BM25(query);
            bm25.update_IDF(doc);
            bh.consume(bm25);
        }
    }

    @Benchmark
    public void test_IDF(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            double idf = bm25.IDF(0);
            bh.consume(idf);
        }
    }

    @Benchmark
    public void test_get_most_relevant_doc(Blackhole bh) {
        for (int i = 0; i < e2; i++) {
            String doc = bm25_heavy.get_most_relevant_doc();
            bm25_heavy.resetAvgdl();
            bh.consume(doc);
        }
    }

    @Benchmark
    public void test_auction_challenge_highest_bidder_doc(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            double rnd = random.nextDouble();
            bm25_copy.auction.challenge_highest_bidder(rnd, doc);
            bh.consume(rnd);
        }
    }

    @Benchmark
    public void test_auction_spawn_thread() {
        for (int i = 0; i < e2; i++) {
            bm25_copy.auction.spawn_thread(bm25_copy.get_doc_list(), query);
        }
    }
}
*/