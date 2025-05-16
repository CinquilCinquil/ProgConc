package JMHTests;

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import Serialv.Models.BM25;
import Serialv.Models.DocumentData;
import Serialv.Models.Query;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 3)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Mode.Throughput)
public class BM25_BenchTest {

    public static int e4 = 10000, e3 = 1000, e2 = 100, e1 = 10;
    public static Query query = new Query("cool example");
    public static BM25 bm25 = new BM25(query);
    public static BM25 bm25_heavy = new BM25(query);
    public static DocumentData doc;
    public final static String test_dir = "../../data/tests/";
    public final static String name1 = test_dir + "CUDA Thread-Indexing Cheatsheet.pdf";

    @Setup
    public void setup() {
        try {
            doc = new DocumentData(name1, query);
            bm25.add(doc);

            for (int i = 0; i < e2; i++) {
                bm25_heavy.add(doc);
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
        BM25 bm25 = new BM25(query);
        for (int i = 0; i < e4; i++) {
            bm25.add(doc);
        }
        bh.consume(bm25);
    }

    @Benchmark
    public void test_update_IDF(Blackhole bh) {
        BM25 bm25 = new BM25(query);
        for (int i = 0; i < e4; i++) {
            bm25.update_IDF(doc);
        }
        bh.consume(bm25);
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
}