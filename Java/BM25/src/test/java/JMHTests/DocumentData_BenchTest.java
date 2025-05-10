package JMHTests;

import JCStressTests.DocumentData_JCStressTest;
import Models.DocumentData;
import Models.Query;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 3)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.SECONDS)
@BenchmarkMode(Mode.Throughput)
public class DocumentData_BenchTest {

    public static int e4 = 10000, e3 = 1000, e2 = 100, e1 = 10;
    public static Query query;
    public static DocumentData doc;
    public static DocumentDataCopy doc_copy;
    public static String longText;
    public static Random random;
    public final static String test_dir = "../../data/tests/";
    public final static String name1 = test_dir + "CUDA Thread-Indexing Cheatsheet.pdf";

    public static class DocumentDataCopy extends DocumentData {

        public CounterCopy counter;

        public DocumentDataCopy() {
            super();
            counter = new CounterCopy();
        }

        public static class CounterCopy extends Counter {
            public CounterCopy() {
                super(new CountDownLatch(0));
            }
        }
    }

    @Setup
    public void setup() {
        query = new Query("cool example");
        doc_copy = new DocumentDataCopy();
        random = new Random();
        try {
            doc = new DocumentData(name1, query);
            PDDocument document = PDDocument.load(new File(name1));
            longText = (new PDFTextStripper()).getText(document);
            document.close();
        } catch (IOException e) {
            System.out.println("JMH_Test docs IOException (id3). This should not happen.");
        }
    }

    @Benchmark
    @Measurement(iterations = 4)
    public void test_constructor(Blackhole bh) {
        for (int i = 0; i < e3; i++) {
            try {
                DocumentData doc = new DocumentData(name1, query);
                bh.consume(doc);
            } catch (Exception e) {
                System.out.println("JMH_Test docs IOException (id2). This should not happen.");
            }
        }
    }

    @Benchmark
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    public void test_get_tokenizers(Blackhole bh) {
        for (int i = 0; i < e3; i++) {
            ArrayList<StringTokenizer> tokenizer = doc.get_tokenizers(longText);
            bh.consume(tokenizer);
        }
    }

    @Benchmark
    public void test_get_token_frequency(Blackhole bh) {
        for (int i = 0; i < e2; i++) {
            ArrayList<StringTokenizer> sts = doc.get_tokenizers(longText);
            double freq = doc.get_token_frequency("very cool example", sts);
            bh.consume(freq);
        }
    }

    @Benchmark
    public void test_cleanString(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            String s = Double.toString(random.nextDouble());
            s = DocumentData.cleanString(s);
            bh.consume(s);
        }
    }

    @Benchmark
    public void test_counter_spawn_thread(Blackhole bh) {
        for (int i = 0; i < e2; i++) {
            ArrayList<StringTokenizer> sts = doc.get_tokenizers(longText);
            doc_copy.counter.spawn_thread(sts, "very cool example");
            bh.consume(sts);
        }
    }
}
