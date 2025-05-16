package JMHTests;

import Platformv.Models.Query;
import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.TimeUnit;

@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 3)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Mode.Throughput)
public class Query_BenchTest {

    public static int e4 = 10000, e3 = 1000, e2 = 100, e1 = 10;
    public static String example_string = "very cool example";

    @Setup
    public void setup() {
    }

    @Benchmark
    public void test_constructor(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            Query q = new Query(example_string);
            bh.consume(q);
        }
    }
}
