package JMHTests;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.infra.Blackhole;

import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import static Atomicv.Models.Utils.create_batch;

@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 3)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Mode.Throughput)
public class Utils_BenchTest {

    public static int e4 = 10000, e3 = 1000, e2 = 100, e1 = 10;
    public static ArrayList<Integer> int_list;
    public static Integer[] int_array;
    public static Random random;

    @Setup
    public void setup() {
        random = new Random();
        int_array = new Integer[e2];
        int_list = new ArrayList<>();

        for (int i = 0; i < e2; i++) {
            int_array[i] = random.nextInt();
            int_list.add(random.nextInt());
        }
    }

    @Benchmark
    public void test_create_batch_ArrayList(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            Integer[] xs = create_batch(0, 3, int_array);
            bh.consume(xs);
        }
    }

    @Benchmark
    public void test_create_batch_Array(Blackhole bh) {
        for (int i = 0; i < e4; i++) {
            ArrayList<Integer> xs = create_batch(0, 3, int_list);
            bh.consume(xs);
        }
    }
}
