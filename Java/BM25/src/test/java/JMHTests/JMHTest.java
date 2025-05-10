package JMHTests;

import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

import java.util.concurrent.TimeUnit;

public class JMHTest {
    //private static final Log log = LogFactory.getLog(JMHTest.class);

    public static void main(String[] args) throws Exception {

        Options opt = new OptionsBuilder()
                .include(BM25_BenchTest.class.getSimpleName())
                .warmupIterations(3)
                .shouldDoGC(true)
                .measurementIterations(8)
                .forks(1)
                .timeUnit(TimeUnit.MILLISECONDS)
                .mode(Mode.Throughput)
                .jvmArgs()
                .build();

        new Runner(opt).run();
        //org.openjdk.jmh.Main.main(args);
    }
}
