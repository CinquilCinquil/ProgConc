package JMHTests;

import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

public class JMHTest {
    //private static final Log log = LogFactory.getLog(JMHTest.class);

    public static void main(String[] args) throws Exception {

        Options opt = new OptionsBuilder()
                .include(BM25_BenchTest.class.getSimpleName()
                        + "|" + DocumentData_BenchTest.class.getSimpleName()
                        + "|" + Query_BenchTest.class.getSimpleName()
                        + "|" + Utils_BenchTest.class.getSimpleName())
                .shouldDoGC(true)
                .jvmArgs()
                .build();

        new Runner(opt).run();
        //org.openjdk.jmh.Hybridv.Main.main(args);
    }
}
