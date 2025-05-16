package JCStressTests;

import Atomicv.Models.DocumentData;
import Atomicv.Models.Query;
import org.openjdk.jcstress.annotations.*;
import org.openjdk.jcstress.infra.results.I_Result;

import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

public class DocumentData_JCStressTest {

    public final static String test_dir = "../../data/tests/";
    public final static String file = test_dir + "CUDA Thread-Indexing Cheatsheet.pdf";
    public static Query query = new Query("cool example");
    public static final Random random = new Random();

    @State
    public static class DocumentDataState extends DocumentData {

        public CounterState counterState;

        public DocumentDataState() {
            super();
            counterState = new CounterState();
        }

        public static class CounterState extends Counter {
            public CounterState() {
                super(new CountDownLatch(2));
            }

            public CountDownLatch myController() {
                return this.controller;
            }
        }
    }

    @JCStressTest
    @Description("Test for the DocumentData 'get_token_frequency' method")
    @Outcome(id="1", expect = Expect.ACCEPTABLE)
    public static class DocumentData_get_token_frequency_StressTest {

        public static AtomicInteger increment_amount1 = new AtomicInteger(Math.abs(random.nextInt() % 10));
        public static AtomicInteger increment_amount2 = new AtomicInteger(Math.abs(random.nextInt() % 10));

        @Actor
        public void actor1(DocumentDataState myState) {
            for (int i = 0; i < increment_amount1.get(); i++) {
                myState.counterState.increment();
                myState.counterState.myController().countDown();
            }
        }
        @Actor
        public void actor2(DocumentDataState myState) {
            for (int i = 0; i < increment_amount2.get(); i++) {
                myState.counterState.increment();
                myState.counterState.myController().countDown();
            }
        }

        @Arbiter
        public void arbiter(DocumentDataState myState, I_Result r) {
            int actualAmount = increment_amount1.get() + increment_amount2.get();
            r.r1 = myState.counterState.get() == actualAmount ? 1 : 0;
        }
    }

}