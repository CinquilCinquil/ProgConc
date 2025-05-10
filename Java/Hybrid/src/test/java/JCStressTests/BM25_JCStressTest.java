package JCStressTests;

import Models.BM25;
import Models.DocumentData;
import Models.Query;
import org.openjdk.jcstress.annotations.*;
import org.openjdk.jcstress.infra.results.II_Result;
import org.openjdk.jcstress.infra.results.III_Result;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicReference;

public class BM25_JCStressTest {

    public final static String test_dir = "../../data/tests/";
    public final static String name1 = test_dir + "CUDA Thread-Indexing Cheatsheet.pdf";
    public final static String name2 = test_dir + "A Framework for Building Extensible C++ Class Libraries - 1993.pdf";
    public static Query query = new Query("cool example");
    public static final Random random = new Random();

    @State
    public static class BM25State extends BM25 {

        public AuctionState auctionState;

        public BM25State() {
            super(query);
            auctionState = new AuctionState();
        }

        public ArrayList<DocumentData> get_doc_list() {
            return this.docs;
        }

        public class AuctionState extends Auction {
            public AuctionState() {
                super(new CountDownLatch(2));
            }

            public double get_highest_bidder_score() {
                return this.highest_bidder_score;
            }

            public CountDownLatch myController() {
                return this.controller;
            }
        }

    }

    public static class DocumentDataStub extends DocumentData {
        public DocumentDataStub(String filename) throws IOException {
            super(filename, query);
        }
    }

    @JCStressTest
    @Description("Test for the BM25 'add' method")
    @Outcome(id="1, 2, 2", expect = Expect.ACCEPTABLE, desc = "get back doc1-doc2")
    @Outcome(id="2, 1, 2", expect = Expect.ACCEPTABLE, desc = "get back doc2-doc1")
    public static class BM25_add_StressTest {

        @Actor
        public void actor1(BM25State myState) {
            try {
                myState.add(new DocumentDataStub(name1));
            } catch(IOException e) {
                System.out.println("JCStress_Test docs IOException (id1). This should not happen.");
            }
        }
        @Actor
        public void actor2(BM25State myState) {
            try {
                myState.add(new DocumentDataStub(name2));
            } catch(IOException e) {
                System.out.println("JCStress_Test docs IOException (id2). This should not happen.");
            }
        }

        @Arbiter
        public void arbiter(BM25State myState, III_Result r) {
            ArrayList<DocumentData> docs = myState.get_doc_list();
            String firstElementName = docs.get(0).get_name();
            String secondElementName = docs.get(1).get_name();
            r.r1 = firstElementName.equals(name1) ? 1 : (firstElementName.equals(name2) ? 2 : 0);
            r.r2 = secondElementName.equals(name1) ? 1 : (secondElementName.equals(name2) ? 2 : 0);
            r.r3 = docs.size();
        }
    }

    @JCStressTest
    @Description("Test for the BM25 'challenge_highest_bidder' method")
    @Outcome(id="1, 1", expect = Expect.ACCEPTABLE, desc = "Correct")
    @Outcome(id="1, 0", expect = Expect.FORBIDDEN, desc = "Incorrect score")
    @Outcome(id="0, 1", expect = Expect.FORBIDDEN, desc = "Incorrect document name")
    @Outcome(id="0, 0", expect = Expect.FORBIDDEN, desc = "Incorrect")
    public static class BM25_challenge_highest_bidder_StressTest {

        public static AtomicReference<Double> score1 = new AtomicReference<>(random.nextDouble());
        public static AtomicReference<Double> score2 = new AtomicReference<>(random.nextDouble());

        @Actor
        public void actor1(BM25State myState) {
            try {
                myState.auctionState.challenge_highest_bidder(score1.get(), new DocumentDataStub(name1));
                myState.auctionState.myController().countDown();
            } catch(IOException e) {
                System.out.println("JCStress_Test docs IOException (id3). This should not happen.");
            }
        }
        @Actor
        public void actor2(BM25State myState) {
            try {
                myState.auctionState.challenge_highest_bidder(score2.get(), new DocumentDataStub(name2));
                myState.auctionState.myController().countDown();
            } catch(IOException e) {
                System.out.println("JCStress_Test docs IOException (id4). This should not happen.");
            }
        }

        @Arbiter
        public void arbiter(BM25State myState, II_Result r) {

            try {
                myState.auctionState.myController().await();
            }
            catch (InterruptedException e) {
                System.out.println("JCStress_Test docs IOException (id5). This should not happen.");
            }

            double actualScore1 = score1.get();
            double actualScore2 = score2.get();
            double auction_highest_bidder_score = myState.auctionState.get_highest_bidder_score();
            String auction_highest_bidder_name = myState.auctionState.get_highest_bidder().get_name();

            if (actualScore1 == actualScore2) {
                boolean condition = auction_highest_bidder_name.equals(name1) || auction_highest_bidder_name.equals(name2);
                r.r1 = condition ? 1 : 2;
                r.r2 = auction_highest_bidder_score == actualScore2 ? 1 : 2;

                return;
            }

            String highest_bidder_name;
            double highest_bidder_score;

            if (actualScore1 > actualScore2) {
                highest_bidder_name = name1;
                highest_bidder_score = actualScore1;
            }
            else {
                highest_bidder_name = name2;
                highest_bidder_score = actualScore2;
            }

            r.r1 = auction_highest_bidder_name.equals(highest_bidder_name) ? 1 : 0;
            r.r2 = auction_highest_bidder_score == highest_bidder_score ? 1 : 0;
        }
    }


}
