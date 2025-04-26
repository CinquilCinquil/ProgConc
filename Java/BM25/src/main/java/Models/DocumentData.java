package Models;

import java.io.File;
import java.io.IOException; 
import org.apache.pdfbox.pdmodel.PDDocument; 
import org.apache.pdfbox.text.PDFTextStripper;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.concurrent.CountDownLatch;

import static Models.Utils.create_batch;
import static java.lang.Math.max;

public class DocumentData {

    private final static int n_threads = 40;
    private final int block_size = 5000;
    private final String DEFAULT_SEPARATION = " ";

    private final String name;
    private final int n_tokens;
    private final ArrayList<Integer> token_freq;
    private final int n_blocks;

    public DocumentData(String filepath, Query query) throws IOException {

        // Extracting text from document
        PDDocument document = PDDocument.load(new File(filepath));
        var my_text = (new PDFTextStripper()).getText(document);
        document.close();

        StringTokenizer tokenizer = new StringTokenizer(my_text, DEFAULT_SEPARATION);

        this.name = filepath;
        this.n_tokens = tokenizer.countTokens();
        this.n_blocks = max(n_tokens/block_size, 1);

        this.token_freq = new ArrayList<>();
        for (String token : query.get_tokens()) {
            this.token_freq.add(get_token_frequency(token, get_tokenizers(my_text)));
        }

    }

    /*
        Splits 'text' in chunks (of size 'block_size') and creates a StringTokenizer for each
     */
    private ArrayList<StringTokenizer> get_tokenizers(String text) {
        ArrayList<StringTokenizer> tokenizers = new ArrayList<StringTokenizer>();
        for (int i = 0; i < n_blocks; i++) {
            int block_end = i < n_blocks - 1 ? block_size * (i + 1) : text.length();
            String my_text_block = text.substring(block_size * i, block_end);
            tokenizers.add(new StringTokenizer(my_text_block, DEFAULT_SEPARATION));
        }
        return tokenizers;
    }

    public int get_token_frequency(String token, ArrayList<StringTokenizer> sts) {

        /*
            Reads each chunk of the text in a separate thread
         */

        final int actual_n_threads = Math.min(n_threads, sts.size());
        final CountDownLatch controller = new CountDownLatch(actual_n_threads);
        Counter counter = new Counter(controller);

        for (int i = 0; i < actual_n_threads; i++) {
            ArrayList<StringTokenizer> tokenizer_batch = create_batch(i, actual_n_threads, sts);
            counter.spawn_thread(tokenizer_batch, token);
        }

        try {
            controller.await();
        }
        catch (InterruptedException e) {
            System.out.println("One or more threads have been interrupted in DocumentData");
        }

        return counter.get();
    }

    public int get_token_frequency(int i) {
        return token_freq.get(i);
    }

    public boolean has_token(int i) {
        return get_token_frequency(i) > 0;
    }

    public int get_n_tokens() {
        return n_tokens;
    }

    public String get_name() {
        return this.name;
    }

    static class Counter {
        private int total = 0;
        private final CountDownLatch controller;

        public Counter(CountDownLatch controller) {
            this.controller = controller;
        }

        public synchronized void increment() {
            total++;
        }

        public synchronized int get() {
            return total;
        }

        public void spawn_thread(ArrayList<StringTokenizer> sts, String token) {
            Thread t = new Thread(() -> {
                for (StringTokenizer st : sts) {
                    while (st.hasMoreTokens())
                        if (st.nextToken().equalsIgnoreCase(token))
                            increment();
                }
                controller.countDown();
            });
            t.start();
        }
    }

}