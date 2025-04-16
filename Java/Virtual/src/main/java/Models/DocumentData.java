package Models;

import java.io.File;
import java.io.IOException; 
import org.apache.pdfbox.pdmodel.PDDocument; 
import org.apache.pdfbox.text.PDFTextStripper;

import java.util.ArrayList;
import java.util.StringTokenizer;

import static java.lang.Math.max;

public class DocumentData {

    private String name;
    private int n_tokens;
    private ArrayList<Integer> token_freq;
    private final String DEFAULT_SEPARATION = " ";
    private final int block_size = 5000;
    private int n_blocks;

    public DocumentData(String filepath, Query query) throws IOException {

        this.name = filepath;

        PDDocument document = PDDocument.load(new File(filepath));
        var my_text = (new PDFTextStripper()).getText(document);
        document.close();
        StringTokenizer tokenizer = new StringTokenizer(my_text, DEFAULT_SEPARATION);

        this.n_tokens = tokenizer.countTokens();

        this.n_blocks = max(n_tokens/block_size, 1);

        this.token_freq = new ArrayList<>();
        for (String token : query.get_tokens()) {
            this.token_freq.add(get_token_frequency(token, get_tokenizers(my_text)));
        }

    }

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

        class Counter {
            private int total = 0;
            public synchronized void increment() {
                total++;
            }
            public synchronized int get() {
                return total;
            }
        }

        Counter counter = new Counter();

        WorkerManager workerManager = new WorkerManager();

        for (StringTokenizer st : sts) {
            Runnable runnable = new Runnable() {
                @Override
                public void run() {
                    while (st.hasMoreTokens()) {
                        if (st.nextToken().equalsIgnoreCase(token)) {
                            counter.increment();
                        }
                    }
                }
            };
            Thread t = Thread.ofVirtual().start(runnable);
            workerManager.addWorker(t);
        }

        workerManager.wait_workers();

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

}