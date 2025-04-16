package Models;

import java.io.File;
import java.io.IOException; 
import org.apache.pdfbox.pdmodel.PDDocument; 
import org.apache.pdfbox.text.PDFTextStripper;

import java.util.ArrayList;
import java.util.StringTokenizer;

import static java.lang.Math.max;

public class DocumentData {

    private String my_text, name;
    private int n_tokens;
    private final String DEFAULT_SEPARATION = " ";
    private final int block_size = 5000;
    private int n_blocks;

    class Counter {
        private int total = 0;
        public synchronized void increment() {
            total++;
        }
        public synchronized int get() {
            return total;
        }
    }

    public DocumentData(String filepath) {

        this.name = filepath;

        try (PDDocument document = PDDocument.load(new File(filepath))) {
            PDFTextStripper pdfStripper = new PDFTextStripper();
            this.my_text = pdfStripper.getText(document);

            this.n_tokens = (new StringTokenizer(this.my_text, DEFAULT_SEPARATION)).countTokens();

            this.n_blocks = max(n_tokens / block_size, 1);

            System.out.println("Successfully read the file " + filepath);

        } catch (IOException e) {
            System.out.println("KILL: " + filepath);
            e.printStackTrace();
        }

    }

    private ArrayList<StringTokenizer> get_tokenizers() {
        ArrayList<StringTokenizer> tokenizers = new ArrayList<StringTokenizer>();
        for (int i = 0; i < n_blocks; i++) {
            int block_end = i < n_blocks - 1 ? block_size * (i + 1) : my_text.length();
            String my_text_block = my_text.substring(block_size * i, block_end);
            tokenizers.add(new StringTokenizer(my_text_block, DEFAULT_SEPARATION));
        }
        return tokenizers;
    }

    public int get_n_tokens() {
       return n_tokens;
    }

    public int get_token_frequency(String token) {

        /*
            Reads each chunk of the text in a separate thread
         */

        Counter counter = new Counter();

        WorkerManager workerManager = new WorkerManager();
        ArrayList<StringTokenizer> sts = get_tokenizers();

        for (StringTokenizer st : sts) {
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    while (st.hasMoreTokens()) {
                        if (st.nextToken().equalsIgnoreCase(token)) {
                            counter.increment();
                        }
                    }
                }
            });
            t.start();
            workerManager.addWorker(t);
        }

        workerManager.wait_workers();

        return counter.get();

    }

    public boolean has_token(String token) {

        Counter counter = new Counter();

        WorkerManager workerManager = new WorkerManager();
        ArrayList<StringTokenizer> sts = get_tokenizers();

        for (StringTokenizer st : sts) {
            Thread t = new Thread(new Runnable() {
                @Override
                public void run() {
                    while (counter.get() == 0 && st.hasMoreTokens()) {
                        if (st.nextToken().equalsIgnoreCase(token)) {
                            counter.increment();
                        }
                    }
                }
            });
            t.start();
            workerManager.addWorker(t);
        }

        workerManager.wait_workers();

        return counter.get() > 0;
    }

    public String get_name() {
        return this.name;
    }

}