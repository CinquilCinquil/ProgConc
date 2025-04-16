package Models;

import java.io.File;
import java.io.IOException; 
import org.apache.pdfbox.pdmodel.PDDocument; 
import org.apache.pdfbox.text.PDFTextStripper;

import java.util.ArrayList;
import java.util.StringTokenizer;

public class DocumentData {

    private String name;
    private int n_tokens;
    private ArrayList<Integer> token_freq;
    private final String DEFAULT_SEPARATION = " ";

    public DocumentData(String filepath, Query query) throws IOException {

        this.name = filepath;

        PDDocument document = PDDocument.load(new File(filepath));
        var my_text = (new PDFTextStripper()).getText(document);
        StringTokenizer tokenizer = new StringTokenizer(my_text, DEFAULT_SEPARATION);

        this.n_tokens = tokenizer.countTokens();
        this.token_freq = new ArrayList<>();
        for (String token : query.get_tokens()) {
            this.token_freq.add(get_token_frequency(token, tokenizer));
        }

    }

    private int get_token_frequency(String token, StringTokenizer st) {

        int total = 0;

        while (st.hasMoreTokens()) {
            if (st.nextToken().equalsIgnoreCase(token)) {
                total ++;
            }
        }

        return total;

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