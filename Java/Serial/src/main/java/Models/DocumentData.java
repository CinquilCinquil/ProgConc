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

    // TODO: finish converting this part of the code

    public DocumentData(String filepath, Query query) {

        this.name = filepath;

        try (PDDocument document = PDDocument.load(new File(filepath))) {
            PDFTextStripper pdfStripper = new PDFTextStripper();
            var my_text = pdfStripper.getText(document);

            this.n_tokens = get_tokenizer().countTokens();
            this.token_freq = new ArrayList<Integer>();
            for (String token : query.get_tokens()) {
                this.token_freq.add(get_token_frequency(token));
            }

            System.out.println("Successfully read the file " + filepath);

        } catch (IOException e) {
            System.out.println("COULD NOT OPEN: " + filepath);
            e.printStackTrace();
        }

    }

    private StringTokenizer get_tokenizer() {
        return new StringTokenizer(my_text, DEFAULT_SEPARATION);
    }

    public int get_n_tokens() {
       return n_tokens;
    }

    private int get_token_frequency(String token) {

        int total = 0;
        StringTokenizer st = get_tokenizer();

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

    public boolean has_token(String token) {

        StringTokenizer st = get_tokenizer();

        while (st.hasMoreTokens()) {
            if (st.nextToken().equalsIgnoreCase(token)) {
                return true;
            }
        }

        return false;

    }

    public String get_name() {
        return this.name;
    }

}