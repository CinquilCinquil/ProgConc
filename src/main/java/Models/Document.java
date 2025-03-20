package Models;

import java.io.File;
import java.io.IOException; 
import org.apache.pdfbox.pdmodel.PDDocument; 
import org.apache.pdfbox.text.PDFTextStripper;

import java.util.StringTokenizer;

public class Document {

    private String my_text, name;
    private int n_tokens;
    private final String DEFAULT_SEPARATION = " ";

    public Document(String filepath) {

        this.name = filepath;

        try (PDDocument document = PDDocument.load(new File(filepath))) {
            PDFTextStripper pdfStripper = new PDFTextStripper();
            this.my_text = pdfStripper.getText(document);

            this.n_tokens = get_tokenizer().countTokens();

            System.out.println("Successfully read the file " + filepath);

        } catch (IOException e) {
            System.out.println("KILL: " + filepath);
            e.printStackTrace();
        }

    }

    private StringTokenizer get_tokenizer() {
        return new StringTokenizer(my_text, DEFAULT_SEPARATION);
    }

    public int get_n_tokens() {
       return n_tokens;
    }

    public int get_token_frequency(String token) {

        int total = 0;
        StringTokenizer st = get_tokenizer();

        while (st.hasMoreTokens()) {
            if (st.nextToken().equalsIgnoreCase(token)) {
                total ++;
            }
        }

        return total;

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