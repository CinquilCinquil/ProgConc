package Models;

import java.util.StringTokenizer;

public class Query {

    private int n_tokens;
    private String my_text;
    private final String DEFAULT_SEPARATION = " ";

    public Query(String text) {
        this.my_text = text;
        this.n_tokens = get_tokenizer().countTokens();
    }

    private StringTokenizer get_tokenizer() {
        return new StringTokenizer(this.my_text, DEFAULT_SEPARATION);
    }

    public int get_length() {
        return n_tokens;
    }

    public String get_qi(int i) {

        StringTokenizer st = get_tokenizer();

        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            if (i <= 0) {
                return token;
            }
            i --;
        }

        System.out.println("Hey something is wrong :(");
        return null;
    }
}