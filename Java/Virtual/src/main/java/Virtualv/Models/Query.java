package Virtualv.Models;

import java.util.ArrayList;
import java.util.StringTokenizer;

public class Query {

    private final ArrayList<String> tokens;
    private final String DEFAULT_SEPARATION = " ";

    public Query(String text) {

        StringTokenizer st = new StringTokenizer(text, DEFAULT_SEPARATION);

        tokens = new ArrayList<>();
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            tokens.add(token);
        }
    }

    public int get_length() {
        return tokens.size();
    }

    public ArrayList<String> get_tokens() {
        return tokens;
    }
}