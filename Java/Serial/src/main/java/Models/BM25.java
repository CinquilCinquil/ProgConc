package Models;

import java.util.ArrayList;

public class BM25 {

    public double k = 1.5, b = 0.75;
    private double avgdl; // Average document length
    private Query query;
    private ArrayList<DocumentData> docs;
    private ArrayList<Integer> amount_of_documents_with_token;

    public BM25(Query query) {
        this.query = query;
        this.amount_of_documents_with_token = new ArrayList<>();
        this.docs = new ArrayList<>();

        for (int i = 0; i < query.get_length(); i++) {
            amount_of_documents_with_token.add(0);
        }
    }

    public double score(DocumentData doc, Query query) {

        double sum = 0;
        double doc_rate = (double) doc.get_n_tokens() / this.avgdl;

        for (int i = 0;i < query.get_length();i ++) {
            double freq = doc.get_token_frequency(i);
            sum += IDF(i) * freq / (freq + k*(1 + b*(-1 + doc_rate)));
        }

        return sum;
    }

    public void add(DocumentData doc) {
        this.docs.add(doc);
        this.avgdl += doc.get_n_tokens();
        update_IDF(doc);
    }

    public void update_IDF(DocumentData doc) {
        for (int i = 0;i < query.get_length();i ++) {
            if (doc.has_token(i)) {
                amount_of_documents_with_token.set(i,
                        amount_of_documents_with_token.get(i) + 1);
            }
        }
    }

    public double IDF(int i) {

        int N = docs.size();
        int nqi = amount_of_documents_with_token.get(i);

        return Math.log(1 + (N - nqi +  0.5)/(nqi + 0.5));
    }

    public String get_most_relevant_doc() {
        this.avgdl *= 1.0 / docs.size();

        DocumentData most_relevant_doc = null;
        double highest_score = Double.NEGATIVE_INFINITY;

        for (DocumentData doc : docs) {

            double scr = score(doc, query);
            if (scr >= highest_score) {
                most_relevant_doc = doc;
                highest_score = scr;
            }

        }

        return most_relevant_doc == null ? "" : most_relevant_doc.get_name();
    }

    public int size() {
        return docs.size();
    }
}