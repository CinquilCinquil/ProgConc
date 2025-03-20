package Models;

import java.util.ArrayList;

public class BM25 {

    public double k = 1.5, b = 0.75;
    private int ndocs;
    private int avgdl; // Average document length
    private ArrayList<Document> docs;

    public BM25(ArrayList<Document> docs) {
        int sum = 0;
        for (Document doc : docs) {
            sum += doc.get_n_tokens();
        }
        this.avgdl = !docs.isEmpty() ? sum/docs.size() : 1;
        this.ndocs = docs.size();
        this.docs = docs;
    }

    public double score(Document doc, Query query) {

        double sum = 0;
        double doc_rate = (double) doc.get_n_tokens() / this.avgdl;

        for (int i = 0;i < query.get_length();i ++) {
            
            String qi = query.get_qi(i); // i'th Keyword
            double freq_of_qi_in_doc = doc.get_token_frequency(qi); // Frequency of qi in D

            sum += IDF(qi) * freq_of_qi_in_doc / (freq_of_qi_in_doc + k*(1 - b*(1 + doc_rate)));

        }

        return sum;

    }

    public int get_amount_of_documents_with(String str) {
        int total = 0;
        for (Document doc : docs) {
            if (doc.has_token(str)) {
                total++;
            }
        }

        return total;
    }

    public double IDF(String qi) {

        int N = this.ndocs;
        int nqi = get_amount_of_documents_with(qi);

        return Math.log(1 + (N - nqi +  0.5)/(nqi + 0.5));
    }

    public String get_most_relevant_doc(Query query) {
        if (ndocs == 0) return "No document provided";

        Document most_relevant_doc = null;
        double highest_score = Double.NEGATIVE_INFINITY;

        for (Document doc : docs) {

            double scr = score(doc, query);
            if (scr >= highest_score) {
                most_relevant_doc = doc;
                highest_score = scr;
            }

        }

        return most_relevant_doc.get_name();
    }

}