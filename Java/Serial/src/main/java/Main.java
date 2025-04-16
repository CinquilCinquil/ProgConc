import Models.BM25;
import Models.DocumentData;
import Models.Query;

import java.io.File;
import java.util.ArrayList;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    static public void main(String args[]) {

        String path = "../../data/subset/";
        File[] files = (new File(path)).listFiles();

        if (files != null) {

            Query query = new Query("something");
            BM25 bm25 = new BM25(query);

            for (File file : files) {
                DocumentData doc = new DocumentData(path + file.getName(), query);
                bm25.add(doc);
            }

            System.out.println("Most relevant doc: " + bm25.get_most_relevant_doc());
        }
		else {
            System.out.println("No documents found");
        }
    }

}