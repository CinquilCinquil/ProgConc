import Models.BM25;
import Models.Document;
import Models.Query;

import java.io.File;
import java.util.ArrayList;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    static public void main(String args[]) {

        ArrayList<Document> document_list = new ArrayList<Document>();
        String path = "../../data/pdfs/";
        File directory = new File(path);
        File[] files = directory.listFiles();

        if (files != null) {
            for (File file : files) {
                document_list.add(new Document(path + file.getName()));
            }
        }

        System.out.println("Finished reading docs. Now calculating score...");

        BM25 bm25 = new BM25(document_list);
        String most_relevant_doc_name = bm25.get_most_relevant_doc(new Query("sex"));

        System.out.println("Most relevant doc: " + most_relevant_doc_name);

    }

}