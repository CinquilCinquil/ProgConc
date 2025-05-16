package Serialv;

import Serialv.Models.BM25;
import Serialv.Models.DocumentData;
import Serialv.Models.Query;

import java.io.File;
import java.io.IOException;

/*
    Pdfs sourced from https://github.com/tpn/pdfs
 */

public class Main {

    public static String query_text = "partial function";
    public static String path = "../../data/subset/";
    public static int total_processed_docs = 0;

    static public void main(String args[]) {

        File[] files = (new File(path)).listFiles();

        if (files != null) {

            Query query = new Query(query_text);
            BM25 bm25 = new BM25(query);

            for (File file : files) {
                String filename = file.getName();

                try {
                    bm25.add(new DocumentData(path + filename, query));
                    System.out.println("Successfully read the file " + filename);
                }
                catch (IOException e) {
                    System.out.println("Could not read the file " + filename);
                }
            }

            System.out.println("Processed "  + bm25.size() + " out of " + files.length + " files");
            System.out.println("Most relevant doc: " + bm25.get_most_relevant_doc());
            total_processed_docs = bm25.size();
        }
		else {
            System.out.println("No documents found");
        }
    }

}